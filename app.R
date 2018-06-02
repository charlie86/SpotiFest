library(spotifyr)
library(tidyverse)
library(shinymaterial)
library(lubridate)
library(httr)
library(shiny)
library(shinycssloaders)
library(DT)
library(shinyjs)
library(furrr)
plan(multiprocess)

rm(list = ls())

load('festival_details.RData')
load('festival_artists_spotify_2.RData')

# festivals <- filter(festivals, festival_start >= Sys.Date())
festival_details <- filter(festival_details, festival_dates != 'Cancelled')

if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    REDIRECT_URI <- 'http://localhost:8100/'
} else {
    # deployed URL
    REDIRECT_URI <- 'http://rcharlie.net/SpotiFest/'
}

auth_url <- GET('https://accounts.spotify.com/authorize',
                query = list(
                    client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                    response_type = 'token',
                    redirect_uri = REDIRECT_URI,
                    scope = 'user-read-email user-top-read'
                )) %>% .$url

login_js <- paste0("shinyjs.login = function(callback) {
                 var url = '", auth_url, "';
                 var parseResult = new DOMParser().parseFromString(url, 'text/html');
                 var parsedUrl = parseResult.documentElement.textContent;
                 window.location = parsedUrl;
        };"
)

ui <- material_page(
    useShinyjs(),
    extendShinyjs(text = login_js),
    nav_bar_color = 'black',
    font_color = 'white',
    background_color = '#828282',
    title = HTML('<span>SpotiFest</span> <span style="font-size:12px"><a href="http://www.rcharlie.com" target="_blank">by RCharlie</a></span>'),
    material_row(id = 'first_row',
                 material_parallax('festival.jpg'),
                 material_column(width = 8, align = 'center', offset = 2,
                                 material_card(
                                     h5('Find music festivals based on your top artists on Spotify', style = 'text-align:center;'),
                                     actionButton('go', 'Log in with Spotify')
                                 )
                 )
    ),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css')),
    material_row(id = 'second_row',
                 material_column(width = 3,
                                 material_card(
                                     material_dropdown('country_select', 'Where', c('Anywhere', sort(unique(festival_details$country))), color = 'black'),
                                     material_dropdown('dates', 'When', c('Next 12 months' = as.character(Sys.Date() + years(1)), 'Next 6 months' = as.character(Sys.Date() + months(6))), color = 'black'),
                                     p('Festival data from ',
                                       a('Music Festival Wizard', href = 'https://www.musicfestivalwizard.com', target = '_blank')
                                     ),
                                     p('Artist data from ',
                                       a('Spotify', href = 'https://beta.developer.spotify.com/documentation/web-api/', target = '_blank'),
                                       ' pulled with ',
                                       a('spotifyr', href = 'https://www.github.com/charlie86/spotifyr', target = '_blank')
                                     )
                                 )
                 ),
                 material_column(width = 9,
                                 withSpinner(uiOutput('festivals_tbl'), color = '#1ED760', type = 7, proxy.height = '1000px', size = 2)
                 )
    )
)

server <- function(input, output, session) {
    
    hide('second_row')
    
    get_access_token <- reactive({
        url_hash <- getUrlHash()
        access_token <- url_hash %>% str_replace('&.*', '') %>% str_replace('.*=', '')
    })
    
    observeEvent(input$go, {
        js$login()
    })
    
    get_top_artists <- reactive({
        res <- GET('https://api.spotify.com/v1/me/top/artists/', 
                   query = list(limit = 50), 
                   add_headers(.headers = c('Authorization' = paste0('Bearer ', get_access_token())))
        ) %>% content %>% .$items
        
        map_df(res, function(x) {
            list(
                artist_name = x$name,
                artist_uri = x$id
            )
        })
    })
    
    output$top_artists_table <- renderDataTable({
        req(nrow(get_degrees()) > 0)
        get_degrees() %>% 
            filter(degree == 0) %>% 
            transmute(Artist = artist_name, Score = score) %>% 
            datatable(options = list(searching = F, scrollY = 250, scroller = T, info = F), extensions = 'Scroller', rownames = F)
    })
    
    output$related_artists_table <- renderDataTable({
        req(nrow(get_degrees()) > 0)
        get_degrees() %>% 
            filter(degree == 1) %>% 
            transmute(Artist = artist_name, Score = score) %>% 
            datatable(options = list(searching = F, scrollY = 250, scroller = T, info = F), extensions = 'Scroller', rownames = F)
    })
    
    get_degrees <- reactive({
        req(nrow(get_top_artists()) > 0)
        hide('first_row')
        show('second_row')
        future_map_dfr(1:nrow(get_top_artists()), function(i) {
            first_degree <- get_related_artists(artist_uri = get_top_artists()$artist_uri[i], use_artist_uri = TRUE) %>%
                mutate(original_artist_name = get_top_artists()$artist_name[i],
                       degree = 1) %>%
                select(artist_name, artist_uri, degree) %>%
                rbind(tibble(artist_name = get_top_artists()$artist_name[i], artist_uri = get_top_artists()$artist_uri[i], degree = 0)) %>%
                mutate(rank = i)
        }) %>% group_by(artist_name, artist_uri) %>%
            unique %>%
            arrange(degree, rank) %>% 
            ungroup %>% 
            mutate(score = round(case_when(
                degree == 0 ~ 50/rank,
                degree == 1 ~ 1/rank,
                TRUE ~ 0
            ), 2))
    })
    
    output$festivals_tbl <- renderUI({
        
        req(nrow(get_degrees()) > 0)
        num_festivals <- 100
        
        festivals_filtered <- unique(festival_details)
        
        if (input$country_select != 'Anywhere') {
            festivals_filtered <- filter(festivals_filtered, country == input$country_select)
        }
        
        lineup_affinities <- festivals_filtered %>%
            unique %>% 
            filter(festival_start <= input$dates) %>% 
            left_join(select(festival_artists_spotify, -c(artist_name, festival_url, festival_poster)), by = c('festival_title' = 'festival_name')) %>% 
            inner_join(get_degrees(), by = c('spotify_artist_uri' = 'artist_uri'))
        
        festival_info <- lineup_affinities %>%
            filter(festival_start >= Sys.Date()) %>% 
            mutate(festival_url = ifelse(!is.na(festival_url), festival_url, festival_mfw_url),
                   festival_location = ifelse(country == 'United States', str_glue('{festival_location}, USA'), festival_location)) %>% 
            group_by(festival_title, festival_start, festival_location, festival_url, festival_dates, festival_img_big, festival_poster) %>%
            summarise(lineup_score = round(sum(score), 2)) %>%
            ungroup %>%
            arrange(-lineup_score) %>%
            slice(1:num_festivals)
        
        festival_top_artists <- festival_info %>% 
            mutate(festival_rank = row_number()) %>% 
            left_join(lineup_affinities, by = 'festival_title') %>% 
            select(festival_title, festival_rank, spotify_artist_name, spotify_artist_img, degree, rank, score) %>% 
            group_by(festival_title, festival_rank, spotify_artist_name, spotify_artist_img) %>% 
            summarise(score = sum(score)) %>% 
            ungroup %>% 
            group_by(festival_title, festival_rank) %>% 
            arrange(-score) %>% 
            ungroup
        
        if (nrow(festival_info) > 0) {
            layout_matrix <- map(1:(num_festivals), function(x) x)
            material_column(width = 12,
                            map(1:length(layout_matrix), function(this_row) {
                                material_row(
                                    map(layout_matrix[[this_row]], function(this_festival) {
                                        if (!is.na(festival_info$festival_title[this_festival])) {
                                            material_column(width = 12, align = 'center',
                                                            
                                                            material_card(style = 'height:800px',
                                                                img(src=festival_info$festival_poster[this_festival], style = 'max-width:50%;float:right;max-height:750px;'),
                                                                p(style = 'float:left;',
                                                                  h2(a(paste0(str_glue('#{this_festival} '), gsub(' 2018| Festival| Music Festival', '', festival_info$festival_title[this_festival])), href = festival_info$festival_url[this_festival], target = '_blank')),
                                                                  h4(festival_info$festival_location[this_festival]),
                                                                  h4(festival_info$festival_dates[this_festival]), 
                                                                  h3("Who you'll like"),
                                                                  div(
                                                                  map(1:6, function(this_artist) {
                                                                      top_artist_df <- festival_top_artists %>% 
                                                                          filter(festival_rank == this_festival) %>% 
                                                                          slice(this_artist)
                                                                      if (nrow(top_artist_df) > 0) {
                                                                            div(style="max-width:150px; font-size:100%; text-align:center; display:inline-block",
                                                                                img(src=top_artist_df$spotify_artist_img, alt="alternate text", style="padding-bottom:0.5em; max-width:150px;"),
                                                                                top_artist_df$spotify_artist_name
                                                                            )
                                                                      } else {
                                                                          HTML('&nbsp;')
                                                                      }
                                                                  })
                                                                  )
                                                                )
                                                            )
                                                            
                                            )
                                        }
                                    })
                                )
                            })
            )
        } else {
            h2("Sorry, we couldn't find any festivals for you with those options.", style = "color:white")
        }
    })
}

shinyApp(ui, server)