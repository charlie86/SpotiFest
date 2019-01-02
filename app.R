library(DT)
library(httr)
library(shiny)
library(furrr)
library(shinyjs)
library(spotifyr)
library(lubridate)
library(tidyverse)
library(countrycode)
library(shinymaterial)
library(shinycssloaders)

plan(multiprocess)

rm(list = ls())

load('festivals.RData')
load('festival_artists_spotify.RData')
festivals <- unique(festivals)

festival_urls <- festival_artists_spotify %>% 
    select(festival_name, festival_urls) %>% 
    unique()
festival_urls <- map_df(1:nrow(festival_urls), function(festival) {
    this_festival <- slice(festival_urls, festival) %>% 
        pull(festival_urls) %>%
        .[[1]] %>% 
        mutate(festival_name = festival_urls$festival_name[festival])
})
official_festival_urls <- festival_urls %>% 
    filter(title == 'OFFICIAL WEBSITE') %>% 
    select(festival_title = festival_name, festival_url = url)

festival_details <- festivals %>% 
    left_join(official_festival_urls, by = 'festival_title') %>% 
    mutate(country = location,
           festival_year = str_extract(festival_dates, '[[:digit:]]{4}$'),
           festival_start = str_replace_all(festival_dates, '-.*|–.*', ''),
           festival_start = str_replace_all(festival_start, ',.*', ''),
           festival_start = as.Date(paste(festival_start, festival_year), '%B %d %Y')) %>% 
    select(-festival_year)

festival_details$country[is.na(festival_details$country)] <- 'NOT FOUND'
festival_details$country[festival_details$country == 'South Korea'] <- 'Republic of Korea'
festival_details$country[festival_details$country == 'Czechia'] <- 'Czech Republic'

continent_lookup <- codelist %>% 
    select(continent, region, country.name.en) %>% 
    mutate(continent = case_when(
        continent == 'Oceania' ~ 'Australia',
        region == 'South America' ~ 'South America',
        region %in% c('Northern America', 'Central America', 'Caribbean') ~ 'North America',
        TRUE ~ continent
    ))

festival_details <- festival_details %>% 
    left_join(continent_lookup, by = c('country' = 'country.name.en'))

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
                    client_id = Sys.getenv('SPOTIFEST_CLIENT_ID'),
                    response_type = 'token',
                    redirect_uri = REDIRECT_URI,
                    scope = 'user-top-read'
                )) %>% .$url

# login_js <- paste0("shinyjs.login = function(callback) {
#                  var url = '", auth_url, "';
#                  var parseResult = new DOMParser().parseFromString(url, 'text/html');
#                  var parsedUrl = parseResult.documentElement.textContent;
#                  window.location = parsedUrl;
#         };"
# )
login_js <- str_glue("shinyjs.login = function(callback) {
                    var url = '{{auth_url}}';
                     var parseResult = new DOMParser().parseFromString(url, 'text/html');
                     var parsedUrl = parseResult.documentElement.textContent;
                     window.location = parsedUrl;
                     };", .open = '{{', .close = '}}'
)


ui <- material_page(
    useShinyjs(),
    extendShinyjs(text = login_js),
    nav_bar_color = 'black',
    font_color = 'white',
    background_color = '#828282',
    title = HTML('<span>SpotiFest</span> <span style="font-size:12px"><a href="http://www.rcharlie.com" target="_blank">by RCharlie</a></span>'),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
              includeScript('www/ga.js'),
              includeScript('www/hotjar.js')),
    material_row(
        material_column(width = 3,
                        material_card(align = 'center', depth = 5,
                                      h4(style = 'text-align:left', 'Find music festivals based on your favorite Spotify tunes'),
                                      div(id = 'inputs',
                                          br(),
                                          material_dropdown('region', 'Where', c('Anywhere', unique(festival_details$continent[!is.na(festival_details$continent)])), color = 'black'),
                                          material_dropdown('dates', 'When', c('Next 12 months' = as.character(Sys.Date() + years(1)), 
                                                                               'Next 6 months' = as.character(Sys.Date() + months(6)),
                                                                               'Next 3 months' = as.character(Sys.Date() + months(3)),
                                                                               'Next 30 days' = as.character(Sys.Date() + days(30))
                                          ), color = 'black'),
                                          p(style = 'text-align:left', 'Festival data from ',
                                            a('Music Festival Wizard', href = 'https://www.musicfestivalwizard.com', target = '_blank')
                                          ),
                                          p(style = 'text-align:left', 'Artist data from ',
                                            a('Spotify', href = 'https://beta.developer.spotify.com/documentation/web-api/', target = '_blank'),
                                            ' pulled with ',
                                            a('spotifyr', href = 'https://www.github.com/charlie86/spotifyr', target = '_blank')
                                          ),
                                          p(style = 'text-align:left', 'See code on ',
                                            a('GitHub', href = 'https://www.github.com/charlie86/SpotiFest', target = '_blank')
                                          )
                                      )
                        ),
                        material_card(id = 'login_button', depth = 5, align = 'center',
                                      actionButton('go', 'Log in with Spotify')
                        )
        ),
        material_column(width = 9,
                        withSpinner(uiOutput('festivals_tbl'), color = '#1ED760', type = 7, proxy.height = '1000px', size = 2)
        )
    )
)

server <- function(input, output, session) {
    
    hide('inputs')
    
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
    
    get_degrees <- reactive({
        req(nrow(get_top_artists()) > 0)
        hide('login_button')
        shinyjs::show('inputs')
        future_map_dfr(1:nrow(get_top_artists()), function(i) {
            related_artists <- get_related_artists(get_top_artists()$artist_uri[i])
            if (nrow(related_artists) > 0) {
                first_degree <- related_artists %>% 
                    mutate(original_artist_name = get_top_artists()$artist_name[i],
                           degree = 1) %>%
                    select(artist_name, artist_uri, degree) %>%
                    rbind(tibble(artist_name = get_top_artists()$artist_name[i], artist_uri = get_top_artists()$artist_uri[i], degree = 0)) %>%
                    mutate(rank = i)
            } else {
                first_degree <- tibble()
            }
            return(first_degree)
        }) %>% 
            group_by(artist_name, artist_uri) %>%
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
        
        if (input$region != 'Anywhere') {
            festivals_filtered <- filter(festivals_filtered, continent == input$region)
        }
        
        lineup_affinities <- festivals_filtered %>%
            unique %>% 
            filter(festival_start <= input$dates) %>%
            left_join(select(festival_artists_spotify, -c(artist_name)), by = c('festival_title' = 'festival_name')) %>% 
            inner_join(get_degrees(), by = c('spotify_artist_id' = 'artist_uri'))
        
        festival_info <- lineup_affinities %>%
            filter(festival_start >= Sys.Date()) %>%
            mutate(festival_url = ifelse(!is.na(festival_url), festival_url, festival_mfw_url),
                   festival_location = ifelse(country == 'United States of America', str_glue('{festival_location}, USA'), festival_location)) %>% 
            group_by(festival_title, festival_location, festival_url, festival_dates, festival_image, festival_poster) %>%
            summarise(lineup_score = round(sum(score), 2)) %>%
            ungroup %>%
            arrange(-lineup_score) %>%
            slice(1:num_festivals)
        
        festival_top_artists <- festival_info %>% 
            mutate(festival_rank = row_number()) %>% 
            left_join(lineup_affinities, by = 'festival_title') %>% 
            select(festival_title, festival_rank, spotify_artist_name, spotify_artist_img, spotify_artist_id, degree, rank, score) %>% 
            group_by(festival_title, festival_rank, spotify_artist_name, spotify_artist_img, spotify_artist_id) %>% 
            summarise(score = sum(score)) %>% 
            ungroup %>% 
            group_by(festival_title, festival_rank) %>% 
            arrange(-score) %>% 
            ungroup
        
        if (nrow(festival_info) > 0) {
            map(1:num_festivals, function(this_row) {
                material_row(
                    map(this_row, function(this_festival) {
                        if (!is.na(festival_info$festival_title[this_festival])) {
                            material_column(width = 12, align = 'center',
                                            material_card(style = 'height:700px', depth = 5,
                                                          div(style = 'width:50%;float:right',
                                                              a(img(class = 'lineup-image', src=coalesce(festival_info$festival_poster[this_festival], festival_info$festival_img_big[this_festival]), style = 'max-height:600px;max-width:100%;'), href = festival_info$festival_url[this_festival], target = '_blank')
                                                          ),
                                                          div(style = 'float:left;width:50%;',
                                                              h3(a(paste0(str_glue('#{this_festival} '), gsub(' 2018| Festival| Music Festival', '', festival_info$festival_title[this_festival])), href = festival_info$festival_url[this_festival], target = '_blank')),
                                                              h5(festival_info$festival_location[this_festival]),
                                                              h5(festival_info$festival_dates[this_festival]), 
                                                              h4("We think you'll like..."),
                                                              map(1:8, function(this_artist) {
                                                                  top_artist_df <- festival_top_artists %>% 
                                                                      filter(festival_rank == this_festival) %>% 
                                                                      slice(this_artist)
                                                                  if (nrow(top_artist_df) > 0) {
                                                                      spotify_url <- str_glue('https://open.spotify.com/artist/{top_artist_df$spotify_artist_id}')
                                                                      a(class = 'artist-card-text', href = spotify_url, target = '_blank', style = 'color:black',
                                                                        div(class = 'artist-card', style="max-width:125px; font-size:100%; text-align:center; display:inline-block",
                                                                            img(src=top_artist_df$spotify_artist_img, alt="alternate text", style="padding-bottom:0.5em; max-width:125px;"),
                                                                            top_artist_df$spotify_artist_name
                                                                        )
                                                                      )
                                                                  } else {
                                                                      HTML('&nbsp;')
                                                                  }
                                                              })
                                                          )
                                            )
                            )
                        }
                    })
                )
            })
        } else {
            h2("Sorry, we couldn't find any festivals for you with those options.", style = "color:white")
        }
    })
}

shinyApp(ui, server)