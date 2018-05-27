library(spotifyr)
library(tidyverse)
library(shinymaterial)
library(lubridate)
library(httr)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(furrr)
plan(multiprocess)

rm(list= ls())

options(shiny.port = 8100)

load('festivals.RData')
load('festival_artists_spotify.RData')

if (interactive()) {
    # testing url
    options(shiny.port = 8100)
    REDIRECT_URI <- "http://localhost:8100/"
} else {
    # deployed URL
    REDIRECT_URI <- "http://rcharlie.net/SpotiFest/"
}

auth_url <- GET('https://accounts.spotify.com/authorize',
                query = list(
                    client_id = Sys.getenv('SPOTIFY_CLIENT_ID'),
                    response_type = 'token',
                    redirect_uri = REDIRECT_URI,
                    scope = 'user-read-email user-top-read'
                )) %>% .$url

login_js <- paste0("shinyjs.login = function(callback) {
                 var width = 450,
                 height = 730,
                 left = (screen.width / 2) - (width / 2),
                 top = (screen.height / 2) - (height / 2);
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
    withSpinner(uiOutput('festivals_tbl'), color = '#1ed760', type = 7, proxy.height = '800px', size = 2)
)

server <- function(input, output, session) {
    
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
        shinyjs::hide('first_row')
        df <- future_map_dfr(1:nrow(get_top_artists()), function(i) {
            first_degree <- get_related_artists(artist_uri = get_top_artists()$artist_uri[i], use_artist_uri = TRUE) %>%
                mutate(original_artist_name = get_top_artists()$artist_name[i],
                       degree = 2) %>%
                select(artist_name, artist_uri, degree) %>%
                rbind(tibble(artist_name = get_top_artists()$artist_name[i], artist_uri = get_top_artists()$artist_uri[i], degree = 1)) %>%
                mutate(rank = i)
        }) %>% group_by(artist_name, artist_uri) %>%
            filter(degree == min(degree), rank == min(rank)) %>%
            unique %>%
            arrange(degree, rank)
    })
    
    output$festivals_tbl <- renderUI({
        num_festivals <- 8
        
        # festivals_df <<- festivals
        
        lineup_affinities <<- festivals %>%
            unique %>% 
            left_join(select(festival_artists_spotify, -artist_name), by = c('festival_title' = 'festival_name')) %>% 
            inner_join(get_degrees(), by = c('spotify_artist_uri' = 'artist_uri')) %>% 
            mutate(score = case_when(
                degree == 1 ~ 1,
                degree == 2 ~ .5,
                TRUE ~ 0
            ))
        
        festival_info <<- lineup_affinities %>%
            filter(festival_start >= Sys.Date()) %>% 
            mutate(festival_url = ifelse(!is.na(festival_url), festival_url, festival_mfw_url),
                   festival_location = ifelse(country == 'United States', str_glue('{festival_location}, USA'), festival_location)) %>% 
            group_by(festival_title, festival_start, festival_location, festival_url, festival_dates, festival_image) %>%
            summarise(lineup_affinity = sum(score)) %>%
            ungroup %>%
            arrange(-lineup_affinity) %>%
            slice(1:num_festivals)
        
        festival_top_artists <<- festival_info %>% 
            mutate(festival_rank = row_number()) %>% 
            left_join(lineup_affinities, by = 'festival_title') %>% 
            select(festival_title, festival_rank, spotify_artist_name, spotify_artist_img, degree, rank, score) %>% 
            group_by(festival_title, festival_rank) %>% 
            arrange(-score, rank) %>% 
            slice(1:10) %>% 
            ungroup
        
        map(1:2, function(this_row) {
            layout_matrix <- list(1:4, 5:8)
            material_row(
                map(layout_matrix[[this_row]], function(this_festival) {
                    material_column(width = 3, align = 'center',
                                    material_card(
                                        h4(gsub(' 2018| Festival| Music Festival', '', festival_info$festival_title[this_festival])),
                                        p(
                                            img(src=festival_info$festival_image[this_festival], align='center'), 
                                            br(),
                                            festival_info$festival_dates[this_festival], 
                                            br(), 
                                            festival_info$festival_location[this_festival],
                                            br(),
                                            
                                            HTML(str_glue('
                                                <button id="go_to_website{this_row}{this_festival}" type="button" class="btn btn-default action-button mdc-button" style="display:inline-block" onclick="window.open(\'{festival_info$festival_url[this_festival]}\', \'_blank\')">
                                                    <i class="material-icons mdc-button__icon">launch</i>
                                                    Website
                                                </button>
                                                          '))
                                            
                                            # a('Website', href=festival_info$festival_url[this_festival])
                                        ),
                                        h5("Artists you might like"),
                                        map(1:5, function(this_artist) {
                                            p(
                                                festival_top_artists %>% 
                                                    filter(festival_rank == this_festival) %>% 
                                                    slice(this_artist) %>% 
                                                    pull(spotify_artist_name)
                                            )
                                        })
                                    )
                    )
                })
            )
        })
    })
    
}

shinyApp(ui, server)