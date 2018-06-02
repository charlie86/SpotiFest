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

load('festivals_2.RData')
load('festival_artists_spotify_2.RData')

# festivals <- filter(festivals, festival_start >= Sys.Date())
festivals <- filter(festivals, festival_dates != 'Cancelled')

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
             material_column(width = 8, align = 'center', offset = 2,
                    h5('Find music festivals based on your top artists on Spotify', style = 'text-align:center;'),
                    actionButton('go', 'Log in with Spotify')
             )
    ),
    tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
              tags$link(rel = 'stylesheet', href = 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css'),
              tags$script(src = 'https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js'),
              tags$script(src = 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js')),
    material_row(id = 'second_row',
             div(style = 'display:inline-block;', selectInput('country_select', 'Where', c('All', sort(unique(festivals$country))))),
             div(style = 'display:inline-block;', 
                 selectInput('dates', 'When', c('Next 12 months' = as.character(Sys.Date() + years(1)),
                                                'Next 6 months' = as.character(Sys.Date() + months(6)),
                                                'Next 3 months' = as.character(Sys.Date() + months(3)),
                                                'Next 30 days' = as.character(Sys.Date() + days(30))
                 )
                 )
                 ),
             p('Festival data from ', 
               a('Music Festival Wizard', href = 'https://www.musicfestivalwizard.com', target = '_blank')
             ),
             p('Artist data from ', 
               a('Spotify', href = 'https://beta.developer.spotify.com/documentation/web-api/', target = '_blank'), 
               ' pulled with ', 
               a('spotifyr', href = 'https://www.github.com/charlie86/spotifyr', target = '_blank')
             ),
             uiOutput('festivals_tbl')
             
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
        
        festivals_filtered <- unique(festivals)
        
        if (input$country_select != 'All') {
            festivals_filtered <- filter(festivals_filtered, country == input$country_select)
        }
        
        lineup_affinities <- festivals_filtered %>%
            unique %>% 
            filter(festival_start <= input$dates) %>% 
            left_join(select(festival_artists_spotify, -artist_name), by = c('festival_title' = 'festival_name')) %>% 
            inner_join(get_degrees(), by = c('spotify_artist_uri' = 'artist_uri'))
        
        festival_info <- lineup_affinities %>%
            filter(festival_start >= Sys.Date()) %>% 
            mutate(festival_url = ifelse(!is.na(festival_url), festival_url, festival_mfw_url),
                   festival_location = ifelse(country == 'United States', str_glue('{festival_location}, USA'), festival_location)) %>% 
            group_by(festival_title, festival_start, festival_location, festival_url, festival_dates, festival_image) %>%
            summarise(lineup_score = round(sum(score), 2)) %>%
            ungroup %>%
            arrange(-lineup_score) %>%
            slice(1:num_festivals)
        
        festival_top_artists <<- festival_info %>% 
            mutate(festival_rank = row_number()) %>% 
            left_join(lineup_affinities, by = 'festival_title') %>% 
            select(festival_title, festival_rank, spotify_artist_name, spotify_artist_img, degree, rank, score) %>% 
            group_by(festival_title, festival_rank, spotify_artist_name, spotify_artist_img) %>% 
            summarise(score = sum(score)) %>% 
            ungroup %>% 
            group_by(festival_title, festival_rank) %>% 
            arrange(-score) %>% 
            slice(1:10) %>% 
            ungroup
        
        if (nrow(festival_info) > 0) {
            material_card(
            # div(class = 'container',
                div(id = 'myCarousel', class = 'carousel slide', 'data-ride' = 'carousel', align = 'center',
                    tags$ol(class = 'carousel-indicators',
                            tagList(
                                map(1:10, function(x) {
                                    class <- ifelse(x == 1, 'active', '')
                                    tags$li('data-target'="#myCarousel", 'data-slide-to'=x-1, class = class, style = 'border:1px solid black;')
                                })
                            )
                    ),
                    div(class = 'carousel-inner',
                        tagList(
                            map(1:10, function(this_festival) {
                                class <- ifelse(this_festival==1, 'item active', 'item')
                                div(
                                    class = class, 
                                    h1(style = 'font-size:50px', a(paste0('#', this_festival, ' ', gsub(' 2018| Festival| Music Festival', '', festival_info$festival_title[this_festival])), href=festival_info$festival_url[this_festival], target='_blank')),
                                    p(
                                        span(festival_info$festival_location[this_festival], style='font-size:40px'),
                                        br(),
                                        span(festival_info$festival_dates[this_festival], style='font-size:40px'),
                                        br(),
                                        img(src=gsub('150x75', '300x150', festival_info$festival_image[this_festival]), align='center'),
                                        br(),
                                        br()
                                    ),
                                    div(style = 'column-count: 2;',
                                        map(1:10, function(this_artist) {
                                            top_artist_df <- festival_top_artists %>%
                                                filter(festival_rank == this_festival) %>%
                                                slice(this_artist)
                                            
                                            if (nrow(top_artist_df) > 0) {
                                                p(top_artist_df$spotify_artist_name, style = 'text-align:center; font-size:30px;')
                                            } else {
                                                p(HTML('&nbsp;'), style = 'font-size:30px;')
                                            }
                                        })
                                    )
                                )
                            })
                        )
                    ),
                    a(class = 'left carousel-control', href="#myCarousel", 'data-slide'="prev",
                      span(class = 'glyphicon glyphicon-chevron-left', style = 'color:black;'),
                      span(class = 'sr-only', 'Previous')
                    ),
                    a(class = 'right carousel-control', href="#myCarousel", 'data-slide'="next",
                      span(class = 'glyphicon glyphicon-chevron-right', style = 'color:black;'),
                      span(class = 'sr-only', 'Next')
                    )
                )
            # )
            )
            # layout_matrix <- map(1:(num_festivals/3), function(x) ((x*3)-2):(x*3))
            # material_column(width = 12,
            #                 map(1:length(layout_matrix), function(this_row) {
            #                     material_row(
            #                         map(layout_matrix[[this_row]], function(this_festival) {
            #                             if (!is.na(festival_info$festival_title[this_festival])) {
            #                                 material_column(width = 4, align = 'center',
            #                                                 material_card(
            #                                                     a(h4(gsub(' 2018| Festival| Music Festival', '', festival_info$festival_title[this_festival])), href=festival_info$festival_url[this_festival], target='_blank'),
            #                                                     p(
            #                                                         span(festival_info$festival_location[this_festival], style = 'font-size:20px'),
            #                                                         br(),
            #                                                         span(festival_info$festival_dates[this_festival], style = 'font-size:20px'), 
            #                                                         br(), 
            #                                                         img(src=festival_info$festival_image[this_festival], align='center'),
            #                                                         br(),
            #                                                         br()
            #                                                         # br(),
            #                                                         #         HTML(str_glue('
            #                                                         # <button id="go_to_website{this_row}{this_festival}" type="button" class="btn btn-default action-button mdc-button" style="display:inline-block" onclick="window.open(\'{festival_info$festival_url[this_festival]}\', \'_blank\')">
            #                                                         #     <i class="material-icons mdc-button__icon">launch</i>
            #                                                         #     Website
            #                                                         # </button>'))
            #                                                     ),
            #                                                     # h5('Lineup Score: ', span(festival_info$lineup_score[this_festival], style = 'background-color: #2ebd59; color:white; padding-left:5px; padding-right:5px;')),
            #                                                     div(style = 'column-count: 2;',
            #                                                         map(1:10, function(this_artist) {
            #                                                             top_artist_df <- festival_top_artists %>% 
            #                                                                 filter(festival_rank == this_festival) %>% 
            #                                                                 slice(this_artist)
            #                                                             
            #                                                             if (nrow(top_artist_df) > 0) {
            #                                                                 p(top_artist_df$spotify_artist_name, style = 'text-align:center;')
            #                                                             } else {
            #                                                                 p(HTML('&nbsp;'))
            #                                                             }
            #                                                         })
            #                                                     )
            #                                                 )
            #                                 )
            #                             }
            #                         })
            #                     )
            #                 })
            # )
        } else {
            h2("Sorry, we couldn't find any festivals for you with those options.", style = "color:white")
        }
    })
}

shinyApp(ui, server)