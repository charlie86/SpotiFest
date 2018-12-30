library(glue)
library(httr)
library(furrr)
library(rvest)
library(ggmap)
library(stringr)
library(spotifyr)
library(tidyverse)

base_url <- 'https://www.musicfestivalwizard.com/all-festivals/'

# Find number of pages to search through ----------------------------------
num_festivals <- read_html(base_url) %>%
    html_node('.search-res') %>%
    html_text %>%
    str_extract('returned [[:digit:]]{1,} results') %>%
    parse_number

num_pages <- ceiling(num_festivals / 15)

date_pulled <- Sys.time()

plan(multiprocess)
# Loop through pages and scrape festival details and links ----------------
festivals <- future_map_dfr(1:num_pages, function(this_page) {
    
    festival_list <- read_html(paste0(base_url, 'page/', this_page)) %>%
        html_nodes('.lineup-item')
    
    future_map_dfr(1:length(festival_list), function(this_festival) {
        
        festival_image <- festival_list[[this_festival]] %>% 
            html_node('img') %>% 
            html_attr('src')
        
        festival_header <- festival_list[[this_festival]] %>% 
            html_node('.search-title')
        
        festival_title <- festival_header %>% 
            html_node('h2') %>% 
            html_text()
        
        festival_mfw_url <- festival_header %>% 
            html_node('h2>a') %>% 
            html_attr('href')
        
        festival_meta_text <- festival_header %>% 
            html_node('.search-meta') %>% 
            html_text() %>% 
            str_split('\n') %>% 
            unlist()
        
        festival_location <- str_trim(festival_meta_text[2])
        festival_dates <- str_replace_all(festival_meta_text[3], '/.*', '') %>% str_trim()
        
        list(
            festival_title = festival_title,
            festival_dates = festival_dates,
            festival_location = festival_location,
            festival_image = festival_image,
            festival_mfw_url = festival_mfw_url,
            date_pulled = date_pulled
        )
        
    }, .progress = TRUE)
    
}, .progress = TRUE)

# Loop through festival pages to scrape lineups ---------------------------
festival_artists <- future_map_dfr(1:nrow(festivals), function(this_festival) {
    
    festival_html <- read_html(festivals$festival_mfw_url[this_festival])
    
    artists <- festival_html %>%
        html_node('.hublineup') %>%
        html_nodes('li') %>% 
        html_text()
    
    festival_urls <- festival_html %>%
        html_node('.websitebuttonfree') %>%
        html_nodes('a') %>% 
        map_df(function(x) list(title = html_text(x), url = html_attr(x, 'href')))
    
    festival_poster <- festival_html %>%
        html_node('.article-poster>a>img') %>%
        html_attr('src')
    
    test = tibble(
        artist_name = artists,
        festival_name = festivals$festival_title[this_festival],
        festival_urls = list(festival_urls),
        festival_poster = festival_poster
    )
    
}, .progress = TRUE)

locations <- unique(festivals$festival_location)

locations_geo <- future_map_dfr(locations[1:10], function(location) {
    tries <- 0
    while (tries < 5) {
        geo_info <- geocode(location, output = 'more')
        if (is.na(geo_info$lat) & is.na(geo_info$lon)) {
            tries <- tries + 1
            Sys.sleep(tries * 2)
        } else {
            break
        }
    }
    
    if (is.null(geo_info$country)) {
        lon <- NA
        lat <- NA
        country <- NA
    } else {
        lon <- geo_info$lon
        lat <- geo_info$lat
        country <- geo_info$country
    }
    
    tibble(
        festival_location = location,
        lon = lon,
        lat = lat,
        country = country
    )
}, .progress = TRUE)

festivals <- left_join(festivals, locations_geo, by = 'festival_location')
save(festivals, file = 'festivals.RData')

# Get artist discography audio features with spotifyr ----------------------
unique_artists <- unique(festival_artists$artist_name)

pb <- txtProgressBar(min = 1, max = length(unique_artists), style = 3)
spotify_artist_names <- map_df(unique_artists, function(artist) {
    
    tryCatch({
        spotify_artist <- search_spotify(artist, type = 'artist')
    }, error = function(e) {
        spotify_artist <- tibble()
    })
    
    if (exists('spotify_artist')) {
        
        if (nrow(spotify_artist) > 0) {
            
            exact_matches <- spotify_artist %>% 
                filter(name == artist)
            
            if (nrow(exact_matches) == 0) {
                closest_artist <- slice(spotify_artist, 1)
            } else {
                closest_artist <- slice(exact_matches, 1)
            }
            df <- closest_artist %>% 
                mutate(festival_artist_name = artist,
                       spotify_artist_img = ifelse(is.null(images[[1]]$url[1]), NA, images[[1]]$url[1])) %>% 
                select(festival_artist_name,
                       spotify_artist_name = name,
                       spotify_artist_id = id)
        } else {
            df <- tibble()
        }
    } else {
        df <- tibble()
    }
    setTxtProgressBar(pb, match(artist, unique_artists))
    return(df)
})

spotify_artist_names <- spotify_artist_names %>% 
    mutate_at(c('spotify_artist_name', 'spotify_artist_uri', 'spotify_artist_img'), funs(ifelse(festival_artist_name != 'Fuglar' & spotify_artist_name == 'Fuglar', NA, .))) %>% 
    filter(!is.na(spotify_artist_uri))

spotify_artist_names_exact_matches <- filter(spotify_artist_names, tolower(festival_artist_name) == tolower(spotify_artist_name))

festival_artists_spotify <- festival_artists %>% 
    left_join(spotify_artist_names_exact_matches, by = c('artist_name' = 'festival_artist_name'))

save(festival_artists_spotify, file = 'festival_artists_spotify.RData')