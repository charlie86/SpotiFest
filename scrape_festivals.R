library(glue)
library(furrr)
library(httr)
library(rvest)
library(stringr)
library(ggmap)
library(tidyverse)
library(spotifyr)

base_url <- 'https://www.musicfestivalwizard.com/all-festivals/'

# Find number of pages to search through ----------------------------------
num_festivals <- read_html(base_url) %>%
    html_node('.festival-count') %>%
    html_text %>%
    str_extract('[[:digit:]]{1,} festivals') %>%
    parse_number

num_pages <- ceiling(num_festivals / 15)

date_pulled <- Sys.time()

plan(multiprocess)
# Loop through pages and scrape festival details and links ----------------
festivals <- future_map_dfr(1:num_pages, function(this_page) {
    # this_page <- 1
    
    # pb <- txtProgressBar(min = 1, max = num_pages, style = 3)
    
    festival_list <- read_html(glue('{base_url}page/{this_page}')) %>%
        html_nodes('.singlefestlisting')
    
    festivals_df <- future_map_dfr(1:length(festival_list), function(this_festival) {
        # this_festival <- 4
        
        festival_image <- festival_list[[this_festival]] %>%
            html_node('.festivalright') %>%
            html_node('img') %>%
            as.character %>%
            str_extract('https://.*\\.(png|jpg)')
        
        left_node <- festival_list[[this_festival]] %>%
            html_node('.festivalleft')
        
        list(
            festival_start = html_node(left_node, '.festivalstart') %>% html_text %>% as.Date('%b %d %Y'),
            festival_title = html_node(left_node, '.festivaltitle') %>% html_text,
            festival_mfw_url = html_node(left_node, '.festivaltitle') %>% html_node('a') %>% str_extract(., '(?<=").*?(?=")'),
            festival_location = html_node(left_node, '.festivallocation') %>% html_text,
            festival_dates = html_node(left_node, '.festivaldate') %>% html_text,
            festival_image = festival_image,
            date_pulled = date_pulled
        )
        
    }, .progress = T)
    
    return(festivals_df)
}, .progress = T)

# Loop through festival pages to scrape lineups ---------------------------
festival_artists <- future_map_dfr(1:nrow(festivals), function(this_festival) {
    
    festival_html <- read_html(festivals$festival_mfw_url[this_festival])
    
    artists <- festival_html %>%
        html_nodes('.lineupguide>ul>li') %>%
        html_text
    
    festival_url <- festival_html %>%
        html_nodes('#festival-basics>a') %>%
        str_extract('http://.*/\\" ') %>%
        str_replace('" ', '')
    
    festival_poster <- festival_html %>%
        html_node('#festival-poster>img') %>%
        str_extract('https://.*\\.png')
    
    artist_info <- tibble(
        artist_name = artists,
        festival_name = festivals$festival_title[this_festival],
        festival_url = festival_url,
        festival_poster = festival_poster
    )
    
    return(artist_info)
    
}, .progress = T)

locations <- unique(festivals$festival_location)

# pb <- txtProgressBar(min = 0, max = length(locations), style = 3)
locations_geo <- future_map_dfr(locations, function(location) {
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
    
    df <- tibble(
        festival_location = location,
        lon = lon,
        lat = lat,
        country = country
    )
    # setTxtProgressBar(pb, match(location, locations))
    return(df)
}, .progress = T)

festivals <- left_join(festivals, locations_geo, by = 'festival_location')
save(festivals, file = 'festivals.RData')


# Get artist discography audio features with spotifyr ----------------------
unique_artists <- unique(festival_artists$artist_name)

pb <- txtProgressBar(min = 1, max = length(unique_artists), style = 3)
spotify_artist_names <- map_df(unique_artists, function(artist) {
    
    tryCatch({
        spotify_artist <- get_artists(artist)
    }, error = function(e) {
        spotify_artist <- tibble()
    })
    
    if (exists('spotify_artist')) {
        
        if (nrow(spotify_artist) > 0) {
            
            exact_matches <- spotify_artist %>% 
                filter(artist_name == artist)
            
            if (nrow(exact_matches) == 0) {
                closest_artist <- slice(spotify_artist, 1)
            } else {
                closest_artist <- slice(exact_matches, 1)
            }
            df <- closest_artist %>% 
                mutate(festival_artist_name = artist) %>% 
                select(festival_artist_name,
                       spotify_artist_name = artist_name,
                       spotify_artist_uri = artist_uri, 
                       spotify_artist_img = artist_img)
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