library(glue)
library(httr)
library(furrr)
library(rvest)
library(ggmap)
library(stringr)
library(spotifyr)
library(tidyverse)
library(countrycode)

base_url <- 'https://www.musicfestivalwizard.com/all-festivals/'

# Find number of pages to search through ----------------------------------
num_festivals <- read_html(base_url) %>% 
    html_node('.search-res') %>% 
    html_text() %>% 
    str_extract('returned [[:digit:]]{1,} festivals') %>% 
    parse_number()

num_pages <- ceiling(num_festivals / 15)

date_pulled <- Sys.time()

plan(multiprocess)
# Loop through pages and scrape festival details and links ----------------
festivals <- future_map_dfr(1:num_pages, function(this_page) {
    
    # this_page <- 1
    
    festival_list <- read_html(paste0(base_url, 'page/', this_page)) %>%
        html_nodes('.lineup-item')
    
    future_map_dfr(1:length(festival_list), function(this_festival) {
        
        # this_festival <- 1
        
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
        html_node('.hubwebsite') %>%
        html_nodes('a') %>% 
        map_df(function(x) list(title = html_text(x), url = html_attr(x, 'href')))
    
    festival_poster <- festival_html %>%
        html_node('.article-poster>a>img') %>%
        html_attr('src')
    
    tibble(
        artist_name = artists,
        festival_name = festivals$festival_title[this_festival],
        festival_urls = list(festival_urls),
        festival_poster = festival_poster
    )
    
}, .progress = TRUE)

locations <- unique(festivals$festival_location)

classify_location <- function(location) {
    if (is.na(location)) {
        return(NA)
    }
    last_four <- str_sub(location, start = nchar(location) - 3, end = nchar(location))
    if (str_detect(last_four, ', [[:upper:]]{2}')) {
        last_two <- str_sub(last_four, start = 3, end = 4)
        if (last_two == 'UK') {
            return('UK')
        } else if (last_two %in% state.abb) {
            return('United States')
        }
    }
    
    text_after_comma <- str_replace_all(location, '.*, ', '')
    
    if (text_after_comma %in% unique(codelist_panel$country.name.en)) {
        return(text_after_comma)
    }
    
    if (text_after_comma %in% state.name) {
        return('United States')
    }
    
    if (text_after_comma %in% c('BC', 'QC', 'QB', 'AB', 'ON', 'NB', 'PE', 'MB', 'Alberta', 'Victoria')) {
        return('Canada')
    }
    
    if (str_detect(location, 'Australia')) {
        return('Australia')
    }
    
    if (str_detect(text_after_comma, 'México')) {
        return('Mexico')
    }
    
    if (str_detect(text_after_comma, 'Netherlands') | location == 'Amsterdam/Eindhoven/Rotterdam') {
        return('Netherlands')
    }
    
    if (text_after_comma == 'Korea') {
        return('South Korea')
    }
    
    if (location == 'Florence, Itlay' | text_after_comma == 'Sicily') {
        return('Italy')
    }
    
    if (location == 'Buckinghamshire UK' | text_after_comma %in% c('Scotland', 'Wales', 'Kent')) {
        return('UK')
    }
    
    if (text_after_comma == 'Latvija') {
        return('Latvia')
    }
    
    if (text_after_comma %in% c('Czech', 'Czech Republic')) {
        return('Czech Republic')
    }
    
    if (text_after_comma == 'SA') {
        return('South Africa')
    }
    
    if (location == 'Palma/Corsica/Ibiza') {
        return('Spain')
    }
    
    if (location == 'Saint Martin') {
        return('Saint Martin')
    }
    
    if (location == 'Santa Marta, Columbia') {
        return('Colombia')
    }
    
    return(NA)
}

locations_geo <- festivals %>% 
    mutate(location = map(festival_location, classify_location) %>% as.character) %>% 
    select(location, festival_location)

festivals <- left_join(festivals, locations_geo, by = 'festival_location')
save(festivals, file = 'festivals.RData')

# Get artist discography audio features with spotifyr ----------------------
unique_artists <- unique(festival_artists$artist_name[!festival_artists$artist_name == ''])

# pb <- txtProgressBar(min = 1, max = length(unique_artists), style = 3)

spotify_artist_names <- future_map_dfr(unique_artists, function(artist) {
    artist_name_lower <- tolower(artist)

    spotify_artist <- search_spotify(artist_name_lower, 'artist')
    
    if (exists('spotify_artist')) {
        
        if (nrow(spotify_artist) > 0) {
            
            exact_matches <- spotify_artist %>% 
                filter(tolower(name) == artist_name_lower)
            
            if (nrow(exact_matches) == 0) {
                closest_artist <- slice(spotify_artist, 1)
            } else {
                closest_artist <- slice(exact_matches, 1)
            }
            df <- closest_artist %>% 
                mutate(festival_artist_name = artist,
                       spotify_artist_img = ifelse(is.null(images[[1]]$url[1]), NA, images[[1]]$url[1])) %>% 
                select(festival_artist_name,
                       spotify_artist_img,
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
}, .progress = TRUE)

spotify_artist_names <- spotify_artist_names %>% 
    filter(!is.na(spotify_artist_id))

spotify_artist_names_exact_matches <- filter(spotify_artist_names, tolower(festival_artist_name) == tolower(spotify_artist_name))

festival_artists_spotify <- festival_artists %>% 
    left_join(spotify_artist_names_exact_matches, by = c('artist_name' = 'festival_artist_name'))

save(festival_artists_spotify, file = 'festival_artists_spotify.RData')
