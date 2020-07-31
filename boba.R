################################################################################
# Libraries

library(tidyverse)
library(httr)
library(yelpr)
library(geosphere)
library(tools)
library(usmap)
library(ggrepel)
library(scales)
library(rvest)
library(dooRdash)
library(knitr)
library(clipr)

theme_set(theme_bw(base_size = 18))

################################################################################
# TOC:
#
# 1. Initialize Yelp API
# 2. Get list of cities to look at
# 3. Plots related to chosen cities
# 4. Yelp function to pull in data
# 5. Pull in data from yelp
# 6. Analyze data: City-Level
# 7. Analyze data: NYC
# 8. Analyze data: Store-level

################################################################################
# 1. Initialize Yelp API

# More info on parameters here: https://www.yelp.com/developers/documentation/v3/business_search
# API Info: https://www.yelp.com/developers/v3/manage_app?api_key_refreshed=True
# API Documentation: https://www.yelp.com/developers/documentation/v3/business_search

client_secret <- readLines('~/Desktop/inputs/yelp_api.txt', warn = F)

################################################################################
# 2. Get list of cities to look at
# Data from wikipedia: 
# https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population

url <- 'https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population'
html_data <- read_html(url)
pop_df_raw <- html_table(html_data, fill = T)[[5]]

cities_df <-
  pop_df_raw %>% 
  setNames(1:11) %>% 
  select(city = `2`,
         state = `3`,
         population = `4`,
         sq_miles = `7`,
         coords = `11`) %>%
  mutate(city = str_extract(city, '^[[:alpha:] ]+'),
         state = str_extract(state, '^[[:alpha:] ]+'),
         state_abb = state.abb[match(state, state.name)],
         population = as.numeric(population %>% str_replace_all(',', '')),
         sq_miles = as.numeric(str_extract(sq_miles, '[[0-9]\\.]+')),
         lat = str_extract(coords, '[0-9]{2}\\.[0-9]{4}') %>% as.numeric,
         lng = str_extract(coords, '-[0-9]+\\.[0-9]{4}') %>% as.numeric,
         # Clean washington DC
         city = ifelse(city == 'Washington' & state == 'District of Columbia',
                       'Washington, DC', city),
         state_abb = ifelse(state == 'District of Columbia',
                            'DC', state_abb),
         city_state = paste0(city, ', ', state_abb)) %>%
  group_by(state) %>%
  mutate(is_label = population == max(population) |
           population > 4e5) %>%
  ungroup

################################################################################
# 3. Plots related to chosen cities

# Check cities on map
plot_usmap(fill = '#f0dfc5', alpha = .85,
           exclude = 'PR',
           color = 'gray50') +
  geom_point(data = 
               cities_df %>%
               select(lng,
                      lat,
                      population) %>%
               usmap_transform(), 
             aes(x = lng.1, y = lat.1,
                 size = population),
             color = '#583b39',
             alpha = .75) +
  geom_text_repel(data = 
                    cities_df %>%
                    filter(is_label) %>%
                    select(lng,
                           lat,
                           city) %>%
                    usmap_transform(),
                  aes(x = lng.1, y = lat.1,
                      label = city),
                  hjust = 0,
                  size = 5) +
  scale_size_continuous(range = c(1, 20)) +
  guides(size = F)

################################################################################
# 4. Yelp function to pull in data

# Categories:
# Bubble Tea (bubbletea)

get_city_data <- function(city,
                          radius = 40000){
  
  # First step: Get businesses
  return_list <- list()
  
  return_list[[1]] <-
    business_search(api_key = client_secret,
                    location = city,
                    categories = 'bubbletea',
                    radius = radius,
                    limit = 50)
  
  total_num <- return_list[[1]][[2]]
  
  return_list[[1]] <- return_list[[1]][[1]]
  
  cat('Reading Results for', city, '/ Total stores:', total_num, '\n')
  
  if(total_num == 0) {
    return(NULL)
  }
  
  if(total_num > 50) {
    
    for(i in 2:ceiling(total_num/50)) {
      
      return_list[[i]] <-
        business_search(api_key = client_secret,
                        location = city,
                        categories = 'bubbletea',
                        radius = radius,
                        limit = 50,
                        offset = 50 * (i - 1))[[1]]
    }
  }
  
  # Second step: Clean df 
  for(i in 1:length(return_list)) {
    
    temp_df <- return_list[[i]]
    
    return_list[[i]] <-
      temp_df %>%
      select(id, alias, name, url, review_count, rating, distance, is_closed) %>%
      bind_cols(temp_df$coordinates) %>%
      bind_cols(temp_df$location) %>%
      bind_cols(lapply(temp_df$categories, 
                       function(x) {paste(x$alias, collapse = ', ')}) %>%
                  unlist %>%
                  data.frame(categories = .))
      
  }
  
  return_df <- 
    return_list %>%
    bind_rows() %>%
    mutate(city = city) %>%
    select(-display_address)
  
  return(return_df)
}

################################################################################
# 5. Pull in data from yelp for all cities

get_all_data <- function(limit = 1000,
                         verbose = T) {
  
  start_time <- Sys.time()
  
  all_cities <- 
    cities_df %>%
    pull(city_state) %>%
    head(limit)
  
  return_list <- list()
  
  i <- 1
  
  for(city in all_cities) {
    return_list[[city]] <-
      get_city_data(city)
    
    if(verbose){
      print(i)
    }
    
    i <- i + 1
  }
  
  return_df <- 
    bind_rows(return_list) %>%
    distinct(id, .keep_all = T)
  
  cat('Elapsed Time:', Sys.time() - start_time)
  
  return(return_df)
}

df_1_raw <- get_all_data()

# Clean NYC Data
df_1 <-
  df_1_raw %>%
  filter(!is_closed) %>%
  rowwise() %>%
  mutate(dist_from_nyc = distHaversine(c(-74.0060, 40.7128),
                                       c(longitude, latitude))) %>%
  ungroup %>% 
  mutate(nyc_city = ifelse(state == 'NY' & dist_from_nyc <= 2.5e4,
                           city, NA),
         city = ifelse(dist_from_nyc <= 2.5e4,
                       'New York', city)) %>%
  mutate(city_state = paste0(city, ', ', state)) 
  
# No need to reload after this
df_1 %>% saveRDS('~/Desktop/Personal/boba.RDS')
df_1 <- readRDS('~/Desktop/Personal/boba.RDS')

################################################################################
# 6. Analyze data: City-Level

# People per store
df_1 %>%
  group_by(city_state) %>%
  summarise(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  arrange(-n_stores) %>% 
  left_join(cities_df %>%
              select(city_state,
                     population,
                     sq_miles), by = 'city_state') %>%
  mutate(people_per_store = round(population/n_stores),
         store_per_sqmi = n_stores/sq_miles) %>%
  arrange(people_per_store) %>%
  head(25) %>%
  ggplot(aes(x = fct_reorder(city_state, -people_per_store), y = people_per_store)) +
  geom_bar(stat = 'identity',
           fill = 'mediumpurple4') +
  coord_flip() +
  theme_bw() +
  labs(y = 'People/Store',
       x = '',
       title = '# Residents Per Boba Shop') +
  geom_text(aes(label = abbreviate_number(people_per_store, 2)),
            color = 'white',
            size = 8,
            hjust = 1.2)

# Stores per square mile
df_1 %>%
  group_by(city_state) %>%
  summarise(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  arrange(-n_stores) %>% 
  left_join(cities_df %>%
              select(city_state,
                     population,
                     sq_miles), by = 'city_state') %>%
  mutate(people_per_store = round(population/n_stores),
         store_per_sqmi = n_stores/sq_miles) %>% 
  arrange(-store_per_sqmi) %>%
  head(25) %>%
  ggplot(aes(x = fct_reorder(city_state, store_per_sqmi), y = store_per_sqmi)) +
  geom_bar(stat = 'identity',
           fill = 'firebrick4') +
  coord_flip() +
  theme_bw() +
  labs(y = 'Stores Per Square Mile',
       x = '',
       title = '# Stores Per Square Mile') +
  geom_text(aes(label = round(store_per_sqmi, 2)),
            color = 'white',
            size = 8,
            hjust = 1.2)

# Review count vs. review grade
df_1 %>%
  group_by(city_state) %>%
  summarise(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  arrange(-n_stores) %>% 
  mutate(is_eligible = n_stores >= 5 & num_reviews >= 500) %>%
  inner_join(cities_df %>%
              select(city_state,
                     population,
                     sq_miles), by = 'city_state') %>%
  ggplot(aes(x = num_reviews,
             y = avg_review)) +
  geom_point(aes(size = n_stores,
                 color = is_eligible),
             alpha = .6) +
  geom_text(data = . %>% 
              filter(num_reviews >= 9000),
              aes(label = city_state),
            hjust = 1.1,
            vjust = 1.2) +
  scale_size_continuous(range = c(1, 20)) +
  scale_color_manual(values = c('gray60', 'forestgreen')) +
  guides(color = F) +
  labs(title = 'By City: # Reviews vs. Average Review',
       x = 'Total # Reviews',
       y = 'Average Review Score',
       size = '# Stores in City')

df_1 %>%
  group_by(city_state) %>%
  summarise(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  arrange(-n_stores) %>% 
  mutate(is_eligible = n_stores >= 5 & num_reviews >= 500) %>%
  filter(is_eligible) %>%
  arrange(-avg_review) %>%
  select(-is_eligible) %>%
  inner_join(cities_df %>%
               select(city_state,
                      population,
                      sq_miles,
                      lat,
                      lng), by = 'city_state') %>%
  mutate(label = paste0('Rank ', row_number(), ': ',
                        city_state)) %>%
  head(30) ->
  p1

plot_usmap(fill = '#f0dfc5', alpha = .85,
           exclude = 'PR',
           color = 'gray50') +
  geom_point(data = p1 %>% select(lng, lat) %>% head(20) %>% usmap_transform(), 
             aes(x = lng.1, y = lat.1),
             color = '#583b39',
             alpha = .75,
             size = 5) +
  geom_text_repel(data = p1 %>% select(lng, lat, label) %>% head(20) %>% usmap_transform(), 
                  aes(x = lng.1, y = lat.1,
                      label = label),
                  hjust = 0,
                  size = 5) +
  scale_size_continuous(range = c(1, 20)) +
  guides(size = F)

p1 %>%
  mutate(rank = row_number()) %>%
  select(rank, 1:4) %>%
  mutate(avg_review = round(avg_review, 2)) %>%
  write_clip()
  

################################################################################
# 7. NYC

df_1 %>%
  filter(city_state == 'New York, NY') %>%
  count(nyc_city) %>%
  arrange(-n)

df_1 %>%
  filter(city_state == 'New York, NY') %>% 
  select(latitude, longitude) %>%
  write_csv('~/Desktop/to_kepler.csv')

################################################################################
# 8. Store Level

# Review Count per store
df_1 %>%
  filter(!is_closed) %>%
  mutate(city_state = paste0(city, ', ', state)) %>%
  group_by(city_state) %>%
  summarise(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  mutate(reviews_per_store = num_reviews/n_stores) %>%
  arrange(-n_stores) %>% 
  left_join(cities_df %>%
              select(city_state,
                     population,
                     sq_miles), by = 'city_state') %>%
  mutate(people_per_store = round(population/n_stores),
         store_per_sqmi = n_stores/sq_miles) %>% 
  arrange(-num_reviews)
# Try to get above in a US Map

# Scatter plot: ratings vs. avg.rating
df_1 %>%
  arrange(-review_count) %>%
  mutate(rank = row_number()) %>%
  select(rank, name, review_count, rating, address = address1, city_state) %>%
  head(20) %>%
  write_clip()

df_1 %>%
  group_by(city_state) %>%
  mutate(n_stores = n(),
            num_reviews = sum(review_count),
            avg_review = sum(rating * review_count)/sum(review_count)) %>%
  mutate(rating_delta = rating - avg_review) %>%
  filter(review_count >= 500) %>%
  arrange(-rating_delta) %>%
  ungroup %>%
  mutate(rank = row_number()) %>%
  select(rank, name, review_count, city_avg_rating = avg_review, rating, 
         rating_delta,
         address = address1, city_state) %>%
  head(30) %>%
  write_clip()
