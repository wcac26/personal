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

################################################################################
# TOC:
#
# 1. Initialize Yelp API
# 2. Get list of cities to look at
# 3. Plots related to chosen cities
# 4. Yelp function to pull in data
# 5. Pull in data from yelp for all states
# 6. Analyze data
# 7. Regional trends

################################################################################
# 1. Initialize Yelp API

# More info on parameters here: https://www.yelp.com/developers/documentation/v3/business_search
# API Info: https://www.yelp.com/developers/v3/manage_app?api_key_refreshed=True
# API Documentation: https://www.yelp.com/developers/documentation/v3/business_search

client_secret <- readLines('~/Desktop/inputs/yelp_api.txt', warn = F)

################################################################################
# 2. Get list of cities to look at
# Data from: https://simplemaps.com/data/us-cities

cities_df <-
  read_csv('~/Desktop/uscities.csv') %>%
  filter(state_id != 'PR') %>%
  group_by(county_fips) %>%
  arrange(-population) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  mutate(city_state = paste0(city, ', ', state_id)) %>%
  group_by(state_id) %>%
  arrange(-population) %>%
  mutate(is_chosen = row_number() <= 10 | population >= 1e5,
         is_label = row_number() <= 2 | population >= 7.5e5) %>%
  ungroup

cities_df %>%
  group_by(is_chosen) %>%
  summarise(n_cities = n(),
            pop = sum(population)) %>%
  mutate(pct = pop/sum(pop))


################################################################################
# 3. Plots related to chosen cities

# Check cities on map
plot_usmap(fill = 'yellow', alpha = .25,
           exclude = 'PR',
           color = 'gray50') +
  geom_point(data = 
               cities_df %>%
               filter(is_chosen) %>%
               select(lng,
                      lat,
                      population) %>%
               usmap_transform(), 
             aes(x = lng.1, y = lat.1,
                 size = population),
             color = 'forestgreen',
             alpha = .5) +
  geom_text_repel(data = 
                    cities_df %>%
                    filter(is_label) %>%
                    select(lng,
                           lat,
                           city) %>%
                    usmap_transform(),
                  aes(x = lng.1, y = lat.1,
                      label = city),
                  hjust = 0) +
  scale_size_continuous(range = c(1, 20)) +
  guides(size = F)

################################################################################
# 4. Yelp function to pull in data

# Categories:
# |alias              |title                |
#   |:------------------|:--------------------|
#   |brazilianjiujitsu  |Brazilian Jiu-jitsu  |
#   |muaythai           |Muay Thai            |
#   |martialarts        |Martial Arts         |
#   |selfdefenseclasses |Self-defense Classes |
#   |boxing             |Boxing               |
#   |taekwondo          |Taekwondo            |
#   |kickboxing         |Kickboxing           |
#   |karate             |Karate               |
#   |chinesemartialarts |Chinese Martial Arts |

get_city_data <- function(city,
                          radius = 40000){
  
  # First step: Get businesses
  return_list <- list()
  
  return_list[[1]] <-
    business_search(api_key = client_secret,
                    location = city,
                    categories = 'martialarts,boxing,muaythai,selfdefenseclasses,taekwondo,kickboxing,karate,chinesemartialarts',
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
                        categories = 'martialarts,boxing,muaythai,selfdefenseclasses,taekwondo,kickboxing,karate,chinesemartialarts',
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
      select(id, alias, name, url, review_count, rating, distance) %>%
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

get_clean_category <- function(input_df) {
  
  input_df %>%
    mutate(name_category = 
             case_when(grepl('karate', name, ignore.case = T) ~ 'karate',
                       grepl('jitsu|gracie|bjj|10th planet', name, ignore.case = T) ~ 'brazilianjiujitsu',
                       grepl('aikido', name, ignore.case = T) ~ 'aikido',
                       grepl('judo', name, ignore.case = T) ~ 'judo',
                       grepl('hapkido', name, ignore.case = T) ~ 'hapkido',
                       grepl('mma|ufc|mixed martial arts', name, ignore.case = T) ~ 'mma',
                       grepl('kung.*fu', name, ignore.case = T) ~ 'kungfu',
                       grepl('capoeira', name, ignore.case = T) ~ 'capoeira',
                       grepl('sambo', name, ignore.case = T) ~ 'sambo',
                       grepl('muay.*thai', name, ignore.case = T) ~ 'muaythai',
                       grepl('krav.*maga', name, ignore.case = T) ~ 'kravmaga',
                       grepl('tai.*chi', name, ignore.case = T) ~ 'taichi',
                       grepl('tae.*kwon', name, ignore.case = T) ~ 'taekwondo',
                       grepl('wushu', name, ignore.case = T) ~ 'chinesemartialarts',
                       grepl('wrestling', name, ignore.case = T) ~ 'wrestling',
                       grepl('kick.?boxing', name, ignore.case = T) ~ 'kickboxing',
                       grepl('boxing|box', name, ignore.case = T) ~ 'boxing',
                       grepl('kendo', name, ignore.case = T) ~ 'kendo',
                       grepl('fencing', name, ignore.case = T) ~ 'fencing',
                       grepl('kempo', name, ignore.case = T) ~ 'kempo'
                       ),
           clean_category = 
             case_when(!is.na(name_category) ~ name_category,
                    grepl('brazilianjiujitsu', categories) ~ 'brazilianjiujitsu',
                    grepl('muaythai', categories) ~ 'muaythai',
                    grepl('boxing', categories) ~ 'boxing',
                    grepl('taekwondo', categories) ~ 'taekwondo',
                    grepl('kickboxing', categories) ~ 'kickboxing',
                    grepl('karate', categories) ~ 'karate',
                    grepl('chinesemartialarts', categories) ~ 'chinesemartialarts',
                    grepl('taichi', categories) ~ 'taichi',
                    TRUE ~ 'uncategorized')) %>%
    return
  
}

################################################################################
# 5. Pull in data from yelp for all states

get_all_data <- function(states) {
  
  start_time <- Sys.time()
  
  all_cities <- 
    cities_df %>%
    filter(is_chosen,
           state_id %in% states) %>%
    pull(city_state)
  
  return_list <- list()
  
  for(city in all_cities) {
    return_list[[city]] <-
      get_city_data(city)
  }
  
  return_df <- 
    bind_rows(return_list) %>%
    distinct(id, .keep_all = T) %>%
    get_clean_category()
  
  cat('Elapsed Time:', Sys.time() - start_time)
  
  return(return_df)
}

df_1 <- get_all_data(cities_df %>% 
                          filter(is_chosen) %>% 
                          pull(state_id) %>% 
                          unique)

# No need to reload after this
df_1 %>% saveRDS('~/Desktop/mma.RDS')
df_1 <- readRDS('~/Desktop/mma.RDS')

################################################################################
# 6. Analyze data: General Trends

# Check clean categories
df_1 %>%
  group_by(state, clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count)) %>%
  arrange(-n_stores, .by_group = T) %>%
  filter(row_number() <= 5) %>%
  as.data.frame()

# Overall summary
df_1 %>%
  group_by(clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count)) %>%
  arrange(-n_stores, .by_group = T) %>%
  #filter(clean_category != 'uncategorized') %>%
  mutate(pct_stores = n_stores/sum(n_stores),
         pct_reviews = tot_reviews/sum(tot_reviews)) %>%
  as.data.frame()

# Overall by number of stores, graphed
df_1 %>%
  group_by(clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count)) %>%
  arrange(-n_stores, .by_group = T) %>%
  filter(clean_category != 'uncategorized') %>%
  mutate(pct_stores = n_stores/sum(n_stores),
         pct_reviews = tot_reviews/sum(tot_reviews)) %>%
  ggplot(aes(x = fct_reorder(clean_category, pct_stores), y = pct_stores)) +
  geom_bar(stat = 'identity',
           fill = 'dodgerblue4') +
  coord_flip() +
  theme_bw() +
  labs(y = 'Percent of Gyms on Yelp',
       x = '',
       title = 'Most Popular Gym Type in America, Percent of Gyms on Yelp') +
  geom_text(aes(label = percent(pct_stores)),
            color = 'white',
            hjust = 1,
            size = 8) +
  scale_y_continuous(label = percent) +
  theme(axis.text.y = element_text(size = 18))

# Overall by number of reviews, graphed
df_1 %>%
  group_by(clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count)) %>%
  arrange(-n_stores, .by_group = T) %>%
  filter(clean_category != 'uncategorized') %>%
  mutate(pct_stores = n_stores/sum(n_stores),
         pct_reviews = tot_reviews/sum(tot_reviews)) %>%
  ggplot(aes(x = fct_reorder(clean_category, pct_reviews), y = pct_reviews)) +
  geom_bar(stat = 'identity',
           fill = 'darkseagreen4') +
  coord_flip() +
  theme_bw() +
  labs(y = 'Percent of Reviews on Yelp',
       x = '',
       title = 'Most Popular Gym Type in America, by Number of Yelp Reviews') +
  geom_text(aes(label = percent(pct_reviews)),
            color = 'white',
            size = 8,
            hjust = 1) +
  scale_y_continuous(label = percent)+
  theme(axis.text.y = element_text(size = 18))

# Avg number of reviews per sport
df_1 %>%
  group_by(clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count),
            n_reviews = mean(review_count)) %>%
  ggplot(aes(x = fct_reorder(clean_category, n_reviews), y = n_reviews)) +
  geom_bar(stat = 'identity',
           fill = 'firebrick4') +
  coord_flip() +
  theme_bw() +
  labs(y = '# of reviews on Yelp',
       x = '',
       title = 'Average Number of Yelp Reviews Per Gym Type') +
  geom_text(aes(label = round(n_reviews, 1)),
            color = 'white',
            size = 8,
            hjust = 1.2) +
  theme(axis.text.y = element_text(size = 18))

# Avg number of rating per sport
df_1 %>%
  group_by(clean_category) %>%
  summarise(n_stores = n(),
            tot_reviews = sum(review_count),
            n_reviews = mean(review_count),
            avg_rating = sum(review_count * rating)/sum(review_count)) %>%
  filter(tot_reviews > 100) %>%
  ggplot(aes(x = fct_reorder(clean_category, avg_rating), y = avg_rating)) +
  geom_bar(stat = 'identity',
           fill = 'mediumpurple4') +
  coord_flip() +
  theme_bw() +
  labs(y = 'Average Rating',
       x = '',
       title = 'Average Yelp Rating',
       subtitle = 'Only sports with 100+ reviews') +
  geom_text(aes(label = round(avg_rating, 2)),
            color = 'white',
            size = 8,
            hjust = 1.2) +
  theme(axis.text.y = element_text(size = 18))

################################################################################
# 7. Regional trends

# By State
df_1 %>%
  filter(clean_category != 'uncategorized') %>%
  group_by(state, clean_category) %>%
  summarise(n_stores = n(),
            n_reviews = sum(review_count)) %>%
  mutate(pct_stores = n_stores/sum(n_stores)) %>%
  arrange(-n_stores, .by_group = T) %>%
  mutate(rk = row_number()) %>%
  filter(!clean_category %in% c('muaythai', 'aikido')) ->
  df_2

# US Map Plot
plot_usmap(data = 
             df_2 %>% 
             select(state, clean_category, rk) %>%
             filter(rk == 2),
           values = 'clean_category',
            alpha = .5,
           exclude = 'PR',
           color = 'gray50',
           labels = T) +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 15),
        title = element_text(size = 20)) +
  scale_fill_viridis_d() +
  labs(title = 'Second Most Popular Sport by State, Number of Gyms')

# Percentage
df_2 %>%
  ggplot(aes(x = state, y = pct_stores)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~clean_category, ncol = 1,
             scales = 'free') +
  coord_flip() +
  theme_bw()

# By top cities
df_1 %>%
  filter(clean_category != 'uncategorized') %>%
  group_by(city, state, clean_category) %>%
  summarise(n_stores = n(),
            n_reviews = sum(review_count)) %>%
  filter(n_stores == max(n_stores),
         n_stores >= 5) %>%
  arrange(-n_stores) %>%
  as.data.frame

# By City
get_map <- function(sport = 'boxing',
                    n_cities = 50,
                    clr = '#440154FF') {
  
  df_1 %>%
    filter(clean_category == sport) %>%
    group_by(city) %>%
    summarise(lat = median(latitude),
              lng = median(longitude),
              n_stores = n(),
              n_reviews = sum(review_count)) %>%
    mutate(label = paste0(city, ' (', n_stores, ')')) %>%
    arrange(-n_stores) %>%
    head(n_cities) ->
    df_3
  
  plot_usmap(fill = 'gray75', alpha = .25,
             exclude = 'PR',
             color = 'gray50') +
    geom_point(data = 
                 df_3 %>%
                 select(lng,
                        lat,
                        n_stores) %>%
                 usmap_transform(), 
               aes(x = lng.1, y = lat.1,
                   size = n_stores),
               color = clr,
               alpha = .5) +
    geom_text_repel(data = 
                      df_3 %>%
                      select(lng,
                             lat,
                             label) %>%
                      usmap_transform(),
                    aes(x = lng.1, y = lat.1,
                        label = label),
                    hjust = 0) +
    scale_size_continuous(range = c(1, 20)) +
    guides(size = F) %>%
    return
  
}

get_map('boxing', n_cities = 75) +
  labs(title = 'Cities with the most boxing gyms') +
  theme(title = element_text(size = 18))

get_map('brazilianjiujitsu', n_cities = 75,
        clr = '#3B528BFF') +
  labs(title = 'Cities with the most BJJ gyms') +
  theme(title = element_text(size = 18))



