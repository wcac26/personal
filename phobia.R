################################################################################
# Libraries

library(tidyverse)
library(jsonlite)

options(scipen=10000)
theme_set(theme_bw(base_size = 18))

################################################################################
# TOC:
#
# 1. Get all phobia subreddits
# 2. Get data of all subreddits
# 3. Explore

################################################################################
# 1. Get all phobia subreddits
# Resource: https://www.reddit.com/r/ListOfSubreddits/comments/8drbn3/i_created_a_txt_list_of_all_subreddits_1082444_of/

phobia_raw <- readLines('~/Desktop/Personal/allsubreddits.txt')

phobia_df <- 
  data.frame(subreddit_url = phobia_raw[grepl('phobia', phobia_raw, ignore.case = T)]) %>%
  mutate(subreddit_name = 
           str_extract(subreddit_url, 'r/[[:alnum:]]+') %>%
           str_replace('r/', '')) %>%
  distinct(subreddit_name, .keep_all = T) %>%
  arrange(subreddit_name)

################################################################################
# 2. Get data of all subreddits

get_subreddit_data <- function(subreddit_name) {
  
  url <- 
    paste0('https://www.reddit.com/r/',
           subreddit_name,
           '/about.json?utm_source=reddit&utm_medium=usertext&utm_name=redditdev&utm_content=t1_chmqabt')
  
  json_data <-  try({fromJSON(url)}, silent = T)
  
  if(class(json_data) == 'try-error') {
    
    return_df <- data.frame(subreddit_name = subreddit_name)
    
  } else {
    
    return_df <-
      data.frame(subreddit_name = subreddit_name,
                 title = json_data[[2]]$title,
                 description = json_data[[2]]$description,
                 public_description = json_data[[2]]$public_description,
                 subscriber_count = json_data[[2]]$subscribers,
                 active_user_count = json_data[[2]]$active_user_count,
                 is_quarantine = json_data[[2]]$quarantine,
                 is_over_18 = json_data[[2]]$over18)
    
  }

  return(return_df)
}

loop_through_subreddits <- function(subreddits,
                                    verbose = T){
  
  start_time <- Sys.time()
  
  data_list <- list()
  
  for(i in 1:length(subreddits)){
    
    if(verbose) {
      print(paste0(i, ': ', subreddits[i]))
    }
    
    data_list[[i]] <-
      get_subreddit_data(subreddits[i])
  }
  
  cat('Elapsed Time:', Sys.time() - start_time)
  
  data_list %>%
    bind_rows %>%
    return
}

subreddit_df <-
  loop_through_subreddits(phobia_df$subreddit_name)

################################################################################
# 3. Explore

# Top 10
subreddit_df %>%
  arrange(-subscriber_count) %>%
  select(-description) %>%
  head(18)

# Graphed
subreddit_df %>%
  mutate(name = paste0('r/',
                       subreddit_name)) %>%
  arrange(-subscriber_count) %>%
  head(40) %>%
  ggplot(aes(x = fct_reorder(name, subscriber_count), y = subscriber_count)) +
  geom_bar(stat = 'identity',
           fill = 'slateblue') +
  coord_flip() +
  scale_y_continuous(trans = 'log10') +
  labs(y = 'Subscriber Count',
       x = '',
       title = 'Subscriber Count, Top 40 Phobia Subreddits') +
  geom_text(aes(label = prettyNum(subscriber_count, big.mark = ',')),
            color = 'white',
            size = 8,
            hjust = 1.2)
