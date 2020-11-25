################################################################################
# Libraries

library(tidyverse)
library(dooRdash)
library(scales)
library(tools)
library(knitr)
library(magrittr)
library(clipr)
library(geosphere)
library(pwr)
library(ggrepel)
library(lubridate)

library(googlesheets4)
sheets_auth(path = '~/Documents/sheets_api.json')

theme_set(theme_dd(base_size = 16))
options(scipen = 999)

################################################################################
# TOC:
#
# 1.Load + clean data

################################################################################
# 1.Load + clean data

df1_raw <- 
  read_sheet('https://docs.google.com/spreadsheets/d/1_BaOnrUHGGX9Z7u5dkTP-s4rhmiG-d7Gb9PFwL_YG_c/edit#gid=0')

df1 <-
  df1_raw %>% 
  setNames(paste0('col_', 1:4)) %>% 
  mutate(col_4 = unlist(col_4)) %>% 
  mutate(company = str_extract(col_1, '^[^\n]+'),
         location = 
           str_extract(col_1, '\n[^|]+') %>%  
           str_replace('\n', '') %>% trimws,
         date_added = 
           str_extract(col_1, '[0-9]+/[0-9]+/[0-9]+') %>% as.Date(format = '%m/%d/%y'),
         yac = str_extract(col_3, '^[0-9]+') %>% as.numeric,
         yoe = str_extract(col_3, '[0-9]+$') %>% as.numeric,
         tc = 
           str_extract(col_4, '\\$[[0-9],]+') %>% 
           str_replace_all('[^[0-9]]', '') %>% 
           as.numeric,
         tc = if_else(is.na(tc),
                      as.numeric(col_4),
                      tc),
         base = str_extract(col_4, '[0-9]+k') %>% 
           str_replace('k', '') %>% 
           as.numeric * 1e3,
         stock = str_extract(col_4, ' [0-9]+k ') %>% 
           str_replace('k', '') %>% trimws %>% 
           as.numeric * 1e3,
         bonus = str_extract(col_4, '[0-9]+k$') %>% 
           str_replace('k', '') %>% trimws %>% 
           as.numeric * 1e3,
         level = str_extract(col_2, '^[^\n]+')) %>% 
  mutate(yoe_label = 
           case_when(yoe <=  1 ~ '0 - 1',
                     yoe >= 9 ~ '9+',
                     TRUE ~ as.character(yoe)) %>% 
           factor(., levels = sort(unique(.))),
         is_us = grepl(', [A-Z]{2}$', location)) %>% 
  filter(is_us)


################################################################################
# 2. Visualize

# % in top 6
df1 %>% 
  count(company %in% c('Facebook', 'Google', 'Amazon', 'Microsoft', 'Apple', 'Uber')) %>% 
  mutate(pct = n/sum(n))
df1 %>% 
  count(is_us) %>% 
  mutate(pct = n/sum(n))

# Silicon Valley
(226+61+56+47+44+40+21+13+10)/1261
(123)/1261
(188+86+25)/1261

# Companies in dataset
df1 %>% 
  count(company) %>% 
  arrange(-n) %>% 
  head(25) %>% 
  ggplot(aes(x = fct_reorder(company, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'forestgreen') + 
  coord_flip() +
  geom_text(aes(label = n), color = 'white',
            hjust = 1.25,
            size = 6) +
  labs(y = '# of Reported Compensation',
       x = '',
       title = '# of Self-Reported Compensation, by Company',
       subtitle = 'Source: levels.fyi')

# Company x level
df1 %>% 
  filter(company %in% c('Facebook', 'Google', 'Amazon', 'Microsoft', 'Apple', 'Uber')) %>% 
  group_by(company, level) %>% 
  summarise(n = n(),
            med_tc = median(tc),
            med_yoe = median(yoe)) %>% 
  filter(n >= 5) %>% 
  arrange(-n, .by_group = T) %>% 
  slice(1:6) %>% 
  ggplot(aes(x = med_yoe, y =  med_tc)) +
  scale_size_continuous(range = c(1, 20)) +
  geom_point(aes(size = n, color = company),
             alpha = .5) +
  geom_text_repel(aes(label = paste(company, level))) +
  guides(color = F, size = F) +
  scale_y_continuous(labels = dollar, breaks = seq(150000, 600000, by = 50000)) +
  labs(y = 'Median Total Compensation',
         x = 'Median Years of Experience') +
  scale_x_continuous(breaks = 0:13)

# Company x level graph
df1 %>% 
  filter(company %in% c('Facebook', 'Google', 'Amazon', 'Microsoft', 'Apple', 'Uber')) %>% 
  group_by(company, level) %>% 
  summarise(n = n(),
            med_tc = median(tc),
            med_yoe = median(yoe)) %>% 
  arrange(-n, .by_group = T) %>% 
  slice(1:6) %>% 
  ggplot(aes(x = level, y = med_tc)) +
  geom_bar(stat = 'identity') +
  facet_wrap(~company, scales = 'free_x')

# YOE
df1 %>% 
  count(yoe_label) %>% 
  ggplot(aes(x = yoe_label, y = n)) +
  geom_bar(stat = 'identity', fill = 'navyblue') + 
  coord_flip() +
  geom_text(aes(label = n), color = 'white',
            hjust = 1.25,
            size = 6) +
  labs(y = '# of Reported Compensation',
       x = 'Years of Experience',
       title = '# of Self-Reported Compensation, by Years of Experience (YOE)',
       subtitle = 'Source: levels.fyi')

# Locations
df1 %>% 
  filter(is_us) %>% 
  count(location) %>% 
  arrange(-n) %>% 
  head(20) %>% 
  ggplot(aes(x = fct_reorder(location, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'coral4') + 
  coord_flip() +
  geom_text(aes(label = n), color = 'white',
            hjust = 1.25,
            size = 6) +
  labs(y = '# of Reported Compensation',
       x = '',
       title = '# of Self-Reported Compensation, by location (US Only)',
       subtitle = 'Source: levels.fyi')

# Locations
df1 %>% 
  filter(!is_us) %>% 
  count(location) %>% 
  arrange(-n) %>% 
  head(10) %>% 
  ggplot(aes(x = fct_reorder(location, n), y = n)) +
  geom_bar(stat = 'identity', fill = 'orange') + 
  coord_flip() +
  geom_text(aes(label = n), color = 'white',
            hjust = 1.25,
            size = 6) +
  labs(y = '# of Reported Compensation',
       x = '',
       title = '# of Self-Reported Compensation, by location (Intl)',
       subtitle = 'Source: levels.fyi')

# yoe vs tc
df1 %>% 
  filter(is_us, tc < 1e6) %>% 
  ggplot(aes(x = yoe_label, y = tc)) +
  geom_boxplot(width =  .5, fill = 'gray', alpha = .5) +
  geom_text(data = . %>% group_by(yoe_label) %>% summarise(s = median(tc)),
            aes(label = paste0('$', round(s/1000, 0), 'k'),
                y = s),
            vjust = 0,
            hjust = -.75,
            size = 5) +
  geom_text(data = . %>% group_by(yoe_label) %>% summarise(s = quantile(tc, .25)),
            aes(label = paste0('$', round(s/1000, 0), 'k'),
                y = s),
            vjust = 0,
            hjust = -.75,
            size = 5) +
  geom_text(data = . %>% group_by(yoe_label) %>% summarise(s = quantile(tc, .75)),
            aes(label = paste0('$', round(s/1000, 0), 'k'),
                y = s),
            vjust = 0,
            hjust = -.75,
            size = 5) +
  scale_y_continuous(labels = dollar) +
  labs(x = 'Years of Experience',
       y = 'Total Compensation',
       title = 'Total Compensation, by YoE',
       subtitle = '25th, 50th, and 75th percentile of compensation labelled')

# Dates
df1 %>% 
  mutate(yr = format(date_added, '%m/1/%Y') %>% as.Date(format = '%m/%d/%Y'),
         y = year(yr)) %>% 
  count(y, yr) %>% 
  ggplot(aes(x = yr, y = n, fill = y)) +
  geom_bar(stat = 'identity') +
  guides(fill = F)

df1 %>% 
  group_by()

# FB example
df1 %>% 
  filter(company == 'Facebook',
         level == 'IC3',
         !is.na(bonus)) %>% 
  select(company, location, date_added, level, yoe, tc, base, stock, bonus) %>% 
  kable

# Company x overall
df1 %>% 
  group_by(company) %>% 
  summarise(n = n(),
            med_tc = median(tc),
            med_yoe = median(yoe)) %>% 
  filter(n >= 9) %>% 
  ggplot(aes(x = med_yoe, y =  med_tc)) +
  scale_size_continuous(range = c(1, 20)) +
  geom_point(aes(size = n, color = company),
             alpha = .5) +
  geom_text_repel(aes(label = company)) +
  guides(color = F, size = F) +
  scale_y_continuous(labels = dollar, breaks = seq(150000, 600000, by = 50000)) +
  labs(y = 'Median Total Compensation',
       x = 'Median Years of Experience') +
  scale_x_continuous(breaks = 0:13)
  
