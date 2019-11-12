 
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

data <- fread('catalegBCN_2019-11-11_17-59.csv')
cols <- colnames(data)

#### GENERAL INFO ####

# 462 observations, 97 variables
# name, dataset title (ca,es,en) ...
glimpse(data)

# All of the files are subject to Creative Commons Attribution 4.0 license
data %>% 
  distinct(license_title)

# Number of datasets by openness_score 
data %>% 
  group_by(openness_score) %>% 
  summarise(number_of_datasets = n())

# Number of datasets by fuente (source) 
data %>% 
  group_by(organization_parent_name_ca,organization_name_ca,fuente) %>% 
  summarise(number_of_datasets = n()) %>% 
  arrange(desc(number_of_datasets)) %>% view

# Number of datasets by frequency 
data %>% 
  group_by(frequency) %>% 
  summarise(number_of_datasets = n())

# Frequently updated datasets should have api available 
data %>% 
  group_by(frequency,api) %>% 
  summarise(number_of_datasets = n()) %>%
  spread(api,number_of_datasets) %>% 
  replace(.,is.na(.),0)

# Is the token required for the api? No - there is only one token 
data %>% 
  group_by(api,token_required) %>% 
  summarise(number_of_datasets = n()) %>%
  spread(token_required,number_of_datasets) %>% 
  replace(.,is.na(.),0)

# What is in the tagslist?
data %>% 
  distinct(tags_list)

# What is in the notes?
data %>% 
  distinct(notes_en)

# Mapping catalan frequency description to english
# Maybe using google translate api for R

# Number of datasets published by year
data %>% 
  mutate(year = year(date_published)) %>% 
  group_by(year) %>% 
  summarise(n = n())

#### GENERAL INFO ####

#### POPULARITY ####

# Which datasets where most popular (views)?
data %>% 
  select(name,notes_en,total_views) %>% 
  arrange(desc(total_views)) %>% 
  mutate(rank_views = row_number()) -> data_rank_views

# Which datasets where most popular (downloads)?
data %>% 
  select(name,notes_en,total_downloads_by_ip) %>% 
  arrange(desc(total_downloads_by_ip)) %>% 
  mutate(rank_downloads = row_number()) -> data_rank_downloads

# Which datasets where most popular (api)?
data %>% 
  select(name,notes_en,total_api_access_by_api) %>% 
  arrange(desc(total_api_access_by_api)) %>% 
  mutate(rank_api = row_number()) -> data_rank_api


# Place in the popularity rank based on those 3 factors
data_rank <- left_join(data_rank_views,data_rank_downloads)
data_rank <- left_join(data_rank,data_rank_api)
rm(data_rank_views,data_rank_downloads,data_rank_api)

#### POPULARITY ####

#### EXTRA INFO ####

# Additional info about those datasets
data %>% 
  select(name,`complementary description`,field_definition,openness_score,
         organization_parent_name_ca,organization_name_ca) -> data_rank_add_info

# Number of available file formats
data %>% 
  select(name,35:66) %>%
  gather(key = "format", value = "available",-name) %>% 
  mutate(available = recode(available, N = 0, S = 1)) %>% 
  group_by(name) %>% 
  summarise(n_formats_available = sum(available)) -> data_available_formats

# Number of years available
data %>% 
  select(name,67:97) %>%
  gather(key = "year", value = "available",-name) %>% 
  mutate(available = recode(available, N = 0, S = 1)) %>% 
  group_by(name) %>% 
  summarise(n_years_available = sum(available)) -> data_available_years

data_available <- left_join(data_available_formats,data_available_years)
rm(data_available_formats,data_available_years)

data_rank_add_info <- left_join(data_rank_add_info,data_available)
rm(data_available)

#### EXTRA INFO ####

# Searching for shapefile 
data %>% 
  filter(shp == "S") %>% view

# What are the determinants of dataset popularity 
data_rank <- left_join(data_rank,data_rank_add_info)

# Datasets popularity table - which are often viewed and rarely used?
data_rank %>% 
  select(name,notes_en,openness_score,starts_with("rank"),everything()) -> data_rank


