library(tidyverse)
library(here)
library(maps)
library(lubridate)
library(tidycensus)
library(tsibble)
library(zoo)
source('get_data.R')


# Import Data -------------------------------------------------------------

new_data_path <- point_data_path(data_name = 'syphilis_data.csv') 

data <- read_csv(new_data_path)

county_map_data <- map_data("county", region = "idaho") %>%
  filter(subregion %in% c(
    "adams",
    "canyon",
    "gem",
    "owyhee",
    "payette",
    "washington"
  )) %>%
  select("county" = subregion, long, lat) %>% 
  mutate('county' = str_to_sentence(county))

tidycensus::census_api_key('0966e26f2f55c47002ab820be619af3c6a426769')

census_data <- get_decennial(geography = 'county', 
                             year = 2020, 
                             state = 'ID', 
                             variables = c('P1_001N')) %>% 
  mutate('county' = str_remove(NAME, ' County, Idaho')) %>% 
  select(county, 'population' = value) %>% 
  filter(county %in% c('Adams', 
                       'Canyon', 
                       'Gem', 
                       'Owyhee', 
                       'Payette', 
                       'Washington'))

# Create Data -------------------------------------------------------------

all_combs <- expand.grid(year = c(unique(year(mdy(data$investigation_start_date)))), 
                         county = c('Adams', 
                                    'Canyon', 
                                    'Gem', 
                                    'Owyhee', 
                                    'Payette', 
                                    'Washington')) %>% 
  filter(!is.na(year))

county_incidence_data <- data %>% 
  mutate('year' = year(mdy(investigation_start_date))) %>% 
  group_by(year, patient_county) %>% 
  summarize('count' = n()) %>% 
  select(year, 'county' = patient_county, count) %>% 
  full_join(all_combs) %>% 
  mutate(count = replace_na(count, 0), 
         'year' = as.character(year))

county_incidence_data <- county_incidence_data %>% 
  group_by(county) %>% 
  summarize('year' = 'All', 
            'count' = sum(count)) %>% 
  full_join(county_incidence_data) %>% 
  full_join(census_data) %>% 
  mutate('incidence_rate' = (count/population) * 10000)

start_date <- min(yearmonth(mdy(data$investigation_start_date)))

end_date <- max(yearmonth(mdy(data$investigation_start_date)))

all_dates <- seq(start_date, end_date, by = month(1))

moving_average_data <- data %>% 
  mutate('month' = yearmonth(mdy(investigation_start_date))) %>% 
  group_by(month) %>% 
  summarize('count' = n()) %>% 
  as_tsibble(index = month) %>% 
  fill_gaps(count = 0) %>% 
  mutate('moving_average' = rollmean(count, 
                                     k = 3, 
                                     align = 'right', 
                                     fill = 'extend'))

outbreak_moving_average_data <- data %>% 
  filter(outbreak_name == '2021_012') %>% 
  mutate('month' = yearmonth(mdy(investigation_start_date))) %>% 
  group_by(month) %>% 
  summarize('count' = n()) %>% 
  as_tsibble(index = month) %>% 
  fill_gaps(count = 0) %>% 
  mutate('moving_average' = rollmean(count, 
                                     k = 3, 
                                     align = 'right', 
                                     fill = 'extend'))

count_year <- data %>% 
  mutate('year' = year(mdy(investigation_start_date))) %>% 
  group_by(year, diagnosis) %>% 
  summarize('count' = n())


table_data <- data %>% 
  filter(outbreak_name == '2021_012') %>% 
  select(investigation_start_date,
         patient_county, 
         patient_sex, 
         patient_race, 
         patient_age,
         patient_hispanic,
         pregnant_interview,
         incarcerated_12:num_transgender_partners
         ) %>% 
  mutate('date' = yearmonth(mdy(investigation_start_date)))
  
# Save Data ---------------------------------------------------------------

save(county_map_data, file = here('data', 'county_map_data.RData'))

save(count_year, file = here('data', 'count_year.RData'))

save(moving_average_data, file = here('data', 'moving_average_data.RData'))

save(county_incidence_data, file = here('data', 'county_incidence_data.RData'))

save(outbreak_moving_average_data, file = here('data', 
                                               'outbreak_moving_average_data.RData'))

# Create new data and move old data ---------------------------------------

old_data <- read_csv(here('old_data', 'syphilis_data.csv'))

if (ncol(old_data) != ncol(data)){
  print('Old data and new data have different number of variables.\nHave research analyst manually move data.')
  
} else if (isTRUE(all.equal(old_data, data))){
  print('Old data and new data are exactly the same. Stopping data movement.')
  
} else {
  file.remove(here('old_data', 'syphilis_data.csv'))
  file.remove(here('old_data', 'date_created.txt'))
  file.copy(from = here('new_data', 'syphilis_data.csv'), 
            to = here('old_data', 'syphilis_data.csv'))
  
  file.copy(from = here('new_data', 'date_created.txt'), 
            to = here('old_data', 'date_created.txt'))
  
  file.remove(here('new_data', 'syphilis_data.csv'))
  file.remove(here('new_data', 'date_created.txt'))
  
  file.copy(from = new_data_path, 
            to = here('new_data', 'syphilis_data.csv'))
  
  now_time <- now(tzone = 'America/Denver')
  
  now_string <- format(now_time, "%Y-%m-%d %H:%M:%S %Z")
  
  writeLines(now_string, here('new_data', 'date_created.txt'))
}




