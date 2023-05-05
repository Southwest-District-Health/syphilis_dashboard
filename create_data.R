library(tidyverse)
library(here)
library(maps)
library(lubridate)
library(tidycensus)
library(tsibble)
library(zoo)
source('get_data.R')


# Import Data -------------------------------------------------------------

data <- point_data_path(data_name = 'syphilis_data.csv') %>% 
  read_csv()

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

all_combs <- expand.grid(year = c(unique(year(mdy_hms(data$investigation_start_date)))), 
                         county = c('Adams', 
                                    'Canyon', 
                                    'Gem', 
                                    'Owyhee', 
                                    'Payette', 
                                    'Washington')) %>% 
  filter(!is.na(year))

county_incidence_data <- data %>% 
  mutate('year' = year(mdy_hms(investigation_start_date))) %>% 
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

start_date <- min(yearmonth(mdy_hms(data$investigation_start_date)))

end_date <- max(yearmonth(mdy_hms(data$investigation_start_date)))

all_dates <- seq(start_date, end_date, by = month(1))

moving_average_data <- data %>% 
  mutate('month' = yearmonth(mdy_hms(investigation_start_date))) %>% 
  group_by(month) %>% 
  summarize('count' = n()) %>% 
  mutate('moving_average' = rollmean(count, 
                                     k = 3, 
                                     align = 'right', 
                                     fill = 'extend'))


count_year <- data %>% 
  mutate('year' = year(mdy_hms(investigation_start_date))) %>% 
  group_by(year, diagnosis) %>% 
  summarize('count' = n())


# Save Data ---------------------------------------------------------------

save(county_map_data, file = here('data', 'county_map_data.RData'))

save(count_year, file = here('data', 'count_year.RData'))

save(moving_average_data, file = here('data', 'moving_average_data.RData'))

save(county_incidence_data, file = here('data', 'county_incidence_data.RData'))
