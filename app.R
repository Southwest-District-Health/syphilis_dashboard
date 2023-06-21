library(shiny)
library(here)
library(tidyverse)
library(ggiraph)
library(patchwork)
library(tsibble)
library(knitr)
library(kableExtra)
library(lubridate)

last_updated <- "2023-06-21 12:24:48 MDT"

new_date <- readLines(here('new_data', 'date_created.txt'))
old_date <- readLines(here('old_data', 'date_created.txt'))

# App Parameters ----------------------------------------------------------

# County line colors
county_colors <- c(
  "Owyhee" = "#deffff",
  "Canyon" = "#f0daea",
  "Gem" = "#fdf5cc",
  "Payette" = "#dff6db",
  "Washington" = "#ffbfbf",
  "Adams" = "#ffe4ca"
)


# Load data ---------------------------------------------------------------
load(here('data', 'count_year.RData'))
load(here('data', 'county_incidence_data.RData'))
load(here('data', 'county_map_data.RData'))
load(here('data', 'moving_average_data.RData'))
load(here('data', 'outbreak_moving_average_data.RData'))

new_data <- read_csv(here('new_data', 'syphilis_data.csv'))
new_date <- readLines(here('new_data', 'date_created.txt'))

old_data <- read_csv(here('old_data', 'syphilis_data.csv'))
old_date <- readLines(here('old_data', 'date_created.txt'))
# Create static table -----------------------------------------------------

create_big_table <- function(data){
  
  data_year <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate('year' = year(mdy(investigation_start_date))) %>% 
    select(year) %>% 
    group_by(year) %>% 
    summarize('n' = n()) %>% 
    mutate('year' = as.character(year), 
           'percent' = (n/sum(n)) * 100) %>% 
    select('variable' = year, n, percent)
  
  sex <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    select(patient_sex) %>% 
    mutate(patient_sex = str_replace(patient_sex, 
                                     'Cisgender/Not Transgender', 
                                     'Unknown')) %>% 
    group_by(patient_sex) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = patient_sex, n, percent)
  
  race <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(patient_race = replace_na(patient_race, 'Unknown')) %>% 
    group_by(patient_race) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = patient_race, n, percent)
  
  ethnicity <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(patient_hispanic = replace_na(patient_hispanic, 'Unknown')) %>% 
    group_by(patient_hispanic) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = patient_hispanic, n, percent)
  
  age <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate('age_group' = case_when(
      between(patient_age, 0, 10) ~ '0-4', 
      between(patient_age, 5, 9) ~ '5-9', 
      between(patient_age, 10, 14) ~ '10-14', 
      between(patient_age, 15, 19) ~ '15-19', 
      between(patient_age, 20, 24) ~ '20-24', 
      between(patient_age, 25, 29) ~ '25-29', 
      between(patient_age, 30, 34) ~ '30-34', 
      between(patient_age, 35, 39) ~ '35-39', 
      between(patient_age, 40, 44) ~ '40-44', 
      between(patient_age, 45, 49) ~ '45-49', 
      between(patient_age, 50, 54) ~ '50-54', 
      between(patient_age, 55, 59) ~ '55-59', 
      between(patient_age, 60, 64) ~ '60-64', 
      patient_age >= 65 ~ '65+'
    ), 
    age_group = factor(age_group, levels = c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65+'))) %>% 
    group_by(age_group, .drop = FALSE) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = age_group, n, percent)
  
  pregnant <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    select(pregnant_interview) %>% 
    mutate(pregnant_interview = replace_na(pregnant_interview, 'No')) %>% 
    group_by(pregnant_interview) %>% 
    summarize('n' = n()) %>%
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = pregnant_interview, n, percent)
  
  sex_male_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>%
    mutate(sex_male_12 = replace_na(sex_male_12, 'Unknown'),
           sex_male_12 = str_replace(sex_male_12, ".*Yes.*", 'Yes'),
           sex_male_12 = str_replace(sex_male_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown'), 
           sex_male_12 = str_replace(sex_male_12, '.*No*.', 'No'),
           sex_male_12 = factor(sex_male_12, levels = c('No', 'Yes', 'Unknown'))) %>% 
    select(sex_male_12) %>% 
    group_by(sex_male_12, .drop = FALSE) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = sex_male_12, n, percent) %>% 
    arrange(desc(variable))
  
  sex_female_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    select(sex_female_12) %>% 
    mutate(sex_female_12 = replace_na(sex_female_12, 'Unknown'),
           sex_female_12 = str_replace(sex_female_12, ".*Yes.*", 'Yes'),
           sex_female_12 = str_replace(sex_female_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown'), 
           sex_female_12 = str_replace(sex_female_12, '.*No*.', 'No'),
           sex_female_12 = factor(sex_female_12, levels = c('No', 'Yes', 'Unknown'))) %>% 
    group_by(sex_female_12, .drop = FALSE) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = sex_female_12, n, percent) %>% 
    arrange(desc(variable))
  
  sex_transgender_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(sex_transgender_12 = replace_na(sex_transgender_12, 'Unknown'),
           sex_transgender_12 = str_replace(sex_transgender_12, ".*Yes.*", 'Yes'),
           sex_transgender_12 = str_replace(sex_transgender_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown'), 
           sex_transgender_12 = str_replace(sex_transgender_12, '.*No*.', 'No'),
           sex_transgender_12 = factor(sex_transgender_12, levels = c('No', 'Yes', 'Unknown'))) %>% 
    group_by(sex_transgender_12, .drop = FALSE) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = sex_transgender_12, n, percent) %>% 
    arrange(desc(variable))
  
  sex_anonymous_partner_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(sex_anonymous_partner_12 = replace_na(sex_anonymous_partner_12, 'Unknown'),
           sex_anonymous_partner_12 = str_replace(sex_anonymous_partner_12, ".*Yes.*", 'Yes'),
           sex_anonymous_partner_12 = str_replace(sex_anonymous_partner_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown'), 
           sex_anonymous_partner_12 = str_replace(sex_anonymous_partner_12, '.*No*.', 'No'),
           sex_anonymous_partner_12 = factor(sex_anonymous_partner_12, levels = c('No', 'Yes', 'Unknown'))) %>%  
    group_by(sex_anonymous_partner_12, .drop = FALSE) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = sex_anonymous_partner_12, n, percent) %>% 
    arrange(desc(variable))
  
  incarcerated_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(incarcerated_12 = replace_na(incarcerated_12, 'Unknown'), 
           incarcerated_12 = str_replace(incarcerated_12, 'unknown', 'Unknown'), 
           incarcerated_12 = str_replace(incarcerated_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown')) %>% 
    group_by(incarcerated_12) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = incarcerated_12, n, percent) %>% 
    arrange(desc(variable))
  
  idu_12 <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(idu_12 = replace_na(idu_12, 'Unknown'), 
           idu_12 = str_replace(idu_12, '(.*Did not ask.*|.*Refused.*)', 'Unknown')) %>% 
    group_by(idu_12) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = idu_12, n, percent) %>% 
    arrange(desc(variable))
  
  partner_internet <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(met_sex_partner_internet = replace_na(met_sex_partner_internet, 'Unknown'), 
           met_sex_partner_internet = str_replace(met_sex_partner_internet, '(.*Did not ask.*|.*Refused.*)', 'Unknown')) %>% 
    group_by(met_sex_partner_internet) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = met_sex_partner_internet, n, percent) %>% 
    arrange(desc(variable))
  
  hiv_result <<- data %>% 
    filter(outbreak_name == '2021_012') %>% 
    mutate(hiv_result = replace_na(hiv_result, 'Negative'), 
           hiv_result = str_replace(hiv_result, '(?i)negative.*', 'Negative'),
           hiv_result = str_replace(hiv_result, '(?i)positive.*', 'Positive')
    ) %>% 
    group_by(hiv_result) %>% 
    summarize('n' = n()) %>% 
    mutate('percent' = (n/sum(n)) * 100) %>% 
    select('variable' = hiv_result, n, percent)
  
  big_table <- data_year %>% 
    bind_rows(sex) %>% 
    bind_rows(race) %>% 
    bind_rows(ethnicity) %>% 
    bind_rows(age) %>% 
    bind_rows(pregnant) %>% 
    bind_rows(sex_male_12) %>% 
    bind_rows(sex_female_12) %>% 
    bind_rows(sex_transgender_12) %>% 
    bind_rows(sex_anonymous_partner_12) %>% 
    bind_rows(incarcerated_12) %>% 
    bind_rows(idu_12) %>% 
    bind_rows(partner_internet) %>% 
    bind_rows(hiv_result)
  
  
  return(big_table)
}

big_table <- create_big_table(new_data)

row_numbers <- data.frame('begin_row' = NA, 'end_row' = NA)

subtable_list <- list(data_year, sex, race, ethnicity, age, pregnant, sex_male_12, sex_female_12, 
                      sex_transgender_12, sex_anonymous_partner_12, incarcerated_12, idu_12, partner_internet, hiv_result)

for (table in 1:length(subtable_list)){
  this_table <- subtable_list[[table]]
  
  num_rows <- nrow(this_table)
  
  if (table == 1){
    first_row <- 1
    last_row <- num_rows
    
    row_numbers$begin_row[table] <- first_row
    row_numbers$end_row[table] <- last_row
  } else {
    last_table <- table - 1
    
    first_row <- row_numbers$end_row[last_table] + 1
    last_row <- num_rows + row_numbers$end_row[last_table]
    
    created_row <- data.frame('begin_row' = first_row, 'end_row' = last_row)
    
    row_numbers <- row_numbers %>% 
      bind_rows(created_row)
  }
}


old_big_table <- create_big_table(old_data)

if (nrow(old_big_table) != nrow(big_table)){
  stop('number of rows between old data table and new table are not equal.\n ')
}

big_table <- big_table %>% 
  mutate('old_percent' = old_big_table$percent,
         'delta_percent' = scales::percent(percent - old_percent, scale = 1, accuracy = 0.01L), 
         'percent' = scales::percent(percent, scale = 1, accuracy = 0.01L)) %>% 
  select(' ' = variable, 
         'Percentage' = percent, 
         '\U0394 Percent' = delta_percent)

final_table <- big_table %>% 
  kable(format = 'html', caption = 'Case Demographics') 
  

headers <- c('Year', 'Sex', 'Race', 'Ethnicity', 'Age', 'Pregnant at interview', 'Male partner last 12 months', 'Female partner last 12 months', 'Partner who is transgender last 12 months', 'Anonymous partner last 12 months', 'Incarcerated last 12 months', 'Persons who inject drugs last 12 months', 'Met partner over the internet', 
             'HIV status')

for (subtable in 1:length(headers)){
  final_table <- pack_rows(final_table, headers[subtable], row_numbers$begin_row[subtable], row_numbers$end_row[subtable])
}

final_table <- final_table %>%
  column_spec(1, width = "2in") %>%
  column_spec(2, width = "1in") %>%
  column_spec(3, width = "1in")

# App ---------------------------------------------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
  titlePanel(
    h1(paste(
      "Syphilis in Southwest Idaho, ",
      min(count_year$year),
      "-",
      max(count_year$year)
    ), style = "background-color:#004478;
                padding-left: 15px;
                color:#FFFFFF"),
    windowTitle = "Epidemiology - Southwest District Health"
  ),
  tabsetPanel(
    tabPanel('Public',   
   fluidRow(
      column(2, 
             selectInput('year', label = NULL, 
                         choices = unique(county_incidence_data$year)))
    ),
    fluidRow(
      column(4, ggiraphOutput('map_plot', height = '600px')
      ), 
      column(8, ggiraphOutput('ma_year_plots')
      )
      
    ), 
   fluidRow(paste0('Data last updated on: ', last_updated)
      
    )), 
    tabPanel('Professional',
             fluidRow(paste0('New data report run on: ', 
                             new_date, 
                             '. ', 
                             'Old data report run on: ', 
                             old_date, 
                             '.')),
             fluidRow(
               column(3, htmlOutput('this_table')),
               column(9, ggiraphOutput('outbreak_ggiraph'))
               
             )
             )

)
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create Map Incidence Plot
  map_plot <- reactive({
    county_map_data %>% 
      left_join(county_incidence_data) %>% 
      filter(year == input$year) %>% 
      ggplot() + geom_polygon_interactive(
        aes(
          data_id = county, 
          x = long, 
          y = lat, 
          fill = incidence_rate, 
          group = county, 
          color = county, 
          tooltip = sprintf(
            'Cumulative incidence rate per 10,000: %s\nCounty: %s', 
            ifelse(count < 5, "Count < 5", 
                   as.character((round(incidence_rate, digits = 2)))), 
            str_to_sentence(county)
          )
        ), linewidth = .75, hover_nearest = TRUE
      ) + 
      coord_fixed(1.3) +
      ggtitle(paste(
        "Cumulative Incidence Rate by County, ",
        min(count_year$year),
        "-",
        max(count_year$year)
      )) +
      scale_fill_gradient(low = "grey", high = "red", "Incidence\nRate") +
      scale_color_manual(
        values = county_colors, name = "County",
        guide = "none"
      ) +
      theme_void() + 
      theme(plot.title = element_text(size = 12.5), 
            legend.text = element_text(size = 14), 
            legend.title = element_text(size = 14))
  })
  
  output$map_plot <- renderggiraph({
    plot <- girafe(ggobj = map_plot(), width_svg = 4.25, height_svg = 6)
    plot <- girafe_options(
      plot,
      opts_hover(
        css = "stroke-width:5;"
      )
    )
  })
  
  ma_year_plot <- reactive({
    plot1 <- moving_average_data %>% 
      ggplot(aes(x = month, y = moving_average)) + 
      geom_line(color = 'red',
                linewidth = 5.5) +
      geom_point_interactive(color = 'red',
                             aes(
                               data_id = month, 
                               tooltip = sprintf(
                                 'Month: %s\n90-day moving average: %s', 
                                 as.character(month), as.character(round(moving_average, 2))
                               )
                             ), 
                             size = 5) + 
      xlab('Date') + 
      ylab('90-day Average of Cases') +
      theme(text = element_text(size = 30))
    
    plot2 <- count_year %>% 
      ggplot(aes(x = year, y = count)) + 
      geom_col_interactive(aes(fill = diagnosis, 
                               data_id = diagnosis, 
                               tooltip = sprintf(
                                 'Diagnosis: %s\nCase Count: %s', diagnosis, 
                                 as.character(round(count, 2))
                               ))) +
      theme(text = element_text(size = 30)) +
      ylab('Cases Count') + 
      xlab('Date') +
      guides(fill = guide_legend(title = 'Diagnosis')) +
      scale_fill_manual(values = c('#b3e0a6', '#87cc79', '#49964f', '#24693d', 
                                            '#194a2b', 
                                            '#6b8baa'))
    
    this_list <- list(plot1, plot2)
    
    design <- '
    A
    B'
    these_plots <- wrap_plots(this_list, design = design)
    
  })
  
  
  output$ma_year_plots <- renderggiraph({
    plot <- girafe(ggobj = ma_year_plot(), width_svg = 20, 
                   height_svg = 10)
    plot
  })

  output$this_table <- renderText({
    final_table
  })

  outbreak_plot <- reactive({
    outbreak_moving_average_data %>% 
      ggplot(aes(x = month, y = moving_average)) + 
      geom_line(color = 'red',
                linewidth = 5) +
      geom_point_interactive(color = 'red',
                             aes(
                               data_id = month, 
                               tooltip = sprintf(
                                 'Month: %s\n90-day moving average: %s', 
                                 as.character(month), as.character(round(moving_average, 2))
                               )
                             ), 
                             size = 4) + 
      xlab('Date') + 
      ylab('90-day Average of Cases') +
      theme_bw() + 
      theme(text = element_text(size = 35), 
            axis.text.x = element_text(angle = 90)) + 
      ggtitle('Outbreak cases 90-day moving average') 
      
  })
    
  output$outbreak_ggiraph <- renderggiraph({
    girafe(ggobj = outbreak_plot(), width_svg = 20, height_svg = 8)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
