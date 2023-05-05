library(shiny)
library(here)
library(tidyverse)
library(ggiraph)
library(patchwork)

last_updated <- 'blah'

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
  fluidRow(
    column(2, 
           selectInput('year', label = NULL, choices = unique(county_incidence_data$year)))
  ),
  fluidRow(
    column(4, ggiraphOutput('map_plot', height = '600px')
           ), 
    column(8, ggiraphOutput('ma_year_plots')
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
      theme(plot.title = element_text(size = 12.5))
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
  
  ma_year_plots <- reactive({
    plot1 <- moving_average_data %>% 
      ggplot(aes(x = month, y = moving_average)) + 
      geom_line(color = 'red',
                linewidth = 5.5) +
      geom_point_interactive(color = 'red',
                             aes(
                               data_id = month, 
                               tooltip = sprintf(
                                 'Month: %s\n90-day moving average: %s', 
                                 month, as.character(round(moving_average, 2))
                               )
                             ), 
                             size = 5) + 
      xlab('Date') + 
      ylab('90-day Average of Cases') +
      theme(text = element_text(size = 25))
    
    plot2 <- count_year %>% 
      ggplot(aes(x = year, y = count)) + 
      geom_col_interactive(aes(fill = diagnosis, 
                               data_id = diagnosis, 
                               tooltip = sprintf(
                                 'Diagnosis: %s\nCase Count: %s', diagnosis, 
                                 as.character(round(count, 2))
                               ))) +
      theme(text = element_text(size = 25)) +
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
    plot <- girafe(ggobj = ma_year_plots(), width_svg = 20, 
                   height_svg = 10)
  })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
