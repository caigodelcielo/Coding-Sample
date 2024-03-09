# Clean environment
rm(list=ls())

# Set Working Directory
setwd("~/GitHub/Coding-Sample")

# Set path
path <- "~/GitHub/Coding-Sample"

# Loading Libraries
library(shiny)
library(tidyverse)
library(wesanderson)
library(spData)
library(sf)
library(plotly)
library(shinyFeedback)

#-------------------------------Shiny App---------------------------------------#

# UI
ui <- fluidPage(
  useShinyFeedback(),
  titlePanel("Homicides and Shootings in Chicago"),
  sidebarLayout(
    sidebarPanel(
      img(src = "https://home.chicagopolice.org/wp-content/themes/cpd-bootstrap/img/CPD%20Logo.png",
          height = 90,
          width = 300), 
      selectInput("datasetInput", "Select Data:",
                  choices = c("Homicides", "Shootings")), # I googled how to select from different datasets
      textInput("yearInput", "Enter Year:", value = "2017")
    ),
      mainPanel(
        plotlyOutput("mapOutput", width = "100%", height = "80vh"),# I googled how to adjust the size of the plot because it looked weird
      uiOutput("feedbackWarning")
    )
  )
) 

# Server

server <- function(input, output) {
  
  districts_sf <- st_read("geo_export_abd0bfc6-8bdf-495f-b323-f2d06990ce40.shp") |>
    rename(district = dist_num) |>
    mutate(district = as.integer(district))
  
  observe({ #I had an error if I just left it as the code in class, I googled how to fix it
    is_valid_year <- as.numeric(input$yearInput) >= 2017 && as.numeric(input$yearInput) <= 2024
    feedbackWarning("yearInput", 
                    !is_valid_year, 
                    "Warning! Remember to select a year between 2017 and 2024")
  })
  
  homicides_r <- reactive({
    homicides_data <- read.csv("homicides.csv") |>
      filter(year == as.numeric(input$yearInput)) |>
      left_join(districts_sf, by = "district") 
    
    homicides_data
  })
  
  shootings_r <- reactive({
    shootings_data <- read.csv("shootings.csv") |>
      filter(year == as.numeric(input$yearInput)) |>
      left_join(districts_sf, by = "district") 
    
    shootings_data
  })
  
  output$mapOutput <- renderPlotly({
    selectedYear <- as.numeric(input$yearInput)
    
    dataset <- req(if(input$datasetInput == "Homicides") {
      homicides_r()
    } else {
      shootings_r()
    })
    
    fill_column <- if (input$datasetInput == "Homicides") {
      "total_homicides"
    } else {
      "total_shootings"
    }
    
    pal <- wes_palette("Zissou1", 100, type = "continuous")
    
    plt <- ggplot(data = dataset) +
        geom_sf(aes(fill = !!sym(fill_column), geometry = geometry)) + 
        theme_void() +
        labs(title = paste("Total", input$datasetInput, "per district - per year"),
             subtitle = "From 2017 to 2024",
             caption = "Source: Chicago Data Portal",
             fill = paste("Total", input$datasetInput)) +
      scale_fill_gradientn(colours = pal)
        
  ggplotly(plt)
  
  })
}


shinyApp(ui = ui, server = server)


### Shiny Link https://caigodelcielo.shinyapps.io/crime_app/
