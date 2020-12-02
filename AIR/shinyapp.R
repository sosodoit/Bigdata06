# 환경설정 -----
library(tidyverse)
library(lubridate)
library(DT)
library(shiny)

pm10 <- readRDS(file = "./data/air_PM10_df.rds")
no2 <- readRDS(file = "./data/air_NO2_df.rds")

ui <- shinyUI(fluidPage(
  
  titlePanel("Air Quality - PM10"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput('aq_metric', 'Air Quality Metric', choices = c("PM10"="pm10",
                                                                 "NO2"="no2"), selected = "pm10")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2("PM10 Metrics by Province"),
      
      conditionalPanel(condition = "input.aq_metric == 'pm10'",
                       h3("PM10 Trend"),
                       plotOutput("pm10_plot"),
                       h3("PM10 Table"),
                       DT::dataTableOutput("pm10_table")
      ),
      conditionalPanel(condition = "input.aq_metric == 'no2'",
                       h3("NO2 Trend"),
                       plotOutput("no2_plot"),
                       h3("NO2 Table"),
                       DT::dataTableOutput("no2_table")
      )
    )
  )))

server <- shinyServer(function(input, output) {
  # PM10 -----   
  output$pm10_plot <- renderPlot({
    pm10 %>% 
      gather(측정소, pm10, -week, convert = TRUE) %>% 
      ggplot(aes(x=week, y=pm10, group=측정소, color=측정소)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%y-%m") +
      labs(x="") +
      theme_minimal()
  })
  
  output$pm10_table = DT::renderDataTable({
    
    pm10 %>% 
      datatable()
  })
  # no2 -----   
  output$no2_plot <- renderPlot({
    no2 %>% 
      gather(측정소, no2, -week, convert = TRUE) %>% 
      ggplot(aes(x=week, y=no2, group=측정소, color=측정소)) +
      geom_line() +
      geom_point() +
      scale_x_date(date_labels = "%y-%m") +
      labs(x="") +
      theme_minimal()
  })
  
  output$no2_table = DT::renderDataTable({
    
    no2 %>% 
      datatable()
  })
  
})

shinyApp(ui=ui,server=server)
