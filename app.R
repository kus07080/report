setwd("C:/Users/A/Desktop")

#packages we need
library(ggmap)
library(caret)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(tidyverse)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)


library(shiny)

#Load files
load(file = "report\\population.RData")

# Define UI 
ui <- fluidPage(
  titlePanel("Percentage of population by administrative district"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Population ratio by age
               youth population is age 1 to 14.
               producible population is age 15 to 64.
               elderly population is over age 65"),
      
      selectInput("age",
                   label= "Age Selection",
                   choices = c('youth','producible','elderly'),
                   selected="youth"),
      
    ),
    mainPanel(
      tabsetPanel(type="tab",
                  tabPanel("Data",tableOutput("pop")),
                  tabPanel("Summary",verbatimTextOutput("summ")),
                  tabPanel("Bar Chart",plotOutput("bar"))
                  ))
  )
      
)

# Define server logic ----
server <- function(input, output) {
 popreact<-reactive({
   pop[,c("city",input$age)]
 })
 
 output$pop<- renderTable({
   popreact()
 })
 
 output$summ <-renderPrint({
   summary(popreact())
 })
 
 output$bar<- renderPlot({
    if(input$age=='youth'){
       ggplot(data=pop,aes(x=city,y=youth, fill=youth))+
          geom_col(col='red')
       
    }else if(input$age == 'producible'){
       ggplot(pop, aes(x=city,y=producible, fill=producible))+
          geom_col(col='yellow')
       
    }else if(input$age =='elderly'){
       ggplot(pop, aes(x=city,y=elderly, fill=elderly))+
          geom_col(col='white')
    }
 })
}

# Run the app ----
shinyApp(ui = ui, server = server)

