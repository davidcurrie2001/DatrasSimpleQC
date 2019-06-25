#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simple DATRAS QC"),
  
  fluidRow(
    column(12,plotlyOutput("mainPlot"))
  )
  
  # Sidebar with a slider input for number of bins 
  #sidebarLayout(
    #sidebarPanel(
      #selectInput("species","Select a species",choices= c("Gadus morhua","Melanogrammus aeglefinus"))
      #selectInput("species","Select a species",choices= speciesList)
    #),
    
    # Show a plot of the generated distribution
    #mainPanel(
      #plotlyOutput("mainPlot")
      # ,plotOutput("secondPlot")
      # ,plotOutput("thirdPlot")
    #)
  #)
))
