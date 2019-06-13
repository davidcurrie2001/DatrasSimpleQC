#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simple DATRAS QC"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("species","Select a species",choices= c("Gadus morhua","Melanogrammus aeglefinus"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("mainPlot")
       ,plotOutput("secondPlot")
    )
  )
))
