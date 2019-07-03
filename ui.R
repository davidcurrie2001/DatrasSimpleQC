#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#



shinyUI(fluidPage(
  
  # Application title
  titlePanel("Simple DATRAS QC"),
  
  fluidRow(
    column(12,plotlyOutput("mainPlot"))
  )
  

))
