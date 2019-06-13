#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DATRAS)
library(maps)
library(mapdata)


d <- readExchange("data/Exchange.zip")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$mainPlot <- renderPlot({
    
    dd <- subset(d,Species==input$species)
    dd <- addSpectrum(dd)
    plot(dd,col="red")
    
  })
  
  output$secondPlot <- renderPlot({
    
    dd <- subset(d,Species==input$species)
    dd <- addSpectrum(dd)
    dd <- addWeightByHaul(dd)
    bubblePlot(dd)
    
  })
  
})
