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
  
  output$thirdPlot <- renderPlot({
    
    dd <- subset(d,Species==input$species)
    CA <- dd[["CA"]]

    attach(CA)
    
    exponential.model <- lm(log(IndWgt)~ log(LngtClas))
    #summary(exponential.model)
    
    lenvalues <- seq(min(LngtClas), max(LngtClas), 10)
    WeightsModelled <- exp(predict(exponential.model, list(LngtClas=lenvalues)))
    
    WeightModel <- exp(predict(exponential.model, LngtClas=LngtClas))
    CA$Diffs <- abs(IndWgt - WeightModel)
    
    detach(CA)
    
    IQRMutiplier = 3
    
    outliers <- CA[CA$Diffs > median(CA$Diffs) + IQRMutiplier*IQR(CA$Diffs),]
    
    
    plot(CA$LngtClas,CA$IndWgt,xlab="Length class (mm)",ylab="Weight (g)")
    points(outliers$LngtClas,outliers$IndWgt,col="red")
    lines(lenvalues,WeightsModelled,type="l",col="blue")
    
    
    
  })
  
  
})
