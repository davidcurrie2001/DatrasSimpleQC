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
library(plotly)

# File names

AllDataFile <- "data/DATRAS_Exchange_Data.csv"
filteredFile <- "data/filteredData.rds"
myFilters <- "data/myFilters.csv"


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  

  # if (file.exists(filteredFile)){
  #   d <- readRDS(filteredFile)
  #   
  # } else {
  #   
  #   d <- readExchange("data/Exchange.zip")
  # }
  
  # Use reactive poll so that our data will be updated when the data/filteredData.rds is updated
  DataAndFilters <- reactivePoll(1000, session,
                       # This function returns the time that files were last modified
                       checkFunc = function() {
                         myValue <- ''
                         if (file.exists(AllDataFile)) {
                           myValue <- paste(myValue , file.info(AllDataFile)$mtime[1])
                         }
                         if (file.exists(myFilters)) {
                           myValue <- paste(myValue , file.info(myFilters)$mtime[1])
                         }
                           myValue
                       },
                       # This function returns the content the files
                       valueFunc = function() {
                         print('Loading data')
                        allData <- ''
                        filters <- ''
                          if (file.exists(AllDataFile)) {
                             allData <- readICES("data/DATRAS_Exchange_Data.csv" ,strict=TRUE)
                          } else if (file.exists(myFilters)){
                             filters <- read.csv(myFilters)
                          }
                        list(allData,filters)
                       }
  )
  
  
  #head(d[["CA"]])
  #d <- readExchange("data/Exchange.zip")
  #speciesNames <- aggregate(RecordType ~ ScientificName_WoRMS, data = d[["CA"]], FUN = "length")
  #speciesList <- speciesNames[order(-speciesNames$RecordType)  , "ScientificName_WoRMS"]
  

  output$mainPlot <- renderPlotly({
    
    #dd <- subset(d,Species==input$species)
    #CA <- dd[["CA"]]
    
    d <-DataAndFilters()[1]
    f <-DataAndFilters()[2]

    CA <- d[["CA"]]
    
    PlotTitle <- ""
    
    # get the title for the plot
    SpeciesNames <- unique(as.character(CA[,"ScientificName_WoRMS"]))
    if (length(SpeciesNames)> 1){
      PlotTitle <- "Multiple species"
    } else {
      PlotTitle<-SpeciesNames
    }
    
    # Only try and plot the chart if we have lengths and weights
    if (sum(is.na(CA$IndWgt)) >0 && sum(is.na(CA$LngtClas)) >0 ) {
    
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
      
      
      #plot(CA$LngtClas,CA$IndWgt,xlab="Length class (mm)",ylab="Weight (g)")
      #p <- plot_ly(data = CA, x = ~LngtClas, y = ~IndWgt, type="scatter", name = 'Data', mode = "markers", marker=list(color="black", size=3)) %>%
      #  add_trace(x = lenvalues, y = WeightsModelled, type="scatter",mode = "lines", name = 'Model') %>%
      #  add_trace(data = outliers, x = ~LngtClas, y = ~IndWgt, type="scatter", name = 'Outliers', mode = "markers", marker=list(color="red", size=7))
      p <- plot_ly(x = lenvalues, y = WeightsModelled, type="scatter",mode = "lines", name = 'Fit') %>%
        add_trace(data = CA, x = ~LngtClas, y = ~IndWgt, type="scatter", name = 'Data', mode = "markers", marker=list(color="black", size=3)) %>%
        add_trace(data = outliers, x = ~LngtClas, y = ~IndWgt, type="scatter", name = 'Outliers', mode = "markers", marker=list(color="rgba(255, 182, 193, .9)'", size=7, line=list(color="rgba(152, 0, 0, .8)", width=2))) %>%
        layout(title = PlotTitle, xaxis = list(title = 'Length Class'),yaxis = list(title = 'Weight'))
      
      #points(outliers$LngtClas,outliers$IndWgt,col="red")
      #identify(outliers$LngtClas,outliers$IndWgt)
      #lines(lenvalues,WeightsModelled,type="l",col="blue")
    
    } else {
      
      p<- plotly_empty()
    }
    
  })
  
  output$secondPlot <- renderPlot({
    
    dd <- subset(d,Species==input$species)
    dd <- addSpectrum(dd)
    plot(dd,col="red")
    
  })
  
  output$thirdPlot <- renderPlot({
    
    dd <- subset(d,Species==input$species)
    dd <- addSpectrum(dd)
    dd <- addWeightByHaul(dd)
    bubblePlot(dd)
    
  })
  
  
})
