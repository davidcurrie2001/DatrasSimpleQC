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


DefaultText <- "Any"

# Use the filters on the data supplied
FilterData<-function(allData,filtersToUse){
  
  # Filter the data using the selected values
  filteredData <- allData
  
  if ("Survey" %in% colnames(filtersToUse)){
    selectedSurvey <- as.character(filtersToUse$Survey)
    if (selectedSurvey != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, Survey==selectedSurvey)
    }
  }
  
  if ("Year" %in% colnames(filtersToUse)){
    selectedYear <- as.character(filtersToUse$Year)
    if (selectedYear != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, Year==selectedYear)
    }
  }
  
  if ("Quarter" %in% colnames(filtersToUse)){
    selectedQuarter <- as.character(filtersToUse$Quarter)
    if (selectedQuarter != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, Quarter==selectedQuarter)
    }
  }
  
  if ("HaulNo" %in% colnames(filtersToUse)){
    selectedHaul <- as.character(filtersToUse$HaulNo)
    if (selectedHaul != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, HaulNo==selectedHaul)
    }
  }
  
  if ("ScientificName_WoRMS" %in% colnames(filtersToUse)){
    selectedSpecies <- as.character(filtersToUse$ScientificName_WoRMS)
    if (selectedSpecies != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, ScientificName_WoRMS==selectedSpecies)
    }
  }
  
  if ("Sex" %in% colnames(filtersToUse)){
    selectedSex <- as.character(filtersToUse$Sex)
    if (selectedSex != DefaultText){
      filteredData <- subset.DATRASraw(filteredData, Sex==selectedSex)
    }
  }
  
  filteredData
  
}

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
                             allData <- readICES(AllDataFile ,strict=TRUE)
                          } 
                          if (file.exists(myFilters)){
                             filters <- read.csv(myFilters, header = TRUE)
                          }
                        list(allData,filters)
                       }
  )
  
  #allData <- readICES(AllDataFile ,strict=TRUE)
  #filters <- read.csv(myFilters, header = TRUE)
  
  #DataAndFilters <- list(allData,filters)
  
  
  #head(d[["CA"]])
  #d <- readExchange("data/Exchange.zip")
  #speciesNames <- aggregate(RecordType ~ ScientificName_WoRMS, data = d[["CA"]], FUN = "length")
  #speciesList <- speciesNames[order(-speciesNames$RecordType)  , "ScientificName_WoRMS"]
  

  output$mainPlot <- renderPlotly({
    
    #dd <- subset(d,Species==input$species)
    #CA <- dd[["CA"]]
    
    d <-DataAndFilters()[[1]]
    f <-DataAndFilters()[[2]]
    
    #d <-DataAndFilters[[1]]
    #f <-DataAndFilters[[2]]

    dataToUse <- FilterData(d,f)
    
    #head(f)
    
    filterString <- ''
    
    # Get all the filter values
    for (i in colnames(f)){
      if (i!='X'){
        filterString <- paste(filterString,i,":", f[[i]], ",", sep ="" )
      }
    }
    print(filterString)  
    
    
    CA <- dataToUse[["CA"]]
    
    PlotTitle <- ""
    
    # get the title for the plot
    SpeciesNames <- unique(as.character(CA[,"ScientificName_WoRMS"]))
    #print(SpeciesNames)
    if (length(SpeciesNames)> 1){
      PlotTitle <- "Multiple species"
    } else {
      PlotTitle<-SpeciesNames
    }
    
    # Only try and plot the chart if we have single species with lengths and weights
    #if (PlotTitle!="Multiple species" &&  sum(!is.na(CA$IndWgt)) >0 && sum(!is.na(CA$LngtClas)) >0 ) {
    if (sum(!is.na(CA$IndWgt)) >0 && sum(!is.na(CA$LngtClas)) >0 ) {
        
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
      
      p<- plotly_empty()  %>%
        layout(title = PlotTitle, xaxis = list(title = 'Length Class'),yaxis = list(title = 'Weight'))
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
