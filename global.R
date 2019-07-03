library(shiny)
library(DATRAS)
library(plotly)

DefaultText <- "Any"

# File names
AllDataFile <- "data/DATRAS_Exchange_Data.csv"
#filteredFile <- "data/filteredData.rds"
myFilters <- "data/myFilters.csv"

# Use the filters on the data supplied
FilterData<-function(allData,filtersToUse){
  
  # Filter the data using the selected values
  filteredData <- allData
  
  # Try and filter the data by all vlaues in filtersToUse 
  for (i in colnames(filtersToUse)){
    filteredData <- filterDataByParameter(filteredData,filtersToUse,i)
  }
  
  filteredData
  
}

# Use the filter values to subset the DATRAS data
filterDataByParameter <- function(dataToFilter,filtersToUse,paramName){
  
  selectedValue <- ''
  dataToReturn <- dataToFilter
  
  if (paramName %in% colnames(filtersToUse)){
    selectedValue <- as.character(filtersToUse[1,paramName])
    if (selectedValue != DefaultText){
      
      conditionToCheck <- paste("dataToReturn <- subset.DATRASraw(dataToFilter,",paramName,"=='",selectedValue, "')",sep = "")
      #print(conditionToCheck)
      # Need to use eval and parse so we can dynamically build the command, otherwise the values passed to ... in the subset.DATRASraw 
      # function will be taken literally
      eval(parse(text=conditionToCheck))
    }
  }
  
  dataToReturn
}