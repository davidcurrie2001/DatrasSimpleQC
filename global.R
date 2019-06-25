#library(DATRAS)

#filteredFile <- "data/filteredData.rds"

#if (file.exists(filteredFile)){
#  d <- readRDS(filteredFile)
#  
#} else {
#  
#  d <- readExchange("data/Exchange.zip")
#}
#head(d[["CA"]])
#d <- readExchange("data/Exchange.zip")
#speciesNames <- aggregate(RecordType ~ ScientificName_WoRMS, data = d[["CA"]], FUN = "length")
#speciesList <- speciesNames[order(-speciesNames$RecordType)  , "ScientificName_WoRMS"]