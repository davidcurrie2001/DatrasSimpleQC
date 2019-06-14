library(DATRAS)

d <- readExchange("data/Exchange.zip")
speciesNames <- aggregate(RecordType ~ ScientificName_WoRMS, data = d[["CA"]], FUN = "length")
speciesList <- speciesNames[order(-speciesNames$RecordType)  , "ScientificName_WoRMS"]