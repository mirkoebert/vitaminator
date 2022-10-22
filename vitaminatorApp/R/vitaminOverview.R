library(readr)
library(data.table)

VitaminModel <- read_delim("./Vitaminator-Data-Modell.csv", 
                                      delim = ";", 
                                      escape_double = FALSE, 
                                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                      trim_ws = TRUE)
VitaminModel = data.table(VitaminModel)

getLatestOfType = function(type){
  typeOnly = g[Type == type]
  typeLatest = typeOnly[order(-Date)][1,]  
}


getLatestMesurmentsByType = function() {
  return(unique(g[order(-Date)], by="Type"));
}


enrichWithStatisticValues = function(latestValues) {
  x = latestValues
  setkey(x, Type)
  
  y = g[ , .N, by = Type ]
  setkey(y, Type)
  x = x[y]
  cn = colnames(x)
  cn[length(cn)] = "Count"
  colnames(x) = cn
  
  y = g[ , median(Value), by = Type ]
  setkey(y, Type)
  x = x[y]
  cn = colnames(x)
  cn[length(cn)] = "Median"
  colnames(x) = cn
  
  y = g[ , mean(Value), by = Type ]
  setkey(y, Type)
  x = x[y]
  cn = colnames(x)
  cn[length(cn)] = "Mittelwert"
  colnames(x) = cn
  
 
  x = enricheWithBorders(x) 
  return(x)
}

enricheWithBorders = function(enrichedLatestValues) {
  x = enrichedLatestValues
  setkey(x, Type)
  x$Unit = NULL
  
  y = VitaminModel
  setkey(y, Name)
  x = x[y]
  
  return(x)
}
