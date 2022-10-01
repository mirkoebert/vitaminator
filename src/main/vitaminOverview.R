library(readr)
library(data.table)

Vitaminator_Data_Modell <- read_delim("./Vitaminator-Data-Modell.csv", 
                                      delim = ";", 
                                      escape_double = FALSE, 
                                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                      trim_ws = TRUE)
VitaminModel = data.table(Vitaminator_Data_Modell)

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
  
  y = g[ , median(Value), by = Type ]
  setkey(y, Type)
  x = x[y]
  
  y = g[ , mean(Value), by = Type ]
  setkey(y, Type)
  x = x[y]
  
  cn = colnames(x)
  cn[6] = "Count"
  cn[7] = "Median"
  cn[8] = "Mittelwert"
  colnames(x) = cn
 
  x = enricheWithBorders(x) 
  return(x)
}

enricheWithBorders = function(enrichedLatestValues) {
  x = enrichedLatestValues
  setkey(y, Type)
  x$Unit = NULL
  
  y = VitaminModel
  
  setkey(y, Name)
  x = x[y]
  
  return(x)
}
