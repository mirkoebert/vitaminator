library(readr)
library(data.table)

Vitaminator_Data_Modell <- read_delim("~/Desktop/Vitaminator-Data-Modell.csv", 
                                      delim = ";", 
                                      escape_double = FALSE, 
                                      locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                      trim_ws = TRUE)
VitaminModel = data.table(Vitaminator_Data_Modell)

rateVitaminValue = function(type, value) {
  typeBorders = VitaminModel[VitaminModel$Name == type, ]
  if (!is.na(typeBorders$`Border High Red`) && (value > typeBorders$`Border High Red`)) {
    return("red")
  }
  if (!is.na(typeBorders$`Border High Yellow`) && (value > typeBorders$`Border High Yellow`)) {
    return("yellow")
  }
  
  if (!is.na(typeBorders$`Border Low Red`) && (value < typeBorders$`Border Low Red`)) {
    return("red")
  }
  
  if (!is.na(typeBorders$`Border Low Yellow`) && (value < typeBorders$`Border Low Yellow`)) {
    return("yellow")
  }
  
  return("green")
}

