library(readr)
library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)



g <- read_delim("./daten.csv", 
                                delim = ";", 
                                escape_double = FALSE, 
                                col_types = cols(Date = col_date(format = "%d.%m.%Y")), 
                                locale = locale(decimal_mark = "."), 
                                trim_ws = TRUE)

g = data.table(g)
g = Filter(function(x)!all(is.na(x)), g)


getLatestOfType = function(type){
  typeOnly = g[Type == type]
  typeLatest = typeOnly[order(-Date)][1,]  
}


getLatestTrend = function(type){
  typeOnly = g[Type == type]
  typeSecondLatest = typeOnly[order(-Date)][2,]
  typeLatest = typeOnly[order(-Date)][1,]
  if (typeLatest$Value > typeSecondLatest$Value){
    return("arrow-up")
  } else if (typeLatest$Value == typeSecondLatest$Value) {
    return("arrow-right")
  }
  return("arrow-down")
}

getYtitle = function(mType, mUnit){
  result = mType
  if (!is.na(mUnit)){
    result = paste(mType,'in', mUnit)
  }
  return(result)
}

getYMaxForPlot = function(mType){
  highBorder = VitaminModel[Name == mType]$`Border High Red`
  maxValue = max(g[Type == mType]$Value)
  
  return(max(c(highBorder, maxValue), na.rm = T) * 1.2)
}
