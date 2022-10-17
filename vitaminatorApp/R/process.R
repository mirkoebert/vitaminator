library(readr)
library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)



gesundheit_daten1 <- read_delim("./daten.csv", 
                                delim = ";", 
                                escape_double = FALSE, 
                                col_types = cols(Date = col_date(format = "%d.%m.%Y")), 
                                locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                trim_ws = TRUE)

g = data.table(gesundheit_daten1)
g = Filter(function(x)!all(is.na(x)), g)
vitaminD = g[Type == "Vitamin D"]
vitaminD = vitaminD[order(Date)]

ds = g[Type == "Vitamin D Stoffwechsel"]
ds = ds[order(Date)]

blutfett = g[g$Type %in% c("LDL Cholesterin", "HDL Cholesterin", "Gesamt Cholesterin" , "Triglyceride" )]
blutfett = blutfett[order(Date)]
blutfettWide = reshape(blutfett[ , c ("Date", "Type", "Value")], direction="wide", idvar = "Date", timevar = "Type")
blutfettWide[ , cholesterinFactor:=(blutfettWide$`Value.Gesamt Cholesterin` / blutfettWide$`Value.HDL Cholesterin`) ]

p = ggplot(vitaminD, aes(x=Date, y=Value, colour=Type))  + 
  geom_ribbon(aes(ymin=91, ymax=150), fill = "yellow", outline.type = "lower") + 
  geom_ribbon(aes(ymin=40, ymax=91), fill = "lightgreen", outline.type = "lower") + 
  geom_ribbon(aes(ymin=11, ymax=40), fill = "yellow", outline.type = "lower") + 
  geom_ribbon(aes(ymin=0, ymax=11), fill = "red", outline.type = "lower") + 
  geom_line() + 
  geom_point()
#print(p)



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

  
