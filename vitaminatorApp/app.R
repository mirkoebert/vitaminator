library(shiny)
library(shinydashboard)
library(DTedit)

ui <- dashboardPage(
  dashboardHeader(title = "Vitaminator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",         tabName = "dashboard",      icon = icon("dashboard")),
      menuItem("Immun System",      tabName = "immunSystem",    icon = icon("th")),
      menuItem("Heart",             tabName = "immunSystem",    icon = icon("th")),
      menuItem("All Data Details",  tabName = "allDataDetails", icon = icon("th")),
      menuItem("All Data",          tabName = "allData",        icon = icon("th")),
      menuItem("Edit Data",         tabName = "editData",       icon = icon("th"))
    )
  ),
  dashboardBody(tabItems(
    tabItem(
      tabName = "dashboard",
      fluidRow(
        infoBoxOutput("vitaminDlatestBox"),
        infoBoxOutput("cholesterinTotalLatestBox"),
        infoBoxOutput("cholesterinHdlLatestBox"),
        infoBoxOutput("cholesterinLdlLatestBox"),
        infoBoxOutput("cholesterinRatioTotalHdlLatestBox")
      )
    ),
    

    tabItem(tabName = "allDataDetails", fluidRow(column(
      12, dataTableOutput('modelTable')
    ))),
    
    tabItem(tabName = "allData", fluidPage(
      dataTableOutput('table'),
      plotOutput('plot2')
    )),
    
    tabItem(tabName = "editData", fluidPage(
      uiOutput('mycontacts')
    )),
    
    tabItem(
      tabName = "addNewData",
      mainPanel(
        selectInput("type", "Type", VitaminModel$Name),
        textInput("value", "Wert", ""),
        dateInput("date", "Date",  value = Sys.Date()),
        actionButton("submit", strong("Save"), icon("refresh"))
      )
    )
  ))
)

server <- function(input, output) {
  output$plot1 <- renderPlot(print(p))
  
  output$plot2 <- renderPlot(print(p))
  
  output$vitaminDlatestBox <- renderInfoBox({
    type = "Vitamin D"
    latest = getLatestOfType(type)
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type)),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinTotalLatestBox <- renderInfoBox({
    type = "Gesamt Cholesterin"
    latest = getLatestOfType(type)
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type)),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinHdlLatestBox <- renderInfoBox({
    type = "HDL Cholesterin"
    latest = getLatestOfType(type)
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type)),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinLdlLatestBox <- renderInfoBox({
    type = "LDL Cholesterin"
    latest = getLatestOfType(type)
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type)),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  
  output$cholesterinRatioTotalHdlLatestBox <- renderInfoBox({
    type = "Cholesterin Ratio Gesamt / HDL"
    latestValue = getLatestOfType("Gesamt Cholesterin")$Value / getLatestOfType("HDL Cholesterin")$Value
    infoBox(
      type,
      latestValue,
      icon = icon("sun"),
      color = rateVitaminValue(type, latestValue),
      fill = TRUE
    )
  })
  
  
  output$table = renderDataTable(g[order(-Date)])
  
  output$modelTable = renderDataTable(enrichWithStatisticValues(getLatestMesurmentsByType())[order(-Date)])
  
  #observeEvent(input$modelTable_rows_selected, {
  #  str(input$table1_rows_selected)
  #})
  
  mydata <- g
  
  ##### Callback functions.
  my.insert.callback <- function(data, row) {
    mydata <- rbind(data, mydata)
    return(mydata)
  }
  
  my.update.callback <- function(data, olddata, row) {
    mydata[row,] <- data[1,]
    return(mydata)
  }
  
  my.delete.callback <- function(data, row) {
    mydata <- mydata[-row,]
    return(mydata)
  }
  
  ##### Create the DTedit object
  DTedit::dtedit(input, output,
                 name = 'mycontacts',
                 thedata = mydata,
                 edit.cols = colnames(g),
                 edit.label.cols = colnames(g),
                 input.types = c(Person='textAreaInput'),
                 view.cols = colnames(g),
                 callback.update = my.update.callback,
                 callback.insert = my.insert.callback,
                 callback.delete = my.delete.callback)
  
}


# Run the application
shinyApp(ui = ui, server = server)