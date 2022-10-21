library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title = "Vitaminator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",         tabName = "dashboard",      icon = icon("dashboard")),
      menuItem("Immun System",      tabName = "immunSystem",    icon = icon("fa-regular fa-shield-virus")),
      menuItem("Heart",             tabName = "immunSystem",    icon = icon("fa-light fa-heart")),
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
      DT::dataTableOutput("table"),
      plotOutput('plot2')
    )),
    
    tabItem(tabName = "editData", fluidPage(
      
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
  
  
  output$table = DT::renderDataTable({g[order(-Date)]})
  
  output$modelTable = renderDataTable(enrichWithStatisticValues(getLatestMesurmentsByType())[order(-Date)])
  
  observeEvent(input$modelTable_rows_selected, {
    print(input$modelTable_rows_selected)
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
