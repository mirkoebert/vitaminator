library(shiny)
library(shinydashboard)
source("~/vitaminator/src/main/process.R")
source("~/vitaminator/src/main/rate.R")
source("~/vitaminator/src/main/vitaminOverview.R")

ui <- dashboardPage(
  dashboardHeader(title = "Vitaminator"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem("Immun System",      tabName = "immunSystem",    icon = icon("th")),
      menuItem("Heart",             tabName = "immunSystem",    icon = icon("th")),
      menuItem("All Data Details",  tabName = "allDataDetails", icon = icon("th")),
      menuItem("All Data",          tabName = "allData",        icon = icon("th")),
      menuItem("Add New Data",      tabName = "addNewData",     icon = icon("th"))
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
    
    tabItem(
      tabName = "addNewData",
      mainPanel(
        textInput("company", "Company Name", ""),
        textInput("poc", "Point of Contact", ""),
        textInput("sales_rep", "Sales Rep", ""),
        sliderInput(
          'chanceofsale',
          "Probability of Sale",
          min = 1,
          max = 10,
          value = 5
        ),
        actionButton("submit", strong("Submit"))
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
}


# Run the application
shinyApp(ui = ui, server = server)
