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
    

    tabItem(tabName = "allDataDetails",  fluidPage(
      DT::dataTableOutput("modelTable"),
      plotOutput('timelinePlot')
    )),
    
    tabItem(tabName = "allData", fluidPage(
      DT::dataTableOutput("table")
    )),
    
    tabItem(tabName = "editData", fluidPage(
      # TODO add button to load demo data
      fileInput("file1", "Choose Data File (CSV)", accept = ".csv"),
      tableOutput("head")
    ))
    
  ))
)

server <- function(input, output) {
  g <- reactive({
    print(input$file1)
    req(input$file1)
    readDataFromFile(input$file1$datapath)
  })
  
  output$head <- renderTable({
    head(g(), 5)
  })
  
  output$plot2 <- renderPlot(print(p))
  
  output$vitaminDlatestBox <- renderInfoBox({
    type = "Vitamin D"
    latest = getLatestOfType(type, g())
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type, g())),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinTotalLatestBox <- renderInfoBox({
    type = "Gesamt Cholesterin"
    latest = getLatestOfType(type, g())
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type, g())),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinHdlLatestBox <- renderInfoBox({
    type = "HDL Cholesterin"
    latest = getLatestOfType(type, g())
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type, g())),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinLdlLatestBox <- renderInfoBox({
    type = "LDL Cholesterin"
    latest = getLatestOfType(type, g())
    infoBox(
      latest$Type,
      paste(latest$Value, latest$Unit),
      icon = icon(getLatestTrend(type, g())),
      color = rateVitaminValue(type, latest$Value),
      fill = TRUE
    )
  })
  
  output$cholesterinRatioTotalHdlLatestBox <- renderInfoBox({
    type = "Cholesterin Ratio Gesamt / HDL"
    latestValue = getLatestOfType("Gesamt Cholesterin", g())$Value / getLatestOfType("HDL Cholesterin", g())$Value
    infoBox(
      type,
      latestValue,
      icon = icon("sun"),
      color = rateVitaminValue(type, latestValue),
      fill = TRUE
    )
  })
  
  output$table = DT::renderDataTable({g()[order(-Date)]})
  
  output$modelTable = renderDataTable({
    modelTable = enrichWithStatisticValues(getLatestMesurmentsByType(g()), g())[order(-Date)]
  }, selection = "single")
  
  output$timelinePlot <- renderPlot({
    s = input$modelTable_rows_selected
    if(is.null(s)){s = 1}
    modelTable = enrichWithStatisticValues(getLatestMesurmentsByType(g()), g())[order(-Date)]
    mType = modelTable[s]$Type
    plot(
      g()[Type == mType,][order(-Date)]$Date, 
      g()[Type == mType,][order(-Date)]$Value,
      type = "b",
      xlab = 'Time',
      ylab = getYtitle(mType, modelTable[s]$Unit),
      ylim = c(0, getYMaxForPlot(mType, g())),
      col = 'green',
      lwd = 3
      )
    
    highBorderRed = VitaminModel[Name == mType]$`Border High Red`
    if(!is.na(highBorderRed)){
      abline(h = highBorderRed, col = "red", lty = 2)
    }
    highBorderY = VitaminModel[Name == mType]$`Border High Yellow`
    if(!is.na(highBorderY)){
      abline(h = highBorderY, col = "yellow", lty = 3)
    }
    lowBorderRed = VitaminModel[Name == mType]$`Border Low Red`
    if(!is.na(lowBorderRed)){
      abline(h = lowBorderRed, col = "red", lty = 2)
    }
    lowhBorderY = VitaminModel[Name == mType]$`Border Low Yellow`
    if(!is.na(lowhBorderY)){
      abline(h = lowhBorderY, col = "yellow", lty = 3)
    }
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
