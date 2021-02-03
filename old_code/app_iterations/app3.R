library(shiny)
library(dplyr)
library(DT)

data <- mtcars
SubRegionList <- unique(data$cyl)
LCCList <- unique(data$gear)
ENBIDList <- unique(data$am)
SiteNumberList <- unique(data$vs)
# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("KPI DrillDown"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    selectizeInput("SubRegionInput", "SubRegion/cyl", SubRegionList ,selected = NULL, multiple = TRUE),
    uiOutput("LCCInput"),
    uiOutput("ENBIDInput"),
    uiOutput("SiteNumInput"),
    uiOutput("Search"),
    
    mainPanel(
      verbatimTextOutput("view")
    )
  )
)

# Define server logic required 
server <- function(input, output, session) {
  SiteInfo <- data
  # temp <- ""
  observe({
    if (!is.null(input$SubRegionInput)){
      subRegionSelected <- input$SubRegionInput
      ## Create a temp dataset with the selected sub regions.
      temp <- SiteInfo[SiteInfo$cyl %in% subRegionSelected, ]
      
      ## Push the newly created selectizeInput to UI
      output$LCCInput <- renderUI({
        selectizeInput("LCCInput", "LCC/gear", unique(temp$gear), selected = NULL, multiple = TRUE)
      })
      output$ENBIDInput <- renderUI({
        selectizeInput("ENBIDInput", "ENBID/am", unique(temp$am),selected = NULL, multiple = TRUE)
      })
      output$SiteNumInput <- renderUI({
        selectizeInput("SiteNumInput", "SiteNumber/vs", unique(temp$vs), selected = NULL, multiple = TRUE)
      })
      output$Search <- renderUI({
        actionButton("Search", "Search")
      })
      
      ## Function that linked to the actionButton
      display <- eventReactive(input$Search,{
        temp <- SiteInfo[SiteInfo$cyl %in% input$SubRegionInput, ]
        # ## manually change all the empty searching parameter to select all in order to avoid filtering to NA
        LCC <- input$LCCInput
        if (is.null(input$LCCInput)){LCC <- unique(temp$gear)}
        ENBID <- input$ENBIDInput
        if (is.null(input$ENBIDInput)){EBVID <- unique(temp$am)}
        SiteNum <- input$SiteNumInput
        if (is.null(input$SiteNumInput)){LCC <- unique(temp$vs)}
        
        ## Dplyr::filter data
        temp <- temp %>% 
          filter(gear %in% LCC & am %in% ENBID & vs %in% SiteNum)
        temp
      })
      
      ## Run the actionButton
      output$view <- renderPrint({
        display()
      })
      
    } else {
      ## Display waht the data looks like when no Sub Region is selected 
      output$view<- renderPrint(data)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)