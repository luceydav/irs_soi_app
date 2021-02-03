library(shiny)
library(shinydashboard)
library(data.table)
library(magrittr)
library(DT)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(fst)
library(shinycssloaders)

# Data
irs_app_data <- read_fst("irs_app.fst")
irs_app_data <- setDT(irs_app_data, key = "zipcode")

header <- dashboardHeader(title = "IRS SOI Tax by Zip Dashboard")
  
sidebar <- dashboardSidebar(
          
          ## Sidebar content
          dashboardSidebar(
            sidebarMenu(
              menuItem("Table", tabName = "table", icon = icon("table")),
              menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o")),
              selectInput(
                    "State",
                    label = "Choose State",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    selected = "",
                    multiple = TRUE,
                    selectize = TRUE
                  ),
              selectInput(
                    "County",
                    label = "Choose County",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    #options = list(max.options = 20),
                    selected = "",
                    multiple = TRUE
                  ),
              selectInput(
                    "City",
                    label = "Choose City",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    selected = "",
                    multiple = TRUE,
                    selectize = TRUE
                  ),
              selectInput(
                    "Zipcode",
                    label = "Choose Zipcode",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    selected = "",
                    multiple = TRUE,
                    selectize = TRUE
                  ),
              selectInput(
                    "AGI",
                    label = "Choose Income",
                    choices = unique(irs_app_data$agi_level),
                    #options = list(`actions-box` = TRUE),
                    selected = unique(irs_app_data$agi_level),
                    multiple = TRUE
                  )
                )
          ))
        
body <- dashboardBody(
            
            tabItems(
              # First tab content
              tabItem(tabName = "table",
                      fluidRow(
                        shinydashboard::box(dataTableOutput("Table"), width = 12)
                        )),
              tabItem(tabName = "charts",
                      fluidRow(
                        shinydashboard::box(plotlyOutput("AGI_Level"), width = 12)
                      ),
                      fluidRow(
                        shinydashboard::box(plotlyOutput("TaxRate"), width = 12)
                      ))
            ))

server <- function(input, output, session) {
  
    data <- irs_app_data
  
    # Adjust County choices based on State selected
    observeEvent(input$State, {
      choices <- unique(data$state)
      updateSelectizeInput(
        session,
        inputId = "State",
        choices = choices,
        selected = choices,
        server = TRUE)
    })
    
    # Adjust County choices based on State selected
    observeEvent(input$County, {
      choices <- 
        unique(data$county)
      updateSelectizeInput(
        session,
        inputId = "County",
        choices = choices,
        selected = choices,
        server = TRUE)
    })
    
    # Adjust City choices based on County selected
    observeEvent(input$City, {
      choices <- unique(data$post_office_city)
      updateSelectizeInput(
        session,
        inputId = "City",
        choices = choices,
        selected = choices,
        server = TRUE)
    })
    
    observeEvent(input$Zipcode, {
      choices <- unique(data$zipcode)
      updateSelectizeInput(
        session,
        inputId = "Zipcode",
        choices = choices,
        selected = choices,
        server = TRUE)
    })
    
    observe({
      
      if( all(sapply(list(input$State, input$County, input$City, input$Zipcode), is.null))) {
        
        df <- reactive({
          data

        })
        
      } else {
        
        df <- reactive({
            data[state %chin% input$State &
                         county %chin% input$County &
                         city %chin% input$City &
                         zipcode %chin% input$Zipcode &
                         agi_level %chin% input$AGI]
        })
        
      }
    
    })
    
    output$Table <- renderDT({
      
      # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
      datatable(
        df()[,
             .(
               tot_agi = sum(as.numeric(a00100), na.rm = TRUE) / 1000000,
               tot_tax = sum(as.numeric(total_tax), na.rm = TRUE) /
                 1000000,
               tot_returns = sum(as.numeric(n1), na.rm = TRUE) / 1000000,
               unique_zips = length(unique(zipcode))
             ),
             by = year],
        colnames = c(
          "Year",
          "Total AGI ($B)",
          "Federal Tax ($B)",
          "Total Returns (m)",
          "Unique Zips"
        ),
        options =
          list(
            pageLength = 15,
            scrollY = TRUE,
            dom = 't'
          ),
        rownames = FALSE
      ) %>%
        formatRound(
          columns = c(2:4),
          mark = ",",
          digits = 1
        ) %>%
        formatRound(
          columns = 5,
          mark = ",",
          digits = 0
        )
    })

    output$AGI_Level <- renderPlotly({
      df()[,
           {
             agi_sum = sum(a00100, na.rm = TRUE)
             total = sum(n1, na.rm = TRUE)
             agi_return = agi_sum / total
             list(`Income Amount` = agi_return ,
                  `Returns` = total)
           },
           by = .(Year = year,
                  `Income Group` = agi_level)][, 
          plot_ly(
            .SD,
            x = ~ Year,
            y = ~ `Income Amount`,
            color =  ~ `Income Group`,
            type = "scatter",
            mode = "lines",
            showlegend = F,
            fill = ~ '',
            hoverinfo = 'text',
            text = ~ paste(
              '</br> Year: ',
              Year,
              '</br> After Tax Income: ',
              paste0("$", format(`Income Amount`,
                                 digits = 2), "k"),
              '</br> Income Group: ',
              `Income Group`,
              '</br> # of Returns: ',
              format(
                `Returns`,
                big.mark = ",",
                digits = 2,
                scientific = FALSE
              )
            )
          ) %>%
            add_markers(size = ~ `Returns`,
                        mode = "markers")]
    })

    output$TaxRate <- renderPlotly({
      df()[,
           {
             agi_sum = sum(a00100 , na.rm = TRUE)
             tot_tax = sum(total_tax, na.rm = TRUE)
             tax_rate = tot_tax / agi_sum
             total = sum(n1 , na.rm = TRUE)
             list(`Returns` = total,
                  `Tax Rate` = tax_rate)
           },
           by = .(Year = year, `Income Group` = agi_level)][, 
          plot_ly(
            .SD,
            x = ~ Year,
            y = ~ `Tax Rate`,
            color =  ~ `Income Group`,
            type = "scatter",
            mode = "lines",
            fill = ~'',
            showlegend = F,
            hoverinfo = 'text',
            text = ~ paste(
              '</br> Year: ',
              Year,
              '</br> Tax Rate: ',
              paste0(format(`Tax Rate` * 100,
                            digits = 2), "%"),
              '</br> Income Group: ',
              `Income Group`,
              '</br> # of Returns: ',
              format(
                `Returns`,
                big.mark = ",",
                digits = 0,
                scientific = FALSE
              )
            )
          ) %>%
            add_markers(size = ~ `Returns`,
                        mode = "markers")]
    })
}

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui, server)
