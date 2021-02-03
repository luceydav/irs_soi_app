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

header <- dashboardHeader(title = "IRS Tax Dashboard")
  
sidebar <- dashboardSidebar(
          
          ## Sidebar content
          dashboardSidebar(
            sidebarMenu(
              menuItem("Table", tabName = "table", icon = icon("table")),
              menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o")),
              selectizeInput(
                    "State",
                    label = "Choose State",
                    choices = c("All", unique(irs_app_data$state)),
                    #options = list(`actions-box` = TRUE),
                    selected = "All",
                    multiple = TRUE
                  ),
              selectizeInput(
                    "County",
                    label = "Choose County",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    options = list(maxOptions = 3500),
                    selected = "All",
                    multiple = TRUE
                  ),
              selectizeInput(
                    "City",
                    label = "Choose City",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    options = list(maxOptions = 26000),
                    selected = "All",
                    multiple = TRUE
                  ),
              selectizeInput(
                    "Zipcode",
                    label = "Choose Zipcode",
                    choices = NULL,
                    #options = list(`actions-box` = TRUE),
                    options = list(maxOptions = 35000),
                    selected = "All",
                    multiple = TRUE
                  ),
              selectInput(
                    "AGI",
                    label = "Choose Income",
                    choices = c("All", unique(irs_app_data$agi_level)),
                    #options = list(`actions-box` = TRUE),
                    selected = "All",
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
  
    state <- reactive({
      req(input$State)
      if ("All" %chin% input$State) {
        irs_app_data
      } else {
        irs_app_data[state %chin% input$State]
      }
    })
  
    # Adjust County choices based on State selected
    observeEvent(state(), {
      choices <- 
        unique(state()$county)
      updateSelectizeInput(
        session,
        inputId = "County",
        choices = c("All", choices),
        selected = "All",
        server = TRUE)
    })
    
    county <- reactive({
      req(input$County)
      if ("All" %chin% input$County){
        state()[county %chin% setdiff(unique(county), "All")]
      } else {
        state()[county %chin% input$County]
      }
    })
    
    # Adjust City choices based on County selected
    observeEvent(county(), {
      choices <- unique(county()$post_office_city)
      updateSelectizeInput(
        session,
        inputId = "City",
        choices = c("All", choices),
        selected = "All",
        server = TRUE)
    })
    
    city <- reactive({
      req(input$City)
      if ("All" %chin% input$City){
        county()[post_office_city %chin% setdiff(unique(post_office_city), "All")]
      } else {
        county()[post_office_city %chin% input$City]
      }
    })
    
    observeEvent(city(), {
      choices <- unique(city()$zipcode)
      updateSelectizeInput(
        session,
        inputId = "Zipcode",
        choices = c("All", choices),
        selected = "All",
        server = TRUE)
    })
    
    zipcode <- reactive({
      req(input$Zipcode)
      if ("All" %chin% input$Zipcode){
        city()[zipcode %chin% setdiff(unique(zipcode), "All")]
      } else {
        city()[zipcode %chin% input$Zipcode]
      }
    })  
    
    df <- reactive({
      req(input$AGI)
      if ("All" %chin% input$AGI){
        zipcode()[agi_level %chin% setdiff(unique(agi_level), "All")]
      } else {
        zipcode()[agi_level %chin% input$AGI]
      }
    })
    
    output$Table <- renderDT({
      
      if( length(unique(df()$zipcode)) < 50) {
        digits <- 3
      } else {
        digits <- 1
      }
      
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
          columns = c(2:3),
          mark = ",",
          digits = digits
        )  %>%
        formatRound(
          columns = 4,
          mark = ",",
          digits = digits
        )%>%
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
