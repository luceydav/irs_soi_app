
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
  dashboardSidebar(sidebarMenu(
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Charts", tabName = "charts", icon = icon("bar-chart-o")),
    selectizeGroupUI(
      id = "my-filters",
      inline = FALSE,
      params = list(
        var_one = list(
          inputId = "state",
          title = "Specify States",
          placeholder = 'select'
        ),
        var_two = list(
          inputId = "county",
          title = "Specify Counties",
          placeholder = 'select'
        ),
        var_three = list(
          inputId = "post_office_city",
          title = "Specify Cities",
          placeholder = 'select'
        ),
        var_four = list(
          inputId = "zipcode",
          title = "Specify Zipcodes",
          placeholder = 'select'
        ),
        var_five = list(
          inputId = "agi_level",
          title = "Specify Income Levels",
          placeholder = 'select'
        )
      )
    )
  ))
)

body <- dashboardBody(
  
  tabItems(
    # First tab content
    tabItem(tabName = "table",
            fluidRow(shinydashboard::box(dataTableOutput("Table"), width = 12))),
    tabItem(
      tabName = "charts",
      fluidRow(shinydashboard::box(plotlyOutput("AGI_Level"), width = 12)),
      fluidRow(shinydashboard::box(plotlyOutput("TaxRate"), width = 12)
            ))
  ))

server <- function(input, output, session) {
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = irs_app_data,
    vars = c("state", "county", "post_office_city", "zipcode", "agi_level")
  )
  
  output$Table <- renderDT({
    
    if (length(res_mod()$zipcode) < 50) {
      digits <- 3
    } else {
      digits <- 1
    }

    # https://taxfoundation.org/federal-tax-revenue-source-1934-2018/
    datatable(
      setDT(res_mod())[,
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
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: center;',
        '', htmltools::em('Annual Aggregated AGI, Federal Tax, Total Returns and Unique Zipcodes by Selection')
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
    
    plotly::layout(
      setDT(res_mod())[,
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
                      mode = "markers")],
      title = "Annual Aggregate AGI by Selected Group"
    )
  })
  
  output$TaxRate <- renderPlotly({
    
    plotly::layout(
      setDT(res_mod())[,
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
                      mode = "markers")],
      title = "Annual Effective Tax Rate by Selected Group"
    )
  })
}

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui, server)