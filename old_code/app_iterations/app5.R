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
library(purrr)

# Data
irs_app_data <- read_fst("irs_app.fst")
irs_app_data <- setDT(irs_app_data, key = "zipcode")
cols <- c("zipcode", "state", "county", "agi_level", "post_office_city")
data <- irs_app_data[, ..cols]
data[, (cols) := lapply(.SD, as.factor), .SDcols = cols]

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    sliderInput(var, var, min = rng[1], max = rng[2], value = rng)
  } else if (is.factor(x)) {
    levs <- levels(x)
    selectizeInput(var, var, choices = levs, selected = levs, multiple = TRUE)
  } else {
    # Not supported
    NULL
  }
}

filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      map(names(data), ~ make_ui(data[[.x]], .x))
    ),
    mainPanel(
      tableOutput("data")
    )
  )
)
server <- function(input, output, session) {
  selected <- reactive({
    each_var <- map(names(data), ~ filter_var(data[[.x]], input[[.x]]))
    reduce(each_var, ~ .x & .y)
  })
  
  output$data <- renderTable(head(data[selected(), ], 12))
}

shinyApp(ui, server)