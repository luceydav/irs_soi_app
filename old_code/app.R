

library(shiny)

file <- 'irs_flexdash.Rmd'
dir <- dirname(file)

ui <- rmarkdown:::rmarkdown_shiny_ui(dir, file)
render_args <- list()
render_args$envir <- parent.frame()
server <- rmarkdown:::rmarkdown_shiny_server(dir, file, 'UTF-8', T, render_args)

shinyApp(ui, server)