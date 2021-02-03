library(shiny)
library(profvis)
p <- profvis({
  runApp("app.R")
})

