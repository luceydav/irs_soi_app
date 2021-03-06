# Function to match colClasses based on regex with names
get_col_classes <- function(file) {
  
  data <- fread(file, nrows = 1)
  names <- tolower(names(data))
  col_class <-
    fifelse(str_detect(names, "year|state|zip|class|stub"),
            "character",
            "numeric")
  return(col_class)
}