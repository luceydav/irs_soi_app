
cube_function <- function(var_num, var_denom) {
  
  #Line by state
  cube(d, .(var = sum(var_num, na.rm = TRUE) / sum(var_denom, na.rm = TRUE)),
       by = c("year", "state"))[
       ][, dcast(.SD, state ~ year)][
       ][order(`2018`, decreasing = TRUE)]
  
}
  