#' Remove NAs and prints the values that are greater than five in a column
#'
#' @param filepath The path to the tab_delimited file you want to use
#' @param column what column you wish to filter
#'
#' @return The problem values
#' @export
new_function <- function(filepath, column){
  dataframe <- read_csv(filepath) %>%
    select( {{column}} ) %>%
    na.omit( )
  new_cal <- dataframe %>%
    mutate(new_cal = weight / 2)
  problem_values <- new_cal %>%
    filter( new_cal > 5)
  print(problem_values)
  return(new_cal)
}


