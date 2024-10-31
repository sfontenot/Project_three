#' Title bubbles
#'
#' @param filepath Give the function the filepath
#' @param column give the functions a column you want to observe
#'
#' @return Problem values
#' @export
bubble <- function(filepath, column){
  dataframe <- read_csv(filepath) %>%
    select( {{column}} ) %>%
    na.omit( )
  new_cal <- dataframe %>%
    mutate(new_cal = weight / 2)
  problem_values <- new_cal %>%
    filter( new_cal <= 5)
  print(problem_values)
  return(new_cal)
}
