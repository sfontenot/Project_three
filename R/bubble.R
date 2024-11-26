#' Title bubbles
#'
#' @param file give the function a file path
#' @param column give the function the column you wish to observe
#'
#' @return Problem values
#' @export
bubble <- function(file, column){
  df <- clean(file)
  new_cal <- df %>%
    mutate(new_cal = !!sym(column)/ 2)
  problem_values <- new_cal %>%
    filter( new_cal <= 5)
  print(problem_values)
  return(new_cal)
}
