#This is it
new_function <- function(filepath, column){
  dataframe <- read_csv(filepath) %>%
    select( {{column}} ) %>% 
    na.omit( )
  #return(dataframe)
  new_cal <- dataframe %>% 
    mutate(new_col = weight / 2)
  problem_values <- new_cal %>% 
    filter( new_col > 5)
  print(problem_values)
  return(new_cal)
}