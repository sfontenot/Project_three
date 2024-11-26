#' Title clean dataframe
#'
#' @param file_path gives the function a file_path to use
#'
#' @return a cleaned data set
#' @export
clean <- function(file_path) {
  df <- read.csv(file_path)
  df <- na.omit(df)# remove rows with missing data
  df <- df[!duplicated(df), ]
  df <- df %>%
    mutate_if(is.character, as.factor)
  return(df)
}

