#' Title Season
#'
#' @param df this is the data frame to be used.
#'
#' @return an overview of the butterfly data related to the Summer/Spring
#' @export
season <- function(file) {
  df <- clean(file)
  summary_data <- df %>%
    group_by(ButterflySpecies) %>%
    summarize(
      Avg_SpringTemp = mean(SpringTemp, na.rm = TRUE),
      Avg_SummerTemp = mean(SummerTemp, na.rm = TRUE)
    )

  # Determine the better season for each species
  summary_data <- summary_data %>%
    mutate(Better_Season = ifelse(Avg_SpringTemp > Avg_SummerTemp, "Spring", "Summer"))

  return(summary_data)
}
