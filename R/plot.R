
#' Title Plot
#'
#' @param file give the function the file path to be used
#'
#' @return a plot of the clean data
#' @export
plot <- function(file) {
  data <- clean(file)
  # Ensure the data has the required columns
  if(!all(c('ButterflySpecies', 'Cumulative_Rain', 'Site') %in% colnames(data))) {
    stop("Data must contain 'ButterflySpecies', 'Cumulative_Rain', and 'Site' columns")
  }

  # Create the plot
  ggplot(data, aes(x = Cumulative_Rain, y = Site, color = ButterflySpecies)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(title = "Distribution of Cumulative Rain",
         x = "Cumulative Rain (mm)",
         y = "Site") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12)
    )
}
