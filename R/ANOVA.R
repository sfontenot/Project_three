
#' Title ANOVA
#'
#' @param file the file path
#' @param species_col provide column you wish to use
#' @param rain_col state the "Cumulative_Rain" to be used
#'
#' @return analysis of the butterfly data and plots the outcome
#' @export
ANOVA <- function(file, species_col, rain_col) {
  data <- clean(file)
  if (!all(c(species_col, rain_col) %in% colnames(data))) {
    stop("Data must contain the specified columns")
  }
  if (length(unique(data[[species_col]])) < 2) {
    stop("The species column must have at least two unique values")
  }
  data[[rain_col]] <- as.numeric(as.character(data[[rain_col]]))
  if (any(is.na(data[[rain_col]]))) {
    warning("NAs introduced by coercion in the rain column")
  }
  # Perform ANOVA
  anova_result <- aov(as.formula(paste(rain_col, "~", species_col)), data = data)
  print(summary(anova_result))

  # Perform Tukey's HSD post-hoc test
  tukey_result <- TukeyHSD(anova_result)
  print(tukey_result)

  # Extract Tukey's HSD results for plotting
  tukey_df <- as.data.frame(tukey_result[[1]])
  tukey_df$comparison <- rownames(tukey_df)

  # Plot the results
  ggplot(tukey_df, aes(x = comparison, y = diff)) +
    geom_point() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
    labs(title = "Tukey's HSD Post-Hoc Test Results",
         x = "Comparison",
         y = "Difference in Means") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
