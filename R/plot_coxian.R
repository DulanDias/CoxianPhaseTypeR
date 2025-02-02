#' Plot the fitted Coxian Phase-Type distribution
#'
#' @param fit A fitted Coxian-PH model object from fit_coxian()
#' @param max_t Maximum time to display on the x-axis
#' @return A ggplot object
#' @export
plot_coxian <- function(fit, max_t = 10) {
  library(ggplot2)

  t_values <- seq(0, max_t, length.out = 100)
  pdf_values <- sapply(t_values, function(t) {
    sum(fit$lambda * exp(-fit$lambda * t))
  })

  df <- data.frame(t = t_values, pdf = pdf_values)

  ggplot(df, aes(x = t, y = pdf)) +
    geom_line(color = "blue") +
    labs(title = "Fitted Coxian Phase-Type PDF",
         x = "Time",
         y = "Density") +
    theme_minimal()
}
