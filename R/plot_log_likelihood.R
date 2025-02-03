#' Plot Log-Likelihood Progression
#'
#' @param fit A fitted Coxian-PH model object (output of fit_coxian)
#' @return A ggplot object showing log-likelihood progression across iterations
#' @export
plot_log_likelihood <- function(fit) {
  if (is.null(fit$likelihood_history) || length(fit$likelihood_history) < 2) {
    stop("Log-likelihood history is not available or insufficient for plotting.")
  }

  library(ggplot2)

  data <- data.frame(
    Iteration = seq_along(fit$likelihood_history),
    LogLikelihood = fit$likelihood_history
  )

  ggplot(data, aes(x = Iteration, y = LogLikelihood)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    theme_minimal() +
    labs(title = "Log-Likelihood Progression",
         x = "Iteration",
         y = "Log-Likelihood") +
    theme(text = element_text(size = 14))
}
