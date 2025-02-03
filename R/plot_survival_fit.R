#' Plot survival function with goodness-of-fit metrics
#'
#' @param fit A fitted Coxian-PH model
#' @param data Observed survival times
#' @return A ggplot object with RMSE and KS statistic displayed
#' @export
plot_survival_fit <- function(fit, data) {
  library(ggplot2)
  library(survival)

  # Compute survival curves
  t_values <- seq(0, max(data), length.out = 100)
  model_survival <- sapply(t_values, function(t) {
    sum(sapply(1:length(fit$lambda), function(i) {
      (fit$mu[i] / (fit$lambda[i] + fit$mu[i])) * exp(-(fit$lambda[i] + fit$mu[i]) * t)
    }))
  })

  km_fit <- survfit(Surv(data) ~ 1)

  # Compute goodness-of-fit metrics
  fit_metrics <- compute_goodness_of_fit(fit, data)
  rmse <- round(fit_metrics$RMSE, 4)
  ks_stat <- round(fit_metrics$KS_statistic, 4)

  # Generate plot
  ggplot(data.frame(t = t_values, survival_prob = model_survival), aes(x = t, y = survival_prob)) +
    geom_line(color = "red", linetype = "dashed", size = 1.2, aes(label = "Model")) +
    geom_step(aes(x = km_fit$time, y = km_fit$surv), color = "blue", size = 1, aes(label = "Kaplan-Meier")) +
    annotate("text", x = max(data) * 0.6, y = 0.1,
             label = sprintf("RMSE: %.4f\nKS: %.4f", rmse, ks_stat),
             color = "black", size = 4, hjust = 0) +
    labs(title = "Survival Function: Model vs Kaplan-Meier",
         x = "Time",
         y = "Survival Probability") +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 1))
}
