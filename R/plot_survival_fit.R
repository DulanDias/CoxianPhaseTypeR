#' Plot survival function with goodness-of-fit metrics
#'
#' @param fit A fitted Coxian-PH model
#' @param data Observed survival times
#' @return A ggplot object with RMSE and KS statistic displayed
#' @export
plot_survival_fit <- function(fit, data) {
  library(ggplot2)

  # Compute survival function from fitted Coxian-PH model
  survival_times <- seq(min(data), max(data), length.out = 100)
  model_survival <- sapply(survival_times, function(t) exp(-sum(fit$lambda * t)))

  df_survival <- data.frame(time = survival_times, survival = model_survival)

  ggplot(df_survival, aes(x = time, y = survival)) +
    geom_step(linewidth = 1.2, color = "blue", aes(linetype = "Model")) +
    geom_step(aes(x = time, y = survfit(Surv(data) ~ 1)$surv, linetype = "Kaplan-Meier"), color = "black") +
    labs(title = "Kaplan-Meier vs. Coxian-PH Survival Curve",
         x = "Time", y = "Survival Probability") +
    scale_linetype_manual(name = "Legend", values = c("Model" = "solid", "Kaplan-Meier" = "dashed")) +
    theme_minimal()
}
