#' Plot Kaplan-Meier vs Coxian-PH survival curve
#'
#' @param fit A fitted Coxian-PH model
#' @param data Survival time data used for fitting
#' @export
plot_survival_fit <- function(fit, data) {
  library(survival)
  library(ggplot2)

  # Compute Kaplan-Meier survival estimate
  km_fit <- survfit(Surv(data) ~ 1)
  km_df <- data.frame(time = km_fit$time, survival = km_fit$surv)

  # Compute survival estimates from the Coxian-PH model
  model_times <- seq(min(data), max(data), length.out = 100)
  model_survival <- sapply(model_times, function(t) exp(-sum(fit$lambda * t)))

  model_df <- data.frame(time = model_times, survival = model_survival)

  # Plot both survival curves
  ggplot() +
    geom_step(data = km_df, aes(x = time, y = survival, linetype = "Kaplan-Meier"), color = "black") +
    geom_line(data = model_df, aes(x = time, y = survival, linetype = "Coxian-PH Model"), color = "blue", size = 1) +
    scale_linetype_manual(values = c("Kaplan-Meier" = "dashed", "Coxian-PH Model" = "solid")) +
    labs(title = "Kaplan-Meier vs Coxian-PH Model Survival Curve",
         x = "Time",
         y = "Survival Probability",
         linetype = "Model") +
    theme_minimal()
}
