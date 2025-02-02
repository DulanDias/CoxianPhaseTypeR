#' Plot survival function using Ross's equation
#'
#' @param fit Fitted Coxian-PH model
#' @param data Observed survival times
#' @return A ggplot object
#' @export
plot_survival_fit <- function(fit, data) {
  library(ggplot2)
  library(survival)

  t_values <- seq(0, max(data), length.out = 100)
  survival_estimates <- sapply(t_values, function(t) {
    sum(sapply(1:length(fit$lambda), function(i) {
      (fit$mu[i] / (fit$lambda[i] + fit$mu[i])) * exp(-(fit$lambda[i] + fit$mu[i]) * t)
    }))
  })

  df <- data.frame(t = t_values, survival_prob = survival_estimates)

  km_fit <- survfit(Surv(data) ~ 1)

  ggplot(df, aes(x = t, y = survival_prob)) +
    geom_line(color = "red", linetype = "dashed", size = 1.2) +
    geom_step(aes(x = km_fit$time, y = km_fit$surv), color = "blue", size = 1) +
    labs(title = "Survival Function: Ross’s Model vs Kaplan-Meier",
         x = "Time",
         y = "Survival Probability") +
    theme_minimal()
}
