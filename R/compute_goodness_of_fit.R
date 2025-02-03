#' Compute goodness-of-fit metrics (RMSE and KS test) for Coxian-PH model
#'
#' @param fit A fitted Coxian-PH model
#' @param data Observed survival times
#' @return A list containing RMSE and Kolmogorov-Smirnov (KS) statistic
#' @export
compute_goodness_of_fit <- function(fit, data) {
  library(survival)
  library(stats)

  # Compute empirical survival function (Kaplan-Meier)
  km_fit <- survfit(Surv(data) ~ 1)
  km_survival <- stepfun(km_fit$time, c(1, km_fit$surv))

  # Compute model survival function
  t_values <- seq(0, max(data), length.out = 100)
  model_survival <- sapply(t_values, function(t) {
    sum(sapply(1:length(fit$lambda), function(i) {
      (fit$mu[i] / (fit$lambda[i] + fit$mu[i])) * exp(-(fit$lambda[i] + fit$mu[i]) * t)
    }))
  })

  # Compute RMSE
  empirical_survival <- km_survival(t_values)
  rmse <- sqrt(mean((empirical_survival - model_survival)^2))

  # Compute KS statistic
  ks_stat <- ks.test(empirical_survival, model_survival)$statistic

  return(list(RMSE = rmse, KS_statistic = ks_stat))
}
