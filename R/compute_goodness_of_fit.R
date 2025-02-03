#' Compute goodness-of-fit metrics (RMSE and KS test) for Coxian-PH model
#'
#' @param fit A fitted Coxian-PH model
#' @param data Observed survival times
#' @return A list containing RMSE and Kolmogorov-Smirnov (KS) statistic
#' @export
compute_goodness_of_fit <- function(fit, data) {
  library(survival)
  library(stats)

  # Remove duplicate survival times for KS test robustness
  data <- unique(data)

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

  # Compute RMSE with robust handling for survival probabilities
  empirical_survival <- km_survival(t_values)
  valid_indices <- !is.na(empirical_survival) & !is.na(model_survival)

  if (sum(valid_indices) == 0) {
    warning("No valid survival probabilities for RMSE calculation. Returning NA.")
    rmse <- NA
  } else {
    rmse <- sqrt(mean((empirical_survival[valid_indices] - model_survival[valid_indices])^2))
  }

  # Compute KS statistic, using empirical CDF and interpolated model survival
  ks_stat <- suppressWarnings(ks.test(empirical_survival[valid_indices],
                                      model_survival[valid_indices])$statistic)

  return(list(RMSE = rmse, KS_statistic = ks_stat))
}
