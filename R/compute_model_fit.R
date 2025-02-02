#' Compute model fit statistics for a Coxian-PH model
#'
#' @param fit A fitted Coxian-PH model object (output of fit_coxian)
#' @param num_params Number of estimated parameters
#' @param num_obs Number of observations
#' @return A list with log-likelihood, AIC, and BIC values
#' @export
compute_model_fit <- function(fit, num_params, num_obs) {
  log_likelihood <- fit$log_likelihood
  aic <- -2 * log_likelihood + 2 * num_params
  bic <- -2 * log_likelihood + num_params * log(num_obs)

  return(list(log_likelihood = log_likelihood, AIC = aic, BIC = bic))
}
