#' Determine the optimal number of phases for a Coxian Phase-Type model
#'
#' @param data Observed survival times
#' @param max_phases Maximum number of phases to test
#' @return A list containing the best fitted model and phase selection results
#' @export
determine_optimal_phases <- function(data, max_phases = 5) {
  best_model <- NULL
  best_log_likelihood <- -Inf

  for (m in 1:max_phases) {
    fit <- fit_coxian(data, num_phases = m)
    if (fit$log_likelihood > best_log_likelihood) {
      best_model <- list(
        lambda = fit$lambda,
        mu = fit$mu,
        log_likelihood = fit$log_likelihood
      ) # Ensuring the expected return structure
      best_log_likelihood <- fit$log_likelihood
    }
  }

  return(best_model)
}
