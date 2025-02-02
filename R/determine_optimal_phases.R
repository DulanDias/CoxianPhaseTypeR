#' Determine the optimal number of phases using Ross's formulation
#'
#' @param data Observed survival times
#' @param max_phases Maximum number of phases to test
#' @return A list containing the best-fitting model
#' @export
determine_optimal_phases <- function(data, max_phases = 5) {
  best_bic <- Inf
  best_model <- NULL

  for (m in 1:max_phases) {
    fit <- fit_coxian(data, num_phases = m)
    fit_stats <- compute_model_fit(fit, num_params = 2 * m, num_obs = length(data))

    if (fit_stats$BIC < best_bic) {
      best_bic <- fit_stats$BIC
      best_model <- fit
    }
  }

  return(list(best_model = best_model, best_BIC = best_bic))
}
