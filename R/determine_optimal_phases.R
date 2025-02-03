#' Determine the optimal number of phases for Coxian Phase-Type fitting
#'
#' This function iteratively fits Coxian-PH models with different phase numbers (m = 1 to max_phases)
#' and selects the best model based on the Bayesian Information Criterion (BIC).
#'
#' @param data A numeric vector of observed survival times.
#' @param max_phases The maximum number of phases to test (default: 5).
#' @return A list containing the best model, its estimated parameters, and the corresponding BIC.
#' @export
determine_optimal_phases <- function(data, max_phases = 5) {
  best_model <- NULL
  best_BIC <- Inf
  best_phase <- NULL
  num_obs <- length(data)

  # Store results for each phase
  model_results <- list()

  for (m in 1:max_phases) {
    fit <- tryCatch({
      fit_coxian(data, num_phases = m)
    }, error = function(e) {
      message(paste("Error fitting model with", m, "phases:", e$message))
      return(NULL)
    })

    if (!is.null(fit)) {
      # Compute model fit, passing the correct number of parameters and observations
      model_fit <- tryCatch({
        compute_model_fit(fit, num_params = 2 * m, num_obs = num_obs)
      }, error = function(e) {
        message(paste("Error computing model fit for", m, "phases:", e$message))
        return(NULL)
      })

      # Ensure BIC is valid
      if (!is.null(model_fit) && !is.null(model_fit$BIC) && is.finite(model_fit$BIC)) {
        model_results[[m]] <- list(fit = fit, BIC = model_fit$BIC)

        # Update best model if BIC is lower
        if (model_fit$BIC < best_BIC) {
          best_BIC <- model_fit$BIC
          best_model <- fit
          best_phase <- m
        }
      }
    }
  }

  # If no valid model was found, return an error message
  if (is.null(best_model)) {
    stop("No valid model was found. Ensure input data is suitable for fitting.")
  }

  # Ensure return structure matches expectations
  return(list(
    best_model = list(
      lambda = best_model$lambda,
      mu = best_model$mu,
      log_likelihood = best_model$log_likelihood
    ),
    best_BIC = best_BIC,
    best_phase = best_phase,
    all_results = model_results  # Store results for all tested phases
  ))
}
