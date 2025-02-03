#' Fit a Coxian Phase-Type model using Ross's formulation with log-likelihood tracking
#'
#' @param data Observed survival times
#' @param num_phases Number of phases
#' @param max_iter Maximum EM iterations (default: 500)
#' @param tol Convergence threshold for log-likelihood improvement (default: 1e-6)
#' @param verbose If TRUE, prints log-likelihood at each iteration
#' @return A list containing estimated parameters and convergence diagnostics
#' @export
fit_coxian <- function(data, num_phases, max_iter = 500, tol = 1e-6, verbose = FALSE) {
  n <- length(data)

  # Initialize transition and absorption rates randomly
  lambda <- runif(num_phases, 0.5, 2)  # Ensure positive values
  mu <- runif(num_phases, 0.1, 1)      # Ensure positive values

  # Log-Likelihood function
  log_likelihood <- function(lambda, mu) {
    likelihoods <- sapply(data, function(t) {
      sum(sapply(1:num_phases, function(i) {
        prob_i <- if (i == 1) {
          mu[i] / (lambda[i] + mu[i])
        } else {
          prod(lambda[1:(i - 1)] / (lambda[1:(i - 1)] + mu[1:(i - 1)])) *
            (mu[i] / (lambda[i] + mu[i]))
        }
        prob_i * (lambda[i] + mu[i]) * exp(-(lambda[i] + mu[i]) * t)
      }))
    })

    # Handle NaN or Inf values
    likelihoods[is.na(likelihoods) | is.infinite(likelihoods)] <- .Machine$double.eps
    return(sum(log(likelihoods)))
  }

  # Initialize log-likelihood tracking
  log_likelihood_old <- -Inf
  likelihood_history <- numeric(max_iter)

  for (iter in 1:max_iter) {
    # E-step: Compute expected transitions and expected time spent in each phase
    expected_transitions <- numeric(num_phases)
    expected_times <- numeric(num_phases)

    for (i in 1:n) {
      for (j in 1:num_phases) {
        prob_stay <- exp(-data[i] * (lambda[j] + mu[j]))
        expected_transitions[j] <- expected_transitions[j] + prob_stay
        expected_times[j] <- expected_times[j] + data[i] * prob_stay
      }
    }

    # M-step: Update parameters to maximize expected log-likelihood
    lambda <- pmax(expected_transitions / expected_times, .Machine$double.eps)  # Ensure positivity
    mu <- pmax((1 - expected_transitions) / expected_times, .Machine$double.eps) # Ensure positivity

    # Compute new log-likelihood
    log_likelihood_new <- log_likelihood(lambda, mu)
    likelihood_history[iter] <- log_likelihood_new

    # Print log-likelihood if verbose is enabled
    if (verbose) {
      message(sprintf("Iteration %d: Log-Likelihood = %.4f", iter, log_likelihood_new))
    }

    # Check for NaN or Inf values
    if (is.nan(log_likelihood_new) || is.infinite(log_likelihood_new)) {
      warning("NaN or infinite log-likelihood detected. Stopping EM algorithm.")
      break
    }

    # Convergence check
    if (!is.nan(log_likelihood_new) && abs(log_likelihood_new - log_likelihood_old) < tol) {
      likelihood_history <- likelihood_history[1:iter]  # Trim unused space
      break
    }

    log_likelihood_old <- log_likelihood_new
  }

  return(list(
    lambda = lambda,
    mu = mu,
    log_likelihood = log_likelihood_old,
    iterations = length(likelihood_history),
    likelihood_history = likelihood_history
  ))
}
