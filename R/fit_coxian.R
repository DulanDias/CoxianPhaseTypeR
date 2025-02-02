#' Fit a Coxian Phase-Type model using Ross's formulation
#'
#' @param data Observed survival times
#' @param num_phases Number of phases
#' @param max_iter Maximum EM iterations
#' @param tol Convergence tolerance
#' @return A list containing estimated parameters
#' @export
fit_coxian <- function(data, num_phases, max_iter = 500, tol = 1e-6) {
  n <- length(data)

  # Initialize parameters
  lambda <- runif(num_phases, 0.5, 2)
  mu <- runif(num_phases, 0.1, 1)

  log_likelihood <- function(lambda, mu) {
    likelihoods <- sapply(data, function(t) {
      sum(sapply(1:num_phases, function(i) {
        prob_i <- prod(lambda[1:(i - 1)] / (lambda[1:(i - 1)] + mu[1:(i - 1)])) *
          (mu[i] / (lambda[i] + mu[i]))
        prob_i * (lambda[i] + mu[i]) * exp(-(lambda[i] + mu[i]) * t)
      }))
    })
    sum(log(likelihoods))
  }

  log_likelihood_old <- -Inf
  for (iter in 1:max_iter) {
    # E-step
    expected_transitions <- numeric(num_phases)
    expected_times <- numeric(num_phases)

    for (i in 1:n) {
      for (j in 1:num_phases) {
        prob_stay <- exp(-data[i] * (lambda[j] + mu[j]))
        expected_transitions[j] <- expected_transitions[j] + prob_stay
        expected_times[j] <- expected_times[j] + data[i] * prob_stay
      }
    }

    # M-step: Update lambda and mu
    lambda <- expected_transitions / expected_times
    mu <- (1 - expected_transitions) / expected_times

    # Check for convergence
    log_likelihood_new <- log_likelihood(lambda, mu)
    if (abs(log_likelihood_new - log_likelihood_old) < tol) {
      break
    }
    log_likelihood_old <- log_likelihood_new
  }

  return(list(lambda = lambda, mu = mu, log_likelihood = log_likelihood_old))
}
