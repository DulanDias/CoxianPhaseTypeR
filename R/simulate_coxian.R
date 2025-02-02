#' Simulate survival data using Ross's formulation for Coxian-PH distributions
#'
#' @param n Number of observations to generate
#' @param lambda Vector of transition rates
#' @param mu Vector of absorption rates
#' @return A numeric vector of simulated survival times
#' @export
simulate_coxian <- function(n, lambda, mu) {
  if (length(lambda) != length(mu)) {
    stop("lambda and mu must have the same length (number of phases).")
  }

  survival_times <- numeric(n)
  m <- length(lambda)

  for (i in 1:n) {
    phase <- 1
    time <- 0
    while (phase <= m) {
      transition_rate <- lambda[phase] + mu[phase]
      time_in_phase <- rexp(1, rate = transition_rate)
      time <- time + time_in_phase

      # Probability of absorption at this phase
      if (runif(1) < mu[phase] / transition_rate) {
        break
      }
      phase <- phase + 1
    }
    survival_times[i] <- time
  }
  return(survival_times)
}
