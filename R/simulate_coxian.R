#' Simulate survival data using a Coxian Phase-Type distribution
#'
#' @param n Number of observations to generate
#' @param lambda Vector of transition rates (one per phase). The last element should be 0.
#' @param mu Vector of absorption rates (one per phase)
#' @return A numeric vector of simulated survival times
#' @export
simulate_coxian <- function(n, lambda, mu) {
  # Check that lambda and mu have the same length.
  if (length(lambda) != length(mu)) {
    stop("lambda and mu must have the same length (number of phases).")
  }

  # Ensure the last transition rate is zero (absorbing phase).
  if (lambda[length(lambda)] != 0) {
    warning(sprintf("The last transition rate lambda[%d] = %.4f is not zero. Setting it to 0.",
                    length(lambda), lambda[length(lambda)]))
    lambda[length(lambda)] <- 0
  }

  # Prepare a vector to hold the simulated survival times.
  survival_times <- numeric(n)
  m <- length(lambda)  # Number of phases

  # Simulate each observation.
  for (i in 1:n) {
    phase <- 1      # Start in phase 1
    time <- 0       # Initialize time
    while (phase <= m) {
      # Total rate in the current phase is the sum of the transition (to the next phase)
      # and absorption (exit) rates.
      transition_rate <- lambda[phase] + mu[phase]
      # Draw waiting time from an exponential distribution.
      time_in_phase <- rexp(1, rate = transition_rate)
      time <- time + time_in_phase

      # Decide whether absorption occurs in this phase.
      # With probability mu[phase] / (lambda[phase] + mu[phase]), the event occurs.
      if (runif(1) < mu[phase] / transition_rate) {
        break  # Event occurred; exit the phase loop.
      }
      # Otherwise, move to the next phase.
      phase <- phase + 1
    }
    survival_times[i] <- time
  }
  return(survival_times)
}
