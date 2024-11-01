#' Cumulative Distribution Function of Coxian Phase Type Distribution
#'
#' Computes the cumulative distribution function (CDF) of the Coxian phase type distribution
#' for a given vector of data points.
#'
#' @param q A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#'
#' @return A numeric vector of CDF values for the provided data points.
#'
#' @examples
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' q_values <- seq(0, 10, by = 0.1)
#' cdf_values <- pcoxian(q_values, lambda, mu)
#'
#' @export
pcoxian <- function(q, lambda, mu) {
  n_phases <- length(mu)
  cdf_values <- rep(0, length(q))

  for (i in seq_along(q)) {
    pq <- 0
    for (k in 1:n_phases) {
      prod_term <- prod(exp(-mu[1:k] * q[i]) * (1 - lambda[1:k]))
      pq <- pq + (1 - exp(-mu[k] * q[i])) * prod_term
    }
    cdf_values[i] <- pq
  }

  return(cdf_values)
}
