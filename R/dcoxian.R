#' Density of Coxian Phase Type Distribution
#'
#' Computes the density of the Coxian phase type distribution for a given vector of data points.
#'
#' @param x A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#'
#' @return A numeric vector of density values for the provided data points.
#'
#' @examples
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' x_values <- seq(0, 10, by = 0.1)
#' density_values <- dcoxian(x_values, lambda, mu)
#'
#' @export
dcoxian <- function(x, lambda, mu) {
  n_phases <- length(mu)
  densities <- rep(0, length(x))

  for (i in seq_along(x)) {
    px <- 0
    for (k in 1:n_phases) {
      prod_term <- prod(exp(-mu[1:k] * x[i]) * (1 - lambda[1:k]))
      px <- px + mu[k] * prod_term
    }
    densities[i] <- px
  }

  return(densities)
}
