#' Akaike Information Criterion for Coxian Phase Type Distribution
#'
#' Computes the Akaike Information Criterion (AIC) for the fitted Coxian phase type distribution
#' given the data, lambda, and mu values.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#'
#' @return The computed AIC value.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' aic_value <- coxianAic(data_sample, lambda, mu)
#'
#' @export
coxianAic <- function(data, lambda, mu) {
  k <- length(lambda) + length(mu)  # Number of parameters
  nll <- loss_function(lambda, mu, data, method = "nll")  # Negative log-likelihood
  aic <- 2 * k + 2 * nll
  return(aic)
}
