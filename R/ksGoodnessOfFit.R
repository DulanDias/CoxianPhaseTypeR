#' Kolmogorov-Smirnov Goodness-of-Fit Test
#'
#' Computes the KS statistic for comparing the empirical CDF of the data with
#' the theoretical CDF of the fitted Coxian distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#'
#' @return A numeric value representing the KS statistic.
#'
#' @export
ksGoodnessOfFit <- function(data, lambda, mu) {
  # Calculate the theoretical CDF for each data point
  ecdf_data <- ecdf(data)
  cdf_theoretical <- sapply(data, function(x) coxianCdf(x, lambda, mu))

  # Calculate the KS statistic
  ks_statistic <- max(abs(ecdf_data(data) - cdf_theoretical))
  return(ks_statistic)
}
