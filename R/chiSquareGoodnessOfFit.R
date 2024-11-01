#' Chi-Square Goodness-of-Fit Test
#'
#' Computes the Chi-Square statistic for the goodness-of-fit of the Coxian distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#' @param bins Number of bins to use for the Chi-Square test. Default is 10.
#'
#' @return A list containing the Chi-Square statistic and p-value.
#'
#' @export
chiSquareGoodnessOfFit <- function(data, lambda, mu, bins = 10) {
  hist_data <- hist(data, plot = FALSE, breaks = bins)
  observed_counts <- hist_data$counts

  bin_edges <- hist_data$breaks
  expected_counts <- sapply(1:(length(bin_edges) - 1), function(i) {
    coxianCdf(bin_edges[i+1], lambda, mu) - coxianCdf(bin_edges[i], lambda, mu)
  }) * length(data)

  chi_square_statistic <- sum((observed_counts - expected_counts)^2 / expected_counts)
  p_value <- pchisq(chi_square_statistic, df = bins - length(lambda) - 1, lower.tail = FALSE)

  return(list(chi_square_statistic = chi_square_statistic, p_value = p_value))
}
