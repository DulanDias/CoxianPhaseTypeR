#' Q-Q Plot for Coxian Distribution
#'
#' Creates a Q-Q plot comparing empirical quantiles to theoretical quantiles
#' of the fitted Coxian distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#'
#' @export
plotQqCoxian <- function(data, lambda, mu) {
  empirical_quantiles <- quantile(data, probs = seq(0, 1, length.out = length(data)))
  theoretical_quantiles <- sapply(empirical_quantiles, function(q) {
    uniroot(function(x) coxianCdf(x, lambda, mu) - q, interval = c(0, max(data) * 2))$root
  })

  qqplot(empirical_quantiles, theoretical_quantiles, main = "Q-Q Plot",
         xlab = "Empirical Quantiles", ylab = "Theoretical Quantiles")
  abline(0, 1, col = "red")
}
