#' Plot ECDF vs Theoretical CDF
#'
#' Plots the empirical cumulative distribution function (ECDF) of observed data
#' and overlays it with the theoretical cumulative distribution function (CDF) of
#' the fitted Coxian phase-type distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#' @param main The title of the plot. Default is "ECDF vs Theoretical CDF".
#' @param xlab The x-axis label. Default is "Value".
#' @param ylab The y-axis label. Default is "Cumulative Probability".
#' @param line_col The color of the theoretical CDF line. Default is "red".
#' @param line_lwd The line width of the theoretical CDF line. Default is 2.
#'
#' @return A plot showing the ECDF and the theoretical CDF.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' plotEcdfVsTheoreticalCdf(data_sample, lambda, mu)
#'
#' @export
plotEcdfVsTheoreticalCdf <- function(data, lambda, mu, main = "ECDF vs Theoretical CDF",
                                     xlab = "Value", ylab = "Cumulative Probability",
                                     line_col = "red", line_lwd = 2) {
  # Plot the ECDF of the data
  plot(ecdf(data), main = main, xlab = xlab, ylab = ylab)

  # Overlay the theoretical CDF
  curve(coxianCdf(x, lambda, mu), add = TRUE, col = line_col, lwd = line_lwd)
}
