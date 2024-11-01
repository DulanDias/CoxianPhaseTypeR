#' Plot Histogram with Theoretical PDF Overlay
#'
#' Plots a histogram of the observed data and overlays it with the theoretical
#' probability density function (PDF) of the fitted Coxian phase-type distribution.
#'
#' @param data A numeric vector of observed data points.
#' @param lambda A numeric vector of lambda values for the Coxian distribution.
#' @param mu A numeric vector of mu values for the Coxian distribution.
#' @param breaks The number of breaks for the histogram. Default is 30.
#' @param main The title of the plot. Default is "Histogram vs Theoretical PDF".
#' @param xlab The x-axis label. Default is "Value".
#' @param ylab The y-axis label. Default is "Density".
#' @param line_col The color of the PDF line. Default is "blue".
#' @param line_lwd The line width of the PDF line. Default is 2.
#'
#' @return A plot showing the histogram with the theoretical PDF overlay.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' plotHistVsTheoreticalPdf(data_sample, lambda, mu)
#'
#' @export
plotHistVsTheoreticalPdf <- function(data, lambda, mu, breaks = 30, main = "Histogram vs Theoretical PDF",
                                     xlab = "Value", ylab = "Density",
                                     line_col = "blue", line_lwd = 2) {
  # Plot the histogram of the data with density scaling
  hist(data, probability = TRUE, breaks = breaks, main = main, xlab = xlab, ylab = ylab)

  # Overlay the theoretical PDF
  curve(coxianPdf(x, lambda, mu), add = TRUE, col = line_col, lwd = line_lwd)
}
