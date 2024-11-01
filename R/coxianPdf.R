#' Coxian Probability Density Function
#'
#' @param t A non-negative numeric vector.
#' @param lambda A numeric vector of the arrival rates.
#' @param mu A numeric vector of the service rates.
#' @return The probability density function evaluated at t.
#' @export
coxianPdf <- function(t, lambda, mu) {
  if (!is.numeric(t) || any(t < 0)) {
    stop("t should be a non-negative numeric vector.")
  }
  # Implement the PDF calculation based on your Coxian Phase-Type distribution logic
  # Example placeholder calculation
  pdf_values <- lambda * exp(-mu * t)
  return(pdf_values)
}
