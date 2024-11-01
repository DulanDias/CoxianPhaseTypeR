#' Calculate Loss for Coxian Phase Type Distribution
#'
#' Calculates the loss for the given lambda and mu parameters of a Coxian phase type distribution.
#' The loss can be computed as either the negative log-likelihood (NLL) or the sum of squared
#' differences (SSD).
#'
#' @param lambda A numeric vector of lambda values.
#' @param mu A numeric vector of mu values.
#' @param data A numeric vector of observed data points.
#' @param method A character string specifying the loss function to be used. Can take values
#'   "nll" (negative log-likelihood) or "ssd" (sum of squared differences).
#'
#' @return The computed loss value.
#'
#' @examples
#' lambda <- c(0.5, 0)
#' mu <- c(0.2, 0.4)
#' data_sample <- rexp(100, rate = 0.5)
#' loss_value <- loss_function(lambda, mu, data_sample, method = "nll")
#'
#' @export
loss_function <- function(lambda, mu, data, method = "ssd") {
  if (method == "nll") {
    # Negative log-likelihood loss function
    nll <- -sum(log(dcoxian(data, lambda, mu)))
    return(nll)
  } else if (method == "ssd") {
    # Sum of squared differences loss function
    ecdf_data <- ecdf(data)
    theoretical_cdf <- pcoxian(sort(data), lambda, mu)
    ssd <- sum((ecdf_data(sort(data)) - theoretical_cdf)^2)
    return(ssd)
  } else {
    stop("Invalid method. Choose 'nll' or 'ssd'.")
  }
}
