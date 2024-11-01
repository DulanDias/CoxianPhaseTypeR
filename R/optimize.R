#' Optimize Parameters for Coxian Phase Type Distribution
#'
#' This function optimizes the parameters lambda and mu of a Coxian phase type distribution
#' to fit the provided data. The objective function to be minimized
#' can be controlled through the `objective` parameter.
#'
#' @param data A numeric vector of observed data points.
#' @param init_lambda A numeric vector of initial values for lambda.
#' @param init_mu A numeric vector of initial values for mu.
#' @param objective A character string specifying the objective function to be minimized. Can
#'   take values "nll" (negative log-likelihood) or "ssd" (sum of squared differences). Default
#'   is "ssd".
#'
#' @return A list containing the optimized lambda and mu values.
#'
#' @examples
#' data_sample <- rexp(100, rate = 0.5)
#' init_lambda <- c(0.5, 0)
#' init_mu <- c(0.2, 0.4)
#' result <- optimize(data_sample, init_lambda, init_mu, objective = "nll")
#'
#' @export
optimize <- function(data, init_lambda, init_mu, objective = "ssd") {
  # Ensure init_lambda and init_mu have the same length
  if (length(init_lambda) != length(init_mu)) {
    stop("Length of init_lambda and init_mu should be the same.")
  }

  # Ensure the last term of init_lambda is 0
  if (tail(init_lambda, n = 1) != 0) {
    stop("The last term of init_lambda should be 0.")
  }

  # Objective function to be minimized
  objective_function <- function(params) {
    lambda <- c(params[1:(length(init_lambda) - 1)], 0)  # Set the last lambda value to 0
    mu <- params[(length(init_lambda)):length(params)]

    loss_value <- loss_function(lambda = lambda, mu = mu, data = data, method = objective)

    if(is.infinite(loss_value) || is.nan(loss_value)) {
      print("Non-finite loss value encountered!")
      print("Lambda:")
      print(lambda)
      print("Mu:")
      print(mu)
    }

    return(loss_value)
  }

  # Set initial parameters
  initial_parameters <- c(init_lambda[-length(init_lambda)], init_mu)

  # Defining lower and upper bounds
  lower_bounds <- c(rep(0, length(init_lambda) - 1), rep(0, length(init_mu))) # All parameters should be >= 0
  upper_bounds <- c(rep(Inf, length(init_lambda) - 1), rep(Inf, length(init_mu))) # Setting an upper bound to Inf

  # Optimization
  result <- optim(par = initial_parameters, fn = objective_function, method = "L-BFGS-B", lower = lower_bounds, upper = upper_bounds)

  # Extracting lambda and mu values
  lambda_optimized <- c(result$par[1:(length(init_lambda) - 1)], 0)  # Add the 0 value for the last lambda
  mu_optimized <- result$par[(length(init_lambda)):length(result$par)]

  return(list(lambda = lambda_optimized, mu = mu_optimized))
}
