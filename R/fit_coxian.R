#' Fit a Coxian Phase-Type distribution using an EM algorithm with multiple random initializations
#'
#' This function estimates the parameters of a Coxian Phase-Type distribution using an
#' Expectation-Maximization (EM) algorithm based on the generalized formulation by Sheldon Ross.
#'
#' @param data A numeric vector of observed survival times.
#' @param num_phases Number of phases \(m\) in the Coxian model.
#' @param max_iter Maximum number of EM iterations (default: 1000).
#' @param tol Convergence threshold on the change in log-likelihood (default: 1e-6).
#' @param verbose If TRUE, prints iteration-by-iteration progress.
#' @param n_initializations Number of random EM initializations (default: 20).
#' @return A list with the best estimates for \code{lambda} and \code{mu}, the log-likelihood,
#'         the number of iterations, and the log-likelihood history for each run.
#' @export
fit_coxian <- function(data, num_phases, max_iter = 1000, tol = 1e-6,
                       verbose = FALSE, n_initializations = 20) {
  m <- num_phases
  n <- length(data)

  # Helper function: Compute the convolution density for a given phase path j.
  conv_density <- function(t, j, lambda, mu) {
    r <- lambda[1:j] + mu[1:j]
    A_vals <- numeric(j)
    for (i in 1:j) {
      prod_val <- 1
      for (k in 1:j) {
        if (k != i) {
          denom <- r[k] - r[i]
          if (abs(denom) < 1e-8) denom <- sign(denom) * 1e-8
          prod_val <- prod_val * (r[k] / denom)
        }
      }
      A_vals[i] <- prod_val
    }
    sum(A_vals * r * exp(-r * t))
  }

  # Helper function: Compute the probability p_j for absorption in phase j.
  p_j_func <- function(j, lambda, mu) {
    if (j == 1) {
      return(mu[1] / (lambda[1] + mu[1]))
    } else {
      prod_term <- prod(lambda[1:(j-1)] / (lambda[1:(j-1)] + mu[1:(j-1)]))
      return(prod_term * (mu[j] / (lambda[j] + mu[j])))
    }
  }

  # Helper function: Compute the overall density at time t.
  density_at_t <- function(t, lambda, mu) {
    dens <- 0
    for (j in 1:m) {
      dens <- dens + p_j_func(j, lambda, mu) * conv_density(t, j, lambda, mu)
    }
    dens
  }

  # Log-likelihood function for given parameters.
  log_likelihood_func <- function(lambda, mu) {
    dens_vals <- sapply(data, function(t) {
      d <- density_at_t(t, lambda, mu)
      if (d <= 0) d <- .Machine$double.eps
      d
    })
    sum(log(dens_vals))
  }

  # Helper function: Allocate time for phase i when absorption occurs in phase j.
  time_allocation <- function(t, j, i, lambda, mu) {
    denom <- sum(1 / (lambda[1:j] + mu[1:j]))
    t * (1 / (lambda[i] + mu[i])) / denom
  }

  # EM algorithm for a single initialization.
  run_em <- function() {
    # Random initialization with a broader range.
    lambda <- runif(m, 0.1, 3)
    mu <- runif(m, 0.1, 3)
    lambda[m] <- 0  # Enforce the constraint for the last phase.

    loglik_history <- numeric(max_iter)
    prev_loglik <- -Inf

    for (iter in 1:max_iter) {
      total_T <- numeric(m)         # Expected total time in each phase.
      total_N_trans <- numeric(m)     # Expected transitions from each phase (for phases 1 to m-1).
      total_N_abs <- numeric(m)       # Expected absorptions in each phase.

      # E-step: Process each observation.
      for (t in data) {
        # Compute contributions A_j for each phase path j.
        A_vec <- sapply(1:m, function(j) {
          p_j_func(j, lambda, mu) * conv_density(t, j, lambda, mu)
        })
        f_t <- sum(A_vec)
        if (f_t <= 0) f_t <- .Machine$double.eps
        # Compute posterior probabilities for each phase path.
        r_vec <- A_vec / f_t

        for (i in 1:m) {
          # Expected absorption in phase i.
          total_N_abs[i] <- total_N_abs[i] + (if (i <= length(r_vec)) r_vec[i] else 0)
          # Expected transition from phase i (only for i < m).
          if (i < m) {
            total_N_trans[i] <- total_N_trans[i] + sum(r_vec[(i+1):m])
          }
          # Allocate observed time t to phase i.
          time_contrib <- 0
          for (j in i:m) {
            time_contrib <- time_contrib + r_vec[j] * time_allocation(t, j, i, lambda, mu)
          }
          total_T[i] <- total_T[i] + time_contrib
        }
      }

      # M-step: Update parameter estimates.
      new_lambda <- numeric(m)
      new_mu <- numeric(m)
      for (i in 1:(m-1)) {
        new_lambda[i] <- total_N_trans[i] / (total_T[i] + 1e-10)
        new_mu[i] <- total_N_abs[i] / (total_T[i] + 1e-10)
      }
      new_lambda[m] <- 0
      new_mu[m] <- total_N_abs[m] / (total_T[m] + 1e-10)

      lambda <- new_lambda
      mu <- new_mu

      curr_loglik <- log_likelihood_func(lambda, mu)
      loglik_history[iter] <- curr_loglik

      if (verbose) {
        message(sprintf("Iteration %d: Log-likelihood = %.6f", iter, curr_loglik))
        message(sprintf("  lambda: %s", paste(round(lambda, 4), collapse = ", ")))
        message(sprintf("  mu: %s", paste(round(mu, 4), collapse = ", ")))
      }

      if (abs(curr_loglik - prev_loglik) < tol) {
        loglik_history <- loglik_history[1:iter]
        break
      }
      prev_loglik <- curr_loglik
    }

    list(lambda = lambda,
         mu = mu,
         log_likelihood = curr_loglik,
         iterations = iter,
         likelihood_history = loglik_history)
  }

  # Run multiple random initializations and select the best result.
  results <- vector("list", n_initializations)
  for (init in 1:n_initializations) {
    if (verbose) message(sprintf("Initialization %d/%d", init, n_initializations))
    results[[init]] <- run_em()
  }

  best_index <- which.max(sapply(results, function(res) res$log_likelihood))
  best_result <- results[[best_index]]
  best_result$all_initializations <- results
  best_result
}
