test_that("fit_coxian estimates valid parameters and tracks log-likelihood", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3, verbose = TRUE)

  expect_type(fit$lambda, "double")
  expect_type(fit$mu, "double")
  expect_length(fit$lambda, 3)
  expect_length(fit$mu, 3)
  expect_true(all(fit$lambda > 0))  # Ensure positivity
  expect_true(all(fit$mu > 0))  # Ensure positivity

  # Ensure log-likelihood tracking is valid
  expect_true(!any(is.na(fit$likelihood_history)))
  expect_true(all(is.finite(fit$likelihood_history))) # Ensure no NaN or Inf
})
