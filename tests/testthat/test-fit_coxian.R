test_that("fit_coxian estimates valid parameters", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3)

  expect_type(fit$lambda, "double")
  expect_type(fit$mu, "double")
  expect_length(fit$lambda, 3)
  expect_length(fit$mu, 3)
  expect_true(all(fit$lambda > 0))  # Transition rates must be positive
  expect_true(all(fit$mu > 0))  # Absorption rates must be positive
  expect_true(fit$log_likelihood < 0)  # Log-likelihood should be negative
})
