test_that("compute_model_fit returns correct model selection values", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3)
  fit_stats <- compute_model_fit(fit, num_params = 6, num_obs = 100)

  expect_named(fit_stats, c("log_likelihood", "AIC", "BIC"))
  expect_type(fit_stats$log_likelihood, "double")
  expect_type(fit_stats$AIC, "double")
  expect_type(fit_stats$BIC, "double")
  expect_true(fit_stats$AIC > 0)  # AIC should be positive
  expect_true(fit_stats$BIC > 0)  # BIC should be positive
})
