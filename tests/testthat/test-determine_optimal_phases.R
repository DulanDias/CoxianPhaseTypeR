test_that("determine_optimal_phases selects the best model", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  best_model <- determine_optimal_phases(sim_data, max_phases = 4)

  expect_type(best_model$best_BIC, "double")
  expect_true(length(best_model$best_model$lambda) <= 4)
  expect_named(best_model$best_model, c("lambda", "mu", "log_likelihood"))
})
