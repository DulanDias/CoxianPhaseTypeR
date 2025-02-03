test_that("compute_goodness_of_fit returns valid RMSE and KS statistics", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3)

  gof_metrics <- compute_goodness_of_fit(fit, sim_data)

  expect_named(gof_metrics, c("RMSE", "KS_statistic"))
  expect_type(gof_metrics$RMSE, "double")
  expect_type(gof_metrics$KS_statistic, "double")
  expect_true(gof_metrics$RMSE >= 0)  # RMSE cannot be negative
  expect_true(gof_metrics$KS_statistic >= 0)  # KS statistic is always positive
})
