test_that("simulate_coxian generates valid survival times", {
  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))

  expect_type(sim_data, "double")  # Expect numeric output
  expect_length(sim_data, 100)  # Expect correct length
  expect_true(all(sim_data > 0))  # Survival times must be positive
  expect_false(any(is.na(sim_data))) # No missing values
})
