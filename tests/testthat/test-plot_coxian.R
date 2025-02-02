test_that("plot_coxian generates a ggplot object", {
  library(ggplot2)

  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3)

  plot_obj <- plot_coxian(fit)

  expect_s3_class(plot_obj, "ggplot")
})
