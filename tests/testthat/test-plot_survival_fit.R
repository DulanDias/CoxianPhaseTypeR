test_that("plot_survival_fit generates a ggplot object with goodness-of-fit metrics", {
  library(ggplot2)

  set.seed(123)
  sim_data <- simulate_coxian(n = 100, lambda = c(2.0, 1.5, 1.2), mu = c(1.0, 0.8, 0.6))
  fit <- fit_coxian(sim_data, num_phases = 3)

  plot_obj <- plot_survival_fit(fit, sim_data)

  expect_s3_class(plot_obj, "ggplot")
})
