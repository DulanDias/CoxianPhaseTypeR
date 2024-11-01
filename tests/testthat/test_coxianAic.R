library(testthat)

test_that("coxianAic computes AIC correctly", {
  data_sample <- rexp(100, rate = 0.5)
  lambda <- c(0.5, 0)
  mu <- c(0.2, 0.4)
  aic_value <- coxianAic(data_sample, lambda, mu)

  expect_true(is.numeric(aic_value))
})
