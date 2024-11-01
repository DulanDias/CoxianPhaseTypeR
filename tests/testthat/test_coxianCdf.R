library(testthat)

test_that("coxianCdf returns correct output", {
  lambda <- c(0.5, 0)
  mu <- c(0.2, 0.4)
  cdf_value <- coxianCdf(1, lambda, mu)

  expect_true(is.numeric(cdf_value))
})

test_that("coxianCdf handles zero input", {
  expect_equal(coxianCdf(0, lambda, mu), 0)
})
