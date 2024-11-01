library(testthat)

test_that("fitCoxian returns a list of expected length", {
  data_sample <- rexp(100, rate = 0.5)
  result <- fitCoxian(data_sample, 2)

  expect_true(is.list(result))
  expect_equal(length(result), 5)  # Expecting lambda, mu, nll, ssd, AIC
})

test_that("fitCoxian handles incorrect input", {
  expect_error(fitCoxian(c("a", "b"), 2), "non-numeric argument")
})
