library(testthat)

test_that("ksTestCoxian performs the test correctly", {
  data_sample <- rexp(100, rate = 0.5)
  lambda <- c(0.5, 0)
  mu <- c(0.2, 0.4)
  ks_result <- ksTestCoxian(data_sample, lambda, mu)

  expect_true(inherits(ks_result, "htest"))
})
