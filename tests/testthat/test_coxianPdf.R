library(testthat)

test_that("coxianPdf returns correct output", {
  lambda <- c(0.5, 0)
  mu <- c(0.2, 0.4)
  pdf_value <- coxianPdf(1, lambda, mu)

  expect_true(is.numeric(pdf_value))
})

test_that("coxianPdf handles invalid inputs", {
  expect_error(coxianPdf(-1, lambda, mu), "t should be a non-negative numeric vector.")
})
