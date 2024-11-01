library(testthat)

test_that("optimize returns lambda and mu of correct length", {
  data_sample <- rexp(100, rate = 0.5)
  init_lambda <- c(0.5, 0)
  init_mu <- c(0.2, 0.4)
  result <- optimize(data_sample, init_lambda, init_mu)

  expect_equal(length(result$lambda), length(init_lambda))
  expect_equal(length(result$mu), length(init_mu))
})

test_that("optimize handles unequal lengths of init_lambda and init_mu", {
  expect_error(optimize(data_sample, c(0.5), c(0.2, 0.4)),
               "Length of init_lambda and init_mu should be the same.")
})
