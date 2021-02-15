library(FER)

test_that("Black Scholes Price", {
  value = BlackScholesPrice(sigma = 0.1, spot = 50, intr = 0.02)
  expect_equal(value , 1.99, tolerance = 0.1)
})
