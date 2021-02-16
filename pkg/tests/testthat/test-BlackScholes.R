library(FER)

test_that("Black-Scholes Price and Impvol", {
  strike <- runif(1000, 50, 150)
  sigma <- runif(1000, 0.001, 2)
  texp <- runif(1000, 1e-4, 10)
  intr <- runif(1000, 0, 0.1)
  divr <- runif(1000, 0, 0.1)
  cp <- ifelse(runif(100)>0.5, 1L, -1L)

  p <- FER::BlackScholesPrice(strike, 100, texp, sigma, cp=cp, intr=intr, divr=divr)
  iv <- FER::BlackScholesImpvol(p, strike, 100, texp, cp=cp, intr=intr, divr=divr)
  p2 <- FER::BlackScholesPrice(strike, 100, texp, iv, cp=cp, intr=intr, divr=divr)
  expect_equal(p, p2, tolerance = 1e-8)
})
