library(FER)

test_that("Spread Put-Call Parity", {
  strike <- 10 + (-3:3)*5
  spot1 <- 110
  spot2 <- 100
  texp <- 1.3
  sigma1 <- 0.2
  sigma2 <- 0.3
  corr <- -0.5
  intr <- 0.1
  divr1 <- 0.05
  divr2 <- 0.07

  # SwitchMargrabe
  val2 = exp(-divr1*texp)*(spot1 + strike) - exp(-divr2*texp)*spot2
  c <- SwitchMargrabe(spot1 + strike, spot2, texp, sigma1, sigma2, corr, intr=intr,
                  divr1=divr1, divr2=divr2, cp=1L)
  p <- SwitchMargrabe(spot1 + strike, spot2, texp, sigma1, sigma2, corr, intr=intr,
                  divr1=divr1, divr2=divr2, cp=-1L)
  expect_equal(c - p, val2, tolerance = 1e-10)


  # For the rest
  val2 = exp(-divr1*texp)*spot1 - exp(-divr2*texp)*spot2 - exp(-intr*texp)*strike

  # SpreadKirk
  c <- SpreadKirk(strike, spot1, spot2, texp, sigma1, sigma2, corr, intr=intr,
                  divr1=divr1, divr2=divr2, cp=1L)
  p <- SpreadKirk(strike, spot1, spot2, texp, sigma1, sigma2, corr, intr=intr,
                  divr1=divr1, divr2=divr2, cp=-1L)
  expect_equal(c - p, val2, tolerance = 1e-10)

  # SpreadBjerksund
  c <- SpreadBjerksund(strike, spot1, spot2, texp, sigma1, sigma2, corr, intr=intr,
                       divr1=divr1, divr2=divr2, cp=1L)
  p <- SpreadBjerksund(strike, spot1, spot2, texp, sigma1, sigma2, corr, intr=intr,
                       divr1=divr1, divr2=divr2, cp=-1L)
  expect_equal(c - p, val2, tolerance = 1e-10)

  # SpreadBachelier
  c <- SpreadBachelier(strike, spot1, spot2, texp, sigma1*spot1, sigma2*spot2, corr, intr=intr,
                       divr1=divr1, divr2=divr2, cp=1L)
  p <- SpreadBachelier(strike, spot1, spot2, texp, sigma1*spot1, sigma2*spot2, corr, intr=intr,
                       divr1=divr1, divr2=divr2, cp=-1L)
  expect_equal(c - p, val2, tolerance = 1e-10)
})


