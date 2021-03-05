library(FER)

test_that("CEV Reference Price", {
  value <- round( FER::CevPrice(
    strike=seq(0.4, 2, 0.4)*0.05, spot=0.05, texp=1, sigma=0.4, beta=0.3),
    digits=5 )
  value2 <- c(0.04608, 0.04229, 0.03868, 0.03525, 0.03203)
  expect_equal(value, value2, tolerance = 1e-6)
})

test_that("CEV Put Price vs MassZero", {
  value1 <- FER::CevPrice(
    strike=0.001, spot=1, texp=1:10, sigma=0.5, beta=0.2, cp=-1
  )*1000
  value2 <- FER::CevMassZero(spot=1, texp=1:10, sigma=0.5, beta=0.2)
  expect_equal(value1, value2, tolerance = 1e-5)
})


