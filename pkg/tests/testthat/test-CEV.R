library(FER)

test_that("CEV", {
  value <- round( FER::CevPrice(
    strike=seq(0.4, 2, 0.4)*0.05, spot=0.05, texp=1, sigma=0.4, beta=0.3),
    digits=5 )
  value2 <- c(0.04608, 0.04229, 0.03868, 0.03525, 0.03203)
  expect_equal(value, value2, tolerance = 1e-6)
})
