library(FER)

# Table 1 in Antonov & Spector (2012). https://ssrn.com/abstract=2026350
test_that("SabrHagan2002 BS Volatility Table 1", {
  value <- round( FER::SabrHagan2002(
    strike=seq(0.1, 2, 0.1), spot=1, texp=10, sigma=0.25,
    vov=0.3, rho=-0.8, beta=0.3),
    digits = 4 )
  value2 <- c(
    0.7176, 0.5725, 0.4886, 0.4293, 0.3835,
    0.3462, 0.3148, 0.2876, 0.2638, 0.2427,
    0.2238, 0.2068, 0.1916, 0.1781, 0.1663,
    0.1562, 0.1478, 0.1412, 0.136, 0.1322)
  expect_equal(value, value2, tolerance = 1e-5)
})

# Table 13 in Antonov & Spector (2012). https://ssrn.com/abstract=2026350
test_that("SabrHagan2002 BS Volatility Table 13", {
  value <- round( FER::SabrHagan2002(
    strike=seq(0.1, 2, 0.1), spot=1, texp=20, sigma=0.25,
    vov=0.3, rho=-0.5, beta=0.3),
    digits=4 )
  value2 <- c(
    0.7603, 0.5973, 0.5078, 0.4463, 0.3997,
    0.3626, 0.332, 0.3063, 0.2844, 0.2658,
    0.2498, 0.2364, 0.2251, 0.2158, 0.2083,
    0.2023, 0.1976, 0.1941, 0.1914, 0.1895)
  expect_equal(value, value2, tolerance = 1e-5)
})
