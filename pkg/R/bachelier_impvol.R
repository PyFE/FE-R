#' Calculate Bachelier model implied volatility
#'
#' @param price (vector of) option price
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param intr interest rate
#' @param divr dividend rate
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward forward price. If given, \code{forward} overrides \code{spot}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return Bachelier implied volatility
#'
#' @references Choi, J., Kim, K., & Kwak, M. (2009).
#'   Numerical Approximation of the Implied Volatility Under Arithmetic Brownian
#'   Motion. Applied Mathematical Finance, 16(3), 261-268.
#'   \url{https://doi.org/10.1080/13504860802583436}
#'
#' @export
#'
#' @examples
#' spot <- 100
#' strike <- 100
#' texp <- 1.2
#' sigma <- 20
#' intr <- 0.05
#' price <- 20
#' FER::BachelierImpvol(price, strike, spot, texp, intr=intr)
#'
#' @seealso \code{\link{BachelierPrice}}
#'
BachelierImpvol <- function(
  price, strike=forward, spot, texp=1,
  intr=0, divr=0, cp=1L,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){

  price.straddle <- 2*price/df - cp*(forward - strike)

  # vectors a and b used for rational Chebyshev approximation
  a <- c(3.994961687345134e-1,
       2.100960795068497e1,
       4.980340217855084e1,
       5.988761102690991e2,
       1.848489695437094e3,
       6.106322407867059e3,
       2.493415285349361e4,
       1.266458051348246e4)

  b <- c(1.000000000000000,
       4.990534153589422e1,
       3.093573936743112e1,
       1.495105008310999e3,
       1.323614537899738e3,
       1.598919697679745e4,
       2.392008891720782e4,
       3.608817108375034e3,
      -2.067719486400926e2,
       1.174240599306013e1)

  v <- abs( forward - strike ) / price.straddle

  nu <- ifelse(v<1e-8, 1/(1+v*v*(1/3 + v*v/5)), v/atanh(v))

  poly.nu <- (((((((a[8]*nu+a[7])*nu+a[6])*nu+a[5]))*nu+a[4])*nu+a[3])*nu+a[2])*nu+a[1]
  poly.de <- (((((((((b[10]*nu+b[9])*nu+b[8])*nu+b[7]))*nu+b[6])*nu+b[5])*nu+b[4])*nu+b[3])*nu+b[2])*nu+b[1]

  vol.norm <- sqrt(pi*nu/(2*texp)) * price.straddle * (poly.nu/poly.de)

  return(vol.norm)
}
