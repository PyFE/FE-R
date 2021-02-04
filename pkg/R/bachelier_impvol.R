#' Calculate Bachelier model implied volatility
#'
#' @param type option type either 'call' or 'put'
#' @param price Price
#' @param spot current stock price
#' @param forward forward stock price
#' @param strike strike price
#' @param texp time to expiry
#' @param intr interest rate
#' @param divr dividend rate
#' @return implied vol
#' @examples
#' spot <- 100
#' strike <- 100
#' texp <- 1.2
#' sigma <- 0.2
#' intr <- 0.05
#' price <- 20
#' vol <- FER::BachelierImpvol(price=price, spot=spot, strike=strike, texp=texp, intr=intr)
#' @export
BachelierImpvol <- function(
  type = "call", price, spot, forward = spot*exp((intr-divr)*texp),
  strike = forward, texp = 1, intr = 0, divr = 0
){

  price.forward = price * exp(intr*texp)

  if( type == "call" ) {
    price.straddle <- 2*price.forward - (forward - strike)
  } else if( type == "put" ) {
    price.straddle <- 2*price.forward + (forward - strike)
  } else if( type == "straddle") {
    price.straddle <- price.forward
  }

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

  #implied volatility when current stock price
  #is different from the strike price

  #variable v which is bounded in the range [-1, 1],
  #since the straddle price is always worth more
  #than the intrinsic value |F-K|.
  v <- abs( forward - strike ) / price.straddle
  #transformation of v used for better
  #approximation of h

  nu <- ifelse(v<1e-8, 1/(1+v*v*(1/3 + v*v/5)), v/atanh(v))

  poly.a <- (((((((a[8]*nu+a[7])*nu+a[6])*nu+a[5]))*nu+a[4])*nu+a[3])*nu+a[2])*nu+a[1]
  poly.b <- (((((((((b[10]*nu+b[9])*nu+b[8])*nu+b[7]))*nu+b[6])*nu+b[5])*nu+b[4])*nu+b[3])*nu+b[2])*nu+b[1]

  #approximation of h(n)
  #implied volatility
  vol <- sqrt(pi*nu/(2*texp)) * price.straddle * (poly.a/poly.b)

  return(vol)
}
