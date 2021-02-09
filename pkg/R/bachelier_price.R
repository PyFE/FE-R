#' Calculate Bachelier model option price
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param intr interest rate
#' @param divr dividend rate
#' @param cpsign call/put sign. 1 for call, -1 for put.
#' @param forward forward price. If given, forward overrides spot
#' @param df discount factor. If given, df overrides intr
#' @return option price
#'
#' @export
#'
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' texp <- 1.2
#' sigma <- 20
#' intr <- 0.05
#' price <- FER::BachelierPrice(strike, spot, texp, sigma, intr=intr)
#'
BachelierPrice <- function(
  strike = forward, spot, texp = 1, sigma,
  intr = 0, divr = 0, cpsign=1,
  forward = spot*exp(-divr*texp)/df,
  df = exp(-intr*texp)
){
  stdev <- sigma*sqrt(texp)
  stdev[stdev < 1e-32] <- 1e-32
  dn <- cpsign*(forward-strike) / stdev

  #Option Price
  price <- df*(cpsign*(forward-strike)*pnorm(dn) + stdev*dnorm(dn))
  return( price )
}
