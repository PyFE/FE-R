#' Calculate Black-Scholes implied volatility
#'
#' @param price (vector of) option price
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param intr interest rate
#' @param divr dividend rate
#' @param cpsign call/put sign. 1 for call, -1 for put.
#' @param forward forward price. If given, forward overrides spot
#' @param df discount factor. If given, df overrides intr
#' @return Black-Scholes implied volatility
#'
#' @export
#'
#' @examples
#' spot <- 100
#' strike <- 100
#' texp <- 1.2
#' sigma <- 0.2
#' intr <- 0.05
#' price <- 20
#' vol <- FER::BlackScholesImpvol(price, strike, spot, texp, intr=intr)
#'
BlackScholesImpvol <- function(
  price, strike = forward, spot, texp = 1,
  intr = 0, divr = 0, cpsign = 1,
  forward = spot*exp(-divr*texp)/df,
  df = exp(-intr*texp)
){
  optval = (price/df - pmax(cpsign*(forward-strike), 0))/pmin(forward, strike)

  # we use inverse CDF of inversegaussian distribution
  mu <- 2/abs(log(strike/forward))
  x <- statmod::qinvgauss(optval, mean=mu, lower.tail=F)
  sig <- 2/sqrt(x*texp)
  return( sig )
}
