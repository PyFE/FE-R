#' Constant Elasticity Of Variance (CEV) model option price
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param beta beta
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
#' beta <- 0.5
#' sigma <- 2
#' price <- FER::CevPrice(strike, spot, texp, sigma, beta)
#'
CevPrice <- function(
  strike = forward, spot, texp = 1, sigma, beta=0.5,
  intr = 0, divr = 0, cpsign = 1,
  forward = spot*exp(-divr*texp)/df,
  df = exp(-intr*texp)
){
  betac <- 1.0 - beta
  scale <- (betac*sigma)^2*texp
  strike_cov = strike^(2*betac) / scale # strike change of variable
  forward_cov = forward^(2*betac) / scale # forward change of variable
  deg <- 1/betac  # degree of freedom

  term1 <- stats::pchisq(strike_cov, df=deg+2, ncp=forward_cov, lower.tail=(cpsign<0))
  term2 <- stats::pchisq(forward_cov, df=deg, ncp=strike_cov, lower.tail=(cpsign>0))

  price <- cpsign*df*(forward*term1 - strike*term2)

  return(price)
}
