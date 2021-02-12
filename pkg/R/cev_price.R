#' Calculate the constant elasticity of variance (CEV) model option price
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param beta beta
#' @param intr interest rate
#' @param divr dividend rate
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward forward price. If given, \code{forward} overrides \code{spot}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @references Schroder, M. (1989). Computing the constant elasticity
#'   of variance option pricing formula. Journal of Finance,
#'   44(1), 211-219. https://doi.org/10.1111/j.1540-6261.1989.tb02414.x
#'
#' @export
#'
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' texp <- 1.2
#' beta <- 0.5
#' sigma <- 2
#' FER::CevPrice(strike, spot, texp, sigma, beta)
#'
CevPrice <- function(
  strike=forward, spot, texp=1, sigma, beta=0.5,
  intr=0, divr=0, cp=1L,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){
  betac <- 1.0 - beta
  scale <- (betac*sigma)^2*texp
  strike_cov <- strike^(2*betac) / scale # strike change of variable
  forward_cov <- forward^(2*betac) / scale # forward change of variable
  deg <- 1/betac  # degree of freedom

  term1 <- stats::pchisq(strike_cov, df=deg+2, ncp=forward_cov, lower.tail=(cp<0))
  term2 <- stats::pchisq(forward_cov, df=deg, ncp=strike_cov, lower.tail=(cp>0))

  price <- cp*df*(forward*term1 - strike*term2)

  return(price)
}
