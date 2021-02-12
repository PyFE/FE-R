#' Calculate Black-Scholes option price
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param intr interest rate
#' @param divr dividend rate
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward forward price. If given, \code{forward} overrides \code{spot}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#' @export
#'
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' texp <- 1.2
#' sigma <- 0.2
#' intr <- 0.05
#' FER::BlackScholesPrice(strike, spot, texp, sigma, intr=intr)
#'
#' @seealso \code{\link{BlackScholesImpvol}}
#'
BlackScholesPrice <- function(
  strike=forward, spot, texp=1, sigma,
  intr=0, divr=0, cp=1L,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){
    stdev <- sigma*sqrt(texp)

    # a trick to get the intrinsic value for negative or zero vol
    # also avoid NAN in case forward=strike
    stdev[stdev < 1e-32] <- 1e-32

    d1 <- log(forward/strike)/stdev + 0.5*stdev
    d2 <- d1 - stdev
    price <- df * cp*(forward*stats::pnorm(cp*d1) - strike*stats::pnorm(cp*d2))
    return( price )
}
