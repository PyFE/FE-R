#' Calculate Bachelier model option price
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param intr interest rate (domestic interest rate)
#' @param divr dividend/convenience yield (foreign interest rate)
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward forward price. If given, \code{forward} overrides \code{spot}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#' @export
#'
#' @references Choi, J., Kim, K., & Kwak, M. (2009).
#'   Numerical Approximation of the Implied Volatility Under Arithmetic Brownian
#'   Motion. Applied Mathematical Finance, 16(3), 261-268.
#'   \doi{10.1080/13504860802583436}
#'
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' texp <- 1.2
#' sigma <- 20
#' intr <- 0.05
#' FER::BachelierPrice(strike, spot, texp, sigma, intr=intr)
#'
#' @seealso \code{\link{BachelierImpvol}}
#'
BachelierPrice <- function(
  strike=forward, spot, texp=1, sigma,
  intr=0, divr=0, cp=1L,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){
  stdev <- sigma*sqrt(texp)
  stdev[stdev < 1e-32] <- 1e-32
  dn <- cp*(forward-strike)/stdev

  #Option Price
  price <- df * (cp*(forward-strike)*stats::pnorm(dn) + stdev*stats::dnorm(dn))
  return( price )
}
