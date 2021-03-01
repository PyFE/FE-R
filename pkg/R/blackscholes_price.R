#' Calculate Black-Scholes option price
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
#' @references Black, F., & Scholes, M. (1973). The Pricing of Options and
#'   Corporate Liabilities. Journal of Political Economy, 81(3), 637-654.
#'   \doi{10.1086/260062}
#'
#'   Black, F. (1976). The pricing of commodity contracts. Journal of Financial
#'   Economics, 3(1), 167-179. \doi{10.1016/0304-405X(76)90024-6}
#'
#'   \url{https://en.wikipedia.org/wiki/Black-Scholes_model}
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

    # a trick to get the intrinsic value for zero (or slightly negative) vol
    # also avoid NAN in case forward=strike
    stdev[abs(stdev) < .Machine$double.eps] <- .Machine$double.eps

    d1 <- log(forward/strike)/stdev
    # this way, stdev=Inf can be handled correctly, i.e., d1=-Inf, d2=Inf
    d2 <- d1 - 0.5*stdev
    d1 <- d1 + 0.5*stdev

    price <- df * cp*(forward*stats::pnorm(cp*d1) - strike*stats::pnorm(cp*d2))
    return( price )
}
