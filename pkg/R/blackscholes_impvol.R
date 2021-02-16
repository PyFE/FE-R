#' Calculate Black-Scholes implied volatility
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
#' @return Black-Scholes implied volatility
#'
#' @references Giner, G., & Smyth, G. K. (2016). statmod: Probability Calculations
#'   for the Inverse Gaussian Distribution. The R Journal, 8(1), 339-351.
#'   \url{https://doi.org/10.32614/RJ-2016-024}
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
#' FER::BlackScholesImpvol(price, strike, spot, texp, intr=intr)
#'
#' @seealso \code{\link{BlackScholesPrice}}
#'
BlackScholesImpvol <- function(
  price, strike=forward, spot, texp=1,
  intr=0, divr=0, cp=1L,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){
  timeval <- (price/df - pmax(cp*(forward-strike), 0))/pmin(forward, strike)
  # when the time value is very slightly negative, we correct to give zero vol.
  timeval[price>0 & -8*.Machine$double.eps<timeval & timeval<0] <- 0

  # we use inverse CDF of inversegaussian distribution
  mu <- 2/abs(log(strike/forward))
  x <- statmod::qinvgauss(timeval, mean=mu, lower.tail=F)
  sig <- 2/sqrt(x*texp)
  return( sig )
}
