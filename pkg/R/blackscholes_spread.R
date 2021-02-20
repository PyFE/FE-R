#' Margrabe's formula for exhange option price
#'
#' @description The payout of the exchange option is
#'   \code{max(S1_T - S2_T, 0)} where \code{S1_T} and \code{S2_T} are the
#'   prices at expiry \code{T} of assets 1 and 2 respectively.
#'
#' @param spot1 (vector of) spot price of asset 1
#' @param spot2 (vector of) spot price of asset 2
#' @param texp (vector of) time to expiry
#' @param sigma1 (vector of) volatility of asset 1
#' @param sigma2 (vector of) volatility of asset 2
#' @param corr correlation
#' @param intr interest rate
#' @param divr1 dividend rate of asset 1
#' @param divr2 dividend rate of asset 2
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward1 forward price. If given, overrides \code{spot1}
#' @param forward2 forward price. If given, overrides \code{spot2}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @export
#'
#' @references Margrabe, W. (1978). The value of an option to exchange one
#'   asset for another. The Journal of Finance, 33(1), 177–186.
#'
#' @seealso \code{\link{SpreadKirk}}
#'
#' @examples
#'
#' FER::SwitchMargrabe(100, 120, 1.3, 0.2, 0.3, -0.5)
#'
SwitchMargrabe <- function(
  spot1, spot2, texp=1, sigma1, sigma2, corr,
  intr=0, divr1=0, divr2=0, cp=1L,
  forward1=spot1*exp(-divr1*texp)/df,
  forward2=spot2*exp(-divr2*texp)/df,
  df=exp(-intr*texp)
){
  vol.spread <- sqrt(sigma1^2 - 2*corr*sigma1*sigma2 + sigma2^2)
  val <- df * BlackScholesPrice(
    strike=forward2, forward=forward1, texp=texp, sigma=vol.spread, cp=cp)
  return(val)
}

#' Kirk's approximation for spread option
#'
#' @description The payout of the spread option is
#'   \code{max(S1_T - S2_T - K, 0)} where \code{S1_T} and \code{S2_T} are the
#'   prices at expiry \code{T} of assets 1 and 2 respectively and \code{K} is
#'   the strike price.
#'
#' @param strike (vector of) strike price
#' @param spot1 (vector of) spot price of asset 1
#' @param spot2 (vector of) spot price of asset 2
#' @param texp (vector of) time to expiry
#' @param sigma1 (vector of) volatility of asset 1
#' @param sigma2 (vector of) volatility of asset 2
#' @param corr correlation
#' @param intr interest rate
#' @param divr1 dividend rate of asset 1
#' @param divr2 dividend rate of asset 2
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward1 forward price. If given, overrides \code{spot1}
#' @param forward2 forward price. If given, overrides \code{spot2}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @references Kirk, E. (1995). Correlation in the energy markets. In Managing
#'   Energy Price Risk (First, pp. 71–78). Risk Publications.
#'
#' @export
#'
#' @seealso \code{\link{SwitchMargrabe}}
#'
#' @examples
#'
#' FER::SpreadKirk((-2:2)*10, 100, 120, 1.3, 0.2, 0.3, -0.5)
#'
SpreadKirk <- function(
  strike=0, spot1, spot2, texp=1, sigma1, sigma2, corr,
  intr=0, divr1=0, divr2=0, cp=1L,
  forward1=spot1*exp(-divr1*texp)/df,
  forward2=spot2*exp(-divr2*texp)/df,
  df=exp(-intr*texp)
){
  k.plus = pmax(strike, 0)
  k.minus = pmin(strike, 0)

  sigma1.shift <- sigma1*forward1/(forward1 - k.minus)
  sigma2.shift <- sigma2*forward2/(forward2 + k.plus)

  val <- df * SwitchMargrabe(
    forward1=spot1-k.minus, forward2=spot2+k.plus, texp=texp,
    sigma1=sigma1, sigma2=sigma2.shift, corr=corr, cp=cp)
  return(val)
}
