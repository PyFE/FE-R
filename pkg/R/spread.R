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
#' @param forward1 forward price of asset 1. If given, overrides \code{spot1}
#' @param forward2 forward price of asset 2. If given, overrides \code{spot2}
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
#' @param forward1 forward price of asset 1. If given, overrides \code{spot1}
#' @param forward2 forward price of asset 2. If given, overrides \code{spot2}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @export
#'
#' @references Kirk, E. (1995). Correlation in the energy markets. In Managing
#'   Energy Price Risk (First, pp. 71–78). Risk Publications.
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
    forward1=forward1-k.minus, forward2=forward2+k.plus, texp=texp,
    sigma1=sigma1.shift, sigma2=sigma2.shift, corr=corr, cp=cp)
  return(val)
}

#' Spread option pricing method by Bjerksund & Stensland (2014)
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
#' @param forward1 forward price of asset 1. If given, overrides \code{spot1}
#' @param forward2 forward price of asset 2. If given, overrides \code{spot2}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @export
#'
#' @references Bjerksund, P., & Stensland, G. (2014). Closed form spread option
#'   valuation. Quantitative Finance, 14(10), 1785–1794.
#'   \doi{10.1080/14697688.2011.617775}
#'
#' @examples
#'
#' FER::SpreadBjerksund((-2:2)*10, 100, 120, 1.3, 0.2, 0.3, -0.5)
#'
SpreadBjerksund <- function(
  strike=0, spot1, spot2, texp=1, sigma1, sigma2, corr,
  intr=0, divr1=0, divr2=0, cp=1L,
  forward1=spot1*exp(-divr1*texp)/df,
  forward2=spot2*exp(-divr2*texp)/df,
  df=exp(-intr*texp)
){
  std11 <- sigma1^2 * texp
  std12 <- sigma1*sigma2 * texp
  std22 <- sigma2^2 * texp

  a <- forward2 + strike
  b <- forward2/a
  std <- sqrt(std11 - 2*b*corr*std12 + b^2*std22)

  d3 <- log(forward1/a)
  d1 <- ( d3 + 0.5*std11 - b*(corr*std12 - 0.5*b*std22) ) / std
  d2 <- ( d3 - 0.5*std11 + corr*std12 + b*(0.5*b - 1)*std22 ) / std
  d3 <- ( d3 - 0.5*std11 + 0.5*b^2*std22 ) / std
  val <- cp*(forward1*stats::pnorm(cp*d1) - forward2*stats::pnorm(cp*d2)
             - strike*stats::pnorm(cp*d3))
  return(df*val)
}

#' Spread option under the Bachelier model
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
#' @param sigma1 (vector of) Bachelier volatility of asset 1
#' @param sigma2 (vector of) Bachelier volatility of asset 2
#' @param corr correlation
#' @param intr interest rate
#' @param divr1 dividend rate of asset 1
#' @param divr2 dividend rate of asset 2
#' @param cp call/put sign. \code{1} for call, \code{-1} for put.
#' @param forward1 forward price of asset 1. If given, overrides \code{spot1}
#' @param forward2 forward price of asset 2. If given, overrides \code{spot2}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return option price
#'
#' @export
#'
#' @examples
#'
#' FER::SpreadBachelier((-2:2)*10, 100, 120, 1.3, 20, 36, -0.5)
#'
SpreadBachelier <- function(
  strike=0, spot1, spot2, texp=1, sigma1, sigma2, corr,
  intr=0, divr1=0, divr2=0, cp=1L,
  forward1=spot1*exp(-divr1*texp)/df,
  forward2=spot2*exp(-divr2*texp)/df,
  df=exp(-intr*texp)
){
  forward = forward1 - forward2
  sigma = sqrt(sigma1^2 - 2*corr*sigma1*sigma2 + sigma2^2)

  val <- BachelierPrice(strike, , texp, sigma, cp=cp, forward=forward, df=df)
  return(val)
}
