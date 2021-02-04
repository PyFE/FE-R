#' Constant Elasticity Of Variance (CEV) model option price
#'
#' @param strike strike price
#' @param spot current stock price
#' @param forward forward stock price
#' @param texp time to expiry
#' @param sigma volatility
#' @param beta
#' @param intr interest rate
#' @param divr dividend rate
#' @param type option type either 'call' or 'put'
#'
#' @return option price
#' @export
#'
#' @examples
CEV <- function(
  strike, spot, forward = spot*exp((intr-divr)*texp),
  texp=1, sigma=0.01, beta=0.5,
  intr = 0, divr = 0, type="call"
){
  betac <- 1 - beta
  scale <- (betac*sigma)^2*texp
  strike_cov = strike^(2*betac) / scale # strike change of variable
  forward_cov = forward^(2*betac) / scale # forward change of variable
  df <- 1/betac

  iscall = (type=="call")

  term1 <- stats::pchisq(strike_cov, df=df+2, ncp=forward_cov, lower.tail=!iscall)
  term2 <- stats::pchisq(forward_cov, df=df, ncp=strike_cov, lower.tail=iscall)

  price <- ifelse(iscall, 1, -1)*exp(-intr*texp)*(forward*term1 - strike*term2)

  return(price)
}
