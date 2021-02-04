#' Calculate Black-Scholes option price
#'
#' @param type option type either 'call' or 'put'
#' @param spot current stock price
#' @param forward forward stock price
#' @param strike strike price
#' @param texp time to expiry
#' @param intr interest rate
#' @param divr dividend rate
#' @param sigma volatility
#' @return option price
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' texp <- 1.2
#' sigma <- 0.2
#' intr <- 0.05
#' price <- FER::BlackScholesPrice(spot=spot, texp = texp, sigma=sigma, strike=strike, intr=intr)
#' @export
BlackScholesPrice <- function(
  type = "call", spot, forward = spot*exp((intr-divr)*texp),
  strike = forward, texp = 1, intr = 0, divr = 0, sigma
){
    stdev <- sigma*sqrt(texp)

    # a trick to get the intrinsic value for negative or zero vol
    # also avoid NAN in case forward = strike
    stdev[stdev < 1e-32] <- 1e-32

    d1 <- log(forward/strike)/stdev + 0.5*stdev
    d2 <- d1 - stdev
    disc.factor <- exp(-intr*texp)

    pnorm.d1 <- pnorm(d1)
    pnorm.d2 <- pnorm(d2)

    if (type == "call" ){
        price <- forward*pnorm.d1 - strike*pnorm.d2
        delta <- pnorm.d1
    }else if (type == "put"){
        price <- strike*(1-pnorm.d2) - forward*(1-pnorm.d1)
        delta <- pnorm.d1 - 1
    }else if (type == "straddle"){
        price <- forward*(2*pnorm.d1 - 1) - strike*(2*pnorm.d2 - 1)
        delta <- 2*pnorm.d1 - 1
    }else if (type == "digit"){
        price <- pnorm.d2
        delta <- 1/sqrt(2*pi)*exp(-d2^2/2)/(s*stdev)
    }
    return( disc.factor * price )
}
