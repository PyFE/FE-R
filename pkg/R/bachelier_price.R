#' Calculate Bachelier model option price
#'
#' @param type option type either "call" or "put"
#' @param price Price
#' @param spot current stock price
#' @param forward forward stock price
#' @param t.exp time to expiry
#' @param r interest rate
#' @param div dividend rate
#' @param sigma volatility
#' @return option price
#' @examples
#' spot <- 100
#' strike <- seq(80,125,5)
#' t.exp <- 1.2
#' sigma <- 0.2
#' r <- 0.05
#' price <- FER::BachelierPrice(spot=spot, t.exp = t.exp, sigma=sigma, strike=strike, r=r)
#' @export
BachelierPrice <- function(
  type = "call", spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0, sigma
){

  #------------------------------------------------
  #------------------------------------------------
  #Inputs:
  #       type: "call","put","straddle","digital"
  #       s:  spot price of the underlying asset
  #       k: strike price
  #       t: time to maturity
  #       r: risk-free rate
  #       sigma: volatility
  #Outputs:
  #       price: option price
  #       delta: option delta
  #       gamma: option gamma
  #       vega:  option vega
  #------------------------------------------------
  #------------------------------------------------

  stdev <- sigma*sqrt(t.exp)
  d1 <- (forward-strike) / stdev
  pnorm.d1 <- pnorm(d1) # normal CDF
  dnorm.d1 <- dnorm(d1) # normal PDF =exp(-d1*d1/2)/sqrt(2*pi)
  disc.factor <- exp(-r*t.exp)

  if(type=="call"){
    #Option Price
    price <- (forward-strike)*pnorm.d1 + stdev*dnorm.d1
    #Greeks
    delta <- pnorm.d1 #Delta
    gamma <- 1 / (sigma*t.exp)/sqrt(2*pi)*exp(-d1^2/2)#Gamma
    vega <- sqrt(t.exp)/sqrt(2*pi)*exp(-d1^2/2)#Vega

  }else if (type=="put"){
    #Option Price
    price <- (strike-forward)*(1-pnorm.d1) + stdev*dnorm.d1
    #Greeks
    delta <- pnorm.d1 - 1 #Delta
    gamma <- 1 / (sigma*t.exp)/sqrt(2*pi)*exp(-d1^2/2)#Gamma
    vega <- sqrt(t.exp)/sqrt(2*pi)*exp(-d1^2/2)#Vega

  } else if (type=="straddle"){
    #Straddle price
    price <- (forward-strike)*(2*pnorm.d1-1) + 2*stdev*dnorm.d1
    delta <- 2*pnorm.d1 - 1
    gamma <- 2 / (sigma*t.exp)/sqrt(2*pi)*exp(-d1^2/2)
    vega <- 2 * sqrt(t.exp)/sqrt(2*pi)*exp(-d1^2/2)#Vega

  } else if (type== "digital call"){
    #Digital call price
    price <- pnorm(d1)
    delta <- 1 / sqrt(2*pi)*exp(-d1^2/2)/(sigma*sqrt(t))
    gamma <- 1 / sqrt(2*pi)*exp(-d1^2/2)/(sigma^2*t)
    vega <- -1 / sqrt(2*pi)*exp(-d1^2/2)*(spot-strike)/(sigma^2*sqrt(t))

  } else if (type== "digital put"){
    #Digital put price
    price <- pnorm(-d1)
    delta <- -1 / sqrt(2*pi)*exp(-d1^2/2)/(sigma*sqrt(t))
    gamma <- 1 / sqrt(2*pi)*exp(-d1^2/2)/(sigma^2*t)
    vega <- -1 / sqrt(2*pi)*exp(-d1^2/2)*(spot-strike)/(sigma^2*sqrt(t))
  } else {
    cat("Error! Please input: call/put/straddle/digital call/digital put")
  }
  return( disc.factor * price )
}

