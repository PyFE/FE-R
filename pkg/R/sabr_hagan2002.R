#' Hagan approximation for the SABR model
#'
#' @param strike strike price
#' @param spot current stock price
#' @param forward forward stock price
#' @param texp time to expiry
#' @param sigma volatility
#' @param vov
#' @param rho
#' @param beta
#' @param intr interest rate
#' @param divr dividend rate
#' @param type 'BlackScholes', 'call', 'put'
#'
#' @return Black-Scholes volatility (type="BlackScholes") or option price ("call" or "put")
#' @export
#'
#' @examples
SabrHagan2002 <- function(
  strike, spot, forward = spot*exp((intr-divr)*texp),
  texp=1, sigma=0.01, vov=0, rho=0, beta=1,
  intr = 0, divr = 0, type="BlackScholes"
){
  betac <- 1 - beta
  powFwdStrk <- (forward*strike)^(betac/2)
  logFwdStrk <- log(forward/strike)
  logFwdStrk2 <- logFwdStrk^2

  pre1 = powFwdStrk*( 1 + betac^2/24 * logFwdStrk2*(1 + betac^2/80 * logFwdStrk2) )

  pre2alp0 <- (2-3*rho^2)*vov^2/24
  pre2alp1 <- vov*rho*beta/4/powFwdStrk
  pre2alp2 <- betac^2/24/powFwdStrk^2

  pre2 <- 1 + texp*( pre2alp0 + sigma*(pre2alp1 + pre2alp2*sigma) )

  zz <- powFwdStrk*logFwdStrk*vov/sigma  # need to make sure sig > 0
  yy <- sqrt(1 + zz*(zz-2*rho))

  rho2 <- rho*rho
  xx_zz[I] = 1 + (zz/2)*(rho + zz*((rho2-1/3) + (5*rho2-3)/4*rho*zz))

  I <- (zz >= 1e-5)
  xx_zz[I] = log( (yy[I] + (zz[I]-rho))/(1-rho) ) / zz[I]
  I <- (zz <= -1e-5)
  xx_zz[I] = log( (1+rho)/(yy[I] - (zz[I]-rho)) ) / zz[I]

  volBlks = sigma*pre2/(pre1*xx_zz) # blks vol

  if(type=="BlackScholes"){
    return(volBlks)
  } else if(type=="call" | type=="put") {
    p <- BlackScholesPrice(
      type=type, spot=spot, strike=strike, texp=texp, sigma=volBlks,
      intr=intr, divr=divr
    )
    return(p)
  }
}
