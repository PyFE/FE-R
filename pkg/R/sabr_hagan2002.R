#' Calculate the equivalent BS volatility (Hagan et al. 2002) for
#'   the Stochatic-Alpha-Beta-Rho (SABR) model
#'
#' @param strike (vector of) strike price
#' @param spot (vector of) spot price
#' @param texp (vector of) time to expiry
#' @param sigma (vector of) volatility
#' @param vov (vector of) vol-of-vol
#' @param rho (vector of) correlation
#' @param beta (vector of) beta
#' @param intr interest rate
#' @param divr dividend rate
#' @param cp call/put sign. \code{NULL} for BS vol (default), \code{1} for call price, \code{-1} for put price.
#' @param forward forward price. If given, \code{forward} overrides \code{spot}
#' @param df discount factor. If given, \code{df} overrides \code{intr}
#' @return BS volatility or option price based on \code{cp}
#'
#' @references Hagan, P. S., Kumar, D., Lesniewski, A. S., & Woodward, D. E. (2002).
#'   Managing Smile Risk. Wilmott, September, 84-108.
#'
#' @export
#'
#' @examples
#' sigma <- 0.25
#' vov <- 0.3
#' rho <- -0.8
#' beta <- 0.3
#' texp <- 10
#' strike <- seq(0.1, 2, 0.1)
#' FER::SabrHagan2002(strike, 1, texp, sigma, vov, rho, beta)
#' FER::SabrHagan2002(strike, 1, texp, sigma, vov, rho, beta, cp=1)
#'
SabrHagan2002 <- function(
  strike=forward, spot, texp=1, sigma, vov=0, rho=0, beta=1,
  intr=0, divr=0, cp=NULL,
  forward=spot*exp(-divr*texp)/df, df=exp(-intr*texp)
){
  betac <- 1 - beta
  betac2 <- betac*betac
  rho2 <- rho*rho

  powFwdStrk <- (forward*strike)^(betac/2)
  logFwdStrk <- log(forward/strike)
  logFwdStrk2 <- logFwdStrk^2

  pre1 <- powFwdStrk*( 1 + betac2/24 * logFwdStrk2*(1 + betac2/80 * logFwdStrk2) )

  pre2alp0 <- (2-3*rho2)*vov^2/24
  pre2alp1 <- vov*rho*beta/4/powFwdStrk
  pre2alp2 <- betac2/24/powFwdStrk^2

  pre2 <- 1 + texp*( pre2alp0 + sigma*(pre2alp1 + pre2alp2*sigma) )

  zz <- powFwdStrk*logFwdStrk*vov/sigma  # need to make sure sig > 0
  yy <- sqrt(1 + zz*(zz-2*rho))

  xx_zz <- 1 + (zz/2)*(rho + zz*((rho2-1/3) + (5*rho2-3)/4*rho*zz))

  I <- (zz >= 1e-5)
  xx_zz[I] <- log( (yy[I] + (zz[I]-rho))/(1-rho) ) / zz[I]
  I <- (zz <= -1e-5)
  xx_zz[I] <- log( (1+rho)/(yy[I] - (zz[I]-rho)) ) / zz[I]

  vol.bs <- sigma*pre2/(pre1*xx_zz) # BS volatility

  if(is.null(cp)){
    return(vol.bs)
  } else {
    # if cp is specified, calculate option price
    p <- df * BlackScholesPrice(strike, forward, texp, sigma=vol.bs, cp=cp)
    return(p)
  }
}
