#' Calculate Black-Scholes implied volatility
#'
#' @param type option type either 'call' or 'put'
#' @param price Price
#' @param spot current stock price
#' @param forward forward stock price
#' @param strike strike price
#' @param t.exp time to expiry
#' @param r interest rate
#' @param div dividend rate
#' @return Implied vol
#' @examples
#' spot <- 100
#' strike <- 100
#' t.exp <- 1.2
#' sigma <- 0.2
#' r <- 0.05
#' price <- 20
#' vol <- FER::BlackScholesImpvol(price=price, spot=spot, strike=strike, t.exp=t.exp, r=r)
#' @export
BlackScholesImpvol <- function(
  type = "call", price, spot, forward = spot*exp((r-div)*t.exp),
  strike = forward, t.exp = 1, r = 0, div = 0
){
    price.forward = price * exp( r*t.exp)

    n.price = length(price.forward)
    n.strike = length(strike)

    vol <- rep(NA, n.price )

    if( length(strike) > 1L ) {
      stopifnot(n.price == n.strike )
      strike.vec <- strike
    } else {
      strike.vec <- rep( strike, n.price )
    }

    for(k in 1:n.price) {
      # Be careful here.... Chekc how functional works in R.
      sub <- function(sigma){
        f <- CalcBsmPrice(
          type = type, forward = forward, strike = strike.vec[k], t.exp = t.exp, sigma = sigma
        )[1] - price.forward[k]
        return(f)
      }
      vol[k] <- uniroot(f = sub,interval = c(0,10), tol = .Machine$double.eps * 1e4)$root
    }
    return(vol)
}
