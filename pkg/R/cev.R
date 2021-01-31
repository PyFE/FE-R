CEV <- function(
  strike, spot, t.exp=1, sigma=0.01, vov=0, rho=0, beta=0.5,
  r = 0, div = 0, type="call"
){
  forward = spot*exp((r-div)*t.exp)

  betac <- 1 - beta
  scale <- (betac*sigma)^2*t.exp
  strike_cov = strike^(2*betac) / scale # strike change of variable
  forward_cov = forward^(2*betac) / scale # forward change of variable
  df <- 1/betac

  iscall = (type=="call")

  term1 <- stats::pchisq(strike_cov, df=df+2, ncp=forward_cov, lower.tail=!iscall)
  term2 <- stats::pchisq(forward_cov, df=df, ncp=strike_cov, lower.tail=iscall)

  price <- ifelse(iscall, 1, -1)*exp(-r*t.exp)*(forward*term1 - strike*term2)

  return(price)
}
