# Calculate the equivalent BS volatility (Hagan et al. 2002) for the Stochatic-Alpha-Beta-Rho (SABR) model

Calculate the equivalent BS volatility (Hagan et al. 2002) for the
Stochatic-Alpha-Beta-Rho (SABR) model

## Usage

``` r
SabrHagan2002(
  strike = forward,
  spot,
  texp = 1,
  sigma,
  vov = 0,
  rho = 0,
  beta = 1,
  intr = 0,
  divr = 0,
  cp = NULL,
  forward = spot * exp(-divr * texp)/df,
  df = exp(-intr * texp)
)
```

## Arguments

- strike:

  (vector of) strike price

- spot:

  (vector of) spot price

- texp:

  (vector of) time to expiry

- sigma:

  (vector of) volatility

- vov:

  (vector of) vol-of-vol

- rho:

  (vector of) correlation

- beta:

  (vector of) beta

- intr:

  interest rate (domestic interest rate)

- divr:

  convenience rate (foreign interest rate)

- cp:

  call/put sign. `NULL` for BS vol (default), `1` for call price, `-1`
  for put price.

- forward:

  forward price. If given, `forward` overrides `spot`

- df:

  discount factor. If given, `df` overrides `intr`

## Value

BS volatility or option price based on `cp`

## References

Hagan, P. S., Kumar, D., Lesniewski, A. S., & Woodward, D. E. (2002).
Managing Smile Risk. Wilmott, September, 84-108.

## Examples

``` r
sigma <- 0.25
vov <- 0.3
rho <- -0.8
beta <- 0.3
texp <- 10
strike <- seq(0.1, 2, 0.1)
FER::SabrHagan2002(strike, 1, texp, sigma, vov, rho, beta)
#>  [1] 0.7176366 0.5724892 0.4885728 0.4293146 0.3835131 0.3462106 0.3147744
#>  [8] 0.2876458 0.2638355 0.2426901 0.2237727 0.2067992 0.1916014 0.1781036
#> [15] 0.1662978 0.1562096 0.1478499 0.1411656 0.1360166 0.1321909
FER::SabrHagan2002(strike, 1, texp, sigma, vov, rho, beta, cp=1)
#>  [1] 0.93897315 0.86489947 0.78883147 0.71270818 0.63753690 0.56403165
#>  [7] 0.49280533 0.42444936 0.35957256 0.29881901 0.24286857 0.19241569
#> [13] 0.14811748 0.11050117 0.07983235 0.05597599 0.03832315 0.02586199
#> [19] 0.01739174 0.01177062
```
