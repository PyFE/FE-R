# Calculate the option price under the NSVh model with lambda=1 (Choi et al. 2019)

Calculate the option price under the NSVh model with lambda=1 (Choi et
al. 2019)

## Usage

``` r
Nsvh1Choi2019(
  strike = forward,
  spot,
  texp = 1,
  sigma,
  vov = 0,
  rho = 0,
  intr = 0,
  divr = 0,
  cp = 1L,
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

- intr:

  interest rate

- divr:

  dividend rate

- cp:

  call/put sign. `1` (default) for call price, `-1` for put price,
  `NULL` for Bachelier volatility

- forward:

  forward price. If given, `forward` overrides `spot`

- df:

  discount factor. If given, `df` overrides `intr`

## Value

BS volatility or option price based on `cp`

## References

Choi, J., Liu, C., & Seo, B. K. (2019). Hyperbolic normal stochastic
volatility model. Journal of Futures Markets, 39(2), 186–204.
[doi:10.1002/fut.21967](https://doi.org/10.1002/fut.21967)

## Examples

``` r

spot <- 100
strike <- seq(80,125,5)
texp <- 1.2
sigma <- 20
vov <- 0.2
rho <- -0.5
strike <- seq(0.1, 2, 0.1)

FER::Nsvh1Choi2019(strike, spot, texp, sigma, vov, rho)
#>  [1] 99.90205 99.80207 99.70209 99.60211 99.50213 99.40215 99.30218 99.20220
#>  [9] 99.10222 99.00224 98.90226 98.80229 98.70231 98.60233 98.50236 98.40238
#> [17] 98.30240 98.20243 98.10245 98.00248
```
