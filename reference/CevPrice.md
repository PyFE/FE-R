# Calculate the constant elasticity of variance (CEV) model option price

Calculate the constant elasticity of variance (CEV) model option price

## Usage

``` r
CevPrice(
  strike = forward,
  spot,
  texp = 1,
  sigma,
  beta = 0.5,
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

- beta:

  elasticity parameter

- intr:

  interest rate (domestic interest rate)

- divr:

  dividend/convenience yield (foreign interest rate)

- cp:

  call/put sign. `1` for call, `-1` for put.

- forward:

  forward price. If given, `forward` overrides `spot`

- df:

  discount factor. If given, `df` overrides `intr`

## Value

option price

## References

Schroder, M. (1989). Computing the constant elasticity of variance
option pricing formula. Journal of Finance, 44(1), 211-219.
[doi:10.1111/j.1540-6261.1989.tb02414.x](https://doi.org/10.1111/j.1540-6261.1989.tb02414.x)

## Examples

``` r
spot <- 100
strike <- seq(80,125,5)
texp <- 1.2
beta <- 0.5
sigma <- 2
FER::CevPrice(strike, spot, texp, sigma, beta)
#>  [1] 21.842983 17.934619 14.421747 11.346284  8.727247  6.559546  4.816426
#>  [8]  3.454585  2.420594  1.657276
```
