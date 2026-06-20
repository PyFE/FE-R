# Calculate Black-Scholes implied volatility

Calculate Black-Scholes implied volatility

## Usage

``` r
BlackScholesImpvol(
  price,
  strike = forward,
  spot,
  texp = 1,
  intr = 0,
  divr = 0,
  cp = 1L,
  forward = spot * exp(-divr * texp)/df,
  df = exp(-intr * texp)
)
```

## Arguments

- price:

  (vector of) option price

- strike:

  (vector of) strike price

- spot:

  (vector of) spot price

- texp:

  (vector of) time to expiry

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

Black-Scholes implied volatility

## References

Giner, G., & Smyth, G. K. (2016). statmod: Probability Calculations for
the Inverse Gaussian Distribution. The R Journal, 8(1), 339-351.
[doi:10.32614/RJ-2016-024](https://doi.org/10.32614/RJ-2016-024)

## See also

[`BlackScholesPrice`](https://pyfe.github.io/FE-R/reference/BlackScholesPrice.md)

## Examples

``` r
spot <- 100
strike <- 100
texp <- 1.2
sigma <- 0.2
intr <- 0.05
price <- 20
FER::BlackScholesImpvol(price, strike, spot, texp, intr=intr)
#> [1] 0.4023459
```
