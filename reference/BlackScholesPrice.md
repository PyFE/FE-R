# Calculate Black-Scholes option price

Calculate Black-Scholes option price

## Usage

``` r
BlackScholesPrice(
  strike = forward,
  spot,
  texp = 1,
  sigma,
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

Black, F., & Scholes, M. (1973). The Pricing of Options and Corporate
Liabilities. Journal of Political Economy, 81(3), 637-654.
[doi:10.1086/260062](https://doi.org/10.1086/260062)

Black, F. (1976). The pricing of commodity contracts. Journal of
Financial Economics, 3(1), 167-179.
[doi:10.1016/0304-405X(76)90024-6](https://doi.org/10.1016/0304-405X%2876%2990024-6)

<https://en.wikipedia.org/wiki/Black-Scholes_model>

## See also

[`BlackScholesImpvol`](https://pyfe.github.io/FE-R/reference/BlackScholesImpvol.md)

## Examples

``` r
spot <- 100
strike <- seq(80,125,5)
texp <- 1.2
sigma <- 0.2
intr <- 0.05
FER::BlackScholesPrice(strike, spot, texp, sigma, intr=intr)
#>  [1] 25.535142 21.529004 17.856068 14.568093 11.694956  9.242988  7.197247
#>  [8]  5.526345  4.188286  3.136053
```
