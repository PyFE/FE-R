# Spread option under the Bachelier model

The payout of the spread option is `max(S1_T - S2_T - K, 0)` where
`S1_T` and `S2_T` are the prices at expiry `T` of assets 1 and 2
respectively and `K` is the strike price.

## Usage

``` r
SpreadBachelier(
  strike = 0,
  spot1,
  spot2,
  texp = 1,
  sigma1,
  sigma2,
  corr,
  intr = 0,
  divr1 = 0,
  divr2 = 0,
  cp = 1L,
  forward1 = spot1 * exp(-divr1 * texp)/df,
  forward2 = spot2 * exp(-divr2 * texp)/df,
  df = exp(-intr * texp)
)
```

## Arguments

- strike:

  (vector of) strike price

- spot1:

  (vector of) spot price of asset 1

- spot2:

  (vector of) spot price of asset 2

- texp:

  (vector of) time to expiry

- sigma1:

  (vector of) Bachelier volatility of asset 1

- sigma2:

  (vector of) Bachelier volatility of asset 2

- corr:

  correlation

- intr:

  interest rate

- divr1:

  dividend rate of asset 1

- divr2:

  dividend rate of asset 2

- cp:

  call/put sign. `1` for call, `-1` for put.

- forward1:

  forward price of asset 1. If given, overrides `spot1`

- forward2:

  forward price of asset 2. If given, overrides `spot2`

- df:

  discount factor. If given, `df` overrides `intr`

## Value

option price

## Examples

``` r

FER::SpreadBachelier((-2:2)*10, 100, 120, 1.3, 20, 36, -0.5)
#> [1] 22.357859 17.712844 13.766644 10.486838  7.822698
```
