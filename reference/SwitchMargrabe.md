# Margrabe's formula for exhange option price

The payout of the exchange option is `max(S1_T - S2_T, 0)` where `S1_T`
and `S2_T` are the prices at expiry `T` of assets 1 and 2 respectively.

## Usage

``` r
SwitchMargrabe(
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

- spot1:

  (vector of) spot price of asset 1

- spot2:

  (vector of) spot price of asset 2

- texp:

  (vector of) time to expiry

- sigma1:

  (vector of) volatility of asset 1

- sigma2:

  (vector of) volatility of asset 2

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

## References

Margrabe, W. (1978). The value of an option to exchange one asset for
another. The Journal of Finance, 33(1), 177–186.

## See also

[`SpreadKirk`](https://pyfe.github.io/FE-R/reference/SpreadKirk.md)

## Examples

``` r

FER::SwitchMargrabe(100, 120, 1.3, 0.2, 0.3, -0.5)
#> [1] 12.98974
```
