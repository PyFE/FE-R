# Calculate Bachelier model implied volatility

Calculate Bachelier model implied volatility

## Usage

``` r
BachelierImpvol(
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

Bachelier implied volatility

## References

Choi, J., Kim, K., & Kwak, M. (2009). Numerical Approximation of the
Implied Volatility Under Arithmetic Brownian Motion. Applied
Mathematical Finance, 16(3), 261-268.
[doi:10.1080/13504860802583436](https://doi.org/10.1080/13504860802583436)

## See also

[`BachelierPrice`](https://pyfe.github.io/FE-R/reference/BachelierPrice.md)

## Examples

``` r
spot <- 100
strike <- 100
texp <- 1.2
sigma <- 20
intr <- 0.05
price <- 20
FER::BachelierImpvol(price, strike, spot, texp, intr=intr)
#> [1] 41.13295
```
