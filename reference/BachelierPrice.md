# Calculate Bachelier model option price

Calculate Bachelier model option price

## Usage

``` r
BachelierPrice(
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

Choi, J., Kim, K., & Kwak, M. (2009). Numerical Approximation of the
Implied Volatility Under Arithmetic Brownian Motion. Applied
Mathematical Finance, 16(3), 261-268.
[doi:10.1080/13504860802583436](https://doi.org/10.1080/13504860802583436)

## See also

[`BachelierImpvol`](https://pyfe.github.io/FE-R/reference/BachelierImpvol.md)

## Examples

``` r
spot <- 100
strike <- seq(80,125,5)
texp <- 1.2
sigma <- 20
intr <- 0.05
FER::BachelierPrice(strike, spot, texp, sigma, intr=intr)
#>  [1] 25.828047 21.780194 18.000897 14.547308 11.468864  8.800759  6.558904
#>  [8]  4.737544  3.310126  2.233201
```
