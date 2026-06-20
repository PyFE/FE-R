# Calculate the mass at zero under the CEV model

Calculate the mass at zero under the CEV model

## Usage

``` r
CevMassZero(
  spot,
  texp = 1,
  sigma,
  beta = 0.5,
  intr = 0,
  divr = 0,
  forward = spot * exp(-divr * texp)/df,
  df = exp(-intr * texp)
)
```

## Arguments

- spot:

  (vector of) spot price

- texp:

  (vector of) time to expiry

- sigma:

  (vector of) volatility

- beta:

  beta

- intr:

  interest rate

- divr:

  dividend rate

- forward:

  forward price. If given, `forward` overrides `spot`

- df:

  discount factor. If given, `df` overrides `intr`

## Value

mass at zero

## Examples

``` r
spot <- 100
texp <- 1.2
beta <- 0.5
sigma <- 2
FER::CevMassZero(spot, texp, sigma, beta)
#> [1] 8.024105e-19
```
