# The Black-Scholes implied volatility implementation in FE-R via the inverse Gaussian survival function

## Overview

This note records the mathematical background and shows how
[`BlackScholesImpvol()`](https://pyfe.github.io/FE-R/reference/BlackScholesImpvol.md)
in the `FER` package exploits the quantile function of the inverse
Gaussian distribution to invert the Black–Scholes price for implied
volatility.

The Black–Scholes price has long been recognized as a probability
associated with the *last passage (exit) time* of Brownian motion, most
notably in the work of Madan and co-authors ([Madan, Roynette, and M.
Yor 2008](#ref-madan2008unifying); [Madan, Roynette, and Marc Yor
2008](#ref-madan2008option)); see in particular Theorem 0 of Madan,
Roynette, and M. Yor ([2008](#ref-madan2008unifying)). In their
formulation the random variables $`T`$ and $`G`$ are exactly the
**inverse Gaussian (IG)** and **reciprocal inverse Gaussian (RIG)**
distributions, respectively, although they were not named as such there.

This package recognizes and labels the probability as IG/RIG and uses
the quantile function of the IG distribution from the `statmod` package
in R.

## The inverse Gaussian distribution

[The inverse Gaussian
distribution](https://en.wikipedia.org/wiki/Inverse_Gaussian_distribution),
$`\mathrm{IG}(\mu,\lambda)`$, is supported on $`x > 0`$ and parametrized
by its mean $`\mu > 0`$ and a shape parameter $`\lambda > 0`$. Its
probability density function is
``` math
f_{\mathrm{IG}}(x;\mu,\lambda)
= \sqrt{\frac{\lambda}{2\pi x^{3}}}\,
\exp\!\left(-\frac{\lambda (x-\mu)^{2}}{2\mu^{2} x}\right),
\qquad x > 0,
```
with mean $`\mathbb{E}[X] = \mu`$ and variance
$`\operatorname{Var}(X) = \mu^{3}/\lambda`$. Its cumulative distribution
function admits the closed form

``` math
F_{\mathrm{IG}}(x;\mu,\lambda)
= N\!\left(\sqrt{\tfrac{\lambda}{x}}\Bigl(\tfrac{x}{\mu}-1\Bigr)\right)
+ e^{2\lambda/\mu}\,
N\!\left(-\sqrt{\tfrac{\lambda}{x}}\Bigl(\tfrac{x}{\mu}+1\Bigr)\right),
```
where $`N(\cdot)`$ is the standard normal CDF. The survival function
(complementary CDF) used below is therefore

``` math
\bar F_{\mathrm{IG}}(x;\mu,\lambda)
= 1 - F_{\mathrm{IG}}(x;\mu,\lambda)
= N\!\left(-\sqrt{\tfrac{\lambda}{x}}\Bigl(\tfrac{x}{\mu}-1\Bigr)\right)
- e^{2\lambda/\mu}\,
N\!\left(-\sqrt{\tfrac{\lambda}{x}}\Bigl(\tfrac{x}{\mu}+1\Bigr)\right).
```
Here the shape parameter $`\lambda`$ follows the convention of the
Wikipedia article. The `statmod` package ([Giner and Smyth
2016](#ref-giner2016statmod)) instead uses the dispersion
$`\phi = 1/\lambda`$, which defaults to $`1`$,
i.e. $`\phi = \lambda = 1`$.

## The option time value as an inverse Gaussian survival function

Let $`C`$ be the undiscounted option value, where $`F`$ is the forward
price, $`K`$ the strike, $`T`$ the time to expiry, and $`\sigma`$ the
volatility. The **normalized time value** is
``` math
c = \frac{C - (\theta(F-K))^+}{\min(F,K)} = N(d_1) - e^k\,N(d_2), \qquad
d_{1,2} = -\frac{k}{s} \pm \frac{s}{2},
```
where $`N(\cdot)`$ is the standard normal CDF, $`\theta=\pm 1`$ for a
call or put, respectively, and $`s`$ and $`k`$ are the total standard
deviation and the absolute log-moneyness,
``` math
s = \sigma\sqrt{T}, \qquad k = |\log(F/K)|\ge 0.
```

The key identity is that this normalized time value is the **survival
function (complementary CDF) of an inverse Gaussian distribution**,
$`\mathrm{IG}(2/k,\,1)`$:
``` math
c = \mathbb{P}\left(X > \frac{4}{s^2}\right) = \bar F_{\mathrm{IG}}\left(x = \frac{4}{s^2};\,\mu,\lambda\right),
\qquad
X \sim \mathrm{IG}\!\left(\mu = \frac{2}{k},\; \lambda = 1\right).
```
Using the closed form of the IG distribution with $`\lambda = 1`$,
$`\mu = 2/k`$, and $`x = 4/s^2`$, the survival function reduces to

``` math
\bar F_{\mathrm{IG}}(x)
= N\!\left(- \frac{k}{s} + \frac{s}{2}\right)
- e^{k}\,N\!\left(-\frac{k}{s} - \frac{s}{2}\right),
```
which is precisely the normalized Black–Scholes time value $`c`$. The
reciprocal $`1/X`$ follows the RIG law and corresponds to the variable
$`G`$ in Madan, Roynette, and M. Yor ([2008](#ref-madan2008unifying));
the implied-volatility inversion below needs only the IG side.

## How `BlackScholesImpvol()` uses it

The representation makes the inversion direct. Given an observed price,
form the normalized time value $`c`$, invert the IG survival function,
and read off the volatility:
``` math
x = \bar F_{\mathrm{IG}}^{-1}(c), \qquad
\sigma = \frac{2}{\sqrt{x\,T}} .
```
There is no iterative root-finding: the IG quantile function does the
work. The source code (`R/blackscholes_impvol.R`) maps line for line
onto the formulas above, with `cp` playing the role of $`\theta`$:

``` r

# normalized time value  c = (C - intrinsic) / min(F, K)
timeval <- (price/df - pmax(cp*(forward-strike), 0))/pmin(forward, strike)

# IG mean  mu = 2 / k,  k = |log(F / K)|
mu <- 2/abs(log(strike/forward))

# x = inverse of the IG survival function at c   (lower.tail = FALSE)
x <- statmod::qinvgauss(timeval, mean=mu, lower.tail=F)

# sigma = 2 / sqrt(x * T)
sig <- 2/sqrt(x*texp)
```

The inverse Gaussian quantile $`\bar F_{\mathrm{IG}}^{-1}`$ is supplied
by
[`statmod::qinvgauss()`](https://rdrr.io/pkg/statmod/man/invgauss.html)
([Giner and Smyth 2016](#ref-giner2016statmod)), called with
`lower.tail = FALSE` so that it inverts the survival function.

## References

Giner, Göknur, and Gordon K. Smyth. 2016. “Statmod: Probability
Calculations for the Inverse Gaussian Distribution.” *The R Journal* 8
(1): 339–51. <https://doi.org/10.32614/RJ-2016-024>.

Madan, D., B. Roynette, and M. Yor. 2008. “Unifying Black–Scholes Type
Formulae Which Involve Brownian Last Passage Times up to a Finite
Horizon.” *Asia-Pacific Financial Markets* 15 (2): 97–115.
<https://doi.org/10.1007/s10690-008-9068-y>.

Madan, D., B. Roynette, and Marc Yor. 2008. “Option Prices as
Probabilities.” *Finance Research Letters* 5 (2): 79–87.
<https://doi.org/10.1016/j.frl.2008.02.002>.
