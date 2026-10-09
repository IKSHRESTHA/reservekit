# reservekit

Transparent, validated general insurance loss reserving in R.

> **Status: 0.1.0 development version.** Not yet on CRAN, not yet
> independently reviewed by a second actuary. See
> [`inst/validation/VALIDATION.md`](inst/validation/VALIDATION.md) for what has
> been numerically validated and what has not.

reservekit gives you one triangle class that is explicit about cumulative vs
incremental data and structural vs genuine missing values, a data-quality
report, the standard deterministic methods, Mack standard errors and
diagnostics, all returning the same result object. It has no dependencies
beyond base R.

## Installation

```r
# From a source checkout:
install.packages("path/to/reservekit", repos = NULL, type = "source")
# or, once published on GitHub:
# remotes::install_github("ikshrestha/reservekit")
```

## Example

`claims_synthetic` is simulated claim-level data (not real experience).

```r
library(reservekit)

paid <- as_triangle(claims_synthetic,
                    origin = "accident_year", development = "dev_age",
                    value = "paid", type = "incremental", measure = "paid",
                    dev_step = 1)

validate_triangle(paid)     # data-quality findings by severity
fit <- fit_mack(paid)
fit
#> <reserve_fit> Mack chain ladder | paid | 10 origin periods
#>  origin dev_age    latest    cdf  ultimate   reserve      se    cv
#>    2015      10 4,775,711 1.0000 4,775,711         0       0
#>    2016       9 5,965,003 1.0034 5,985,523    20,520   3,612 17.6%
#>    ...
#>    2024       1 2,085,607 4.1110 8,574,004 6,488,397 471,607  7.3%
#>
#> Total Unpaid (ultimate - paid): 16,418,740  (prediction s.e. 644,527, CV 3.9%)
#> ! sigma extrapolated (mack) for step(s) 9-10.

summary(fit)                 # totals, process/parameter split, assumptions
predict(fit, type = "incremental")   # completed triangle
diagnostics(fit)             # Mack correlation and calendar-year tests, residuals
plot(fit)

prem <- premium_synthetic$earned_premium
compare_methods(
  chain_ladder = fit_chain_ladder(paid, tail = "exponential"),
  bf           = fit_bf(paid, exposure = prem, elr = 0.65),
  cape_cod     = fit_cape_cod(paid, exposure = prem)
)
```

## What is included (0.1.0)

| Area | Functions |
|---|---|
| Triangles | `as_triangle()`, `to_cumulative()`, `to_incremental()`, `latest_diagonal()`, `link_ratios()` |
| Data quality | `validate_triangle()` |
| Development | `dev_factors()` (volume / simple, latest-n diagonals, weights, overrides), `calendar_weights()`, `dev_pattern()`, fixed and exponential tails |
| Methods | `fit_chain_ladder()`, `fit_bf()`, `fit_cape_cod()` (incl. generalised decay), `fit_elr()`, `fit_mack()` |
| Diagnostics | `diagnostics()`, `dev_correlation_test()`, `calendar_test()` |
| Results | `print()`, `summary()`, `predict()`, `coef()`, `as.data.frame()`, `plot()`, `compare_methods()` |

Not yet included: bootstrap/ODP, GLM reserving, Munich chain ladder, Mack
with tail, prediction intervals, multi-line aggregation. See
[ROADMAP.md](ROADMAP.md).

## Validation highlights

* Reproduces Mack's published RAA reserves and standard errors (total 52,135,
  s.e. 26,909) and the Taylor-Ashe total s.e. 2,447,095.
* Matches an exact rational-arithmetic reference calculation on a small
  triangle to 1e-10.
* Matches Python `chainladder` 0.10.1 for factors, BF, Cape Cod, tails,
  Mack and Mack's diagnostic tests; the one default difference (sigma
  extrapolation) is explained in the validation record.

## Important

reservekit supports professional actuarial analysis. It does not replace
actuarial judgement, independent validation, or applicable professional and
regulatory standards, and it makes no claim of compliance with any accounting
or solvency framework.

## Design

See [ARCHITECTURE.md](ARCHITECTURE.md). Contributions: see
[CONTRIBUTING.md](CONTRIBUTING.md).
