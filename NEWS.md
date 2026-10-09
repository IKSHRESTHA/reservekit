# reservekit 0.1.0

First release candidate.

* `triangle` class with explicit cumulative/incremental type and separation
  of structural future cells from genuine missing values.
* `as_triangle()` from long (claim-level) data or matrices, with documented
  aggregation and empty-cell rules.
* `validate_triangle()` data-quality report with error/warning/note severity.
* Development factors: volume-weighted and simple average, latest-n
  diagonals, user weights, calendar decay weights, overrides; fixed and
  exponential tail factors.
* Methods: chain ladder, Bornhuetter-Ferguson, Cape Cod (with optional
  Gluck decay), expected loss ratio, Mack (1993) standard errors.
* Mack (1994) correlation and calendar-year tests; standardised residuals.
* Shared result object with `print()`, `summary()`, `predict()`, `coef()`,
  `as.data.frame()`, `plot()`; `compare_methods()`.
* Datasets: `raa`, `taylor_ashe`, `claims_synthetic`, `premium_synthetic`.
