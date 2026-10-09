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
* Hardening after independent review: stray rows that would move the inferred
  valuation diagonal now stop triangle construction; numeric-like character
  origins sort numerically; unused factor levels and gaps in Date origins are
  refused; all-zero future cells trigger a warning; Mack handles zero latest
  values (s.e. 0, warning) and refuses negative ones; Cape Cod with decay no
  longer reports a single ELR; named exposures are matched by origin.
* `compare_methods()` accepts fit names that are not syntactic R names
  (e.g. `"Chain ladder"`); simple-average patterns print as "simple average"
  rather than "simple-weighted"; `summary()` names overridden steps.
* Validation is reproducible end to end: `inst/validation/chainladder_reference.py`
  regenerates the Python `chainladder` reference values and
  `data-raw/make_fixtures.R` converts the JSON references into test fixtures.
