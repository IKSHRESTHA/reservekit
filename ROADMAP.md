# Roadmap

Status key: [x] done and validated, [~] partial, [ ] not started.

## v0.1.0 (this release candidate): reliable core

* [x] `triangle` class with explicit cumulative/incremental type and
      structural vs genuine missing values
* [x] `as_triangle()` from long data (claim-level aggregation rules documented)
      and from matrices
* [x] cumulative <-> incremental conversion, latest diagonal
* [x] `validate_triangle()` data-quality report
* [x] Link ratios; volume-weighted and simple-average factors; latest-n
      diagonals; user weights and exclusions; calendar decay weights
* [x] Tail factor: fixed value or exponential decay (log-linear) fit
* [x] Chain ladder, Bornhuetter-Ferguson, Cape Cod, expected loss ratio
* [x] Mack (1993) standard errors, process/parameter split, total s.e.
* [x] Mack (1994) correlation and calendar-year tests; standardised residuals
* [x] `compare_methods()`
* [x] print/summary/predict/plot/as.data.frame/coef
* [~] CI workflows written but not yet run on GitHub
* [ ] Independent actuarial review (must be someone other than the author)

## v0.2.0: uncertainty

* [ ] Mack with tail factor (Mack 1999), including tail s.e.
* [ ] Simple-average (alpha = 0) and alpha = 2 Mack variants
* [ ] Prediction intervals from lognormal/normal approximations, clearly
      labelled as approximations
* [ ] Claims development result (one-year) uncertainty (Merz-Wuthrich)
* [ ] Exclusion rules for genuine missing values
* [ ] Inverse-power and other tail curves; tail sensitivity tables

## v0.3.0: simulation

* [ ] ODP bootstrap (England-Verrall) with explicit seed, process
      distribution choice, residual adjustments, negative-increment handling
* [ ] Predictive distributions, quantiles, aggregation across origins
* [ ] Optional parallel execution with reproducible L'Ecuyer streams
* [ ] ODP GLM reserving (equivalence to chain ladder as a test)

## v0.4.0+: breadth

* [ ] Munich chain ladder (paid/incurred)
* [ ] Multiple portfolios / lines; correlation and aggregation
* [ ] Frequency-severity methods; claim count triangles
* [ ] Mixed origin/development granularity (e.g. annual x quarterly)
* [ ] ggplot2 `autoplot()` methods; Quarto report template
* [ ] `generics::tidy()` / `glance()` methods
