# reservekit: architecture

Status: v0.1.0 development. This document records the design decisions behind the
package and is updated as they change.

## 1. Name

The suggested working name `reserveR` was checked first. CRAN already hosts
`reservr` (a loss-reserving/distribution package by A. Rosenstock), and
`reserveR` differs from it by one letter and case only, which invites confusion
and may conflict with CRAN's case-insensitive name uniqueness rule. The
provisional name is therefore **`reservekit`**. It has not been reserved on CRAN;
re-check availability before first submission (the CRAN package index was not
reachable from the build environment).

## 2. Target users and workflows

* Reserving analysts: build paid/incurred triangles from claim-level or
  aggregated data, run chain ladder and expected-loss methods, compare
  methods, and report ultimates and reserves by origin period.
* Reserving actuaries / reviewers: inspect every intermediate quantity (link
  ratios, selected factors, CDFs, sigma estimates), quantify uncertainty
  (Mack), and run diagnostics (Mack 1994 tests, residuals).
* Developers: add methods that plug into the same result object and generics.

Expected inputs: long-format `data.frame`s (one row per origin x development
age, or per claim transaction), or a numeric matrix already in triangle form.

## 3. Survey of existing software

| Software | Strengths | Gaps / issues relevant to us |
|---|---|---|
| R `ChainLadder` (Gesmann et al.) | Broad: Mack, Munich CL, bootstrap (ODP), GLM, multivariate CL, CDR. De-facto R reference. | Heavy dependency tree (systemfit, cplm, tweedie, actuar, ...). Inconsistent result objects across methods. Triangle class does not distinguish structural from genuine NAs. Could not be installed in the build environment (CRAN/GitHub blocked), so it is **not** used as a test oracle here. |
| Python `chainladder` 0.10.1 | Consistent sklearn-style API; Mack, BF, Cape Cod, bootstrap; diagnostics (`DevelopmentCorrelation`, `ValuationCorrelation`). | Installed and used as an independent cross-check. Its default Mack total standard error and the per-origin s.e. of the most mature origins differ from Mack (1993) published figures (see `inst/validation/VALIDATION.md`). |
| `reservr` (CRAN) | Distributional/individual-claims reserving with TensorFlow. | Different scope (individual claims, neural nets). |
| `lossDev` (CRAN) | Bayesian development models (JAGS). | Different scope; needs JAGS. |
| Commercial tools (ResQ, Arius, etc.) | Mature workflows and reporting. | Closed source; not consulted. |

Gap we target: a small, dependency-free core with one triangle class that is
explicit about cumulative vs incremental and structural vs genuine missing
data, one result object shared by every method, and a published validation
record for every number the package computes.

## 4. Module layout

```
R/
  triangle.R        triangle class: construction, validation of structure, coercion
  convert.R         cumulative <-> incremental, diagonals, latest
  validate.R        validate_triangle(): data-quality report (warnings vs errors)
  dev-factors.R     link ratios, factor selection (volume/simple, n_periods,
                    weights, exclusions), calendar decay weights
  tail.R            tail factor: fixed or exponential (log-linear) decay
  pattern.R         development pattern object (CDF, % developed) shared by
                    CL, BF, Cape Cod and ELR
  fit-chain-ladder.R
  fit-expected.R    Bornhuetter-Ferguson, Cape Cod, expected loss ratio
  fit-mack.R        Mack (1993) standard errors
  diagnostics.R     Mack (1994) correlation and calendar-year tests, residuals
  methods.R         print/summary/predict/plot/as.data.frame/coef for fits
  compare.R         compare_methods()
  plot.R            base-graphics plots
  utils.R           internal checks and error helpers
```

## 5. Core data structure: `triangle`

An S3 object: a numeric matrix (rows = origin periods, columns = development
ages) with attributes:

* `type`: `"cumulative"` or `"incremental"`. Required at construction; never
  inferred.
* `measure`: `"paid"`, `"incurred"`, `"reported_count"`, `"closed_count"`,
  or `"other"`. Affects labels only (e.g. reserve vs IBNR).
* `origin`, `dev`: origin labels and numeric development ages.
* `observed`: logical matrix. A cell is in the observed region if its
  calendar index `i + j - 1` is at or before the latest valuation
  diagonal. NA inside the observed region is a **genuine missing value**; NA
  outside it is a **structural future value**. The two are never confused.
* `filled`: logical matrix of cells created as 0 when building an incremental
  triangle from transaction data with no rows for that cell (documented rule).

Assumption (v0.1): origin and development periods have the same length (e.g.
annual x annual). Mixed granularity is on the roadmap.

S3 was chosen over S4/R6: the object is a value (copy semantics are right),
matrices give fast vectorised arithmetic, and S3 generics are what most R users
know.

## 6. Result object: `reserve_fit`

Every method returns a list of class `c("reserve_<method>", "reserve_fit")`:

| Element | Content |
|---|---|
| `method` | short id, e.g. `"chain_ladder"` |
| `call` | matched call |
| `triangle` | the cumulative input triangle |
| `pattern` | `dev_pattern` (age-to-age factors, CDFs, % developed, tail) |
| `results` | data.frame, one row per origin: `origin`, `latest`, `dev_age`, `cdf`, `pct_developed`, `ultimate`, `reserve` (+ method-specific columns such as `prior_ultimate`, `se`, `cv`) |
| `assumptions` | named list of every methodological choice |
| `warnings` | character vector of material assumption warnings raised during fitting |

Shared generics: `print`, `summary`, `predict` (completed triangle, cumulative
or incremental), `as.data.frame`, `coef` (selected factors), `plot`,
`diagnostics`. Method-specific numbers are additional columns, never a
different structure.

`reserve` is ultimate minus the latest diagonal. For paid data it is total
unpaid (case + IBNR); for incurred data it is IBNR (incl. IBNER). The
`measure` attribute drives labels so this is visible in output.

## 7. Numerical conventions

* Double precision throughout; no rounding inside calculations. Rounding is
  for printing only.
* Factor columns with a zero denominator, or no data, are an error unless the
  user supplies a factor or excludes the column; never silently set to 1.
* Individual link ratios with zero denominator are `NA` and reported; simple
  averages exclude them with a warning naming the cells.
* Negative cumulative values and negative increments are allowed (recoveries,
  case reserve releases) and flagged by `validate_triangle()`.
* Genuine missing values in the observed region stop model fitting with an
  error pointing to `validate_triangle()`. (Exclusion rules are on the roadmap.)
* Random numbers (future bootstrap) will take an explicit `seed` and restore
  the caller's RNG state.

## 8. Dependency policy

Imports: `stats`, `graphics`, `grDevices`, `utils` only (base R).
Suggests: `testthat`, `knitr`, `rmarkdown`, `ggplot2` (future optional plots),
`bench` (benchmarks, not run on CRAN). Heavy or compiled dependencies require
a profiling result that justifies them.

## 9. Testing and validation strategy

1. Unit tests for every exported function: valid, invalid, boundary inputs.
2. Hand-calculated fixtures: a 4x4 triangle whose every factor, ultimate and
   Mack s.e. was computed independently (spreadsheet-style arithmetic recorded
   in the test file) and by a separate naive loop implementation.
3. Published results: Mack (1993) RAA reserves and standard errors.
4. Cross-implementation: Python `chainladder` 0.10.1 outputs stored as fixtures
   in `inst/validation/`, discrepancies investigated and documented.
5. Identity tests: incremental <-> cumulative round trips; sum of projected
   increments equals ultimate minus latest; BF with prior ultimates equal to the
   chain-ladder ultimates reproduces chain ladder; Cape Cod ELR identity.

## 10. Roadmap

See `ROADMAP.md`.
