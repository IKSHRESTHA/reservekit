# Contributing to reservekit

Thank you for helping. Because results from this package may inform
financial reporting, the bar for numerical changes is high.

By taking part you agree to follow the
[Code of Conduct](CODE_OF_CONDUCT.md).

## Ground rules

1. **No claims data.** Never include real or confidential claims data in
   issues, tests, examples or fixtures. Use synthetic data or published
   triangles.
2. **Every formula is documented.** New methods need the formula in the
   roxygen documentation, its assumptions, limitations and references.
3. **Every number is validated.** Add an independent reference calculation
   (exact arithmetic, published example, or a second implementation) to
   `inst/validation/` and a test that enforces it. Record it in
   `inst/validation/VALIDATION.md`. Agreement with another package is
   evidence, not proof: explain any discrepancy.
4. **No silent data changes.** Never replace missing values with zero or drop
   observations without an explicit, documented rule and a user-visible
   record.
5. **Small public API.** Discuss new exported functions in an issue first.
6. **Dependencies.** Base R only in `Imports` unless profiling or a clear
   need justifies more; discuss first.

## Setup

You need R (>= 4.1.0) with `devtools` and `lintr` installed. These are
development tools and are not listed in `DESCRIPTION`.

The reference calculations in `inst/validation/` use Python 3:

* `hand_calc.py` uses only the standard library. Regenerate its output with
  `python3 inst/validation/hand_calc.py > inst/validation/hand_calc_output.json`.
* `chainladder_reference.py` writes the `python_chainladder_*.json` files
  using the Python `chainladder` package, version 0.10.1. Run it from
  `inst/validation/`. Use the same version, or record the new version in
  `VALIDATION.md` if you change it.

The tests read `.rds` copies of these JSON files from
`tests/testthat/fixtures/`. After regenerating any JSON, run
`Rscript data-raw/make_fixtures.R` (needs `jsonlite`).

## Workflow

```r
devtools::document()
devtools::test()
devtools::check()
lintr::lint_package()
```

`devtools::check()` must finish with 0 errors, 0 warnings and 0 notes, and
`lintr::lint_package()` must report no lints (CI fails on any lint). Do not
suppress warnings to get there. Update `NEWS.md`.
Benchmarks live in `bench/` and are not run on CRAN.

## Review

Pull requests that change calculations need review by someone other than the
author, ideally a reserving actuary. Where no second reviewer is available,
the author must say so in the pull request and the change must be backed by
an independent reference calculation in `inst/validation/`.
