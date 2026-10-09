# Contributing to reservekit

Thank you for helping. Because results from this package may inform
financial reporting, the bar for numerical changes is high.

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

## Workflow

```r
devtools::document()
devtools::test()
devtools::check()
lintr::lint_package()
```

All checks must pass without suppressing warnings. Update `NEWS.md`.
Benchmarks live in `bench/` and are not run on CRAN.

## Review

Pull requests that change calculations need review by someone other than the
author, ideally a reserving actuary.
