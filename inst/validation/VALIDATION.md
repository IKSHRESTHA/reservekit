# Numerical validation record, reservekit 0.1.0

Every figure below is enforced by an automated test in `tests/testthat/`.
"Agreement" with another implementation was treated as evidence, not proof;
each discrepancy found was traced to its cause.

## Sources of expected values

| Code | Source | Independence |
|---|---|---|
| H | `hand_calc.py`: exact rational arithmetic (Python `fractions`), direct transcription of Mack (1993), explicit double loop over origin pairs for the total s.e. Output in `hand_calc_output.json`. | Separate language, separate code, no floating point until the final square root. |
| P | Published figures: Mack (1994, CAS Forum) RAA example; Mack (1993) Taylor-Ashe total s.e.; Mack (1994) correlation test value for RAA. | Literature. RAA figures confirmed against the paper text; the Taylor-Ashe figure (2,447,095) is widely reproduced and matched by H-style formula, but was not re-read from the original paper in this session. |
| Y | Python `chainladder` 0.10.1 (`python_chainladder_reference.json`). | Independent open-source implementation. |
| I | Mathematical identities checked in tests (round trips, sums, limiting cases). | Internal consistency. |

## Results

| Quantity | Data | Source | Tolerance | Result |
|---|---|---|---|---|
| Volume-weighted factors | small 4x4 | H (exact fractions 167/110, 76/63, 19/18) | 1e-14 | pass |
| Chain-ladder ultimates, reserves | small 4x4 | H | 1e-12 | pass |
| Mack sigma^2 (incl. extrapolated step) | small 4x4 | H | 1e-12 | pass |
| Mack s.e. by origin and total | small 4x4 | H | 1e-10 | pass |
| Reserves by year, total 52,135 | RAA | P | rounding | pass |
| Mack s.e. 206, 623, 747, 1469, 2002, 2209, 5358, 6333, 24566; total 26,909 (52%) | RAA | P | rounding | pass |
| sigma^2_9 = 1.34 (Mack extrapolation) | RAA | P | 2 d.p. | pass |
| Total reserve 18,680,856, total s.e. 2,447,095 | Taylor-Ashe | P | rounding | pass |
| Volume, simple, latest-5 factors | RAA, Taylor-Ashe | Y | 1e-12 | pass |
| Mack s.e. by origin and total, both sigma rules | RAA, Taylor-Ashe | Y | 1e-9 | pass |
| BF ultimates | RAA, Taylor-Ashe | Y | 1e-12 | pass |
| Cape Cod ELR and ultimates, decay 1 and 0.75 | RAA, Taylor-Ashe | Y | 1e-12 | pass |
| Exponential tail factor | RAA, Taylor-Ashe | Y | 1e-7 | pass |
| Correlation test T = 0.070, Var = 1/28 | RAA | P, Y | 1e-12 vs Y | pass |
| Calendar test Z, E[Z], Var[Z] | RAA, Taylor-Ashe | Y; Var/E also vs exact binomial enumeration | 1e-12 | pass |
| Spearman T_j equals `stats::cor(method = "spearman")` | RAA | I | 1.5e-8 (testthat default) | pass |
| Cumulative <-> incremental round trip | RAA | I | 1.5e-8 | pass |
| Sum of projected increments = reserve | CL (with/without tail), BF, ELR | I | 1.5e-8 | pass |
| BF with prior = CL ultimate reproduces CL | RAA | I | 1.5e-8 (testthat default) | pass |
| Cape Cod: expected losses on used-up exposure = losses to date | small | I | 1.5e-8 | pass |
| Mack process^2 + parameter^2 = mse; pairwise total formula | RAA | I | 1.5e-8 (testthat default) | pass |
| Linear scaling of reserves and s.e. | RAA x 1000 | I | 1.5e-8 | pass |
| Mack residuals: sum of sqrt(C) x residual = 0 per step | RAA | I | 1e-10 | pass |
| Triangle totals equal raw transaction totals | synthetic | I | 1.5e-8 | pass |

## Discrepancy investigated: Python chainladder Mack standard errors

With default settings, Python `chainladder` 0.10.1 reports a total Mack s.e.
of 26,881 for RAA and 2,441,364 for Taylor-Ashe, versus Mack's published
26,909 and 2,447,095. Per-origin s.e. for the most mature origins also differ
(RAA 1982: 143 vs 206).

Cause: `chainladder` defaults to `sigma_interpolation = "log-linear"` for the
last development step, while Mack (1993) uses
`sigma^2_{n-1} = min(sigma^4_{n-2} / sigma^2_{n-3}, sigma^2_{n-3}, sigma^2_{n-2})`.
reservekit defaults to Mack's rule and offers `sigma_extrapolation =
"loglinear"`. With matching settings the two implementations agree to 1e-9
relative in both directions, which confirms the formulas and isolates the
difference to this one documented modelling choice.

## Not yet validated

* Against R `ChainLadder` directly: it could not be installed in the build
  environment (no CRAN/GitHub access). Its documented RAA/GenIns outputs
  coincide with the published Mack figures above.
* Mack with non-default weights or `n_periods`: tested for internal
  consistency only.
* Behaviour on very large or pathological real triangles beyond the cases in
  the test suite.
* No independent actuarial review has been performed yet.
