# Benchmarks

Run with `Rscript bench/benchmark.R` (needs the `bench` package) and
`python3 -I bench/python_reference.py` (needs Python `chainladder`).
Not run on CRAN or in pull-request CI.

## Results, 2026-10-08

Environment: R 4.3.3, x86_64-pc-linux-gnu, Intel Xeon @ 2.10GHz, 2 cores
(shared cloud VM; expect run-to-run noise of tens of percent).
Square synthetic cumulative triangles, n origins x n ages; median of at least
5 iterations.

| n | as_triangle (long) | validate | dev_factors | chain ladder | Mack | predict |
|---|---|---|---|---|---|---|
| 10 | 0.35 ms | 1.4 ms | 0.23 ms | 0.86 ms | 1.4 ms | 1.2 ms |
| 50 | 0.69 ms | 7.9 ms | 0.74 ms | 1.5 ms | 3.9 ms | 2.1 ms |
| 100 | 1.7 ms | 16.8 ms | 1.6 ms | 2.4 ms | 8.8 ms | 3.3 ms |
| 200 | 7.6 ms | 42.5 ms | 4.6 ms | 5.8 ms | 27.0 ms | 6.9 ms |

Memory allocated for a 200 x 200 Mack fit: about 16 MB.
Full numbers: `results.csv`.

Python `chainladder` 0.10.1 on triangles of the same sizes (same machine):
chain ladder 31 ms (n = 10) to 135 ms (n = 200); Mack 76 ms to 222 ms. These
timings include pandas set-up overheads in that library and are **indicative
only**, not a like-for-like comparison. R `ChainLadder` could not be installed
in this environment, so it has not been benchmarked.

## Findings

* All operations are interactive-speed up to 200 x 200, far beyond typical
  reserving triangles. No optimisation was needed; nothing was optimised.
* `validate_triangle()` is the slowest step (per-column robust outlier
  screen). If it ever matters, vectorise the column loop.
* `fit_mack()` uses explicit loops over origins and steps (O(n K)); a
  vectorised version is possible but unjustified at current timings.
* Simulation workloads (bootstrap) do not exist yet; benchmark them when
  added, with seeds and memory measurement.
