# Performance benchmarks. Not run on CRAN or in quick CI.
#   Rscript bench/benchmark.R
# Writes bench/results.csv and prints the environment.
suppressPackageStartupMessages({
  library(reservekit)
  library(bench)
})

make_tri <- function(n, seed = 1) {
  set.seed(seed)
  pattern <- diff(c(0, 1 - exp(-0.35 * seq_len(n))))
  m <- matrix(NA_real_, n, n)
  for (i in seq_len(n)) {
    k <- n - i + 1
    m[i, seq_len(k)] <- cumsum(1e6 * pattern[seq_len(k)] * exp(rnorm(k, 0, 0.1)))
  }
  as_triangle(m, type = "cumulative")
}

sizes <- c(10, 25, 50, 100, 200)
rows <- list()
for (n in sizes) {
  tri <- make_tri(n)
  long <- as.data.frame(to_incremental(tri))
  long$origin <- as.integer(long$origin)
  b <- bench::mark(
    as_triangle = as_triangle(long, "origin", "dev_age", "value", type = "incremental"),
    validate = validate_triangle(tri),
    dev_factors = dev_factors(tri),
    chain_ladder = fit_chain_ladder(tri),
    mack = fit_mack(tri),
    predict = predict(fit_chain_ladder(tri)),
    check = FALSE, min_iterations = 5, max_iterations = 200, time_unit = "ms"
  )
  rows[[length(rows) + 1L]] <- data.frame(
    n = n, step = as.character(b$expression), median_ms = round(as.numeric(b$median), 3),
    mem_alloc_kb = round(as.numeric(b$mem_alloc) / 1024, 1)
  )
}
res <- do.call(rbind, rows)
print(res, row.names = FALSE)
utils::write.csv(res, "bench/results.csv", row.names = FALSE)

cat("\nEnvironment\n")
cat(R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("CPU:", tryCatch(trimws(sub(".*: ", "", grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1])),
                     error = function(e) "unknown"), "\n")
cat("Cores:", parallel::detectCores(), "\n")
