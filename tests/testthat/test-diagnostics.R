test_that("correlation test reproduces Mack (1994) RAA value and Python", {
  r <- pyref()
  t <- dev_correlation_test(raa)
  expect_s3_class(t, "reserve_test")
  # Mack (1994): T = 0.070, Var(T) = 1/28; inside the 50% interval.
  expect_equal(round(t$statistic, 3), 0.070)
  expect_equal(t$variance, 1 / 28)
  expect_false(t$reject)
  expect_equal(t$statistic, r$raa$devcorr_T, tolerance = 1e-12)
  expect_equal(dev_correlation_test(taylor_ashe)$statistic, r$genins$devcorr_T, tolerance = 1e-12)
  expect_identical(nrow(t$by_step), 7L)  # step pairs with n >= 2: k = 2..I-2
  expect_equal(t$interval, c(-1, 1) * qnorm(0.75) / sqrt(28))
})

test_that("Spearman statistics agree with stats::cor", {
  t <- dev_correlation_test(raa)
  lr <- link_ratios(raa)
  ok <- !is.na(lr[, 1]) & !is.na(lr[, 2])
  expect_equal(t$by_step$T[1], cor(lr[ok, 1], lr[ok, 2], method = "spearman"))
})

test_that("calendar-year test matches Python and its moments", {
  r <- pyref()
  for (nm in c("raa", "genins")) {
    tri <- if (nm == "raa") raa else taylor_ashe
    ct <- calendar_test(tri)
    expect_equal(ct$statistic, r[[nm]]$valcorr$z)
    expect_equal(ct$expected, r[[nm]]$valcorr$z_expectation, tolerance = 1e-12)
    expect_equal(ct$variance, r[[nm]]$valcorr$z_variance, tolerance = 1e-12)
  }
  # Moments of Z_j for n = 2 by enumeration: Z = 0 w.p. 1/2, 1 w.p. 1/2.
  ct <- calendar_test(raa)
  row2 <- ct$by_diagonal[ct$by_diagonal$L + ct$by_diagonal$S == 2, ][1, ]
  expect_equal(row2$expected, 0.5)
  expect_equal(row2$variance, 0.25)
  expect_false(ct$reject)
})

test_that("calendar test moments match exact binomial enumeration", {
  for (n in 1:12) {
    z <- vapply(0:n, function(l) min(l, n - l), numeric(1))
    p <- dbinom(0:n, n, 0.5)
    e <- sum(z * p)
    v <- sum(z^2 * p) - e^2
    expect_equal(calendar_moments(n), c(e, v), tolerance = 1e-12)
  }
  expect_equal(calendar_moments(0L), c(0, 0))
})

test_that("correlation statistic with ties follows Mack's formula, not Spearman", {
  m <- rbind(c(100, 150, 165, 170), c(100, 150, 165.0001, NA), c(100, 140, NA, NA),
             c(100, NA, NA, NA))
  t <- dev_correlation_test(as_triangle(m, type = "cumulative"))
  lr <- link_ratios(as_triangle(m, type = "cumulative"))
  r <- rank(lr[1:2, 1])
  s <- rank(lr[1:2, 2])
  expect_equal(t$by_step$T[1], 1 - 6 * sum((r - s)^2) / (2^3 - 2))
})

test_that("diagnostics() collects tests, validation and Mack residuals", {
  d <- diagnostics(fit_mack(raa))
  expect_s3_class(d, "reserve_diagnostics")
  expect_s3_class(d$validation, "triangle_validation")
  expect_true(all(c("origin", "step", "calendar", "residual") %in% names(d$residuals)))
  # Volume-weighted residuals: sum of sqrt(C) * residual is zero per step.
  res <- d$residuals
  C <- as.matrix(raa)
  w <- sqrt(C[cbind(match(res$origin, rownames(C)), match(sub("-.*", "", res$step), colnames(C)))])
  sums <- tapply(w * res$residual, res$step, sum)
  expect_equal(as.vector(sums), rep(0, length(sums)), tolerance = 1e-10)
  expect_output(print(d), "correlation test")
  expect_null(diagnostics(fit_chain_ladder(raa))$residuals)
})

test_that("tests fail clearly without enough data", {
  tri <- as_triangle(matrix(c(1, 2, 1, NA), 2, byrow = TRUE), type = "cumulative")
  expect_error(dev_correlation_test(tri), "Not enough")
  expect_error(calendar_test(tri), "Not enough")
  expect_error(dev_correlation_test(raa, ci = 2), "ci")
})

test_that("validate_triangle reports clean data and flags issues by severity", {
  v <- validate_triangle(taylor_ashe)
  expect_s3_class(v, "triangle_validation")
  expect_output(print(validate_triangle(small_tri())), "no issues")
  vr <- validate_triangle(raa)
  expect_true("negative_increment" %in% vr$check)   # 1982 incurred decreases
  expect_identical(unique(as.character(vr$severity[vr$check == "negative_increment"])), "note")
  expect_true("link_ratio_outlier" %in% vr$check)  # 1982 first ratio ~40
  expect_output(print(vr), "error\\(s\\)")
  expect_error(validate_triangle(raa, outlier_mad = 0), "outlier_mad")
  few <- as_triangle(matrix(c(1, 2, 1, NA), 2, byrow = TRUE), type = "cumulative")
  expect_true("few_origins" %in% validate_triangle(few)$check)
})

test_that("calendar test handles diagonals where every ratio equals the median", {
  m <- rbind(c(100, 200, 300), c(100, 200, NA), c(100, NA, NA))
  ct <- calendar_test(as_triangle(m, type = "cumulative"))
  expect_equal(ct$statistic, 0)
  expect_equal(ct$variance, 0)
})

test_that("zero_column message describes a zero sum, not all zeros (review finding 5)", {
  m <- matrix(c(5, 10, 20, -5, 8, NA, 50, NA, NA), 3, byrow = TRUE)
  v <- validate_triangle(as_triangle(m, type = "cumulative"))
  expect_match(v$message[v$check == "zero_column"], "sum to 0")
})
