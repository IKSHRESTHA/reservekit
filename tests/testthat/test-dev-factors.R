test_that("volume-weighted factors match exact hand calculation", {
  h <- hand()
  p <- dev_factors(small_tri())
  expect_equal(unname(p$factors), h$factors, tolerance = 1e-14)
  expect_equal(unname(p$factors), c(167 / 110, 76 / 63, 19 / 18), tolerance = 1e-14)
  expect_named(p$factors, c("12-24", "24-36", "36-48"))
  expect_equal(unname(p$n_obs), c(3L, 2L, 1L))
  # CDFs are products of remaining factors; pct developed is 1 / CDF.
  expect_equal(unname(p$cdf), c(prod(h$factors), prod(h$factors[2:3]), h$factors[3], 1))
  expect_equal(p$pct_developed, 1 / p$cdf)
})

test_that("link ratios are C[j+1] / C[j] and undefined for zero denominators", {
  lr <- link_ratios(small_tri())
  expect_equal(lr[1, 1], 1.5)
  expect_equal(lr[3, 1], 186 / 120)
  expect_true(is.na(lr[4, 1]))
  m <- small_matrix()
  m[1, 1] <- 0
  lr0 <- link_ratios(as_triangle(m, type = "cumulative"))
  expect_true(is.na(lr0[1, 1]))
})

test_that("simple average excludes zero-denominator ratios with a warning", {
  m <- small_matrix()
  m[1, 1] <- 0
  tri <- as_triangle(m, type = "cumulative")
  expect_warning(p <- dev_factors(tri, method = "simple"), "zero denominator")
  expect_equal(p$factors[[1]], mean(c(165 / 110, 186 / 120)))
  # Volume weighting is still defined.
  expect_equal(dev_factors(tri)$factors[[1]], (150 + 165 + 186) / (0 + 110 + 120))
})

test_that("factors match the Python chainladder implementation", {
  r <- pyref()
  for (nm in c("raa", "genins")) {
    tri <- if (nm == "raa") raa else taylor_ashe
    expect_equal(unname(dev_factors(tri)$factors), r[[nm]]$ldf_volume, tolerance = 1e-12)
    expect_equal(unname(dev_factors(tri, method = "simple")$factors), r[[nm]]$ldf_simple,
                 tolerance = 1e-12)
    expect_equal(unname(dev_factors(tri, n_periods = 5)$factors), r[[nm]]$ldf_volume_n5,
                 tolerance = 1e-12)
  }
})

test_that("n_periods uses only the latest calendar diagonals", {
  tri <- small_tri()
  p <- dev_factors(tri, n_periods = 1)
  expect_equal(unname(p$factors), c(186 / 120, 200 / 165, 190 / 180))
  expect_equal(unname(p$n_obs), c(1L, 1L, 1L))
  expect_error(dev_factors(tri, n_periods = 0), "n_periods")
  expect_error(dev_factors(tri, n_periods = 1.5), "whole number")
  # n_periods larger than the history is the same as all data.
  expect_equal(dev_factors(tri, n_periods = 99)$factors, dev_factors(tri)$factors)
})

test_that("weights and calendar decay weights work", {
  tri <- small_tri()
  w <- calendar_weights(tri, decay = 0.5)
  expect_equal(w[, 1], c(0.25, 0.5, 1, NA), ignore_attr = TRUE)
  p <- dev_factors(tri, weights = w)
  expect_equal(p$factors[[1]], (0.25 * 150 + 0.5 * 165 + 186) / (0.25 * 100 + 0.5 * 110 + 120))
  expect_equal(dev_factors(tri, weights = calendar_weights(tri, 1))$factors,
               dev_factors(tri)$factors)
  w0 <- matrix(1, 4, 3)
  w0[1, 1] <- 0
  expect_equal(dev_factors(tri, weights = w0)$factors[[1]], (165 + 186) / (110 + 120))
  expect_error(dev_factors(tri, weights = matrix(1, 2, 2)), "must be a numeric matrix")
  w0[2, 1] <- -1
  expect_error(dev_factors(tri, weights = w0), "non-negative")
  expect_error(calendar_weights(tri, decay = 0), "decay")
})

test_that("a step with no usable data is an error unless overridden", {
  tri <- small_tri()
  w <- matrix(1, 4, 3)
  w[1, 3] <- 0
  expect_error(dev_factors(tri, weights = w), "No usable data to estimate factor\\(s\\) 36-48")
  p <- dev_factors(tri, weights = w, override = c("36-48" = 1.02))
  expect_equal(p$factors[["36-48"]], 1.02)
  expect_true(is.na(p$estimated[["36-48"]]))
})

test_that("overrides replace selected factors and are validated", {
  p <- dev_factors(small_tri(), override = c("24-36" = 1.1))
  expect_equal(p$factors[["24-36"]], 1.1)
  expect_equal(p$estimated[["24-36"]], 76 / 63)
  expect_error(dev_factors(small_tri(), override = c("99-100" = 1)), "Unknown step")
  expect_error(dev_factors(small_tri(), override = 1.1), "named numeric")
  expect_error(dev_factors(small_tri(), override = c("24-36" = -1)), "positive")
})

test_that("fixed tail multiplies every CDF", {
  p1 <- dev_factors(raa)
  p2 <- dev_factors(raa, tail = 1.05)
  expect_equal(p2$cdf, p1$cdf * 1.05)
  expect_equal(p2$tail_info$method, "fixed")
  expect_error(dev_factors(raa, tail = 0), "tail")
  expect_error(dev_factors(raa, tail = "weibull"), "exponential")
})

test_that("exponential tail matches its definition and the Python implementation", {
  r <- pyref()
  p <- dev_factors(raa, tail = "exponential")
  ti <- p$tail_info
  f <- unname(dev_factors(raa)$factors)
  j <- seq_along(f)
  fit <- lm(log(f - 1) ~ j)
  expect_equal(unname(coef(fit)), c(ti$intercept, ti$slope), tolerance = 1e-10)
  expect_equal(p$tail, prod(1 + exp(ti$intercept + ti$slope * (9 + seq_len(ti$steps)))))
  expect_equal(p$tail, r$raa$tail_exponential, tolerance = 1e-7)
  expect_equal(dev_factors(taylor_ashe, tail = "exponential")$tail,
               r$genins$tail_exponential, tolerance = 1e-7)
})

test_that("exponential tail refuses unsuitable data", {
  tri <- small_tri()
  expect_error(dev_factors(tri, tail = "exponential"), NA)  # three factors > 1
  m <- matrix(c(100, 90, 95, 96, 100, 90, 95, NA, 100, 90, NA, NA, 100, NA, NA, NA), 4,
              byrow = TRUE)
  expect_error(dev_factors(as_triangle(m, type = "cumulative"), tail = "exponential"),
               "at least 3 factors")
  m2 <- matrix(c(100, 110, 121, 140, 100, 110, 121, NA, 100, 110, NA, NA, 100, NA, NA, NA), 4,
               byrow = TRUE)  # increasing (f-1)
  expect_error(dev_factors(as_triangle(m2, type = "cumulative"), tail = "exponential"),
               "not decaying")
})

test_that("dev_pattern builds consistent patterns from factors or CDFs", {
  a <- dev_pattern(c(12, 24, 36), factors = c(2, 1.5), tail = 1.1)
  expect_equal(unname(a$cdf), c(3.3, 1.65, 1.1))
  b <- dev_pattern(c(12, 24, 36), cdf = c(3.3, 1.65, 1.1))
  expect_equal(unname(b$factors), c(2, 1.5))
  expect_equal(b$tail, 1.1)
  expect_error(dev_pattern(c(12, 24), factors = 1, cdf = 1), "exactly one")
  expect_error(dev_pattern(c(12, 24), factors = c(1, 2)), "must be 1 positive")
  expect_error(dev_pattern(c(24, 12), factors = 1), "strictly increasing")
  expect_error(dev_pattern(c(12, 24), cdf = c(1, -1)), "positive")
  expect_output(print(a), "tail")
  expect_output(print(dev_factors(raa, n_periods = 3, override = c("9-10" = 1.01))),
                "latest 3 diagonals")
})

test_that("incremental and cumulative inputs give identical factors", {
  expect_equal(dev_factors(to_incremental(raa))$factors, dev_factors(raa)$factors)
})
