test_that("Mack s.e. matches exact rational hand calculation", {
  h <- hand()
  fit <- fit_mack(small_tri())
  expect_equal(as.vector(fit$sigma^2), h$sigma2, tolerance = 1e-12)
  expect_equal(fit$results$se, h$mack_se, tolerance = 1e-10)
  expect_equal(fit$total$se, h$total_mack_se, tolerance = 1e-10)
  expect_equal(fit$results$ultimate, h$ultimate, tolerance = 1e-13)
  expect_identical(unname(attr(fit$sigma, "extrapolated")), c(FALSE, FALSE, TRUE))
})

test_that("Mack reproduces Mack (1993/1994) published RAA results", {
  # Published: reserves and s.e. by accident year 1982-1990, total 52,135
  # with s.e. 26,909 (52%), and sigma^2_9 = 1.34 by extrapolation.
  fit <- fit_mack(raa)
  expect_equal(round(fit$results$se[-1]),
               c(206, 623, 747, 1469, 2002, 2209, 5358, 6333, 24566))
  expect_equal(round(fit$total$reserve), 52135)
  expect_equal(round(fit$total$se), 26909)
  expect_equal(round(100 * fit$total$cv), 52)
  expect_equal(round(fit$sigma[[9]]^2, 2), 1.34)
})

test_that("Mack reproduces the Taylor-Ashe total s.e. 2,447,095 (Mack 1993)", {
  fit <- fit_mack(taylor_ashe)
  expect_equal(round(fit$total$reserve), 18680856)
  expect_equal(round(fit$total$se), 2447095)
})

test_that("Mack matches Python chainladder for both sigma extrapolation rules", {
  r <- pyref()
  for (nm in c("raa", "genins")) {
    tri <- if (nm == "raa") raa else taylor_ashe
    m <- fit_mack(tri)
    expect_equal(m$results$se, r[[nm]]$mack_mackinterp_se, tolerance = 1e-9)
    expect_equal(m$total$se, r[[nm]]$mack_mackinterp_total_se, tolerance = 1e-9)
    l <- fit_mack(tri, sigma_extrapolation = "loglinear")
    expect_equal(l$results$se, r[[nm]]$mack_loglinear_se, tolerance = 1e-9)
    expect_equal(l$total$se, r[[nm]]$mack_loglinear_total_se, tolerance = 1e-9)
  }
})

test_that("Mack point estimates equal chain ladder", {
  expect_equal(fit_mack(raa)$results$ultimate, fit_chain_ladder(raa)$results$ultimate)
})

test_that("process and parameter components add up to mse", {
  fit <- fit_mack(raa)
  r <- fit$results
  expect_equal(r$se^2, r$process_se^2 + r$parameter_se^2)
  expect_equal(fit$total$se^2, fit$total$process_se^2 + fit$total$parameter_se^2)
  expect_equal(fit$total$process_se^2, sum(r$process_se^2))
  # Parameter error is positively correlated across origins.
  expect_gt(fit$total$parameter_se^2, sum(r$parameter_se^2))
  expect_equal(r$se[1], 0)
  expect_true(is.na(r$cv[1]))
})

test_that("Mack s.e. scales linearly with the data", {
  big <- as_triangle(as.matrix(raa) * 1000, type = "cumulative")
  expect_equal(fit_mack(big)$total$se, fit_mack(raa)$total$se * 1000)
})

test_that("total s.e. equals explicit pairwise formula on RAA", {
  fit <- fit_mack(raa)
  C <- as.matrix(raa)
  f <- unname(fit$pattern$factors)
  s2 <- unname(fit$sigma)^2
  S <- unname(fit$S)
  U <- fit$results$ultimate
  I <- 10
  tot <- sum(fit$results$se^2)
  for (i in 2:I) for (l in seq_len(I)[seq_len(I) > i]) {
    k <- (I + 1 - i):(I - 1)
    tot <- tot + 2 * U[i] * U[l] * sum(s2[k] / f[k]^2 / S[k])
  }
  expect_equal(sqrt(tot), fit$total$se)
})

test_that("Mack refuses data it cannot handle", {
  m <- small_matrix()
  m[1, 1] <- 0
  expect_error(fit_mack(as_triangle(m, type = "cumulative")), "positive cumulative values")
  expect_error(fit_mack(small_tri(), n_periods = 1), "Cannot extrapolate sigma")
  m3 <- matrix(c(1, 2, 1, NA), 2, byrow = TRUE)
  expect_error(fit_mack(as_triangle(m3, type = "cumulative")), "Cannot extrapolate")
  expect_error(fit_mack(as_triangle(m3, type = "cumulative"), sigma_extrapolation = "loglinear"),
               "at least 2 positive")
})

test_that("Mack sigma extrapolation handles a zero preceding sigma", {
  expect_equal(extrapolate_sigma2(c(0, 1, NA), "mack"), c(0, 1, 0))
  expect_equal(extrapolate_sigma2(c(4, 2, NA), "mack"), c(4, 2, 1))
  expect_equal(extrapolate_sigma2(c(1, 2, NA), "mack"), c(1, 2, 1))
})

test_that("Mack with n_periods uses the same weighting as dev_factors", {
  fit <- fit_mack(raa, n_periods = 7)
  expect_equal(fit$pattern$factors, dev_factors(raa, n_periods = 7)$factors)
  expect_true(all(is.finite(fit$results$se)))
})

test_that("Mack needs no extrapolation when every step has two ratios", {
  # Trapezoid: two fully developed origins.
  m <- rbind(c(100, 150, 165, 170), c(90, 140, 150, 158), c(110, 160, 178, NA),
             c(95, 150, NA, NA))
  fit <- fit_mack(as_triangle(m, type = "cumulative"))
  expect_false(any(attr(fit$sigma, "extrapolated")))
  expect_length(fit$warnings, 0)
})

test_that("Mack with n_periods matches Python chainladder (both sigma rules)", {
  r <- readRDS(testthat::test_path("fixtures", "python_mack_nperiods.rds"))
  for (nm in c("raa", "genins")) {
    tri <- if (nm == "raa") raa else taylor_ashe
    for (n in c(3, 5)) {
      for (rule in c("mack", "loglinear")) {
        key <- paste0(nm, "_n", n, "_", if (rule == "mack") "mack" else "log-linear")
        fit <- fit_mack(tri, n_periods = n, sigma_extrapolation = rule)
        expect_equal(fit$results$se, r[[key]]$se, tolerance = 1e-9)
        expect_equal(fit$total$se, r[[key]]$total_se, tolerance = 1e-9)
      }
    }
  }
})

test_that("zero latest gives zero s.e. with a warning; negative latest is refused", {
  m <- matrix(c(100, 150, 175, 180, 110, 160, 180, NA, 120, 170, NA, NA, 0, NA, NA, NA),
              4, byrow = TRUE)
  expect_warning(fit <- fit_mack(as_triangle(m, type = "cumulative")), "zero latest")
  expect_equal(fit$results$se[4], 0)
  expect_true(is.finite(fit$total$se))
  # Total equals the fit without the zero origin's contribution.
  ok <- fit_mack(as_triangle(m[1:3, ], type = "cumulative"))
  expect_equal(fit$total$se, ok$total$se)
  m[4, 1] <- -50
  expect_error(fit_mack(as_triangle(m, type = "cumulative")), "non-negative latest")
})
