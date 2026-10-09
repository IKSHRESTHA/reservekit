test_that("chain ladder ultimates and reserves match exact hand calculation", {
  h <- hand()
  fit <- fit_chain_ladder(small_tri())
  expect_s3_class(fit, c("reserve_chain_ladder", "reserve_fit"))
  expect_equal(fit$results$ultimate, h$ultimate, tolerance = 1e-13)
  expect_equal(fit$results$reserve, h$reserve, tolerance = 1e-12)
  expect_equal(sum(fit$results$reserve), h$total_reserve, tolerance = 1e-12)
  expect_equal(fit$results$latest, c(190, 200, 186, 130))
  expect_equal(fit$results$dev_age, c(48, 36, 24, 12))
})

test_that("RAA chain ladder total matches Mack (1993) published reserve 52,135", {
  fit <- fit_chain_ladder(raa)
  expect_equal(round(sum(fit$results$reserve)), 52135)
  expect_equal(round(fit$results$reserve),
               c(0, 154, 617, 1636, 2747, 3649, 5435, 10907, 10650, 16339))
})

test_that("incremental input is accumulated, never reinterpreted", {
  a <- fit_chain_ladder(raa)
  b <- fit_chain_ladder(to_incremental(raa))
  expect_equal(a$results, b$results)
})

test_that("projected increments sum to the reserve and cumulative ends at ultimate", {
  for (fit in list(fit_chain_ladder(raa), fit_chain_ladder(raa, tail = 1.07),
                   fit_chain_ladder(raa, tail = "exponential"))) {
    inc <- predict(fit, type = "incremental")
    cum <- predict(fit)
    obs <- attr(inc, "observed")
    future <- inc
    future[obs] <- 0
    expect_equal(rowSums(as.matrix(future)), fit$results$reserve, ignore_attr = TRUE)
    expect_equal(as.matrix(cum)[, ncol(cum)], fit$results$ultimate, ignore_attr = TRUE)
    # Observed cells are untouched.
    k <- ncol(raa)
    expect_equal(as.matrix(cum)[, seq_len(k)][observed_mask(raa)],
                 as.matrix(raa)[observed_mask(raa)])
  }
  expect_false("ult" %in% colnames(predict(fit_chain_ladder(raa))))
  expect_true("ult" %in% colnames(predict(fit_chain_ladder(raa, tail = 1.07))))
})

test_that("projection equals repeated multiplication by factors", {
  fit <- fit_chain_ladder(small_tri())
  f <- fit$pattern$factors
  cum <- as.matrix(predict(fit))
  expect_equal(cum[4, 2], 130 * f[[1]])
  expect_equal(cum[4, 3], 130 * f[[1]] * f[[2]])
  expect_equal(cum[3, 4], 186 * f[[2]] * f[[3]])
})

test_that("tail factor scales ultimates of every origin", {
  a <- fit_chain_ladder(raa)
  b <- fit_chain_ladder(raa, tail = 1.05)
  expect_equal(b$results$ultimate, a$results$ultimate * 1.05)
})

test_that("a supplied pattern is used as is and its ages are checked", {
  p <- dev_pattern(c(12, 24, 36, 48), factors = c(1.5, 1.2, 1.05))
  fit <- fit_chain_ladder(small_tri(), pattern = p)
  expect_equal(fit$results$ultimate[4], 130 * 1.5 * 1.2 * 1.05)
  bad <- dev_pattern(1:4, factors = c(1.5, 1.2, 1.05))
  expect_error(fit_chain_ladder(small_tri(), pattern = bad), "do not match")
  expect_error(fit_chain_ladder(small_tri(), pattern = list()), "dev_pattern")
  expect_error(fit_chain_ladder(small_tri(), pattern = p, tail = 1.1), "`tail` would be ignored")
  expect_error(fit_chain_ladder(small_tri(), pattern = p, method = "simple"), "`method`")
})

test_that("negative incremental values are handled without silent changes", {
  m <- small_matrix()
  m[1, 4] <- 170  # cumulative decreases: recovery
  tri <- as_triangle(m, type = "cumulative", measure = "paid")
  fit <- fit_chain_ladder(tri)
  expect_equal(fit$pattern$factors[[3]], 170 / 180)
  v <- validate_triangle(tri)
  expect_true("negative_increment" %in% v$check)
  expect_identical(as.character(v$severity[v$check == "negative_increment"]), "warning")
})

test_that("zero latest value gives zero chain-ladder ultimate and a validation warning", {
  m <- small_matrix()
  m[4, 1] <- 0
  tri <- as_triangle(m, type = "cumulative")
  expect_equal(fit_chain_ladder(tri)$results$ultimate[4], 0)
  expect_true("zero_latest" %in% validate_triangle(tri)$check)
})

test_that("a column with all-zero denominators is an error", {
  m <- matrix(c(0, 5, 0, NA), 2, byrow = TRUE)
  tri <- as_triangle(m, type = "cumulative")
  expect_error(fit_chain_ladder(tri), "No usable data")
  expect_true("zero_column" %in% validate_triangle(tri)$check)
})

test_that("chain ladder scales linearly with the data", {
  m <- as.matrix(raa)
  big <- as_triangle(m * 1000, type = "cumulative")
  expect_equal(fit_chain_ladder(big)$results$reserve, fit_chain_ladder(raa)$results$reserve * 1000)
})

test_that("chain ladder works on non-square triangles", {
  m <- matrix(c(10, 20, 11, 22, 12, 24, 13, NA), 4, byrow = TRUE)
  fit <- fit_chain_ladder(as_triangle(m, type = "cumulative"))
  expect_equal(fit$results$ultimate, c(20, 22, 24, 13 * (66 / 33)))
})
