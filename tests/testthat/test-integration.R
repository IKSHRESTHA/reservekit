test_that("end-to-end workflow from claim transactions", {
  paid <- as_triangle(claims_synthetic, origin = "accident_year", development = "dev_age",
                      value = "paid", type = "incremental", measure = "paid")
  inc <- as_triangle(claims_synthetic, origin = "accident_year", development = "dev_age",
                     value = "incurred", type = "incremental", measure = "incurred")
  expect_identical(dim(paid), c(10L, 10L))
  # Triangle totals equal raw data totals: nothing dropped or invented.
  expect_equal(sum(as.matrix(paid), na.rm = TRUE), sum(claims_synthetic$paid))
  expect_equal(sum(as.matrix(inc), na.rm = TRUE), sum(claims_synthetic$incurred))

  v <- validate_triangle(paid)
  expect_false(any(v$severity == "error"))

  prem <- premium_synthetic$earned_premium
  fits <- list(
    cl = fit_chain_ladder(paid),
    mack = fit_mack(paid),
    bf = fit_bf(paid, exposure = prem, elr = 0.65),
    cc = fit_cape_cod(paid, exposure = prem),
    cl_incurred = fit_chain_ladder(inc)
  )
  cmp <- compare_methods(fits[1:4])
  expect_true(all(is.finite(as.matrix(cmp[, -1]))))
  expect_true(all(fits$mack$results$se >= 0))
  # Paid and incurred ultimates should be in the same range for this synthetic book.
  ratio <- sum(fits$cl_incurred$results$ultimate) / sum(fits$cl$results$ultimate)
  expect_gt(ratio, 0.7)
  expect_lt(ratio, 1.3)
  d <- diagnostics(fits$mack)
  expect_s3_class(d, "reserve_diagnostics")
})

test_that("datasets are what they claim to be", {
  expect_identical(triangle_type(raa), "cumulative")
  expect_equal(as.matrix(raa)[1, 10], 18834)
  expect_equal(as.matrix(taylor_ashe)[1, 1], 357848)
  expect_equal(sum(latest_diagonal(raa)$value), 160987)
})
