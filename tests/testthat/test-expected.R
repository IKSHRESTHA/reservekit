test_that("BF formula: latest + prior x (1 - 1/CDF)", {
  tri <- small_tri()
  cl <- fit_chain_ladder(tri)
  fit <- fit_bf(tri, exposure = 400, elr = 0.6)
  cdf <- cl$results$cdf
  expect_equal(fit$results$ultimate, c(190, 200, 186, 130) + 240 * (1 - 1 / cdf))
  expect_equal(fit$results$prior_ultimate, rep(240, 4))
  expect_equal(fit$results$reserve, fit$results$ultimate - fit$results$latest)
})

test_that("BF with prior ultimate equal to chain-ladder ultimate reproduces chain ladder", {
  cl <- fit_chain_ladder(raa)
  bf <- fit_bf(raa, prior_ultimate = cl$results$ultimate)
  expect_equal(bf$results$ultimate, cl$results$ultimate)
})

test_that("BF and Cape Cod match the Python chainladder implementation", {
  r <- pyref()
  for (nm in c("raa", "genins")) {
    tri <- if (nm == "raa") raa else taylor_ashe
    elr <- if (nm == "raa") 0.6 else 0.65
    expect_equal(fit_bf(tri, exposure = r[[nm]]$premium, elr = elr)$results$ultimate,
                 r[[nm]]$bf_ult, tolerance = 1e-12)
    cc <- fit_cape_cod(tri, exposure = r[[nm]]$premium)
    expect_equal(cc$results$ultimate, r[[nm]]$cc_ult, tolerance = 1e-12)
    expect_equal(cc$elr, r[[nm]]$cc_elr[1], tolerance = 1e-12)
    cc2 <- fit_cape_cod(tri, exposure = r[[nm]]$premium, decay = 0.75)
    expect_equal(cc2$results$ultimate, r[[nm]]$cc_decay75_ult, tolerance = 1e-12)
  }
})

test_that("Cape Cod ELR is losses to date over used-up exposure", {
  prem <- c(300, 320, 340, 360)
  fit <- fit_cape_cod(small_tri(), exposure = prem)
  used <- prem / fit_chain_ladder(small_tri())$results$cdf
  expect_equal(fit$elr, sum(c(190, 200, 186, 130)) / sum(used))
  expect_equal(fit$results$elr, rep(fit$elr, 4))
  # Identity: total Cape Cod expected losses on used-up exposure = losses to date.
  expect_equal(sum(fit$results$prior_ultimate * fit$results$pct_developed), 706)
})

test_that("BF input validation", {
  expect_error(fit_bf(raa), "needs `exposure` and `elr`")
  expect_error(fit_bf(raa, exposure = 1, elr = 1, prior_ultimate = 1), "not both")
  expect_error(fit_bf(raa, exposure = 1:3, elr = 0.5), "length 1 or one value per origin")
  expect_error(fit_bf(raa, exposure = c(1, NA, 1:8), elr = 0.5), "no missing")
  expect_error(fit_cape_cod(raa), "needs `exposure`")
  expect_error(fit_bf(raa, exposure = -1, elr = 0.5), "positive")
  expect_error(fit_bf(raa, exposure = 1, elr = -0.5), "non-negative")
  expect_error(fit_elr(raa, exposure = 0, elr = 0.5), "positive")
  expect_error(fit_elr(raa, exposure = 1, elr = -1), "non-negative")
  p <- dev_factors(raa)
  expect_error(fit_bf(raa, exposure = 1e5, elr = 0.5, pattern = p, tail = 1.5),
               "would be ignored")
  expect_error(fit_cape_cod(raa, exposure = 1e5, pattern = p, n_periods = 3),
               "would be ignored")
  expect_error(fit_cape_cod(raa, exposure = -1), "positive")
  expect_error(fit_cape_cod(raa, exposure = 1, decay = 2), "decay")
})

test_that("BF warns when the pattern implies more than 100% developed", {
  p <- dev_pattern(c(12, 24, 36, 48), cdf = c(1.5, 1.1, 0.95, 1))
  expect_warning(fit <- fit_bf(small_tri(), exposure = 400, elr = 0.6, pattern = p),
                 "exceeds 100%")
  expect_length(fit$warnings, 1)
  expect_lt(fit$results$reserve[2], 0)
})

test_that("BF projection: increments sum to reserve and emerge with the pattern", {
  fit <- fit_bf(raa, exposure = seq(30000, 48000, length.out = 10), elr = 0.6, tail = 1.05)
  inc <- as.matrix(predict(fit, type = "incremental"))
  obs <- attr(predict(fit), "observed")
  inc[obs] <- 0
  expect_equal(rowSums(inc), fit$results$reserve, ignore_attr = TRUE)
})

test_that("ELR method: ultimate = exposure x ELR, negative reserves flagged", {
  prem <- seq(30000, 48000, length.out = 10)
  fit <- fit_elr(raa, exposure = prem, elr = 0.8)
  expect_equal(fit$results$ultimate, prem * 0.8)
  expect_true(is.na(fit$results$cdf[1]))
  expect_warning(f2 <- fit_elr(raa, exposure = prem, elr = 0.3), "reserve is negative")
  expect_true(any(f2$results$reserve < 0))
  # Without a pattern the projection puts the reserve in an ultimate column.
  pr <- as.matrix(predict(fit))
  expect_equal(pr[, "ult"], prem * 0.8, ignore_attr = TRUE)
  # With a pattern, increments still sum to the reserve.
  fit3 <- fit_elr(raa, exposure = prem, elr = 0.8, pattern = dev_factors(raa))
  inc <- as.matrix(predict(fit3, type = "incremental"))
  inc[attr(predict(fit3), "observed")] <- 0
  expect_equal(rowSums(inc), fit3$results$reserve, ignore_attr = TRUE)
  expect_error(fit_elr(raa, exposure = prem), "needs `exposure` and `elr`")
})
