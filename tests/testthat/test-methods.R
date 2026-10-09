test_that("all fits share the same core structure", {
  prem <- seq(30000, 48000, length.out = 10)
  fits <- list(fit_chain_ladder(raa), fit_mack(raa), fit_bf(raa, exposure = prem, elr = 0.6),
               fit_cape_cod(raa, exposure = prem), fit_elr(raa, exposure = prem, elr = 0.8))
  core <- c("origin", "dev_age", "latest", "cdf", "pct_developed", "ultimate", "reserve")
  for (f in fits) {
    expect_s3_class(f, "reserve_fit")
    expect_true(all(core %in% names(f$results)))
    expect_true(all(c("method", "label", "call", "triangle", "pattern", "results",
                      "assumptions", "warnings") %in% names(f)))
    expect_equal(f$results$reserve, f$results$ultimate - f$results$latest)
    expect_output(print(f), "<reserve_fit>")
    s <- summary(f)
    expect_s3_class(s, "summary.reserve_fit")
    expect_output(print(s), "actuarial judgement")
    df <- as.data.frame(f)
    expect_identical(nrow(df), 10L)
    expect_identical(unique(df$method), f$method)
    expect_s3_class(predict(f), "projected_triangle")
    expect_output(print(predict(f)), "projected cells marked")
  }
})

test_that("coef returns factors plus tail", {
  cf <- coef(fit_chain_ladder(raa, tail = 1.02))
  expect_length(cf, 10)
  expect_equal(cf[["tail"]], 1.02)
  expect_null(coef(fit_elr(raa, exposure = 1e5, elr = 0.5)))
})

test_that("labels follow the measure", {
  inc <- as_triangle(as.matrix(raa), type = "cumulative", measure = "paid")
  expect_output(print(fit_chain_ladder(inc)), "Unpaid")
  expect_output(print(fit_chain_ladder(raa)), "IBNR")
})

test_that("compare_methods lines up methods and totals", {
  prem <- seq(30000, 48000, length.out = 10)
  cmp <- compare_methods(cl = fit_chain_ladder(raa), bf = fit_bf(raa, exposure = prem, elr = 0.6))
  expect_identical(names(cmp), c("origin", "latest", "cl", "bf"))
  expect_identical(cmp$origin[11], "Total")
  expect_equal(cmp$cl[11], sum(fit_chain_ladder(raa)$results$reserve))
  cmp2 <- compare_methods(list(a = fit_chain_ladder(raa)), value = "ultimate")
  expect_equal(cmp2$a[11], sum(fit_chain_ladder(raa)$results$ultimate))
  unnamed <- compare_methods(fit_chain_ladder(raa), fit_chain_ladder(raa, tail = 1.1))
  expect_identical(names(unnamed)[3:4], c("chain_ladder", "chain_ladder.1"))
  expect_error(compare_methods(fit_chain_ladder(raa), fit_chain_ladder(taylor_ashe)),
               "same origin periods")
  other <- as_triangle(as.matrix(raa) * 1.1, type = "cumulative", measure = "paid")
  expect_error(compare_methods(fit_chain_ladder(raa), fit_chain_ladder(other)),
               "different bases")
  ult <- compare_methods(a = fit_chain_ladder(raa), b = fit_chain_ladder(other), value = "ultimate")
  expect_identical(names(ult), c("origin", "a", "b"))
  expect_error(compare_methods(1), "reserve_fit")
  expect_error(compare_methods(), "at least one")
})

test_that("plots run without error", {
  pdf(NULL)
  on.exit(dev.off())
  expect_invisible(plot(raa))
  expect_invisible(plot(fit_mack(raa)))
  expect_invisible(plot(fit_mack(raa), which = "pattern"))
  expect_invisible(plot(fit_mack(raa), which = "residuals"))
  expect_invisible(plot(fit_chain_ladder(raa, tail = 1.05), which = "pattern"))
  expect_error(plot(fit_chain_ladder(raa), which = "residuals"), "Mack fit")
  expect_error(plot(fit_elr(raa, exposure = 1e5, elr = 0.5), which = "pattern"), "no development")
})
