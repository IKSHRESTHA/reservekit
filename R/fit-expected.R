#' Bornhuetter-Ferguson method
#'
#' Combines the reported/paid experience to date with an a priori expected
#' ultimate for the unreported part:
#' \deqn{\hat U_i = C_{i,a_i} + U^{prior}_i (1 - 1/CDF_{a_i}),}
#' with \eqn{U^{prior}_i = E_i \times ELR_i} (exposure times expected loss
#' ratio) or supplied directly.
#'
#' @section Assumptions:
#' The development pattern is appropriate for the origin, and the a priori
#' expected ultimate is independent of the origin's own experience to date.
#' The future emergence is \eqn{U^{prior}_i \times} (the change in percentage
#' developed), which is how [predict()] projects the triangle.
#'
#' @section Limitations:
#' Results depend directly on the a priori ELR; document how it was chosen.
#' A CDF below 1 (percentage developed above 100\%, e.g. from case reserve
#' redundancy) gives negative expected emergence: a warning is raised.
#'
#' @param x A `triangle`.
#' @param exposure Exposure per origin (e.g. earned premium), length 1 or one
#'   per origin. Required unless `prior_ultimate` is given.
#' @param elr Expected loss ratio(s) applied to `exposure`, length 1 or one per
#'   origin.
#' @param prior_ultimate A priori expected ultimate per origin, as an
#'   alternative to `exposure` and `elr`.
#' @param pattern Optional `dev_pattern`. By default the chain-ladder pattern
#'   is estimated from `x` with `...`.
#' @param ... Passed to [dev_factors()] when `pattern` is `NULL`
#'   (e.g. `tail`, `n_periods`, `method`).
#' @return A `reserve_fit` of subclass `reserve_bf`. `results` gains
#'   `exposure`, `elr` and `prior_ultimate` columns.
#' @references Bornhuetter, R. L. and Ferguson, R. E. (1972). The actuary and
#'   IBNR. *Proceedings of the Casualty Actuarial Society* 59, 181-195.
#' @examples
#' premium <- seq(30000, 48000, length.out = 10)
#' fit_bf(raa, exposure = premium, elr = 0.6)
#' @export
fit_bf <- function(x, exposure = NULL, elr = NULL, prior_ultimate = NULL,
                   pattern = NULL, ...) {
  check_triangle(x, "x")
  assert_fittable(x)
  cum <- to_cumulative(x)
  n <- nrow(cum)
  if (!is.null(prior_ultimate)) {
    if (!is.null(exposure) || !is.null(elr)) {
      abort("Supply either `prior_ultimate`, or `exposure` and `elr`, not both.")
    }
    prior <- per_origin(prior_ultimate, n, "prior_ultimate")
    exposure_v <- rep(NA_real_, n)
    elr_v <- rep(NA_real_, n)
  } else {
    if (is.null(exposure) || is.null(elr)) {
      abort("`fit_bf()` needs `exposure` and `elr`, or `prior_ultimate`.")
    }
    exposure_v <- per_origin(exposure, n, "exposure")
    elr_v <- per_origin(elr, n, "elr")
    if (any(exposure_v <= 0)) abort("`exposure` must be positive.")
    if (any(elr_v < 0)) abort("`elr` must be non-negative.")
    prior <- exposure_v * elr_v
  }
  pattern <- resolve_pattern(cum, pattern, ..., .strict = TRUE)
  out <- bf_core(cum, pattern, prior)
  out$results$exposure <- exposure_v
  out$results$elr <- elr_v
  out$results <- out$results[, c("origin", "dev_age", "latest", "exposure", "elr",
                                 "prior_ultimate", "cdf", "pct_developed",
                                 "ultimate", "reserve")]
  new_reserve_fit(
    method = "bf", label = "Bornhuetter-Ferguson", call = match.call(),
    triangle = cum, pattern = pattern, results = out$results,
    emergence = prior, warnings = out$warnings,
    assumptions = list(
      prior = if (is.null(prior_ultimate)) "exposure x elr" else "prior_ultimate supplied",
      pattern = pattern$method %||% "supplied pattern",
      tail = pattern$tail
    )
  )
}

bf_core <- function(cum, pattern, prior) {
  lat <- latest_diagonal(cum)
  res <- base_results(lat, pattern)
  res$prior_ultimate <- prior
  unreported <- prior * (1 - res$pct_developed)
  res$ultimate <- lat$value + unreported
  res$reserve <- unreported
  w <- character()
  if (any(res$pct_developed > 1)) {
    w <- sprintf(paste("Percentage developed exceeds 100%% for origin(s) %s (CDF < 1);",
                       "expected future emergence is negative."),
                 paste(res$origin[res$pct_developed > 1], collapse = ", "))
    warn(w)
  }
  list(results = res, warnings = w)
}

#' Cape Cod (Stanard-Buhlmann) method
#'
#' Estimates a single expected loss ratio from the triangle itself,
#' \deqn{ELR = \frac{\sum_i C_{i,a_i}}{\sum_i E_i / CDF_{a_i}},}
#' (losses to date over "used-up" exposure) and then applies the
#' Bornhuetter-Ferguson formula with prior ultimate \eqn{E_i \times ELR}.
#'
#' With `decay < 1` this is the generalised Cape Cod of Gluck (1997): each
#' origin gets its own ELR estimated with weights `decay^|i - l|` on the
#' other origins \eqn{l}, so distant years count less.
#'
#' @section Assumptions:
#' Exposure is on a consistent basis across origins (e.g. on-level premium);
#' expected loss ratios are equal (or, with decay, vary smoothly).
#'
#' @inheritParams fit_bf
#' @param exposure Exposure per origin (e.g. on-level earned premium).
#'   Required.
#' @param decay Weight decay in (0, 1]; 1 (default) is the classic method.
#' @return A `reserve_fit` of subclass `reserve_cape_cod`; `results` gains
#'   `exposure`, `elr` (per origin; all equal when `decay = 1`) and
#'   `prior_ultimate`. The overall ELR is in `$elr`.
#' @references
#' Stanard, J. N. (1985). A simulation test of prediction errors of loss
#' reserve estimation techniques. *PCAS* 72, 124-148.
#'
#' Gluck, S. M. (1997). Balancing development and trend in loss reserve
#' analyses. *PCAS* 84, 482-532.
#' @examples
#' premium <- seq(30000, 48000, length.out = 10)
#' fit_cape_cod(raa, exposure = premium)
#' @export
fit_cape_cod <- function(x, exposure, pattern = NULL, decay = 1, ...) {
  check_triangle(x, "x")
  assert_fittable(x)
  cum <- to_cumulative(x)
  n <- nrow(cum)
  if (missing(exposure)) abort("`fit_cape_cod()` needs `exposure`.")
  exposure_v <- per_origin(exposure, n, "exposure")
  if (any(exposure_v <= 0)) abort("`exposure` must be positive.")
  check_number(decay, "decay", lower = 0, upper = 1, lower_inclusive = FALSE)
  pattern <- resolve_pattern(cum, pattern, ..., .strict = TRUE)
  lat <- latest_diagonal(cum)
  used <- exposure_v * unname(pattern$pct_developed[lat$dev_index])
  wmat <- decay^abs(outer(seq_len(n), seq_len(n), "-"))
  elr_v <- as.vector(wmat %*% lat$value) / as.vector(wmat %*% used)
  overall <- sum(lat$value) / sum(used)
  out <- bf_core(cum, pattern, exposure_v * elr_v)
  out$results$exposure <- exposure_v
  out$results$used_exposure <- used
  out$results$elr <- elr_v
  out$results <- out$results[, c("origin", "dev_age", "latest", "exposure",
                                 "used_exposure", "elr", "prior_ultimate", "cdf",
                                 "pct_developed", "ultimate", "reserve")]
  new_reserve_fit(
    method = "cape_cod", label = "Cape Cod", call = match.call(),
    triangle = cum, pattern = pattern, results = out$results,
    emergence = exposure_v * elr_v, warnings = out$warnings,
    extra = list(elr = overall),
    assumptions = list(decay = decay, pattern = pattern$method %||% "supplied pattern",
                       tail = pattern$tail)
  )
}

#' Expected loss ratio method
#'
#' Sets ultimate equal to exposure times an expected loss ratio,
#' ignoring the experience to date: \eqn{\hat U_i = E_i \times ELR_i}.
#' The reserve is \eqn{\hat U_i - C_{i,a_i}} and can be negative when
#' experience to date exceeds the expectation; a warning is raised.
#'
#' A `pattern` is optional. It is used only to report percentage developed
#' and to allocate the reserve to future development periods in [predict()]
#' (in proportion to the remaining pattern).
#'
#' @inheritParams fit_bf
#' @param exposure,elr Exposure and expected loss ratio(s). Required.
#' @param pattern Optional `dev_pattern` (not estimated by default).
#' @return A `reserve_fit` of subclass `reserve_elr`.
#' @examples
#' fit_elr(raa, exposure = seq(30000, 48000, length.out = 10), elr = 0.6)
#' @export
fit_elr <- function(x, exposure, elr, pattern = NULL) {
  check_triangle(x, "x")
  assert_fittable(x)
  cum <- to_cumulative(x)
  n <- nrow(cum)
  if (missing(exposure) || missing(elr)) abort("`fit_elr()` needs `exposure` and `elr`.")
  exposure_v <- per_origin(exposure, n, "exposure")
  elr_v <- per_origin(elr, n, "elr")
  if (any(exposure_v <= 0)) abort("`exposure` must be positive.")
  if (any(elr_v < 0)) abort("`elr` must be non-negative.")
  if (!is.null(pattern)) pattern <- resolve_pattern(cum, pattern)
  lat <- latest_diagonal(cum)
  res <- base_results(lat, pattern)
  res$exposure <- exposure_v
  res$elr <- elr_v
  res$ultimate <- exposure_v * elr_v
  res$reserve <- res$ultimate - lat$value
  w <- character()
  if (any(res$reserve < 0)) {
    w <- sprintf(paste("Experience to date exceeds the expected ultimate for origin(s) %s;",
                       "reserve is negative."),
                 paste(res$origin[res$reserve < 0], collapse = ", "))
    warn(w)
  }
  emergence <- NULL
  if (!is.null(pattern)) {
    rem <- 1 - res$pct_developed
    emergence <- ifelse(abs(rem) > 1e-12, res$reserve / rem, NA_real_)
  }
  new_reserve_fit(
    method = "elr", label = "Expected loss ratio", call = match.call(),
    triangle = cum, pattern = pattern,
    results = res[, c("origin", "dev_age", "latest", "exposure", "elr", "cdf",
                      "pct_developed", "ultimate", "reserve")],
    emergence = emergence, warnings = w,
    assumptions = list(pattern = if (is.null(pattern)) "none" else "supplied")
  )
}
