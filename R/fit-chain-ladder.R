#' Chain ladder (loss development) method
#'
#' Projects each origin period's latest cumulative value to ultimate with
#' cumulative development factors:
#' \deqn{\hat U_i = C_{i,a_i} \times CDF_{a_i},}
#' where \eqn{a_i} is the latest observed development age of origin \eqn{i}
#' and \eqn{CDF_{a}} is the product of the age-to-age factors from age
#' \eqn{a} to the last age times the tail factor. The reserve is
#' \eqn{\hat U_i - C_{i,a_i}}.
#'
#' @section Assumptions:
#' Future development of each origin is proportional to its current
#' cumulative value, with the same expected factors for all origins
#' (no changes in mix, claims handling, case reserving or inflation that
#' alter development). Check with [diagnostics()] and [validate_triangle()].
#'
#' @section Limitations:
#' Immature origins with small or zero latest values give unstable or zero
#' ultimates; Bornhuetter-Ferguson ([fit_bf()]) or Cape Cod ([fit_cape_cod()])
#' are usually preferable there. Negative or zero denominators distort
#' volume-weighted factors.
#'
#' @param x A `triangle` (cumulative or incremental; incremental triangles are
#'   accumulated, never reinterpreted).
#' @param method,n_periods,weights,override,tail,tail_max_steps Passed to
#'   [dev_factors()].
#' @param pattern Optional `dev_pattern` to use instead of estimating
#'   factors from `x` (e.g. an industry benchmark). Its ages must match the
#'   triangle's.
#' @return A `reserve_fit` object of subclass `reserve_chain_ladder`. See
#'   [reserve_fit] for its structure and methods.
#' @examples
#' fit <- fit_chain_ladder(raa)
#' fit
#' summary(fit)
#' predict(fit)
#' fit_chain_ladder(raa, tail = 1.05)
#' @export
fit_chain_ladder <- function(x, method = c("volume", "simple"), n_periods = NULL,
                             weights = NULL, override = NULL, tail = 1,
                             tail_max_steps = 100, pattern = NULL) {
  check_triangle(x, "x")
  assert_fittable(x)
  if (!is.null(pattern)) {
    given <- c(method = !missing(method), n_periods = !is.null(n_periods),
               weights = !is.null(weights), override = !is.null(override),
               tail = !missing(tail), tail_max_steps = !missing(tail_max_steps))
    if (any(given)) {
      abort("`pattern` was supplied, so factor-estimation argument(s) ",
            paste0("`", names(given)[given], "`", collapse = ", "),
            " would be ignored. Build the pattern with those settings instead ",
            "(dev_factors() or dev_pattern()).")
    }
  }
  method <- match.arg(method)
  cum <- to_cumulative(x)
  pattern <- resolve_pattern(cum, pattern, method = method, n_periods = n_periods,
                             weights = weights, override = override, tail = tail,
                             tail_max_steps = tail_max_steps)
  lat <- latest_diagonal(cum)
  cdf <- unname(pattern$cdf[lat$dev_index])
  ult <- lat$value * cdf
  res <- base_results(lat, pattern)
  res$ultimate <- ult
  res$reserve <- ult - lat$value
  new_reserve_fit(
    method = "chain_ladder", label = "Chain ladder", call = match.call(),
    triangle = cum, pattern = pattern, results = res,
    emergence = ult,
    assumptions = list(
      factor_method = pattern$method %||% "supplied pattern",
      n_periods = n_periods,
      weights = if (is.null(weights)) "none" else "user-supplied",
      overrides = override,
      tail = pattern$tail_info %||% list(factor = pattern$tail, method = "supplied")
    )
  )
}

resolve_pattern <- function(cum, pattern, ..., .strict = FALSE) {
  if (is.null(pattern)) return(dev_factors(cum, ...))
  if (.strict && ...length() > 0L) {
    abort("`pattern` was supplied, so argument(s) passed to dev_factors() via `...` (",
          paste0("`", names(list(...)), "`", collapse = ", "), ") would be ignored. ",
          "Build the pattern with those settings instead.")
  }
  if (!inherits(pattern, "dev_pattern")) {
    abort("`pattern` must be a `dev_pattern` (see dev_pattern() or dev_factors()).")
  }
  mismatch <- length(pattern$ages) != ncol(cum) ||
    any(abs(pattern$ages - attr(cum, "dev_ages")) > 1e-8)
  if (mismatch) {
    abort("`pattern` ages (", paste(format_ages(pattern$ages), collapse = ", "),
          ") do not match the triangle's development ages (",
          paste(format_ages(attr(cum, "dev_ages")), collapse = ", "), ").")
  }
  pattern
}

base_results <- function(lat, pattern) {
  idx <- lat$dev_index
  data.frame(
    origin = lat$origin,
    dev_age = lat$dev_age,
    latest = lat$value,
    cdf = if (is.null(pattern)) NA_real_ else unname(pattern$cdf[idx]),
    pct_developed = if (is.null(pattern)) NA_real_ else unname(pattern$pct_developed[idx]),
    stringsAsFactors = FALSE
  )
}

new_reserve_fit <- function(method, label, call, triangle, pattern, results,
                            emergence, assumptions, extra = list(),
                            warnings = character()) {
  structure(c(list(
    method = method, label = label, call = call, triangle = triangle,
    pattern = pattern, results = results, emergence = emergence,
    assumptions = assumptions, warnings = warnings
  ), extra), class = c(paste0("reserve_", method), "reserve_fit"))
}
