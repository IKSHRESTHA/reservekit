#' Individual age-to-age (link) ratios
#'
#' Computes \eqn{F_{ij} = C_{i,j+1} / C_{ij}} for every origin \eqn{i} and
#' development step \eqn{j} where both cumulative values are observed.
#' Ratios with a zero denominator are `NA` (undefined), never `Inf`.
#'
#' @param x A `triangle` (incremental triangles are accumulated first).
#' @return A numeric matrix with one row per origin and one column per
#'   development step (named `"from-to"`).
#' @examples
#' round(link_ratios(raa), 3)
#' @export
link_ratios <- function(x) {
  check_triangle(x, "x")
  cum <- as.matrix(to_cumulative(x))
  k <- ncol(cum)
  if (k < 2L) abort("The triangle has a single development age; there are no link ratios.")
  den <- cum[, -k, drop = FALSE]
  num <- cum[, -1L, drop = FALSE]
  out <- num / den
  out[!is.na(den) & den == 0] <- NA_real_
  dimnames(out) <- list(rownames(cum), step_labels(attr(x, "dev_ages")))
  out
}

step_labels <- function(ages) {
  a <- format_ages(ages)
  paste(a[-length(a)], a[-1L], sep = "-")
}

#' Calendar-period decay weights
#'
#' Builds weights for [dev_factors()] that decline geometrically with the age
#' of the calendar diagonal: the latest diagonal has weight 1, the one before
#' `decay`, then `decay^2`, and so on. Use to give more weight to recent
#' experience while keeping all history.
#'
#' @param x A `triangle`.
#' @param decay Number in (0, 1].
#' @return A numeric matrix of weights aligned with [link_ratios()]; `NA`
#'   where no ratio exists.
#' @examples
#' w <- calendar_weights(raa, decay = 0.8)
#' dev_factors(raa, weights = w)
#' @export
calendar_weights <- function(x, decay) {
  check_triangle(x, "x")
  check_number(decay, "decay", lower = 0, upper = 1, lower_inclusive = FALSE)
  obs <- attr(x, "observed")
  k <- ncol(obs)
  if (k < 2L) abort("The triangle has a single development age.")
  pair <- obs[, -1L, drop = FALSE]
  cal <- row(pair) + col(pair)              # calendar index of numerator cell
  latest <- max(cal[pair])
  w <- decay^(latest - cal)
  w[!pair] <- NA_real_
  dimnames(w) <- list(rownames(x), step_labels(attr(x, "dev_ages")))
  w
}

#' Estimate development factors and a development pattern
#'
#' Estimates age-to-age factors from a triangle, applies optional overrides
#' and a tail factor, and returns the full development pattern (cumulative
#' development factors to ultimate and percentage developed).
#'
#' @section Estimators:
#' For development step \eqn{j} with weights \eqn{w_{ij}}:
#' * `"volume"` (volume-weighted, the chain-ladder estimator):
#'   \eqn{f_j = \sum_i w_{ij} C_{i,j+1} / \sum_i w_{ij} C_{ij}}.
#' * `"simple"` (simple average of link ratios):
#'   \eqn{f_j = \sum_i w_{ij} F_{ij} / \sum_i w_{ij}}. Ratios with a zero
#'   denominator are excluded with a warning naming them.
#'
#' Weights are the product of `weights` (default 1) and the `n_periods`
#' indicator (1 for the latest `n_periods` calendar diagonals, else 0).
#' A weight of 0 excludes a link ratio. A step with no usable ratios, or
#' whose weighted denominator is 0, is an error unless an `override` is given.
#'
#' @section Tail:
#' `tail` is either a fixed factor (default `1`, no development beyond the
#' last age) or `"exponential"`. The exponential method fits
#' \eqn{\log(f_j - 1) = a + b j} by least squares to the estimated factors
#' greater than 1 (factors \eqn{\le 1} are excluded and reported), requires
#' at least 3 points and \eqn{b < 0}, and multiplies the extrapolated factors
#' \eqn{1 + e^{a + b j}} for \eqn{j} beyond the last step until
#' \eqn{f_j - 1 < 10^{-8}} or `tail_max_steps` steps. Extrapolation is
#' judgement-laden: always review the fit and the tail's share of reserves.
#'
#' @param x A `triangle`.
#' @param method `"volume"` (default) or `"simple"`.
#' @param n_periods Use only the latest `n_periods` calendar diagonals of
#'   link ratios. `NULL` (default) uses all.
#' @param weights Optional non-negative weight matrix aligned with
#'   [link_ratios()] (same dimensions). See also [calendar_weights()].
#' @param override Optional named numeric vector of selected factors that
#'   replace the estimates, named by step (e.g. `c("108-120" = 1.005)`).
#' @param tail A positive number, or `"exponential"`.
#' @param tail_max_steps Maximum number of extrapolated steps for the
#'   exponential tail. Default 100.
#' @return A `dev_pattern` object (see [dev_pattern()]) with additional
#'   elements `n_obs` (ratios used per step), `estimated` (factors before
#'   overrides), `weights` (weights actually used) and `method`.
#' @references
#' Mack, T. (1993). Distribution-free calculation of the standard error of
#' chain ladder reserve estimates. *ASTIN Bulletin* 23(2), 213-225.
#' @examples
#' dev_factors(raa)
#' dev_factors(raa, method = "simple", n_periods = 5)
#' dev_factors(raa, tail = "exponential")
#' @export
dev_factors <- function(x, method = c("volume", "simple"), n_periods = NULL,
                        weights = NULL, override = NULL, tail = 1,
                        tail_max_steps = 100) {
  check_triangle(x, "x")
  method <- match.arg(method)
  assert_fittable(x)
  cum <- as.matrix(to_cumulative(x))
  ages <- attr(x, "dev_ages")
  k <- ncol(cum)
  if (k < 2L) abort("The triangle has a single development age; factors cannot be estimated.")
  labels <- step_labels(ages)
  w <- effective_weights(x, n_periods, weights)
  den <- cum[, -k, drop = FALSE]
  num <- cum[, -1L, drop = FALSE]
  use <- !is.na(w) & w > 0

  f <- rep(NA_real_, k - 1L)
  n_obs <- integer(k - 1L)
  zero_ratio <- use & den == 0
  if (method == "simple" && any(zero_ratio)) {
    z <- which(zero_ratio, arr.ind = TRUE)
    warn("Excluded ", nrow(z), " link ratio(s) with zero denominator from the simple ",
         "average at (origin, age): ", fmt_cells(rownames(cum)[z[, 1]], ages[z[, 2]]), ".")
    use <- use & !zero_ratio
  }
  for (j in seq_len(k - 1L)) {
    r <- use[, j]
    n_obs[j] <- sum(r)
    if (!any(r)) next
    if (method == "volume") {
      d <- sum(w[r, j] * den[r, j])
      if (d != 0) f[j] <- sum(w[r, j] * num[r, j]) / d
    } else {
      f[j] <- sum(w[r, j] * num[r, j] / den[r, j]) / sum(w[r, j])
    }
  }
  names(f) <- labels
  estimated <- f
  if (!is.null(override)) {
    bad_override <- !is.numeric(override) || is.null(names(override)) ||
      anyNA(override) || any(override <= 0)
    if (bad_override) {
      abort("`override` must be a named numeric vector of positive factors, ",
            "e.g. c(\"", labels[1], "\" = 1.5).")
    }
    bad <- setdiff(names(override), labels)
    if (length(bad)) {
      abort("Unknown step(s) in `override`: ", paste(bad, collapse = ", "),
            ". Valid steps: ", paste(labels, collapse = ", "), ".")
    }
    f[names(override)] <- override
  }
  if (anyNA(f)) {
    bad <- labels[is.na(f)]
    abort("No usable data to estimate factor(s) ", paste(bad, collapse = ", "),
          " (no ratios after weighting, or zero weighted denominator). ",
          "Supply them via `override` or change `n_periods`/`weights`.")
  }
  tl <- fit_tail(f, tail, tail_max_steps)
  pat <- dev_pattern(ages = ages, factors = unname(f), tail = tl$factor)
  pat$tail_info <- tl
  pat$method <- method
  pat$n_obs <- stats::setNames(n_obs, labels)
  pat$estimated <- estimated
  pat$override <- override
  pat$weights <- w
  pat$n_periods <- n_periods
  pat
}

effective_weights <- function(x, n_periods, weights) {
  obs <- attr(x, "observed")
  pair <- obs[, -1L, drop = FALSE]
  w <- matrix(1, nrow(pair), ncol(pair))
  if (!is.null(weights)) {
    if (!is.matrix(weights) || !is.numeric(weights) || !all(dim(weights) == dim(pair))) {
      abort("`weights` must be a numeric matrix with ", nrow(pair), " rows and ",
            ncol(pair), " columns (aligned with link_ratios()).")
    }
    if (any(weights[pair] < 0, na.rm = TRUE) || anyNA(weights[pair])) {
      abort("`weights` must be non-negative and not NA where link ratios exist.")
    }
    w <- unname(weights)
  }
  if (!is.null(n_periods)) {
    check_number(n_periods, "n_periods", lower = 1)
    if (n_periods != round(n_periods)) abort("`n_periods` must be a whole number.")
    cal <- row(pair) + col(pair)
    latest <- max(cal[pair])
    w <- w * (cal > latest - n_periods)
  }
  w[!pair] <- NA_real_
  dimnames(w) <- list(rownames(x), step_labels(attr(x, "dev_ages")))
  w
}

fit_tail <- function(f, tail, max_steps) {
  if (is.numeric(tail)) {
    check_number(tail, "tail", lower = 0, lower_inclusive = FALSE)
    return(list(factor = tail, method = if (tail == 1) "none" else "fixed"))
  }
  if (!identical(tail, "exponential")) {
    abort("`tail` must be a positive number or \"exponential\".")
  }
  check_number(max_steps, "tail_max_steps", lower = 1)
  j <- seq_along(f)
  keep <- f > 1
  if (sum(keep) < 3L) {
    abort("Exponential tail needs at least 3 factors greater than 1; found ",
          sum(keep), ". Use a fixed `tail` instead.")
  }
  fit <- stats::lm.fit(cbind(1, j[keep]), log(f[keep] - 1))
  a <- fit$coefficients[[1]]
  b <- fit$coefficients[[2]]
  if (!is.finite(b) || b >= 0) {
    abort("Exponential tail fit is not decaying (slope ", signif(b, 4),
          " >= 0); extrapolation would diverge. Use a fixed `tail`.")
  }
  jj <- length(f) + seq_len(max_steps)
  ext <- 1 + exp(a + b * jj)
  ext <- ext[seq_len(max(1L, sum(ext - 1 >= 1e-8)))]
  list(factor = prod(ext), method = "exponential",
       intercept = a, slope = b, used = which(keep), excluded = which(!keep),
       steps = length(ext), extrapolated = ext)
}

#' Development pattern
#'
#' A `dev_pattern` holds age-to-age factors, cumulative development factors
#' (CDFs) to ultimate and the implied percentage developed at each age. It is
#' returned by [dev_factors()] and can be built directly from external
#' (benchmark or selected) factors or CDFs for use in [fit_bf()],
#' [fit_cape_cod()] and [fit_elr()].
#'
#' @param ages Numeric development ages, strictly increasing.
#' @param factors Age-to-age factors, one fewer than `ages`. Supply this or
#'   `cdf`.
#' @param cdf Cumulative development factors to ultimate at each age
#'   (including any tail). Supply this or `factors`.
#' @param tail Tail factor beyond the last age (used with `factors` only).
#' @return A `dev_pattern` list with `ages`, `factors`, `tail`, `cdf`,
#'   `pct_developed`.
#' @examples
#' dev_pattern(ages = c(12, 24, 36), factors = c(1.8, 1.2), tail = 1.05)
#' dev_pattern(ages = c(12, 24, 36), cdf = c(2.268, 1.26, 1.05))
#' @export
dev_pattern <- function(ages, factors = NULL, cdf = NULL, tail = 1) {
  if (!is.numeric(ages) || anyNA(ages) || length(ages) < 1L || any(diff(ages) <= 0)) {
    abort("`ages` must be strictly increasing numbers.")
  }
  k <- length(ages)
  if (is.null(factors) == is.null(cdf)) abort("Supply exactly one of `factors` or `cdf`.")
  if (!is.null(factors)) {
    bad <- !is.numeric(factors) || length(factors) != k - 1L || anyNA(factors) ||
      any(factors <= 0)
    if (bad) {
      abort("`factors` must be ", k - 1L, " positive numbers (one per development step).")
    }
    check_number(tail, "tail", lower = 0, lower_inclusive = FALSE)
    cdf <- rev(cumprod(rev(c(as.numeric(factors), tail))))
  } else {
    if (!is.numeric(cdf) || length(cdf) != k || anyNA(cdf) || any(cdf <= 0)) {
      abort("`cdf` must be ", k, " positive numbers (one per age).")
    }
    tail <- cdf[k]
    factors <- if (k > 1L) cdf[-k] / cdf[-1L] else numeric()
  }
  labels <- step_labels(ages)
  structure(list(
    ages = ages,
    factors = stats::setNames(as.numeric(factors), if (k > 1L) labels else character()),
    tail = tail,
    cdf = stats::setNames(as.numeric(cdf), format_ages(ages)),
    pct_developed = stats::setNames(1 / as.numeric(cdf), format_ages(ages))
  ), class = "dev_pattern")
}

#' @export
print.dev_pattern <- function(x, digits = 4, ...) {
  meth <- ""
  if (!is.null(x$method)) {
    span <- if (is.null(x$n_periods)) "" else paste0(", latest ", x$n_periods, " diagonals")
    meth <- paste0(" (", x$method, "-weighted", span, ")")
  }
  cat("<dev_pattern>", meth, "\n", sep = "")
  tab <- data.frame(
    age = format_ages(x$ages),
    factor = c(x$factors, x$tail),
    cdf = x$cdf,
    pct_developed = 100 * x$pct_developed,
    row.names = NULL
  )
  if (!is.null(x$n_obs)) tab$n_ratios <- c(x$n_obs, NA)
  tab$factor <- round(tab$factor, digits)
  tab$cdf <- round(tab$cdf, digits)
  tab$pct_developed <- round(tab$pct_developed, 1)
  print(tab, row.names = FALSE)
  tail_method <- if (is.null(x$tail_info)) "supplied" else x$tail_info$method
  cat("Last row's factor is the tail (", tail_method, ").\n", sep = "")
  if (!is.null(x$override)) {
    cat("Overridden steps: ", paste(names(x$override), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}
