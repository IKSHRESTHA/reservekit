#' Mack's test for correlation between adjacent development factors
#'
#' Tests the chain-ladder assumption that successive link ratios of an
#' origin are uncorrelated (Mack 1994, Appendix G). For each pair of
#' adjacent development steps with \eqn{n \ge 2} origins having both ratios,
#' Spearman's rank correlation \eqn{T_j} is computed on those origins
#' (ties get average ranks). The weighted mean
#' \eqn{T = \sum (n_j - 1) T_j / \sum (n_j - 1)} has mean 0 and variance
#' \eqn{1 / \sum (n_j - 1)} under the null hypothesis, and is compared with a
#' normal interval at level `ci`.
#'
#' Mack recommends a 50\% interval because the test is meant to detect
#' substantial correlation, and rejecting too rarely is the larger danger.
#'
#' @param x A `triangle`.
#' @param ci Two-sided interval level. Default 0.5.
#' @return A list of class `reserve_test` with `statistic` (T),
#'   `expected`, `variance`, `interval`, `reject` (logical) and `by_step`
#'   (data.frame of \eqn{T_j}, \eqn{n_j}).
#' @references Mack, T. (1994). Measuring the variability of chain ladder
#'   reserve estimates. *CAS Forum*, Spring 1994, 101-182.
#' @examples
#' dev_correlation_test(raa)
#' @export
dev_correlation_test <- function(x, ci = 0.5) {
  check_number(ci, "ci", lower = 0, upper = 1, lower_inclusive = FALSE)
  lr <- link_ratios(x)
  k <- ncol(lr)
  rows <- list()
  for (j in seq_len(k - 1L)) {
    ok <- !is.na(lr[, j]) & !is.na(lr[, j + 1L])
    nj <- sum(ok)
    if (nj < 2L) next
    r <- rank(lr[ok, j])
    s <- rank(lr[ok, j + 1L])
    Tj <- 1 - 6 * sum((r - s)^2) / (nj^3 - nj)
    rows[[length(rows) + 1L]] <- data.frame(steps = paste(colnames(lr)[j], colnames(lr)[j + 1L],
                                                          sep = " vs "),
                                            n = nj, T = Tj, stringsAsFactors = FALSE)
  }
  if (!length(rows)) abort("Not enough link ratios for the correlation test.")
  by <- do.call(rbind, rows)
  wts <- by$n - 1
  stat <- sum(wts * by$T) / sum(wts)
  v <- 1 / sum(wts)
  z <- stats::qnorm(0.5 + ci / 2)
  int <- c(-1, 1) * z * sqrt(v)
  structure(list(name = "Mack development-factor correlation test",
                 statistic = stat, expected = 0, variance = v, interval = int,
                 ci = ci, reject = stat < int[1] || stat > int[2], by_step = by),
            class = "reserve_test")
}

#' Mack's calendar-year effect test
#'
#' Tests for calendar-period (diagonal) effects such as inflation shocks or
#' changes in claims handling (Mack 1994, Appendix H). In each development
#' column, link ratios above the column median are "large" (L) and below are
#' "small" (S); ratios equal to the median are dropped. For each link-ratio
#' diagonal with at least 2 elements, \eqn{Z_j = \min(L_j, S_j)}. Under the
#' null hypothesis, with \eqn{n = L_j + S_j} and
#' \eqn{m = \lfloor (n-1)/2 \rfloor},
#' \deqn{E[Z_j] = \frac{n}{2} - \binom{n-1}{m}\frac{n}{2^n},}
#' \deqn{Var[Z_j] = \frac{n(n-1)}{4} - \binom{n-1}{m}\frac{n(n-1)}{2^n} + E[Z_j] - E[Z_j]^2,}
#' and \eqn{Z = \sum_j Z_j} is compared with a normal interval at level `ci`.
#'
#' @inheritParams dev_correlation_test
#' @param ci Two-sided interval level. Default 0.95.
#' @return A `reserve_test` list (see [dev_correlation_test()]) whose
#'   `by_diagonal` element gives \eqn{L_j, S_j, Z_j, E[Z_j], Var[Z_j]}.
#' @inherit dev_correlation_test references
#' @examples
#' calendar_test(raa)
#' @export
calendar_test <- function(x, ci = 0.95) {
  check_number(ci, "ci", lower = 0, upper = 1, lower_inclusive = FALSE)
  lr <- link_ratios(x)
  cls <- matrix(NA_character_, nrow(lr), ncol(lr))
  for (j in seq_len(ncol(lr))) {
    ok <- !is.na(lr[, j])
    if (!any(ok)) next
    med <- stats::median(lr[ok, j])
    cls[ok & lr[, j] > med, j] <- "L"
    cls[ok & lr[, j] < med, j] <- "S"
  }
  obs_pair <- !is.na(lr)
  diag_id <- row(lr) + col(lr) - 1L
  ids <- sort(unique(diag_id[obs_pair]))
  rows <- list()
  for (d in ids) {
    cells <- diag_id == d & obs_pair
    if (sum(cells) < 2L) next
    L <- sum(cls[cells] == "L", na.rm = TRUE)
    S <- sum(cls[cells] == "S", na.rm = TRUE)
    n <- L + S
    if (n == 0L) {
      e <- 0
      v <- 0
    } else {
      m <- floor((n - 1) / 2)
      e <- n / 2 - choose(n - 1, m) * n / 2^n
      v <- n * (n - 1) / 4 - choose(n - 1, m) * n * (n - 1) / 2^n + e - e^2
    }
    rows[[length(rows) + 1L]] <- data.frame(diagonal = d, L = L, S = S, Z = min(L, S),
                                            expected = e, variance = v)
  }
  if (!length(rows)) abort("Not enough link ratios for the calendar-year test.")
  by <- do.call(rbind, rows)
  stat <- sum(by$Z)
  e <- sum(by$expected)
  v <- sum(by$variance)
  z <- stats::qnorm(0.5 + ci / 2)
  int <- e + c(-1, 1) * z * sqrt(v)
  structure(list(name = "Mack calendar-year effect test", statistic = stat,
                 expected = e, variance = v, interval = int, ci = ci,
                 reject = stat < int[1] || stat > int[2], by_diagonal = by),
            class = "reserve_test")
}

#' @export
print.reserve_test <- function(x, ...) {
  cat("<", x$name, ">\n", sep = "")
  cat(sprintf("  statistic = %.4f, expected = %.4f, sd = %.4f\n",
              x$statistic, x$expected, sqrt(x$variance)))
  cat(sprintf("  %g%% interval: [%.4f, %.4f]\n", 100 * x$ci, x$interval[1], x$interval[2]))
  cat(if (x$reject) "  Result: outside interval - assumption questionable; review.\n"
      else "  Result: inside interval - no evidence against the assumption at this level.\n")
  invisible(x)
}

#' Diagnostics for a reserving fit
#'
#' Collects the standard chain-ladder diagnostics for a fitted model:
#' individual link ratios, the selected factors, Mack's correlation and
#' calendar-year tests and, for Mack fits, standardised residuals
#' \eqn{r_{ij} = (F_{ij} - \hat f_j)\sqrt{w_{ij} C_{ij}} / \hat\sigma_j}.
#' Under Mack's assumptions residuals should show no trend against origin,
#' development or calendar period.
#'
#' @param object A `reserve_fit`.
#' @param ... Unused.
#' @return A list of class `reserve_diagnostics`.
#' @examples
#' diagnostics(fit_mack(raa))
#' @export
diagnostics <- function(object, ...) UseMethod("diagnostics")

#' @rdname diagnostics
#' @export
diagnostics.reserve_fit <- function(object, ...) {
  tri <- object$triangle
  out <- list(link_ratios = if (ncol(tri) > 1L) link_ratios(tri) else NULL,
              factors = if (!is.null(object$pattern)) object$pattern$factors else NULL,
              validation = validate_triangle(tri))
  out$correlation <- tryCatch(dev_correlation_test(tri), error = function(e) NULL)
  out$calendar <- tryCatch(calendar_test(tri), error = function(e) NULL)
  if (inherits(object, "reserve_mack")) out$residuals <- mack_residuals(object)
  class(out) <- "reserve_diagnostics"
  out
}

mack_residuals <- function(fit) {
  C <- as.matrix(fit$triangle)
  K <- ncol(C)
  lr <- link_ratios(fit$triangle)
  w <- fit$pattern$weights
  f <- unname(fit$pattern$factors)
  sig <- unname(fit$sigma)
  den <- C[, -K, drop = FALSE]
  r <- sweep(lr, 2L, f) * sqrt(w * den)
  r <- sweep(r, 2L, ifelse(sig > 0, sig, NA_real_), "/")
  r[is.na(w) | w <= 0] <- NA_real_
  cal <- row(r) + col(r)
  data.frame(origin = rownames(C)[row(r)][!is.na(r)],
             step = colnames(lr)[col(r)][!is.na(r)],
             calendar = cal[!is.na(r)],
             residual = r[!is.na(r)], stringsAsFactors = FALSE)
}

#' @export
print.reserve_diagnostics <- function(x, ...) {
  cat("<reserve_diagnostics>\n\n")
  print(x$validation)
  for (test in list(x$correlation, x$calendar)) {
    if (!is.null(test)) {
      cat("\n")
      print(test)
    }
  }
  if (!is.null(x$residuals)) {
    cat("\nStandardised residuals (Mack):\n")
    print(summary(x$residuals$residual))
  }
  invisible(x)
}
