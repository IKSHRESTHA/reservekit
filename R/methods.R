#' Reserving fit objects
#'
#' Every `fit_*()` function returns a list of class
#' `c("reserve_<method>", "reserve_fit")` with the same core elements:
#'
#' \describe{
#'   \item{`method`, `label`, `call`}{Method id, display name, matched call.}
#'   \item{`triangle`}{The cumulative triangle that was fitted.}
#'   \item{`pattern`}{The `dev_pattern` used (may be `NULL` for [fit_elr()]).}
#'   \item{`results`}{data.frame with one row per origin: `origin`,
#'     `dev_age` (latest), `latest`, `cdf`, `pct_developed`, `ultimate`,
#'     `reserve`, plus method-specific columns.}
#'   \item{`assumptions`}{Named list of methodological choices.}
#'   \item{`warnings`}{Material warnings raised while fitting.}
#' }
#'
#' `reserve` is ultimate minus the latest diagonal: total unpaid for paid
#' data, IBNR (including IBNER) for incurred data.
#'
#' Methods: `print()`, `summary()`, `predict()` (completed triangle),
#' `as.data.frame()` (the `results` table), `coef()` (age-to-age factors and
#' tail), `plot()`, [diagnostics()], and [compare_methods()] across fits.
#'
#' @name reserve_fit
NULL

#' @export
print.reserve_fit <- function(x, digits = 0, ...) {
  cat("<reserve_fit> ", x$label, " | ", attr(x$triangle, "measure"), " | ",
      nrow(x$results), " origin periods\n", sep = "")
  cols <- intersect(c("origin", "dev_age", "latest", "prior_ultimate", "cdf",
                      "ultimate", "reserve", "se", "cv"), names(x$results))
  tab <- x$results[, cols, drop = FALSE]
  tot <- totals(x)
  print(format_results(tab, digits), row.names = FALSE, right = TRUE)
  cat(sprintf("\nTotal %s: %s", measure_labels(attr(x$triangle, "measure")),
              fmt_num(tot$reserve, digits)))
  if (!is.null(tot$se)) {
    cat(sprintf("  (prediction s.e. %s, CV %.1f%%)", fmt_num(tot$se, digits), 100 * tot$cv))
  }
  cat("\n")
  if (length(x$warnings)) cat(paste0("! ", x$warnings, collapse = "\n"), "\n", sep = "")
  invisible(x)
}

fmt_num <- function(x, digits = 0) formatC(x, format = "f", digits = digits, big.mark = ",")

format_results <- function(tab, digits) {
  for (nm in names(tab)) {
    v <- tab[[nm]]
    if (!is.numeric(v) || nm == "dev_age") next
    tab[[nm]] <- if (nm %in% c("cdf", "elr")) {
      formatC(v, format = "f", digits = 4)
    } else if (nm %in% c("cv", "pct_developed")) {
      ifelse(is.na(v), "", sprintf("%.1f%%", 100 * v))
    } else {
      ifelse(is.na(v), "", fmt_num(v, digits))
    }
  }
  tab
}

totals <- function(x) {
  r <- x$results
  out <- list(latest = sum(r$latest), ultimate = sum(r$ultimate), reserve = sum(r$reserve))
  if (!is.null(x$total)) {
    out$se <- x$total$se
    out$cv <- x$total$cv
    out$process_se <- x$total$process_se
    out$parameter_se <- x$total$parameter_se
  }
  out
}

#' Summarise a reserving fit
#'
#' @param object A `reserve_fit`.
#' @param ... Unused.
#' @return A `summary.reserve_fit` list with `method`, `measure`,
#'   `totals` (named list), `results`, `factors`, `assumptions`, `warnings`.
#' @examples
#' summary(fit_mack(raa))
#' @export
summary.reserve_fit <- function(object, ...) {
  structure(list(
    method = object$label, measure = attr(object$triangle, "measure"),
    totals = totals(object), results = object$results,
    factors = if (!is.null(object$pattern)) stats::coef(object) else NULL,
    assumptions = object$assumptions, warnings = object$warnings,
    elr = object$elr
  ), class = "summary.reserve_fit")
}

#' @export
print.summary.reserve_fit <- function(x, ...) {
  cat("Method:  ", x$method, "\n", sep = "")
  cat("Measure: ", x$measure, "\n\n", sep = "")
  t <- x$totals
  lines <- c(Latest = t$latest, Ultimate = t$ultimate, Reserve = t$reserve)
  for (nm in names(lines)) cat(sprintf("%-22s %15s\n", nm, fmt_num(lines[[nm]])))
  if (!is.null(t$se)) {
    cat(sprintf("%-22s %15s\n", "Process s.e.", fmt_num(t$process_se)))
    cat(sprintf("%-22s %15s\n", "Parameter s.e.", fmt_num(t$parameter_se)))
    cat(sprintf("%-22s %15s\n", "Prediction s.e.", fmt_num(t$se)))
    cat(sprintf("%-22s %14.1f%%\n", "CV of reserve", 100 * t$cv))
  }
  if (!is.null(x$elr)) cat(sprintf("%-22s %15.4f\n", "Cape Cod ELR", x$elr))
  cat("\nReserve is ", tolower(measure_labels(x$measure)), ".\n", sep = "")
  if (!is.null(x$factors)) {
    cat("\nSelected age-to-age factors (last = tail):\n")
    print(round(x$factors, 4))
  }
  cat("\nAssumptions:\n")
  a <- x$assumptions
  for (nm in names(a)) {
    v <- a[[nm]]
    v <- if (is.null(v)) {
      "NULL"
    } else if (is.list(v)) {
      paste0(v$method %||% "", " ", signif(v$factor %||% NA, 6))
    } else {
      paste(format(v), collapse = ", ")
    }
    cat("  ", nm, ": ", v, "\n", sep = "")
  }
  if (length(x$warnings)) {
    cat("\nWarnings:\n", paste0("  ! ", x$warnings, collapse = "\n"), "\n", sep = "")
  }
  cat("\nResults support, and do not replace, professional actuarial judgement.\n")
  invisible(x)
}

#' @export
as.data.frame.reserve_fit <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$results
  out$method <- x$method
  out
}

#' @export
coef.reserve_fit <- function(object, ...) {
  if (is.null(object$pattern)) return(NULL)
  c(object$pattern$factors, tail = object$pattern$tail)
}

#' Projected (completed) triangle from a reserving fit
#'
#' Fills the future part of the triangle consistently with the method:
#' future cumulative values are
#' \eqn{C_{ij} = C_{i,a_i} + B_i (p_j - p_{a_i})} where \eqn{p} is the
#' percentage developed and \eqn{B_i} is the method's emergence base
#' (chain-ladder ultimate for chain ladder and Mack, prior ultimate for BF
#' and Cape Cod, reserve / (1 - p) for ELR). For chain ladder this equals
#' multiplying by the age-to-age factors. When the ultimate differs from the
#' value at the last age (tail factor, ELR without full pattern) a final
#' `"ult"` column is added. The sum of projected increments always equals
#' `ultimate - latest`.
#'
#' @param object A `reserve_fit`.
#' @param type `"cumulative"` (default) or `"incremental"`.
#' @param ... Unused.
#' @return A numeric matrix of class `projected_triangle` with attribute
#'   `observed` marking actual (non-projected) cells.
#' @examples
#' predict(fit_chain_ladder(raa))
#' predict(fit_chain_ladder(raa, tail = 1.05), type = "incremental")
#' @export
predict.reserve_fit <- function(object, type = c("cumulative", "incremental"), ...) {
  type <- match.arg(type)
  cum <- as.matrix(object$triangle)
  obs <- attr(object$triangle, "observed")
  n <- nrow(cum)
  K <- ncol(cum)
  res <- object$results
  lat <- latest_diagonal(object$triangle)
  out <- cum
  if (is.null(object$pattern)) {
    out[!obs] <- rep(lat$value, K)[!obs]
  } else {
    p <- unname(object$pattern$pct_developed)
    B <- object$emergence
    for (i in seq_len(n)) {
      a <- lat$dev_index[i]
      if (a < K) {
        jj <- (a + 1L):K
        out[i, jj] <- if (is.na(B[i])) lat$value[i] else lat$value[i] + B[i] * (p[jj] - p[a])
      }
    }
  }
  needs_ult <- any(abs(res$ultimate - out[, K]) > 1e-8 * pmax(1, abs(res$ultimate)))
  if (needs_ult) {
    out <- cbind(out, ult = res$ultimate)
    obs <- cbind(obs, FALSE)
  }
  if (type == "incremental" && ncol(out) > 1L) {
    out[, -1L] <- out[, -1L, drop = FALSE] - out[, -ncol(out), drop = FALSE]
  }
  structure(out, class = "projected_triangle", type = type, observed = obs,
            method = object$label)
}

#' @export
print.projected_triangle <- function(x, digits = 0, ...) {
  cat(sprintf("<projected_triangle> %s, %s (projected cells marked *)\n",
              attr(x, "type"), attr(x, "method")))
  m <- unclass(x)
  obs <- attr(x, "observed")
  attributes(m) <- list(dim = dim(x), dimnames = dimnames(x))
  txt <- formatC(m, format = "f", digits = digits, big.mark = ",")
  txt[!obs] <- paste0(txt[!obs], "*")
  txt[obs] <- paste0(txt[obs], " ")
  dim(txt) <- dim(m)
  dimnames(txt) <- dimnames(m)
  print(txt, quote = FALSE, right = TRUE)
  invisible(x)
}

#' @export
as.matrix.projected_triangle <- function(x, ...) {
  m <- unclass(x)
  attributes(m) <- list(dim = dim(x), dimnames = dimnames(x))
  m
}

#' Compare reserving methods
#'
#' Puts the ultimates and reserves of several fits on the same triangle side
#' by side. It does not choose a method: selection remains an actuarial
#' judgement informed by diagnostics, data and business knowledge.
#'
#' @param ... Named `reserve_fit` objects (names become column labels), or a
#'   single named list of fits.
#' @param value `"reserve"` (default) or `"ultimate"`.
#' @return A data.frame with one row per origin plus a `Total` row. The
#'   `latest` column is included when all fits share the same triangle.
#'   Fits on different triangles with the same origins (e.g. paid and
#'   incurred) can be compared on `value = "ultimate"` only.
#' @examples
#' prem <- seq(30000, 48000, length.out = 10)
#' compare_methods(
#'   chain_ladder = fit_chain_ladder(raa),
#'   bf = fit_bf(raa, exposure = prem, elr = 0.6),
#'   cape_cod = fit_cape_cod(raa, exposure = prem)
#' )
#' @export
compare_methods <- function(..., value = c("reserve", "ultimate")) {
  value <- match.arg(value)
  fits <- list(...)
  if (length(fits) == 1L && is.list(fits[[1]]) && !inherits(fits[[1]], "reserve_fit")) {
    fits <- fits[[1]]
  }
  if (!length(fits)) abort("Supply at least one fit.")
  if (!all(vapply(fits, inherits, logical(1), "reserve_fit"))) {
    abort("All inputs to compare_methods() must be `reserve_fit` objects.")
  }
  nms <- names(fits)
  if (is.null(nms) || any(nms == "")) {
    nms <- make.unique(vapply(fits, function(f) f$method, character(1)))
  }
  org <- fits[[1]]$results$origin
  lat <- fits[[1]]$results$latest
  same_latest <- TRUE
  for (f in fits[-1]) {
    if (!identical(f$results$origin, org)) {
      abort("All fits must have the same origin periods.")
    }
    if (!isTRUE(all.equal(f$results$latest, lat))) same_latest <- FALSE
  }
  if (!same_latest && value == "reserve") {
    abort("Fits are on different triangles (e.g. paid and incurred), so their ",
          "reserves are on different bases. Compare ultimates with ",
          "`value = \"ultimate\"`.")
  }
  out <- data.frame(origin = org, stringsAsFactors = FALSE)
  if (same_latest) out$latest <- lat
  for (i in seq_along(fits)) out[[nms[i]]] <- fits[[i]]$results[[value]]
  tot <- as.data.frame(lapply(out[-1], sum))
  tot <- cbind(origin = "Total", tot, stringsAsFactors = FALSE)
  out <- rbind(out, tot)
  attr(out, "value") <- value
  out
}
