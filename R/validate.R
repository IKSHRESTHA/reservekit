#' Validate a triangle and report data-quality issues
#'
#' Runs structural and data-quality checks and returns every finding with a
#' severity:
#'
#' * `error`: fitting functions will refuse the triangle (e.g. genuine
#'   missing values, a development column whose denominator sums to zero).
#' * `warning`: results can be computed but need actuarial review (negative
#'   paid increments, zero cumulative values that make individual link ratios
#'   undefined, very few origin periods, zero latest values).
#' * `note`: legitimate features worth knowing about (negative incurred
#'   development from case-reserve releases, cells filled with zero, link
#'   ratios far from their column's typical value).
#'
#' The outlier screen flags an individual link ratio when its log differs
#' from the column median log by more than `outlier_mad` scaled median
#' absolute deviations (columns with at least 4 ratios only). It is a screen
#' for review, not a rule for exclusion.
#'
#' @param x A `triangle`.
#' @param outlier_mad Threshold for the link-ratio outlier screen. Default 3.
#' @return A data.frame of class `triangle_validation` with columns
#'   `severity`, `check`, `origin`, `dev_age`, `message`. Zero rows means no
#'   issues were found.
#' @examples
#' validate_triangle(raa)
#' @export
validate_triangle <- function(x, outlier_mad = 3) {
  check_triangle(x, "x")
  check_number(outlier_mad, "outlier_mad", lower = 0, lower_inclusive = FALSE)
  m <- as.matrix(x)
  obs <- attr(x, "observed")
  ages <- attr(x, "dev_ages")
  org <- rownames(m)
  measure <- attr(x, "measure")
  cum <- as.matrix(to_cumulative(x))
  inc <- as.matrix(to_incremental(x))
  issues <- list()
  add <- function(sev, check, r, cidx, msg) {
    if (length(r) == 0L) return()
    issues[[length(issues) + 1L]] <<- data.frame(
      severity = sev, check = check, origin = org[r], dev_age = ages[cidx],
      message = msg, stringsAsFactors = FALSE
    )
  }
  cell_idx <- function(mask) which(mask, arr.ind = TRUE)

  miss <- cell_idx(is.na(m) & obs)
  add("error", "genuine_missing", miss[, 1], miss[, 2],
      "Missing value inside the observed region.")

  fl <- cell_idx(attr(x, "filled"))
  add("note", "filled_zero", fl[, 1], fl[, 2],
      "No transactions for this cell; set to 0.")

  neg_c <- cell_idx(!is.na(cum) & obs & cum < 0)
  add("warning", "negative_cumulative", neg_c[, 1], neg_c[, 2],
      "Negative cumulative value.")

  neg_i <- cell_idx(!is.na(inc) & obs & inc < 0 & col(inc) > 1L)
  sev <- if (measure %in% c("incurred", "other")) "note" else "warning"
  msg <- if (measure == "incurred") {
    "Negative incremental incurred (case reserve release or recovery)."
  } else {
    "Negative incremental value (recovery, salvage/subrogation or correction?)."
  }
  add(sev, "negative_increment", neg_i[, 1], neg_i[, 2], msg)

  n <- nrow(cum)
  k <- ncol(cum)
  if (k > 1L) {
    has_pair <- obs[, -1L, drop = FALSE]
    den <- cum[, -k, drop = FALSE]
    num <- cum[, -1L, drop = FALSE]
    zero_den <- cell_idx(has_pair & !is.na(den) & den == 0)
    add("warning", "zero_denominator", zero_den[, 1], zero_den[, 2],
        "Cumulative value is 0 where a link ratio is needed; the individual ratio is undefined.")
    for (j in seq_len(k - 1L)) {
      rows <- which(has_pair[, j])
      if (length(rows) && sum(den[rows, j], na.rm = TRUE) == 0) {
        add("error", "zero_column", rows[1], j,
            sprintf(paste("Cumulative values at age %s sum to 0;",
                          "the %s-%s factor cannot be volume-weighted."),
                    format_ages(ages[j]), format_ages(ages[j]), format_ages(ages[j + 1L])))
      }
      ok <- rows[!is.na(den[rows, j]) & !is.na(num[rows, j]) &
                   den[rows, j] > 0 & num[rows, j] > 0]
      if (length(ok) >= 4L) {
        lr <- log(num[ok, j] / den[ok, j])
        s <- stats::mad(lr)
        if (s > 0) {
          out <- ok[abs(lr - stats::median(lr)) > outlier_mad * s]
          add("note", "link_ratio_outlier", out, rep(j, length(out)),
              sprintf("Link ratio far from column median (screen threshold %g MAD).",
                      outlier_mad))
        }
      }
    }
  }

  lat <- latest_diagonal(to_cumulative(x))
  z <- which(!is.na(lat$value) & lat$value == 0)
  add("warning", "zero_latest", z, lat$dev_index[z],
      "Latest cumulative value is 0; chain ladder ultimate will be 0 (consider BF/Cape Cod).")

  if (n < 3L) {
    add("warning", "few_origins", 1L, 1L,
        sprintf("Only %d origin period(s); development factors are poorly estimated.", n))
  }
  if (k == 1L) {
    add("note", "single_development", 1L, 1L,
        "Only one development age; no development information.")
  }

  out <- if (length(issues)) do.call(rbind, issues) else
    data.frame(severity = character(), check = character(), origin = character(),
               dev_age = numeric(), message = character(), stringsAsFactors = FALSE)
  out$severity <- factor(out$severity, levels = c("error", "warning", "note"))
  out <- out[order(out$severity), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("triangle_validation", "data.frame")
  out
}

#' @export
print.triangle_validation <- function(x, ...) {
  if (nrow(x) == 0L) {
    cat("<triangle_validation> no issues found.\n")
    return(invisible(x))
  }
  tab <- table(x$severity)
  cat(sprintf("<triangle_validation> %d error(s), %d warning(s), %d note(s)\n",
              tab[["error"]], tab[["warning"]], tab[["note"]]))
  print(as.data.frame(unclass(x), stringsAsFactors = FALSE), right = FALSE, row.names = FALSE)
  invisible(x)
}

# Used by fitting functions: stop on fatal issues.
assert_fittable <- function(x) {
  obs <- attr(x, "observed")
  m <- as.matrix(x)
  bad <- which(is.na(m) & obs, arr.ind = TRUE)
  if (nrow(bad)) {
    abort("The triangle has ", nrow(bad), " genuine missing value(s) in the observed ",
          "region at (origin, age): ",
          fmt_cells(rownames(m)[bad[, 1]], attr(x, "dev_ages")[bad[, 2]]),
          ". Fill or correct them explicitly; see validate_triangle().")
  }
  invisible(x)
}
