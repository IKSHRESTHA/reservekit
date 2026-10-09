#' Create a loss development triangle
#'
#' `as_triangle()` builds a `triangle` object from either long-format data
#' (one row per origin period x development age, or one row per claim
#' transaction) or from a numeric matrix that is already in triangle layout.
#'
#' @section Conventions:
#' * Rows are origin periods (accident, underwriting or report periods);
#'   columns are development ages. Origin and development periods are assumed
#'   to have the same length (for example annual x annual).
#' * `type` must be stated. A cumulative triangle is never interpreted as
#'   incremental, or vice versa.
#' * The **observed region** is every cell whose calendar index
#'   `origin index + development index - 1` is on or before the latest
#'   valuation diagonal present in the data. An `NA` inside the observed
#'   region is a *genuine missing value*; an `NA` beyond it is a *structural*
#'   future value. Fitting functions stop on genuine missing values.
#'
#' @section Long data rules:
#' * Development ages must be numeric, finite and lie on a regular grid
#'   (e.g. 12, 24, 36). Ages inside the grid with no data at all are an
#'   error unless `dev_step` is given, in which case they are added as columns.
#' * Numeric origins must lie on a regular grid; empty origin periods are an
#'   error unless `origin_step` is given, in which case they are added as rows.
#'   Character origins are sorted; factor origins use level order; `Date`
#'   origins are sorted and assumed consecutive.
#' * Several rows for the same origin and age (claim-level data) are summed
#'   when `aggregate = "sum"`. The default (`NULL`) sums incremental data and
#'   refuses duplicated cumulative data, because summing per-claim cumulative
#'   values is only correct when every claim has a row at every age.
#' * For incremental data, an observed-region cell with *no rows at all* is
#'   set to 0 (no transactions) and recorded in `attr(x, "filled")`. For
#'   cumulative data such a cell is a genuine missing value (`NA`).
#'   `NA` values present in `value` are always kept as `NA`.
#'
#' @param data A `data.frame` in long format, or a numeric matrix in triangle
#'   layout (rows = origins, columns = development ages).
#' @param origin,development,value Column names (strings) in `data` for the
#'   origin period, the development age, and the amount. Ignored for matrices.
#' @param type `"cumulative"` or `"incremental"`. Required.
#' @param measure What the values represent: `"paid"`, `"incurred"`,
#'   `"reported_count"`, `"closed_count"` or `"other"` (default). Used for
#'   labels, e.g. reserves on paid data are "unpaid" and on incurred data are
#'   "IBNR".
#' @param aggregate How to combine duplicated origin/age rows: `NULL`
#'   (default, see Long data rules), `"sum"`, or `"error"`.
#' @param dev_step,origin_step Optional grid steps for development ages and
#'   numeric origins. By default the step is the smallest gap in the data, and
#'   a grid value with no data at all is an error, because it usually signals
#'   a data problem. Give the step explicitly to accept empty periods (they
#'   become zero cells for incremental data, missing cells for cumulative).
#'   Inference cannot detect an empty period at the smallest gap (ages 12 and
#'   36 alone imply a step of 24), so state the step whenever it is known.
#' @param origin_labels,dev_ages For matrices: origin labels and numeric
#'   development ages. Default to the row and column names, or `1:n`.
#' @param ... Unused; for future methods.
#'
#' @return An object of class `triangle`: a numeric matrix with attributes
#'   `type`, `measure`, `observed` (logical matrix) and `filled` (logical
#'   matrix of cells set to 0 under the incremental no-rows rule).
#'
#' @examples
#' tri <- as_triangle(claims_synthetic, origin = "accident_year",
#'                    development = "dev_age", value = "paid",
#'                    type = "incremental", measure = "paid")
#' tri
#' to_cumulative(tri)
#'
#' m <- matrix(c(100, 150, 175,
#'               110, 160,  NA,
#'               120,  NA,  NA), 3, byrow = TRUE)
#' as_triangle(m, type = "cumulative", origin_labels = 2021:2023,
#'             dev_ages = c(12, 24, 36))
#' @export
as_triangle <- function(data, ...) {
  UseMethod("as_triangle")
}

#' @rdname as_triangle
#' @export
as_triangle.data.frame <- function(data, origin, development, value,
                                   type = c("cumulative", "incremental"),
                                   measure = "other", aggregate = NULL,
                                   dev_step = NULL, origin_step = NULL, ...) {
  if (missing(type)) {
    abort("`type` must be supplied: \"cumulative\" or \"incremental\". ",
          "reservekit never guesses whether values are cumulative.")
  }
  type <- match.arg(type)
  measure <- check_measure(measure)
  for (arg in c("origin", "development", "value")) {
    nm <- get(arg)
    if (!is.character(nm) || length(nm) != 1L) {
      abort("`", arg, "` must be a single column name (string).")
    }
    if (!nm %in% names(data)) {
      abort("Column `", nm, "` (given as `", arg, "`) not found in `data`. ",
            "Available columns: ", paste(names(data), collapse = ", "), ".")
    }
  }
  if (nrow(data) == 0L) abort("`data` has no rows.")
  o <- data[[origin]]
  d <- data[[development]]
  v <- data[[value]]
  if (!is.numeric(v)) abort("Value column `", value, "` must be numeric.")
  if (any(is.infinite(v))) abort("Value column `", value, "` contains infinite values.")
  if (anyNA(o)) abort("Origin column `", origin, "` contains missing values.")
  if (!is.numeric(d) || anyNA(d) || any(!is.finite(d))) {
    abort("Development column `", development,
          "` must be numeric with no missing or infinite values.")
  }

  dev_ages <- regular_grid(d, "development", dev_step)
  orig <- origin_levels(o, "origin", origin_step)
  oi <- orig$index
  di <- if (length(dev_ages) > 1L) {
    as.integer(round((d - dev_ages[1]) / (dev_ages[2] - dev_ages[1]))) + 1L
  } else {
    rep(1L, length(d))
  }

  key <- (di - 1L) * length(orig$labels) + oi
  dup <- anyDuplicated(key) > 0L
  if (is.null(aggregate)) aggregate <- if (type == "incremental") "sum" else "error"
  aggregate <- match.arg(aggregate, c("sum", "error"))
  if (dup && aggregate == "error") {
    k <- key[duplicated(key)][1]
    abort("`data` has more than one row for some origin/development cells ",
          "(first: origin ", orig$labels[oi[match(k, key)]], ", age ",
          dev_ages[di[match(k, key)]], "). For cumulative data, set ",
          "`aggregate = \"sum\"` only if every claim has a row at every age.")
  }

  n <- length(orig$labels)
  m <- length(dev_ages)
  present <- matrix(FALSE, n, m)
  present[key] <- TRUE
  vals <- rowsum(v, key, reorder = TRUE)
  mat <- matrix(NA_real_, n, m)
  mat[as.integer(rownames(vals))] <- vals[, 1]

  cal <- row(mat) + col(mat) - 1L
  valuation <- max(cal[present])
  observed <- cal <= valuation
  filled <- matrix(FALSE, n, m)
  if (type == "incremental") {
    filled <- observed & !present
    mat[filled] <- 0
  }
  dimnames(mat) <- list(orig$labels, format_ages(dev_ages))
  new_triangle(mat, type = type, measure = measure, dev_ages = dev_ages,
               observed = observed, filled = filled)
}

#' @rdname as_triangle
#' @export
as_triangle.matrix <- function(data, type = c("cumulative", "incremental"),
                               measure = "other", origin_labels = NULL,
                               dev_ages = NULL, ...) {
  if (missing(type)) {
    abort("`type` must be supplied: \"cumulative\" or \"incremental\". ",
          "reservekit never guesses whether values are cumulative.")
  }
  type <- match.arg(type)
  measure <- check_measure(measure)
  if (!is.numeric(data)) abort("Triangle matrix must be numeric.")
  if (any(is.infinite(data))) abort("Triangle matrix contains infinite values.")
  n <- nrow(data)
  m <- ncol(data)
  if (n < 1L || m < 1L) abort("Triangle matrix must have at least one row and column.")
  if (is.null(origin_labels)) {
    origin_labels <- rownames(data) %||% as.character(seq_len(n))
  }
  if (length(origin_labels) != n) {
    abort("`origin_labels` must have one label per row (", n, ").")
  }
  if (anyDuplicated(origin_labels)) abort("`origin_labels` must be unique.")
  if (is.null(dev_ages)) {
    cn <- colnames(data)
    dev_ages <- if (!is.null(cn) && !anyNA(suppressWarnings(as.numeric(cn)))) {
      as.numeric(cn)
    } else {
      seq_len(m)
    }
  }
  bad_ages <- !is.numeric(dev_ages) || length(dev_ages) != m || anyNA(dev_ages) ||
    any(diff(dev_ages) <= 0)
  if (bad_ages) {
    abort("`dev_ages` must be strictly increasing numbers, one per column (", m, ").")
  }
  mat <- matrix(as.numeric(data), n, m)
  if (all(is.na(mat))) abort("Triangle matrix contains no observed values.")
  cal <- row(mat) + col(mat) - 1L
  valuation <- max(cal[!is.na(mat)])
  observed <- cal <= valuation
  dimnames(mat) <- list(as.character(origin_labels), format_ages(dev_ages))
  new_triangle(mat, type = type, measure = measure, dev_ages = dev_ages,
               observed = observed, filled = matrix(FALSE, n, m))
}

#' @export
as_triangle.triangle <- function(data, ...) data

#' @export
as_triangle.default <- function(data, ...) {
  abort("Cannot create a triangle from an object of class <", class(data)[1],
        ">. Supply a data.frame in long format or a numeric matrix.")
}

new_triangle <- function(mat, type, measure, dev_ages, observed, filled) {
  empty_rows <- which(rowSums(observed) == 0L)
  if (length(empty_rows)) {
    abort("Origin period(s) ", paste(rownames(mat)[empty_rows], collapse = ", "),
          " have no observed development (they lie beyond the latest valuation). ",
          "Remove them before building the triangle.")
  }
  structure(mat, class = "triangle", type = type, measure = measure,
            dev_ages = dev_ages, observed = observed, filled = filled)
}

check_measure <- function(measure) {
  ok <- c("paid", "incurred", "reported_count", "closed_count", "other")
  if (!is.character(measure) || length(measure) != 1L || !measure %in% ok) {
    abort("`measure` must be one of: ", paste0("\"", ok, "\"", collapse = ", "), ".")
  }
  measure
}

regular_grid <- function(x, arg, step = NULL) {
  u <- sort(unique(x))
  if (!is.null(step)) {
    check_number(step, paste0(arg, " step"), lower = 0, lower_inclusive = FALSE)
  } else if (length(u) == 1L) {
    return(u)
  }
  inferred <- is.null(step)
  if (inferred) step <- min(diff(u))
  k <- (u - u[1]) / step
  if (any(abs(k - round(k)) > 1e-8)) {
    abort("Values in `", arg, "` are not on a regular grid with step ", step, " (found ",
          paste(utils::head(u, 6), collapse = ", "), if (length(u) > 6) ", ...",
          "). Mixed period lengths are not supported in this version.")
  }
  grid <- u[1] + step * (0:round(max(k)))
  if (inferred && length(grid) > length(u)) {
    gaps <- grid[!(0:round(max(k))) %in% round(k)]
    abort("Values in `", arg, "` do not form a complete regular grid: inferred step ",
          step, " implies value(s) ", paste(utils::head(gaps, 6), collapse = ", "),
          " with no data. If periods with no data are genuine, pass the step ",
          "explicitly (`", if (arg == "origin") "origin_step" else "dev_step", "`).")
  }
  grid
}

origin_levels <- function(o, arg, step = NULL) {
  if (is.factor(o)) {
    lv <- levels(droplevels(o))
    return(list(labels = lv, index = match(as.character(o), lv)))
  }
  if (!is.null(step) && !is.numeric(o)) {
    abort("`origin_step` can only be used with numeric origins.")
  }
  if (inherits(o, "Date")) {
    u <- sort(unique(o))
    return(list(labels = as.character(u), index = match(o, u)))
  }
  if (is.numeric(o)) {
    g <- regular_grid(o, "origin", step)
    idx <- round((o - g[1]) / (if (length(g) > 1L) g[2] - g[1] else 1)) + 1L
    return(list(labels = format(g, trim = TRUE, scientific = FALSE),
                index = as.integer(idx)))
  }
  u <- sort(unique(as.character(o)))
  list(labels = u, index = match(as.character(o), u))
}

format_ages <- function(x) format(x, trim = TRUE, scientific = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Test whether an object is a triangle
#' @param x Any R object.
#' @return `TRUE` or `FALSE`.
#' @export
is_triangle <- function(x) inherits(x, "triangle")

#' Triangle accessors
#'
#' Small helpers to query a triangle's metadata.
#'
#' @param x A `triangle`.
#' @return `triangle_type()`: `"cumulative"` or `"incremental"`.
#'   `dev_ages()`: numeric development ages. `origins()`: origin labels.
#'   `observed_mask()`: logical matrix, `TRUE` in the observed region.
#' @examples
#' triangle_type(raa)
#' dev_ages(raa)
#' @export
triangle_type <- function(x) {
  check_triangle(x, "x")
  attr(x, "type")
}

#' @rdname triangle_type
#' @export
dev_ages <- function(x) {
  check_triangle(x, "x")
  attr(x, "dev_ages")
}

#' @rdname triangle_type
#' @export
origins <- function(x) {
  check_triangle(x, "x")
  rownames(x)
}

#' @rdname triangle_type
#' @export
observed_mask <- function(x) {
  check_triangle(x, "x")
  attr(x, "observed")
}

#' @export
as.matrix.triangle <- function(x, ...) {
  m <- unclass(x)
  attributes(m) <- list(dim = dim(x), dimnames = dimnames(x))
  m
}

#' Convert a triangle to long format
#'
#' @param x A `triangle`.
#' @param row.names,optional Ignored.
#' @param future Include structural future cells (as `NA`)? Default `FALSE`.
#' @param ... Unused.
#' @return A data.frame with columns `origin`, `dev_age`, `value`,
#'   `observed` and `filled`.
#' @examples
#' head(as.data.frame(raa))
#' @export
as.data.frame.triangle <- function(x, row.names = NULL, optional = FALSE,
                                   future = FALSE, ...) {
  m <- as.matrix(x)
  obs <- attr(x, "observed")
  out <- data.frame(
    origin = rownames(m)[row(m)],
    dev_age = attr(x, "dev_ages")[col(m)],
    value = as.vector(m),
    observed = as.vector(obs),
    filled = as.vector(attr(x, "filled")),
    stringsAsFactors = FALSE
  )
  if (!future) out <- out[out$observed, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' @export
print.triangle <- function(x, digits = 0, ...) {
  m <- as.matrix(x)
  obs <- attr(x, "observed")
  n_missing <- sum(is.na(m) & obs)
  cat(sprintf("<triangle> %s %s: %d origin x %d development periods\n",
              attr(x, "type"), attr(x, "measure"), nrow(m), ncol(m)))
  txt <- formatC(m, format = "f", digits = digits, big.mark = ",")
  txt[is.na(m) & !obs] <- ""
  txt[is.na(m) & obs] <- "NA"
  dim(txt) <- dim(m)
  dimnames(txt) <- dimnames(m)
  print(txt, quote = FALSE, right = TRUE)
  if (n_missing > 0L) {
    cat(sprintf("! %d genuine missing value(s) in the observed region (shown as NA).\n",
                n_missing))
  }
  nf <- sum(attr(x, "filled"))
  if (nf > 0L) {
    cat(sprintf("i %d cell(s) had no transactions and were set to 0.\n", nf))
  }
  invisible(x)
}

#' @export
`[.triangle` <- function(x, i, j, ..., drop = TRUE) {
  # Subsetting returns a plain matrix: an arbitrary subset is not guaranteed
  # to be a valid triangle. Use as_triangle() to rebuild one deliberately.
  m <- as.matrix(x)
  if (nargs() - (!missing(drop)) == 2L) return(m[i])  # x[i]: vector-style indexing
  m[i, j, ..., drop = drop]
}
