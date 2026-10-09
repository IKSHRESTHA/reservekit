# Internal helpers. Not exported.

abort <- function(..., call. = FALSE) {
  stop(paste0(...), call. = call.)
}

warn <- function(..., call. = FALSE) {
  warning(paste0(...), call. = call.)
}

check_triangle <- function(x, arg = "triangle") {
  if (!inherits(x, "triangle")) {
    abort("`", arg, "` must be a `triangle` object; create one with ",
          "as_triangle(). Got an object of class <", class(x)[1], ">.")
  }
  invisible(x)
}

check_number <- function(x, arg, lower = -Inf, upper = Inf,
                         lower_inclusive = TRUE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    ok <- if (lower_inclusive) x >= lower else x > lower
    ok <- ok && x <= upper
  }
  if (!ok) {
    rng <- ""
    if (is.finite(lower) || is.finite(upper)) {
      rng <- paste0(" in ", if (lower_inclusive) "[" else "(", lower, ", ", upper, "]")
    }
    abort("`", arg, "` must be a single finite number", rng, ".")
  }
  invisible(x)
}

# Recycle a scalar or check a per-origin vector.
per_origin <- function(x, n, arg, origins = NULL) {
  if (!is.numeric(x) || anyNA(x) || any(!is.finite(x))) {
    abort("`", arg, "` must be numeric with no missing or infinite values.")
  }
  if (length(x) == 1L) return(rep(unname(x), n))
  if (!is.null(names(x)) && !is.null(origins)) {
    if (!setequal(names(x), origins) || anyDuplicated(names(x))) {
      abort("`", arg, "` is named, but its names do not match the triangle's origins (",
            paste(utils::head(origins, 4), collapse = ", "), if (n > 4) ", ...", ").")
    }
    x <- x[origins]
  }
  if (length(x) != n) {
    abort("`", arg, "` must have length 1 or one value per origin period (",
          n, "); got length ", length(x), ".")
  }
  unname(as.numeric(x))
}

fmt_cells <- function(rows, cols, max = 5L) {
  s <- paste0("(", rows, ", ", cols, ")")
  if (length(s) > max) s <- c(s[seq_len(max)], paste0("... ", length(s) - max, " more"))
  paste(s, collapse = ", ")
}

measure_labels <- function(measure) {
  switch(measure,
    paid = "Unpaid (ultimate - paid)",
    incurred = "IBNR (ultimate - incurred)",
    reported_count = "IBNR count",
    closed_count = "Open count (ultimate - closed)",
    "Reserve (ultimate - latest)"
  )
}
