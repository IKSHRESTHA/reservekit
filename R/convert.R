#' Convert between cumulative and incremental triangles
#'
#' `to_cumulative()` accumulates increments along each origin row;
#' `to_incremental()` differences cumulative values. Converting a triangle
#' that is already of the requested type returns it unchanged.
#'
#' A genuine missing value propagates: every cumulative value after a missing
#' increment is unknown, and both increments adjacent to a missing cumulative
#' value are unknown. Structural future cells stay `NA`.
#'
#' @param x A `triangle`.
#' @return A `triangle` of the requested type with the same metadata.
#' @examples
#' inc <- to_incremental(raa)
#' all.equal(to_cumulative(inc), raa)
#' @export
to_cumulative <- function(x) {
  check_triangle(x, "x")
  if (attr(x, "type") == "cumulative") return(x)
  m <- as.matrix(x)
  obs <- attr(x, "observed")
  out <- t(apply(m, 1L, cumsum))
  if (ncol(m) == 1L) out <- t(out)
  out[!obs] <- NA_real_
  dimnames(out) <- dimnames(m)
  restore_attrs(out, x, type = "cumulative")
}

#' @rdname to_cumulative
#' @export
to_incremental <- function(x) {
  check_triangle(x, "x")
  if (attr(x, "type") == "incremental") return(x)
  m <- as.matrix(x)
  out <- m
  if (ncol(m) > 1L) out[, -1L] <- m[, -1L, drop = FALSE] - m[, -ncol(m), drop = FALSE]
  restore_attrs(out, x, type = "incremental")
}

restore_attrs <- function(m, template, type) {
  structure(m, class = "triangle", type = type,
            measure = attr(template, "measure"),
            dev_ages = attr(template, "dev_ages"),
            observed = attr(template, "observed"),
            filled = attr(template, "filled"))
}

#' Latest diagonal of a triangle
#'
#' Returns, for each origin period, the value on the latest valuation
#' diagonal and the development age at which it sits.
#'
#' @param x A `triangle`.
#' @return A data.frame with columns `origin`, `dev_age`, `dev_index`, `value`.
#' @examples
#' latest_diagonal(raa)
#' @export
latest_diagonal <- function(x) {
  check_triangle(x, "x")
  obs <- attr(x, "observed")
  j <- rowSums(obs)
  m <- as.matrix(x)
  data.frame(
    origin = rownames(m),
    dev_age = attr(x, "dev_ages")[j],
    dev_index = j,
    value = m[cbind(seq_len(nrow(m)), j)],
    stringsAsFactors = FALSE
  )
}
