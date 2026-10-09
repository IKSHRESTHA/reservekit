#' Mack chain ladder
#'
#' Chain-ladder reserves with Mack's (1993) distribution-free estimate of
#' their mean squared error of prediction.
#'
#' @section Model:
#' For cumulative claims \eqn{C_{ij}} Mack assumes
#' (1) \eqn{E[C_{i,j+1} | C_{i1},\dots,C_{ij}] = f_j C_{ij}},
#' (2) \eqn{Var[C_{i,j+1} | C_{i1},\dots,C_{ij}] = \sigma_j^2 C_{ij}},
#' (3) origin periods are independent.
#'
#' @section Estimators:
#' With weights \eqn{w_{ij}} (default 1), \eqn{S_j = \sum_i w_{ij} C_{ij}}
#' over the \eqn{n_j} ratios used:
#' \deqn{\hat f_j = \sum_i w_{ij} C_{i,j+1} / S_j,}
#' \deqn{\hat\sigma_j^2 = \frac{1}{n_j - 1}\sum_i w_{ij} C_{ij}(F_{ij} - \hat f_j)^2.}
#' When \eqn{n_j < 2}, \eqn{\sigma_j^2} cannot be estimated and is
#' extrapolated. `sigma_extrapolation = "mack"` (default) uses Mack (1993):
#' \eqn{\sigma_j^2 = \min(\sigma_{j-1}^4/\sigma_{j-2}^2, \sigma_{j-2}^2, \sigma_{j-1}^2)};
#' `"loglinear"` fits \eqn{\log \sigma_j^2} linearly in \eqn{j} over the
#' estimable steps.
#'
#' For origin \eqn{i} with latest development index \eqn{a_i} and ultimate
#' \eqn{\hat C_{iK}} (at the last age, no tail):
#' \deqn{\widehat{mse}(\hat R_i) = \hat C_{iK}^2 \sum_{j=a_i}^{K-1}
#'   \frac{\hat\sigma_j^2}{\hat f_j^2}\left(\frac{1}{\hat C_{ij}} + \frac{1}{S_j}\right),}
#' where the first term is process variance and the second parameter
#' (estimation) error. For the total reserve, Mack adds the parameter-error
#' covariance between origins:
#' \deqn{\sum_{j} \frac{\hat\sigma_j^2/\hat f_j^2}{S_j}
#'   \Big[\big(\sum_{i \in A_j} \hat C_{iK}\big)^2 - \sum_{i \in A_j} \hat C_{iK}^2\Big],}
#' with \eqn{A_j = \{i : a_i \le j\}}.
#'
#' @section Interpretation:
#' `se` is the square root of the estimated mean squared error of
#' prediction: a *prediction* standard error (process + parameter), not the
#' standard error of an estimated mean. It is not a distribution: no
#' quantiles or prediction intervals are produced in this version.
#'
#' @section Limitations:
#' Tail factors are not supported in this version (Mack 1999 tail
#' uncertainty is on the roadmap); use [fit_chain_ladder()] for tailed point
#' estimates. Overridden (judgemental) factors are not supported because the
#' variance estimators assume the estimated factors. Estimates for the most
#' recent origins and for steps with extrapolated sigma are fragile; check
#' [diagnostics()].
#'
#' @param x A `triangle`.
#' @param n_periods,weights Passed to [dev_factors()] (volume-weighted).
#' @param sigma_extrapolation `"mack"` (default) or `"loglinear"`.
#' @return A `reserve_fit` of subclass `reserve_mack`. `results` gains
#'   `process_se`, `parameter_se`, `se` and `cv`; the fit also has `sigma`
#'   (with attribute `extrapolated`), `S` and `total` (a one-row data.frame).
#' @references
#' Mack, T. (1993). Distribution-free calculation of the standard error of
#' chain ladder reserve estimates. *ASTIN Bulletin* 23(2), 213-225.
#'
#' Mack, T. (1994). Measuring the variability of chain ladder reserve
#' estimates. *Casualty Actuarial Society Forum*, Spring 1994, 101-182.
#' @examples
#' fit <- fit_mack(raa)
#' fit
#' fit$total
#' @export
fit_mack <- function(x, n_periods = NULL, weights = NULL,
                     sigma_extrapolation = c("mack", "loglinear")) {
  check_triangle(x, "x")
  assert_fittable(x)
  sigma_extrapolation <- match.arg(sigma_extrapolation)
  cum <- to_cumulative(x)
  pattern <- dev_factors(cum, method = "volume", n_periods = n_periods,
                         weights = weights, tail = 1)
  C <- as.matrix(cum)
  n <- nrow(C)
  K <- ncol(C)
  f <- unname(pattern$factors)
  w <- unname(pattern$weights)
  use <- !is.na(w) & w > 0
  den <- C[, -K, drop = FALSE]
  num <- C[, -1L, drop = FALSE]
  if (any(den[use] <= 0)) {
    abort("Mack's variance estimators need positive cumulative values where ",
          "link ratios are used; found zero or negative values. See validate_triangle().")
  }
  S <- colSums(ifelse(use, w * den, 0))
  n_j <- colSums(use)
  sigma2 <- rep(NA_real_, K - 1L)
  for (j in seq_len(K - 1L)) {
    if (n_j[j] >= 2L) {
      r <- use[, j]
      lr <- num[r, j] / den[r, j]
      sigma2[j] <- sum(w[r, j] * den[r, j] * (lr - f[j])^2) / (n_j[j] - 1L)
    }
  }
  extrap <- is.na(sigma2)
  sigma2 <- extrapolate_sigma2(sigma2, sigma_extrapolation)

  lat <- latest_diagonal(cum)
  a <- lat$dev_index
  # Projected cumulative values C_hat[i, j] for j >= a_i.
  Chat <- C
  for (i in seq_len(n)) {
    if (a[i] < K) {
      for (j in a[i]:(K - 1L)) Chat[i, j + 1L] <- Chat[i, j] * f[j]
    }
  }
  U <- Chat[, K]
  q <- sigma2 / f^2                                    # per step
  proc <- par <- numeric(n)
  for (i in seq_len(n)) {
    if (a[i] < K) {
      jj <- a[i]:(K - 1L)
      proc[i] <- U[i]^2 * sum(q[jj] / Chat[i, jj])
      par[i] <- U[i]^2 * sum(q[jj] / S[jj])
    }
  }
  cross <- 0
  for (j in seq_len(K - 1L)) {
    A <- a <= j
    cross <- cross + q[j] / S[j] * (sum(U[A])^2 - sum(U[A]^2))
  }
  res <- base_results(lat, pattern)
  res$ultimate <- U
  res$reserve <- U - lat$value
  res$process_se <- sqrt(proc)
  res$parameter_se <- sqrt(par)
  res$se <- sqrt(proc + par)
  res$cv <- ifelse(res$reserve != 0, res$se / res$reserve, NA_real_)
  tot_proc <- sum(proc)
  tot_par <- sum(par) + cross
  total <- data.frame(
    latest = sum(lat$value), ultimate = sum(U), reserve = sum(res$reserve),
    process_se = sqrt(tot_proc), parameter_se = sqrt(tot_par),
    se = sqrt(tot_proc + tot_par), stringsAsFactors = FALSE
  )
  total$cv <- total$se / total$reserve
  sig <- stats::setNames(sqrt(sigma2), names(pattern$factors))
  attr(sig, "extrapolated") <- stats::setNames(extrap, names(pattern$factors))
  warns <- character()
  if (any(extrap)) {
    warns <- paste0("sigma extrapolated (", sigma_extrapolation, ") for step(s) ",
                    paste(names(pattern$factors)[extrap], collapse = ", "), ".")
  }
  new_reserve_fit(
    method = "mack", label = "Mack chain ladder", call = match.call(),
    triangle = cum, pattern = pattern, results = res, emergence = U,
    warnings = warns,
    assumptions = list(n_periods = n_periods,
                       weights = if (is.null(weights)) "none" else "user-supplied",
                       sigma_extrapolation = sigma_extrapolation, tail = 1),
    extra = list(sigma = sig, S = stats::setNames(S, names(pattern$factors)),
                 total = total)
  )
}

extrapolate_sigma2 <- function(s2, how) {
  miss <- which(is.na(s2))
  if (!length(miss)) return(s2)
  if (how == "loglinear") {
    ok <- which(!is.na(s2) & s2 > 0)
    if (length(ok) < 2L) {
      abort("Log-linear sigma extrapolation needs at least 2 positive estimated ",
            "sigma^2 values; found ", length(ok), ".")
    }
    cf <- stats::lm.fit(cbind(1, ok), log(s2[ok]))$coefficients
    s2[miss] <- exp(cf[[1]] + cf[[2]] * miss)
    return(s2)
  }
  for (j in miss) {
    if (j < 3L || is.na(s2[j - 1L]) || is.na(s2[j - 2L])) {
      abort("Cannot extrapolate sigma for development step ", j,
            ": Mack's rule needs the two preceding steps to be estimable. ",
            "Use more data (larger `n_periods`) or sigma_extrapolation = \"loglinear\".")
    }
    p1 <- s2[j - 1L]
    p2 <- s2[j - 2L]
    ratio <- if (p2 > 0) p1^2 / p2 else Inf
    s2[j] <- min(ratio, p2, p1)
  }
  s2
}
