#' Plot a triangle
#'
#' Draws cumulative development by origin period (one line per origin).
#'
#' @param x A `triangle`.
#' @param ... Passed to [graphics::matplot()].
#' @return `x`, invisibly.
#' @examples
#' plot(raa)
#' @export
plot.triangle <- function(x, ...) {
  cum <- as.matrix(to_cumulative(x))
  ages <- attr(x, "dev_ages")
  args <- list(x = ages, y = t(cum), type = "o", pch = 16, cex = 0.6, lty = 1,
               col = grDevices::hcl.colors(nrow(cum), "Dark 3"),
               xlab = "Development age", ylab = paste("Cumulative", attr(x, "measure")),
               main = "Cumulative development by origin")
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(graphics::matplot, args)
  graphics::legend("topleft", legend = rownames(cum), col = args$col, lty = 1,
                   bty = "n", cex = 0.7, ncol = 2)
  invisible(x)
}

#' Plot a reserving fit
#'
#' `which = "reserve"` (default) shows latest and reserve stacked per origin
#' (bars sum to the ultimate); for Mack fits, whiskers show
#' \eqn{\pm 1} prediction standard error around the ultimate. These are not
#' prediction intervals. `which = "pattern"` shows the age-to-age factors and
#' percentage developed. `which = "residuals"` (Mack fits) plots standardised
#' residuals against origin, development step and calendar period.
#'
#' @param x A `reserve_fit`.
#' @param which `"reserve"`, `"pattern"` or `"residuals"`.
#' @param ... Unused.
#' @return `x`, invisibly.
#' @examples
#' plot(fit_mack(raa))
#' plot(fit_mack(raa), which = "residuals")
#' @export
plot.reserve_fit <- function(x, which = c("reserve", "pattern", "residuals"), ...) {
  which <- match.arg(which)
  r <- x$results
  if (which == "reserve") {
    h <- rbind(Latest = r$latest, Reserve = r$reserve)
    top <- max(r$ultimate + if (!is.null(r$se)) r$se else 0, r$latest, na.rm = TRUE)
    bp <- graphics::barplot(h, names.arg = r$origin, col = c("grey70", "steelblue"),
                            border = NA, ylim = c(min(0, h), top * 1.05),
                            main = paste(x$label, "- latest and reserve by origin"),
                            xlab = "Origin",
                            ylab = paste0("Amount (", attr(x$triangle, "measure"), ")"),
                            legend.text = c("Latest", measure_labels(attr(x$triangle, "measure"))),
                            args.legend = list(x = "topleft", bty = "n"))
    if (!is.null(r$se)) {
      ok <- r$se > 0
      graphics::arrows(bp[ok], (r$ultimate - r$se)[ok], bp[ok], (r$ultimate + r$se)[ok],
                       angle = 90, code = 3, length = 0.04)
      graphics::mtext("Whiskers: ultimate +/- 1 Mack prediction s.e. (not a prediction interval)",
                      side = 3, line = 0.2, cex = 0.75)
    }
  } else if (which == "pattern") {
    if (is.null(x$pattern)) abort("This fit has no development pattern to plot.")
    p <- x$pattern
    op <- graphics::par(mfrow = c(1, 2))
    on.exit(graphics::par(op))
    graphics::plot(seq_along(p$factors), p$factors, type = "b", pch = 16, xaxt = "n",
                   xlab = "Development step", ylab = "Age-to-age factor",
                   main = "Selected factors")
    graphics::axis(1, at = seq_along(p$factors), labels = names(p$factors), cex.axis = 0.7)
    graphics::abline(h = 1, lty = 2, col = "grey50")
    graphics::plot(p$ages, 100 * p$pct_developed, type = "b", pch = 16, ylim = c(0, 100),
                   xlab = "Development age", ylab = "% developed",
                   main = paste0("Pattern (tail ", signif(p$tail, 4), ")"))
  } else {
    if (!inherits(x, "reserve_mack")) abort("Residual plots need a Mack fit (fit_mack()).")
    res <- mack_residuals(x)
    op <- graphics::par(mfrow = c(1, 3))
    on.exit(graphics::par(op))
    for (v in c("origin", "step", "calendar")) {
      g <- factor(res[[v]], levels = unique(res[[v]][order(match(res[[v]], unique(res[[v]])))]))
      if (v == "calendar") g <- factor(res[[v]])
      graphics::plot(as.integer(g), res$residual, pch = 16, xaxt = "n",
                     xlab = v, ylab = "Standardised residual", main = paste("Residuals by", v))
      graphics::axis(1, at = seq_along(levels(g)), labels = levels(g), cex.axis = 0.7)
      graphics::abline(h = 0, lty = 2, col = "grey50")
    }
  }
  invisible(x)
}
