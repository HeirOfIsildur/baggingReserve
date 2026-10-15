#' Plot a reserve prediction
#'
#' Base-graphics displays of a [reserve_prediction]:
#' \describe{
#'   \item{`"calendar"`}{predicted payments per future calendar year: the bagged mean
#'     (or the single-tree prediction) as a line, the replicate quantile band shaded when
#'     `B > 1`, and the realised payments as points when `truth` is given.}
#'   \item{`"next_year"`, `"ultimate"`}{the replicate predictions of next year's payments,
#'     or of the total through development year `L`: a histogram with a rug for `B >= 20`,
#'     individual points otherwise; vertical lines mark the bagged mean (solid black), the
#'     replicate quantiles (dashed grey), the `truth` (dot-dashed blue) and any `reference`
#'     values (dotted green, e.g. a chain-ladder estimate).}
#' }
#' Bands and quantiles are empirical quantiles over the bootstrap replicates and are
#' labelled as such; they describe resampling variation and are neither prediction
#' intervals nor confidence intervals for the bagged mean.
#'
#' @param x A [reserve_prediction].
#' @param what Which display: `"calendar"` (default), `"next_year"` or `"ultimate"`.
#' @param component `"total"` (default; falls back to `"reported"` without an IBNR
#'   component), `"reported"` or `"ibnr"`.
#' @param probs Two increasing probabilities in `[0, 1]` for the quantile band or lines
#'   (default `c(0.025, 0.975)`).
#' @param truth Optional realised payments (default `NULL`, none drawn): for `"calendar"`
#'   a vector named by calendar year, e.g. `setNames(bt$tables$total$truth,
#'   bt$tables$total$calendar)` from a [backtest()], for the others a single number.
#' @param reference Optional named numeric vector of reference estimates drawn as dotted
#'   vertical lines (`"next_year"`, `"ultimate"`); default `NULL`.
#' @param main Title; `NULL` (default) builds one from the display and the estimator.
#' @param ... Further arguments passed to the underlying plotting function.
#' @return `x`, invisibly.
#' @examples
#' data("synthetic_claims", "synthetic_payments", "synthetic_claims_full",
#'      "synthetic_payments_full", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' bag <- bag_reserve(panel, B = 20, seed = 2026)
#' res <- ibnr_reserve(predict(bag), claim_counts(panel))
#' cl_full <- synthetic_claims_full[synthetic_claims_full$Type == "3", ]
#' bt <- backtest(res, cl_full, synthetic_payments_full[synthetic_payments_full$Id %in% cl_full$Id, ])
#' plot(res, "calendar", truth = setNames(bt$tables$total$truth, bt$tables$total$calendar))
#' plot(res, "next_year", truth = bt$tables$total$truth[1])
#' plot(bag)
#' @export
plot.reserve_prediction <- function(x, what = c("calendar", "next_year", "ultimate"),
                                    component = c("total", "reported", "ibnr"),
                                    probs = c(0.025, 0.975), truth = NULL, reference = NULL,
                                    main = NULL, ...) {
  what <- match.arg(what)
  component <- match.arg(component)
  check_probs_pair(probs)
  if (component == "total" && is.null(x$ibnr)) component <- "reported"
  if (nrow(x$cells) == 0L)
    stop("the prediction has no future cell to plot (the panel is fully developed)", call. = FALSE)
  cy <- calendar_year_payments(x, component = component)
  years <- rownames(cy)[rownames(cy) != "total"]
  lab <- switch(component, reported = "reported claims", ibnr = "IBNR component", total = "all claims")
  est_lab <- if (x$B > 1L || grepl("mean of replicates", x$estimator, fixed = TRUE)) "bagged mean" else "single-tree prediction"
  if (what == "calendar") {
    m <- cy[years, , drop = FALSE]
    mean_m <- rowMeans(m)
    yr <- as.integer(years)
    q <- NULL
    if (x$B > 1L) {
      q <- t(apply(m, 1L, stats::quantile, probs = probs, names = FALSE))
      if (nrow(m) == 1L) q <- matrix(q, 1L)
    }
    if (!is.null(truth) && !is.null(names(truth))) truth <- truth[years]   # the displayed years only
    sc <- axis_scale(c(q, mean_m, truth))
    m <- m / sc$by; mean_m <- mean_m / sc$by; if (!is.null(q)) q <- q / sc$by
    if (!is.null(truth)) truth <- truth / sc$by
    ylim <- range(c(q, mean_m, truth), na.rm = TRUE)
    graphics::plot(yr, mean_m, type = "n", ylim = ylim, xlab = "Calendar year",
                   ylab = paste0("Payments, ", lab, sc$unit), las = 1,
                   main = if (is.null(main)) paste0("Predicted payments by calendar year (",
                                                    x$estimator, ")") else main, ...)
    if (!is.null(q))
      graphics::polygon(c(yr, rev(yr)), c(q[, 1L], rev(q[, 2L])), col = "grey85", border = NA)
    graphics::lines(yr, mean_m, lwd = 2)
    graphics::points(yr, mean_m, pch = 16)
    if (!is.null(truth)) graphics::points(yr, truth, pch = 4, col = "blue", lwd = 2)
    graphics::legend("topright", bty = "n",
                     legend = c(est_lab,
                                if (!is.null(q)) sprintf("replicate quantiles %g-%g", probs[1L], probs[2L]),
                                if (!is.null(truth)) "realised"),
                     lwd = c(2, if (!is.null(q)) NA, if (!is.null(truth)) NA),
                     pch = c(16, if (!is.null(q)) 15, if (!is.null(truth)) 4),
                     col = c("black", if (!is.null(q)) "grey85", if (!is.null(truth)) "blue"))
    return(invisible(x))
  }
  row <- if (what == "next_year") years[1L] else "total"
  v <- cy[row, ]
  ttl <- if (is.null(main))
    paste0(if (what == "next_year") paste0("Next year's payments (", x$t + 1L, "), ")
           else paste0("Payments through development year ", x$L, ", "),
           lab, "\n", x$estimator, ", B = ", x$B) else main
  sc <- axis_scale(c(v, truth, reference))
  v <- v / sc$by
  if (!is.null(truth)) truth <- truth / sc$by
  if (!is.null(reference)) reference <- reference / sc$by
  xlim <- range(c(v, truth, reference), na.rm = TRUE)
  xlim <- xlim + c(-1, 1) * max(0.05 * diff(xlim), 1e-8)
  if (x$B >= 20L) {
    graphics::hist(v, breaks = "FD", main = ttl, xlab = paste0("Payments", sc$unit), col = "grey90",
                   border = "grey60", xlim = xlim, cex.main = 0.95, ...)
    graphics::rug(v)
  } else {
    graphics::stripchart(v, method = "stack", pch = 16, xlim = xlim, main = ttl,
                         xlab = paste0("Payments", sc$unit), col = "grey40", cex.main = 0.95, ...)
  }
  graphics::abline(v = mean(v), lwd = 2)
  if (x$B > 1L) graphics::abline(v = stats::quantile(v, probs = probs, names = FALSE), lty = 2,
                                 col = "grey40")
  if (!is.null(truth)) graphics::abline(v = truth, col = "blue", lwd = 2, lty = 4)
  if (!is.null(reference)) graphics::abline(v = reference, lty = 3, col = "darkgreen", lwd = 2)
  ref_lab <- if (!is.null(reference)) {
    if (is.null(names(reference))) "reference" else paste(names(reference), collapse = ", ")
  }
  graphics::legend("topright", bty = "n", cex = 0.85,
                   legend = c(est_lab,
                              if (x$B > 1L) sprintf("replicate quantiles %g, %g", probs[1L], probs[2L]),
                              if (!is.null(truth)) "realised",
                              ref_lab),
                   lty = c(1, if (x$B > 1L) 2, if (!is.null(truth)) 4, if (!is.null(reference)) 3),
                   col = c("black", if (x$B > 1L) "grey40", if (!is.null(truth)) "blue",
                           if (!is.null(reference)) "darkgreen"),
                   lwd = c(2, if (x$B > 1L) 1, if (!is.null(truth)) 2, if (!is.null(reference)) 2))
  invisible(x)
}

# Choose a display unit so that axis labels stay short.
axis_scale <- function(values) {
  m <- suppressWarnings(max(abs(values), na.rm = TRUE))
  if (!is.finite(m)) return(list(by = 1, unit = ""))
  if (m >= 1e9) list(by = 1e9, unit = " (billions)")
  else if (m >= 1e6) list(by = 1e6, unit = " (millions)")
  else if (m >= 1e4) list(by = 1e3, unit = " (thousands)")
  else list(by = 1, unit = "")
}

#' Plot the out-of-bag scores of a bagged fit
#'
#' @param x A [bag_reserve] fit.
#' @param ... Passed to [graphics::plot()].
#' @return `x`, invisibly.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' plot(bag_reserve(panel, B = 5, seed = 2026))
#' @export
plot.bag_reserve <- function(x, ...) {
  o <- oob_score(x)
  graphics::plot(o$period, o$oob_score, type = "b", pch = 16, xlab = "Development year l (period)",
                 ylab = "Out-of-bag score",
                 main = paste0("Out-of-bag score per development year (B = ", x$B, ")"),
                 ylim = range(c(0, o$oob_score), na.rm = TRUE), ...)
  graphics::abline(h = 0, lty = 3)
  graphics::text(o$period, o$oob_score, labels = paste0("n=", format(o$n_train, big.mark = ",")),
                 pos = 3, cex = 0.7)
  invisible(x)
}

#' Plot the tree of one development year of a fit
#'
#' @param x A [reserve_tree] fit.
#' @param period Development year `l` (`0, ..., L - 1`) whose tree is drawn.
#' @param ... Passed to [rpart::plot.rpart()].
#' @return `x`, invisibly.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' plot(reserve_tree(panel, seed = 1), period = 0)
#' @export
plot.reserve_tree <- function(x, period = 0L, ...) {
  period <- as_scalar_integer(period, "period")
  if (period < 0L || period > x$L - 1L)
    stop("`period` must be between 0 and L - 1 = ", x$L - 1L, call. = FALSE)
  fit <- x$fits[[period + 1L]]
  if (nrow(fit$frame) == 1L) {
    graphics::plot.new()
    graphics::title(main = paste0("Development year ", period, ": root only (predicts ",
                                  signif(fit$frame$yval, 4), ")"))
    return(invisible(x))
  }
  graphics::plot(fit, uniform = TRUE,
                 main = paste0("Regression tree for development year ", period), ...)
  graphics::text(fit, use.n = FALSE, cex = 0.7)
  invisible(x)
}
