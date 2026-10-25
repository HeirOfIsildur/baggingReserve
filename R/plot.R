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
#' Bands and quantiles are empirical quantiles over the columns of the prediction and are
#' labelled by what these are: the bootstrap replicates of a bagged fit (resampling variation,
#' with simulated payments as well for `completion = "draw"`) or the simulated completions of a
#' single-tree fit (process variability given the one fitted model); see the `variability` of
#' [predict.reserve_tree()]. They are neither prediction intervals nor confidence intervals for
#' the mean.
#'
#' @param x A [reserve_prediction].
#' @param what Which display: `"calendar"` (default), `"next_year"` or `"ultimate"`.
#' @param component `"total"` (default; falls back to `"reported"` without an IBNR
#'   component), `"reported"` or `"ibnr"`.
#' @param probs Two increasing probabilities in `[0, 1]` for the quantile band or lines
#'   (default `c(0.025, 0.975)`).
#' @param truth Optional realised payments (default `NULL`, none drawn): for `"calendar"`
#'   a vector named by calendar year, e.g. `setNames(bt$tables$total$truth,
#'   bt$tables$total$calendar)` from a [backtest()], or an unnamed vector with one value per
#'   future calendar year; for the others a single number.
#' @param reference Optional named numeric vector of reference estimates drawn as dotted
#'   vertical lines (`"next_year"`, `"ultimate"`); default `NULL`.
#' @param main Title; `NULL` (default) builds one from the display and the estimator.
#' @param ... Further graphical arguments passed to the underlying plotting function
#'   ([graphics::plot()] for `"calendar"`, [graphics::hist()] or [graphics::stripchart()]
#'   otherwise); they override the defaults (e.g. `ylab`, `ylim`, `xlab`, `col`).
#' @param type Quantile algorithm for the band and the quantile lines, as in
#'   [stats::quantile()] (an integer from 1 to 9; default 7, as in [summary.reserve_prediction()]
#'   and [backtest()]); it must be named.
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
                                    main = NULL, ..., type = 7L) {
  what <- match.arg(what)
  component <- match.arg(component)
  check_probs_pair(probs)
  check_quantile_type(type)
  check_plot_dots(list(...), names(formals(sys.function())))
  if (component == "total" && is.null(x$ibnr)) component <- "reported"
  if (nrow(x$cells) == 0L)
    stop("the prediction has no future cell to plot (the panel is fully developed)", call. = FALSE)
  cy <- calendar_year_payments(x, component = component)
  years <- rownames(cy)[rownames(cy) != "total"]
  lab <- switch(component, reported = "reported claims", ibnr = "IBNR component", total = "all claims")
  info <- pred_info(x)
  est_lab <- if (x$B > 1L || info$collapsed) info$point else "single-tree prediction"
  n_lab <- paste0(if (info$kind == "tree" && info$completion == "draw") "R" else "B", " = ", x$B)
  dots <- list(...)
  if (what == "calendar") {
    m <- cy[years, , drop = FALSE]
    mean_m <- rowMeans(m)
    yr <- as.integer(years)
    q <- NULL
    if (x$B > 1L) {
      q <- t(apply(m, 1L, stats::quantile, probs = probs, names = FALSE, type = type))
      if (nrow(m) == 1L) q <- matrix(q, 1L)
    }
    if (!is.null(truth)) {
      if (!is.numeric(truth)) stop("`truth` must be numeric", call. = FALSE)
      if (!is.null(names(truth))) {
        if (!all(years %in% names(truth)))
          stop("`truth` lacks the calendar year(s) ", paste(setdiff(years, names(truth)), collapse = ", "),
               call. = FALSE)
        truth <- truth[years]                                  # the displayed years only
      } else if (length(truth) != length(years)) {
        stop("an unnamed `truth` must have one value per future calendar year (", length(years),
             "); name it by calendar year otherwise", call. = FALSE)
      }
    }
    sc <- axis_scale(c(q, mean_m, truth))
    m <- m / sc$by; mean_m <- mean_m / sc$by; if (!is.null(q)) q <- q / sc$by
    if (!is.null(truth)) truth <- truth / sc$by
    ylim <- range(c(q, mean_m, truth), na.rm = TRUE)
    plot_args <- list(x = yr, y = mean_m, type = "n", ylim = ylim, xlab = "Calendar year",
                      ylab = paste0("Payments, ", lab, sc$unit), las = 1,
                      main = if (is.null(main)) paste0("Predicted payments by calendar year (", info$fit,
                                                       if (x$B > 1L) paste0(", ", n_lab), ")") else main)
    do.call(graphics::plot, utils::modifyList(plot_args, dots))
    if (!is.null(q))
      graphics::polygon(c(yr, rev(yr)), c(q[, 1L], rev(q[, 2L])), col = "grey85", border = NA)
    graphics::lines(yr, mean_m, lwd = 2)
    graphics::points(yr, mean_m, pch = 16)
    if (!is.null(truth)) graphics::points(yr, truth, pch = 4, col = "blue", lwd = 2)
    draw_legend("topright", bty = "n",
                     legend = c(est_lab,
                                if (!is.null(q)) sprintf("%s %g-%g", info$quantiles, probs[1L], probs[2L]),
                                if (!is.null(truth)) "realised"),
                     lwd = c(2, if (!is.null(q)) NA, if (!is.null(truth)) NA),
                     pch = c(16, if (!is.null(q)) 15, if (!is.null(truth)) 4),
                     col = c("black", if (!is.null(q)) "grey85", if (!is.null(truth)) "blue"))
    return(invisible(x))
  }
  if (!is.null(truth) && (!is.numeric(truth) || length(truth) != 1L))
    stop("`truth` must be a single number for the \"", what, "\" display", call. = FALSE)
  row <- if (what == "next_year") years[1L] else "total"
  v <- cy[row, ]
  ttl <- if (is.null(main))
    paste0(if (what == "next_year") paste0("Next year's payments (", x$t + 1L, "), ")
           else paste0("Payments through development year ", x$L, ", "),
           lab, "\n", info$fit, ", ", n_lab) else main
  sc <- axis_scale(c(v, truth, reference))
  v <- v / sc$by
  if (!is.null(truth)) truth <- truth / sc$by
  if (!is.null(reference)) reference <- reference / sc$by
  xlim <- range(c(v, truth, reference), na.rm = TRUE)
  xlim <- xlim + c(-1, 1) * max(0.05 * diff(xlim), 1e-8)
  if (x$B >= 20L) {
    do.call(graphics::hist, utils::modifyList(list(x = v, breaks = "FD", main = ttl, xlab = paste0("Payments", sc$unit),
                                                   col = "grey90", border = "grey60", xlim = xlim, cex.main = 0.95), dots))
    graphics::rug(v)
  } else {
    do.call(graphics::stripchart, utils::modifyList(list(x = v, method = "stack", pch = 16, xlim = xlim, main = ttl,
                                                         xlab = paste0("Payments", sc$unit), col = "grey40",
                                                         cex.main = 0.95), dots))
  }
  graphics::abline(v = mean(v), lwd = 2)
  if (x$B > 1L) graphics::abline(v = stats::quantile(v, probs = probs, names = FALSE, type = type), lty = 2,
                                 col = "grey40")
  if (!is.null(truth)) graphics::abline(v = truth, col = "blue", lwd = 2, lty = 4)
  if (!is.null(reference)) graphics::abline(v = reference, lty = 3, col = "darkgreen", lwd = 2)
  ref_lab <- if (!is.null(reference)) {
    if (is.null(names(reference))) "reference" else paste(names(reference), collapse = ", ")
  }
  draw_legend("topright", bty = "n", cex = 0.85,
                   legend = c(est_lab,
                              if (x$B > 1L) sprintf("%s %g, %g", info$quantiles, probs[1L], probs[2L]),
                              if (!is.null(truth)) "realised",
                              ref_lab),
                   lty = c(1, if (x$B > 1L) 2, if (!is.null(truth)) 4, if (!is.null(reference)) 3),
                   col = c("black", if (x$B > 1L) "grey40", if (!is.null(truth)) "blue",
                           if (!is.null(reference)) "darkgreen"),
                   lwd = c(2, if (x$B > 1L) 1, if (!is.null(truth)) 2, if (!is.null(reference)) 2))
  invisible(x)
}

# Arguments in `...` go to base graphics, which ignores unknown names with a warning at best;
# reject unnamed extras and names that are neither graphical parameters nor plot() arguments,
# so a misspelt argument (e.g. `compnent =`) fails instead of being silently dropped.
check_plot_dots <- function(dots, own) {
  if (!length(dots)) return(invisible())
  nm <- names(dots)
  if (is.null(nm) || any(!nzchar(nm)))
    stop("unexpected unnamed argument passed to plot(); name it", call. = FALSE)
  known <- c(names(graphics::par(no.readonly = FALSE)), "main", "xlab", "ylab", "xlim", "ylim", "sub",
             "asp", "log", "axes", "frame.plot", "panel.first", "panel.last", "breaks",
             "uniform", "branch", "compress", "margin", "nspace", "minbranch", "border",
             "density", "angle", "type")
  bad <- setdiff(nm, known)
  if (length(bad)) {
    hint <- vapply(bad, function(b) { m <- own[pmatch(b, own)]
      if (is.na(m)) { d <- utils::adist(b, own); m <- if (min(d) <= 2) own[which.min(d)] else NA }
      if (is.na(m)) "" else paste0(" (did you mean `", m, "`?)") }, "")
    stop("unknown argument", if (length(bad) > 1) "s", " to plot(): ",
         paste0("`", bad, "`", hint, collapse = ", "), call. = FALSE)
  }
  invisible()
}

# The legend of the prediction plots (a function of its own so that the tests can read the labels).
draw_legend <- function(...) graphics::legend(...)

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
  check_plot_dots(list(...), "x")
  o <- oob_score(x)
  do.call(graphics::plot, utils::modifyList(list(
    x = o$period, y = o$oob_score, type = "b", pch = 16, xlab = "Development year l (period)",
    ylab = "Out-of-bag score", main = paste0("Out-of-bag score per development year (B = ", x$B, ")"),
    ylim = range(c(0, o$oob_score), na.rm = TRUE)), list(...)))
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
  if (!is.null(x$learner)) stop("plot() draws rpart trees; the fit uses another learner", call. = FALSE)
  check_plot_dots(list(...), c("x", "period"))
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
  do.call(graphics::plot, utils::modifyList(list(x = fit, uniform = TRUE,
                                                 main = paste0("Regression tree for development year ", period)),
                                            list(...)))
  graphics::text(fit, use.n = FALSE, cex = 0.7)
  invisible(x)
}
