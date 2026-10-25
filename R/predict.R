#' Predicted future payments of the reported claims
#'
#' `predict()` on a [reserve_tree] completes the payment histories of all claims
#' reported by the valuation date, recursively one development year ahead: the tree for
#' development year `l` predicts `X_{l+1}` from the features, the reporting delay and
#' `X_0, ..., X_l`, where the cells after the valuation date among `X_0, ..., X_l` are
#' the predictions of the earlier trees. Following the source article, every reported
#' claim is completed, including claims recorded as closed; the claim status is not a
#' covariate unless it is named in `features`. Do not name a status recorded at the valuation
#' date as a feature without care: the panel holds it as a static feature, but in the earlier
#' development years from which the trees learn it was not yet known (a claim closed at `t`
#' was open in the years in which it paid), so the trees learn from information that the
#' claims did not have at the time (see the JSS manuscript on open and closed claims).
#'
#' The result is a `reserve_prediction`: the observed incremental triangle, the
#' coordinates of the cells after the valuation date, and one column per replicate with
#' the predicted reported-claim payments in those cells. A single-tree fit gives one
#' prediction column; [bag_reserve()] gives `B`.
#'
#' @param object A fitted [reserve_tree].
#' @param newdata `NULL` (default) completes the panel the model was fitted on. Otherwise
#'   another [claims_panel] with the same features in the same order, the same `L`, and
#'   factor levels among those seen at fitting time (a compatible subset is re-levelled;
#'   levels unknown to the fit are an error). It may have another valuation date `t` than
#'   the fitted panel: the trees then complete its cells after its own `t`, and the prediction is
#'   labelled by that `t` (its first calendar year is `t + 1` of `newdata`). A panel valued before
#'   the fit's valuation date that shares claims with the fitted panel gives a warning: the trees
#'   were trained on those claims' payments after its valuation date, so a back-test of the
#'   prediction would use information that was not available then; refit at the earlier date
#'   instead (Section 4.3 of the JSS manuscript). A factor level
#'   that the fit knows but that is absent from the training claims of the period in which
#'   a claim is completed gives a warning, here as when the fitted panel is completed: at
#'   each split on that feature the trees treat it as missing (surrogate splits, else the
#'   majority direction).
#' @param type `"replicates"` returns every replicate; `"mean"` averages the completed
#'   replicates (for a single-tree fit the two coincide).
#' @param completion `"mean"` (default, the source article's method) plugs the predicted mean of
#'   each development year into the payment history used for the next, as described above.
#'   `"draw"` simulates instead: each future payment is a training response drawn from the
#'   claim's leaf (for a user-supplied learner, from its `simulate(model, newdata)` function). The
#'   `R` simulated completions are the columns of the result (process variability of the fitted
#'   trees, no estimation variability); `type = "mean"` averages them. Because most later-year
#'   payments are exactly zero, plugging in means can move claims across splits that separate
#'   paying from non-paying claims; simulation avoids this. (For a bagged fit the completion is
#'   chosen in [bag_reserve()].)
#'   Plugging in means is biased for payments beyond next year when the occurrence of a payment
#'   depends on the payment history: in the correctly specified simulation of the JSS manuscript
#'   (Section 4.6) it overstated the total reserve of the reported claims by up to 62% for the
#'   single tree, a bias that does not vanish as the portfolio grows. Simulated completion removes
#'   this mechanism and most of the bias (at most 0.8% remained there for the single tree; the
#'   fitted model's own errors remain). Next year's payments are the same in expectation.
#'   **For totals beyond next year we recommend `"draw"`.**
#' @param R Number of simulated completions for `completion = "draw"` (default 100).
#' @param seed Seed for the simulated completions, set with R's default generator kinds as in
#'   [reserve_tree()] (the caller's kind and state are restored on exit); `NULL` (default) uses
#'   and advances the current random-number state. `R` and `seed` belong to
#'   `completion = "draw"`: given with `completion = "mean"`, they are an error.
#' @param ... Not used; any argument given here (a misspelt one, say) is an error.
#' @return An object of class `"reserve_prediction"`, a list with
#' \describe{
#'   \item{observed}{incremental payments by accident year and development year, `NA`
#'     after the valuation date}
#'   \item{cells}{data frame of the future cells: `acc_year`, `dev`, `calendar`
#'     (`acc_year + dev`), `index` (position in the triangle, column-major)}
#'   \item{reported}{matrix, one row per future cell and one column per replicate, of
#'     predicted payments of the reported claims}
#'   \item{ibnr, ibnr_details}{`NULL` until [ibnr_reserve()] adds the IBNR component and
#'     its details (projected counts, factors, pooled mean payments)}
#'   \item{counts}{the incremental reporting-count triangle of the panel ([claim_counts()]),
#'     used by [ibnr_reserve()] by default}
#'   \item{claim_level}{the completed claim-level payment matrix, one row per reported claim
#'     (columns `X0, ..., XL`): for a single-tree fit its completion, for a bagged fit the mean
#'     of the completed replicates; observed cells keep their observed values. Its future cells add
#'     up to the reported-claim prediction, but they are not a reliable allocation of that reserve
#'     between the claims: every reported claim is completed, open or closed, and the trees do not
#'     know the settlement status, so on the shipped portfolio a third of the reserve falls on
#'     claims already closed while the open claims are underpredicted (supplementary material of the
#'     JSS manuscript, Section S5). See [claim_reserves()]; for case-level allocation a model with a
#'     settlement layer, such as \pkg{hirem}, is needed.}
#'   \item{claim_ids, replicate_ids}{identifiers of the completed claims and of the columns}
#'   \item{claim_acc_years, claim_rep_delays}{the accident year and reporting delay of every
#'     completed claim, which [backtest()] checks against its truth tables}
#'   \item{mapping, beyond_L_ids}{the column names of the panel's input tables, which [backtest()]
#'     uses as its defaults, and the claims the panel set aside with `beyond_L = "drop"`}
#'   \item{t, L, accident_years, B, estimator, call}{metadata; `estimator` is `"tree"`,
#'     `"tree (R simulated completions)"`, `"bagging"` or `"bagging (simulated completion)"`, with
#'     `" (mean of replicates)"` (bagging) or `"tree (mean of R simulated completions)"` after
#'     `type = "mean"`}
#'   \item{completion, variability}{the completion mode (`"mean"` or `"draw"`) and the sources of
#'     variability among the columns: none for a single-tree fit with `completion = "mean"`;
#'     `"simulated payments"` for the `R` simulated completions of one fitted model (process
#'     variability given the fitted model, no estimation variability); `"bootstrap resampling"` for
#'     the replicates of [bag_reserve()]; both for a bagged fit with `completion = "draw"`, whose
#'     replicate `b` is one completion simulated from the `b`-th set of bootstrap trees (its
#'     estimation part is the variation of one set of unpruned bootstrap trees, with the samples
#'     drawn per development year, not that of the bagged mean). The `print()`, `summary()`,
#'     `plot()` and [backtest()] methods describe the columns accordingly. In no case do the
#'     columns contain the uncertainty of the IBNR claim counts or of the model itself.}
#'   \item{provenance}{the fitting settings, the fitting random-number information (`rng`), the
#'     package versions, the `completion` and, for simulated completion, `R` and the random-number
#'     information of the simulation (`completion_rng`: its `seed`, or with `seed = NULL` the
#'     generator state the draws started from)}
#' }
#' @seealso [as_triangle()], [calendar_year_payments()], [summary.reserve_prediction()],
#'   [plot.reserve_prediction()], [backtest()]
#' @aliases reserve_prediction
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' # fit on the claims of one type, then complete another portfolio with the same layout:
#' # the same features in the same order; factor levels must be known to the fit
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' fit <- reserve_tree(panel, seed = 1)
#' cl2 <- synthetic_claims[synthetic_claims$Type == "4" & synthetic_claims$AccWeekday != "Sun", ]
#' pay2 <- synthetic_payments[synthetic_payments$Id %in% cl2$Id, ]
#' other <- claims_panel(cl2, pay2, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' levels(other$claims$AccWeekday)          # a subset of the fit's levels is re-levelled
#' pred2 <- predict(fit, newdata = other)
#' calendar_year_payments(pred2)[c(1, 10), ]
#' @export
predict.reserve_tree <- function(object, newdata = NULL, type = c("replicates", "mean"),
                                 completion = c("mean", "draw"), R = 100L, seed = NULL, ...) {
  check_unused_dots(list(...), "predict", c("newdata", "type", "completion", "R", "seed"))
  type <- match.arg(type)
  completion <- match.arg(completion)
  # R and seed belong to the simulated completion: silently ignoring them would return the plug-in
  # prediction to a caller who asked for simulations
  if (completion == "mean" && (!missing(R) || !missing(seed))) {
    given <- c("`R`", "`seed`")[c(!missing(R), !missing(seed))]
    stop(paste(given, collapse = " and "), " given with completion = \"mean\": ",
         if (length(given) > 1L) "they apply" else "it applies", " to completion = \"draw\" only; pass ",
         "completion = \"draw\" for simulated completions", call. = FALSE)
  }
  panel <- if (is.null(newdata)) object$panel else check_newdata(object, newdata)
  warn_unseen_levels(panel, object$train_levels)
  estimator <- "tree"
  if (completion == "mean") {
    P <- complete_payments(panel, object$fits, learner_predict_fun(object$learner))
  } else {
    R <- as_scalar_integer(R, "R")
    if (R < 1L) stop("`R` must be at least 1", call. = FALSE)
    draw_fun <- if (!is.null(object$learner)) {
      if (!is.function(object$learner$simulate))
        stop("completion = \"draw\" needs a learner with a `simulate(model, newdata)` function ",
             "(see two_part_learner())", call. = FALSE)
      learner_simulate_fun(object$learner)
    } else {
      if (is.null(object$leaf_pools) || any(vapply(object$leaf_pools, is.null, TRUE)))
        stop("this fit holds no leaf responses (fitted by an older version); refit it", call. = FALSE)
      draw_period_rpart(object$leaf_pools)
    }
    if (!is.null(seed)) seed <- as_scalar_integer(seed, "seed")
    rng <- begin_rng(seed)
    on.exit(rng$restore(), add = TRUE)
    cells <- future_cells(panel)
    reported <- matrix(NA_real_, nrow(cells), R, dimnames = list(NULL, paste0("b", seq_len(R))))
    P_sum <- 0
    for (r in seq_len(R)) {
      P_r <- complete_payments(panel, object$fits, draw_fun, "simulated completion")
      reported[, r] <- aggregate_triangle(panel, P_r)[cells$index]
      P_sum <- P_sum + P_r
    }
    P <- P_sum / R
    estimator <- paste0("tree (", R, " simulated completions)")
    # provenance: `rng` is the fitting generator (cross-validation folds of the trees),
    # `completion_rng` the one of the simulated completions, with its seed or, for seed = NULL,
    # the generator state the draws started from
    pred <- new_reserve_prediction(panel, reported, estimator = estimator, claim_level = P,
                                   call = match.call(), completion = "draw",
                                   provenance = list(control = object$control, prune = object$prune,
                                                     rng = object$rng, versions = object$versions,
                                                     completion = "draw", R = R,
                                                     completion_rng = rng$info))
    return(if (type == "mean") collapse_mean(pred) else pred)
  }
  cells <- future_cells(panel)
  reported <- matrix(aggregate_triangle(panel, P)[cells$index], ncol = 1L,
                     dimnames = list(NULL, "b1"))
  new_reserve_prediction(panel, reported, estimator = estimator, claim_level = P,
                         call = match.call(), completion = "mean",
                         provenance = list(control = object$control, prune = object$prune,
                                           rng = object$rng, versions = object$versions,
                                           completion = "mean"))
}

# Checks a panel handed to predict(newdata = ) against the fit's schema: same horizon, same
# features in the same order, same classes, and factor levels known to the fit. A factor whose
# levels are a subset of the fit's levels is re-levelled to the fit's levels; a level the fit
# never saw is an error, because the trees have no branch for it.
check_newdata <- function(object, newdata) {
  check_panel(newdata, "newdata")
  if (newdata$L != object$L)
    stop("`newdata` has L = ", newdata$L, " but the fit was made with L = ", object$L,
         call. = FALSE)
  # an earlier valuation date of the same claims: the trees have seen their later payments
  if (newdata$t < object$t && !is.null(object$panel)) {
    shared <- sum(newdata$claims$id %in% object$panel$claims$id)
    if (shared)
      warning("`newdata` is valued at t = ", newdata$t, ", before the fit's t = ", object$t, ", and shares ", shared,
              " claim(s) with the fitted panel: the trees were trained on their payments after t = ", newdata$t,
              ", so a back-test of this prediction uses information not available at that date. For an honest ",
              "back-test, fit the panel valued at t = ", newdata$t, call. = FALSE)
  }
  if (!identical(newdata$features, object$features))
    stop("`newdata` must have the same features in the same order: ",
         paste(object$features, collapse = ", "), call. = FALSE)
  for (f in object$features) {
    lv_fit <- object$feature_levels[[f]]
    x <- newdata$claims[[f]]
    if (is.null(lv_fit)) {
      if (is.factor(x))
        stop("feature `", f, "` is a factor in `newdata` but was numeric or logical in the fit",
             call. = FALSE)
      x_fit <- object$panel$claims[[f]]
      if (!is.null(x_fit) && is.logical(x_fit) != is.logical(x))
        stop("feature `", f, "` is ", if (is.logical(x)) "logical" else "numeric",
             " in `newdata` but was ", if (is.logical(x_fit)) "logical" else "numeric",
             " in the fit", call. = FALSE)
      next
    }
    if (!is.factor(x))
      stop("feature `", f, "` must be a factor in `newdata`, as in the fit", call. = FALSE)
    unseen <- setdiff(levels(x)[tabulate(x, nbins = nlevels(x)) > 0L], lv_fit)
    if (length(unseen))
      stop("feature `", f, "` has level(s) unknown to the fit: ", paste(unseen, collapse = ", "),
           call. = FALSE)
    if (!identical(levels(x), lv_fit)) {
      newdata$claims[[f]] <- factor(as.character(x), levels = lv_fit, ordered = is.ordered(x))
      newdata$feature_levels[[f]] <- lv_fit
    }
  }
  newdata
}

# reported: matrix of predicted reported-claim payments, future cells (in the order of
# future_cells(panel)) by replicates
new_reserve_prediction <- function(panel, reported, estimator, claim_level = NULL,
                                   call = NULL, provenance = list(), completion = "mean") {
  obs <- as_triangle(panel, what = "paid")
  cells <- future_cells(panel)
  if (nrow(reported) != nrow(cells)) stop("internal: cell count mismatch", call. = FALSE)
  if (is.null(colnames(reported))) colnames(reported) <- paste0("b", seq_len(ncol(reported)))
  replicate_ids <- colnames(reported)
  structure(list(
    observed = obs, cells = cells, reported = reported, ibnr = NULL,
    claim_level = claim_level, claim_ids = panel$claims$id,
    claim_acc_years = panel$claims$acc_year, claim_rep_delays = panel$claims$rep_delay,
    counts = claim_counts(panel),
    t = panel$t, L = panel$L, accident_years = panel$accident_years,
    mapping = panel$mapping, beyond_L_ids = panel$beyond_L_ids,
    B = ncol(reported), replicate_ids = replicate_ids,
    estimator = estimator, completion = completion,
    variability = prediction_variability(estimator, completion),
    provenance = provenance, call = call),
    class = "reserve_prediction")
}

# What the columns of a prediction are and which sources of variability they contain, from the
# estimator and the completion mode (both stored in the prediction; inferred from the estimator
# label for predictions made by earlier versions, which stored neither):
#   single tree, completion "mean"  one column, no variability
#   single tree, completion "draw"  R simulated completions of one fitted model: process
#                                   variability given the fitted model, no estimation variability
#   bagging, completion "mean"      B bootstrap replicates: resampling variation of the trees
#   bagging, completion "draw"      B bootstrap replicates, each completed by simulation: both
prediction_variability <- function(estimator, completion) {
  bag <- startsWith(estimator, "bagging")
  c(if (bag) "bootstrap resampling", if (identical(completion, "draw")) "simulated payments")
}

pred_info <- function(x) {
  est <- if (is.null(x$estimator)) "" else x$estimator
  completion <- x$completion
  if (is.null(completion)) completion <- if (grepl("simulat", est, fixed = FALSE)) "draw" else "mean"
  kind <- if (startsWith(est, "bagging")) "bagging" else "tree"
  variability <- x$variability
  if (is.null(variability)) variability <- prediction_variability(est, completion)
  collapsed <- identical(x$replicate_ids, "mean") || grepl("(mean of", est, fixed = TRUE)
  draw <- completion == "draw"
  list(kind = kind, completion = completion, collapsed = collapsed, variability = variability,
       fit = if (kind == "tree") paste0("single-tree fit", if (draw) ", simulated completion")
             else paste0("bagged fit", if (draw) ", simulated completion"),
       point = if (kind == "tree") (if (draw) "mean of the simulated completions" else "single-tree prediction")
               else (if (draw) "bagged mean (simulated completion)" else "bagged mean"),
       columns = if (kind == "tree" && draw) "simulated completions" else if (kind == "bagging") "replicates"
                 else "prediction column",
       quantiles = if (kind == "tree" && draw) "simulation quantiles" else "replicate quantiles",
       spread = if (kind == "tree" && draw)
                  paste("process variability of payments simulated from one fitted model, given that model",
                        "(no estimation variability)")
                else if (kind == "bagging" && draw)
                  paste("the bootstrap resampling variation of one set of bootstrap trees (samples drawn per",
                        "development year) and the payments of one completion simulated from them")
                else if (kind == "bagging")
                  "bootstrap resampling variation of the bagged trees (no payment noise is simulated)"
                else "none (a single prediction)")
}

#' @export
print.reserve_prediction <- function(x, ...) {
  info <- pred_info(x)
  cat("Reserve prediction (", info$fit, "): ", nrow(x$cells), " future cells; ",
      if (info$collapsed) paste0(info$point, ", the distribution of the ", info$columns, " not retained") else
        if (x$B > 1L) paste0(info$columns, " retained: ", if (info$kind == "tree") "R" else "B", " = ", x$B)
        else "one prediction column",
      "\n  amounts: reported-claim payments", if (!is.null(x$ibnr)) " + IBNR component" else " only",
      "\n", sep = "")
  if (x$B > 1L)
    cat("  spread of the ", info$columns, ": ", info$spread, "\n", sep = "")
  cy <- calendar_year_payments(x, component = if (is.null(x$ibnr)) "reported" else "total")
  m <- rowMeans(cy)
  over <- if (x$B > 1L || info$collapsed) paste0(" (", info$point, ")") else ""
  cat("  next year (", x$t + 1L, "): ", format(round(m[1L]), big.mark = ","), over, "\n",
      "  total through development year ", x$L, ": ",
      format(round(m[length(m)]), big.mark = ","), over, "\n", sep = "")
  # one line on the plug-in recursion, whose multi-year totals can be biased (see ?bag_reserve)
  if (info$completion == "mean" && length(m) > 2L)
    cat("  plug-in completion: the years after next can be biased; completion = \"draw\" is recommended",
        " for totals\n", sep = "")
  invisible(x)
}

#' @rdname as_triangle
#' @param component `"reported"`, `"ibnr"` or `"total"`; without an IBNR component
#'   `"total"` falls back to `"reported"`, with a warning.
#' @param replicate Replicate to display, or `"mean"` for the average over replicates.
#' @export
as_triangle.reserve_prediction <- function(x, component = c("reported", "ibnr", "total"),
                                           replicate = "mean", ...) {
  check_unused_dots(list(...), "as_triangle", c("component", "replicate"))
  component <- match.arg(component)
  if (component == "total" && is.null(x$ibnr)) {   # as in calendar_year_payments(), but asked for explicitly
    warning("the prediction has no IBNR component: the triangle shows the reported-claim payments only ",
            "(attach the IBNR component with ibnr_reserve())", call. = FALSE)
    component <- "reported"
  }
  M <- bare_triangle(x$observed)
  vals <- component_matrix(x, component)
  if (!identical(replicate, "mean")) {
    ok <- length(replicate) == 1L && !is.na(replicate) &&
      (is.numeric(replicate) && replicate == round(replicate) && replicate >= 1 && replicate <= ncol(vals) ||
       is.character(replicate) && replicate %in% colnames(vals))
    if (!ok)
      stop("`replicate` must be \"mean\", a column number from 1 to ", ncol(vals), " or one of the column names (",
           paste(utils::head(colnames(vals), 3L), collapse = ", "), if (ncol(vals) > 3L) ", ..." else "", ")",
           call. = FALSE)
  }
  v <- if (identical(replicate, "mean")) rowMeans(vals) else vals[, replicate]
  if (component == "ibnr") M[] <- 0                    # IBNR has no observed part
  M[x$cells$index] <- v
  new_triangle(M, x$t, component)
}

component_matrix <- function(x, component) {
  switch(component,
         reported = x$reported,
         ibnr = if (is.null(x$ibnr)) stop("no IBNR component; see ibnr_reserve()", call. = FALSE)
                else x$ibnr,
         total = if (is.null(x$ibnr)) stop("no IBNR component; see ibnr_reserve()", call. = FALSE)
                 else x$reported + x$ibnr)
}

#' Payments by future calendar year
#'
#' Sums the predicted future cells by calendar year (`acc_year + dev`), i.e. along the
#' diagonals of the run-off triangle: the first row is next year's payments, the last row
#' the total of all future payments through development year `L`.
#'
#' @param x A [reserve_prediction] (or a [chain_ladder] object, see
#'   [calendar_year_payments.chain_ladder()]).
#' @param ... Not used; an argument given here is an error (for a [chain_ladder] object, which
#'   has a single component, any argument is).
#' @param component Component to aggregate; the default `"total"` sums reported-claim and
#'   IBNR payments when an IBNR component is attached and otherwise uses the reported-claim
#'   payments; `"ibnr"` requires [ibnr_reserve()] to have been applied.
#' @return A matrix with one row per future calendar year (row names are the years) plus a
#'   `total` row, one column per replicate (the column names of the prediction), and the
#'   attribute `"component"` holding the component actually aggregated.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)), claim_counts(panel))
#' cbind(reported = calendar_year_payments(res, "reported")[, 1],
#'       ibnr = calendar_year_payments(res, "ibnr")[, 1],
#'       total = calendar_year_payments(res, "total")[, 1])
#' @export
calendar_year_payments <- function(x, ...) UseMethod("calendar_year_payments")

#' @rdname calendar_year_payments
#' @export
calendar_year_payments.reserve_prediction <- function(x, component = c("total", "reported", "ibnr"),
                                                      ...) {
  check_unused_dots(list(...), "calendar_year_payments", "component")
  component <- match.arg(component)
  if (component == "total" && is.null(x$ibnr)) component <- "reported"
  vals <- component_matrix(x, component)
  years <- sort(unique(x$cells$calendar))
  out <- rowsum(vals, group = factor(x$cells$calendar, levels = years), reorder = TRUE)
  out <- rbind(out, total = colSums(out))
  rownames(out) <- c(years, "total")
  attr(out, "component") <- component
  out
}

#' @rdname calendar_year_payments
#' @export
calendar_year_payments.default <- function(x, ...) {
  stop("`x` must be a reserve_prediction or a chain_ladder object", call. = FALSE)
}

#' Data frames of predictions and back-tests
#'
#' `as.data.frame()` on a [reserve_prediction] gives one row per future cell (accident
#' year, development year, calendar year) with the mean over the replicates of the
#' reported-claim payments and, when attached, of the IBNR component and the total. On a
#' `reserve_backtest` it stacks the per-component tables of [backtest()] with a
#' `component` column.
#'
#' @param x A [reserve_prediction] or a `reserve_backtest`.
#' @param row.names,optional Unused, for compatibility with the generic.
#' @param ... Unused.
#' @return A data frame.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)))
#' head(as.data.frame(res))
#' @export
as.data.frame.reserve_prediction <- function(x, row.names = NULL, optional = FALSE, ...) {
  out <- x$cells[, c("acc_year", "dev", "calendar")]
  out$reported <- rowMeans(x$reported)
  if (!is.null(x$ibnr)) {
    out$ibnr <- rowMeans(x$ibnr)
    out$total <- out$reported + out$ibnr
  }
  rownames(out) <- NULL
  out
}

#' @rdname as.data.frame.reserve_prediction
#' @export
as.data.frame.reserve_backtest <- function(x, row.names = NULL, optional = FALSE, ...) {
  cols <- unique(unlist(lapply(x$tables, names)))     # a reference adds columns to one table
  out <- do.call(rbind, lapply(names(x$tables), function(cmp) {
    tab <- x$tables[[cmp]]
    for (k in setdiff(cols, names(tab))) tab[[k]] <- NA_real_
    cbind(component = cmp, tab[cols], stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' Reserves of the individual reported claims
#'
#' Sums the completed payments of each reported claim over its development years after the
#' valuation date `t` (cells with `acc_year + dev > t`), up to development year `L`. For a
#' bagged prediction these are the bagged means; they add up to the mean reported-claim
#' total of [calendar_year_payments()].
#'
#' The per-claim values are consistent with the aggregate prediction, but they are not a
#' reliable allocation of the reserve between the claims, in particular between open and
#' closed claims. Following the source article, every reported claim is completed, including
#' claims already closed, and the trees see static features and the payment history only, not the
#' settlement status: on the shipped portfolio a third of the predicted reported-claim payments
#' falls on claims closed at the valuation date, and the open claims are underpredicted by about
#' 30% (supplementary material of the JSS manuscript, Section S5). Use the values to aggregate
#' over groups of claims with care, and not as case reserves; allocating a reserve to the open
#' claims needs a model with a settlement layer, such as \pkg{hirem}.
#'
#' @param x A [reserve_prediction] that holds a claim-level matrix (a single-tree prediction,
#'   or a bagged one with `claim_level = TRUE`).
#' @return A data frame with one row per reported claim: `id`, `acc_year` and `reserve`.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' cr <- claim_reserves(predict(reserve_tree(panel, seed = 1)))
#' summary(cr$reserve)
#' @export
claim_reserves <- function(x) {
  if (!inherits(x, "reserve_prediction"))
    stop("`x` must be a reserve_prediction", call. = FALSE)
  if (is.null(x$claim_level))
    stop("the prediction holds no claim-level matrix (fit with claim_level = TRUE)", call. = FALSE)
  acc <- x$claim_acc_years
  if (is.null(acc)) stop("the prediction was made by an older version; predict() again", call. = FALSE)
  future <- outer(acc, seq_len(ncol(x$claim_level)) - 1L, "+") > x$t
  data.frame(id = x$claim_ids, acc_year = acc,
             reserve = rowSums(ifelse(future, as.matrix(x$claim_level), 0)))
}
