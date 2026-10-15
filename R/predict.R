#' Predicted future payments of the reported claims
#'
#' `predict()` on a [reserve_tree] completes the payment histories of all claims
#' reported by the valuation date, recursively one development year ahead: the tree for
#' development year `l` predicts `X_{l+1}` from the features, the reporting delay and
#' `X_0, ..., X_l`, where the cells after the valuation date among `X_0, ..., X_l` are
#' the predictions of the earlier trees. Following the source article, every reported
#' claim is completed, including claims recorded as closed; the claim status is not a
#' covariate unless it is named in `features`.
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
#'   the fitted panel: the trees then complete its cells after its own `t`. A factor level
#'   that the fit knows but that is absent from the training claims of the period in which
#'   a claim is completed gives a warning, here as when the fitted panel is completed: at
#'   each split on that feature the trees treat it as missing (surrogate splits, else the
#'   majority direction).
#' @param type `"replicates"` returns every replicate; `"mean"` averages the completed
#'   replicates (for a single-tree fit the two coincide).
#' @param ... Unused.
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
#'   \item{claim_level}{for a single-tree fit only: the completed claim-level payment matrix}
#'   \item{claim_ids, replicate_ids}{identifiers of the completed claims and of the columns}
#'   \item{t, L, accident_years, B, estimator, provenance, call}{metadata; `estimator` is
#'     `"tree"`, `"bagging"` or `"bagging (mean of replicates)"`}
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
predict.reserve_tree <- function(object, newdata = NULL, type = c("replicates", "mean"), ...) {
  type <- match.arg(type)
  panel <- if (is.null(newdata)) object$panel else check_newdata(object, newdata)
  warn_unseen_levels(panel, object$train_levels)
  P <- complete_payments(panel, object$fits, predict_period_rpart)
  cells <- future_cells(panel)
  reported <- matrix(aggregate_triangle(panel, P)[cells$index], ncol = 1L,
                     dimnames = list(NULL, "b1"))
  new_reserve_prediction(panel, reported, estimator = "tree", claim_level = P,
                         call = match.call(),
                         provenance = list(control = object$control, prune = object$prune,
                                           rng = object$rng, versions = object$versions))
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
                                   call = NULL, provenance = list()) {
  obs <- as_triangle(panel, what = "paid")
  cells <- future_cells(panel)
  if (nrow(reported) != nrow(cells)) stop("internal: cell count mismatch", call. = FALSE)
  if (is.null(colnames(reported))) colnames(reported) <- paste0("b", seq_len(ncol(reported)))
  replicate_ids <- colnames(reported)
  structure(list(
    observed = obs, cells = cells, reported = reported, ibnr = NULL,
    claim_level = claim_level, claim_ids = panel$claims$id, counts = claim_counts(panel),
    t = panel$t, L = panel$L, accident_years = panel$accident_years,
    B = ncol(reported), replicate_ids = replicate_ids,
    estimator = estimator, provenance = provenance, call = call),
    class = "reserve_prediction")
}

#' @export
print.reserve_prediction <- function(x, ...) {
  collapsed <- grepl("mean of replicates", x$estimator, fixed = TRUE)
  label <- if (identical(x$estimator, "tree")) "single-tree fit" else
    if (identical(x$estimator, "bagging")) "bagged fit" else x$estimator
  cat("Reserve prediction (", label, "): ", nrow(x$cells), " future cells; ",
      if (collapsed) "bagged mean, replicate distribution not retained" else
        if (x$B > 1L) paste0("replicates retained: B = ", x$B) else "one prediction column",
      "\n  amounts: reported-claim payments", if (!is.null(x$ibnr)) " + IBNR component" else " only",
      "\n", sep = "")
  cy <- calendar_year_payments(x, component = if (is.null(x$ibnr)) "reported" else "total")
  m <- rowMeans(cy)
  over <- if (x$B > 1L) " (bagged mean)" else if (collapsed) " (bagged mean)" else ""
  cat("  next year (", x$t + 1L, "): ", format(round(m[1L]), big.mark = ","), over, "\n",
      "  total through development year ", x$L, ": ",
      format(round(m[length(m)]), big.mark = ","), over, "\n", sep = "")
  invisible(x)
}

#' @rdname as_triangle
#' @param component `"reported"`, `"ibnr"` or `"total"`; without an IBNR component
#'   `"total"` falls back to `"reported"`.
#' @param replicate Replicate to display, or `"mean"` for the average over replicates.
#' @export
as_triangle.reserve_prediction <- function(x, component = c("reported", "ibnr", "total"),
                                           replicate = "mean", ...) {
  component <- match.arg(component)
  if (component == "total" && is.null(x$ibnr)) component <- "reported"   # as in calendar_year_payments()
  M <- bare_triangle(x$observed)
  vals <- component_matrix(x, component)
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
#' @param ... Unused.
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
  out <- do.call(rbind, lapply(names(x$tables), function(cmp)
    cbind(component = cmp, x$tables[[cmp]], stringsAsFactors = FALSE)))
  rownames(out) <- NULL
  out
}
