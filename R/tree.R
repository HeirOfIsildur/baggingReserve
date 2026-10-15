#' Fit one regression tree per development year
#'
#' Fits a single-tree reserving model: for every development year `l = 0, ..., L - 1`
#' (called `period` in the package's arguments and output), a regression tree
#' (\pkg{rpart}, ANOVA method) that predicts the payment of the next development year,
#' `X_{l+1}`, of a reported claim from its static features, its reporting delay and its
#' payments `X_0, ..., X_l`. The tree for development year `l` is trained on the claims
#' reported by development year `l` whose payment in development year `l + 1` is already
#' observed at the valuation date (reporting delay `<= l` and `acc_year + l + 1 <= t`);
#' every fitted development year needs at least two such claims. Each tree is grown with
#' the given control (default `cp = 0` with ten-fold cross-validation) and, by default,
#' pruned at the first minimum of the cross-validated error, the pruning rule of
#' \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025), the source article.
#'
#' `predict()` completes the payment histories of all reported claims recursively one
#' development year ahead and returns a [reserve_prediction] with a single prediction
#' column; see [predict.reserve_tree()].
#'
#' @param panel A [claims_panel] with `L >= 1`.
#' @param control An [rpart::rpart.control()] object; the default is `cp = 0` with
#'   `xval = 10`.
#' @param prune `"min_xerror"` (default) prunes at the first minimum of the
#'   cross-validated error (requires `xval > 0`); `"none"` keeps the full tree.
#' @param seed Optional integer passed to [set.seed()] before the first fit (the
#'   cross-validation folds are random). With `NULL` the current random number generator
#'   state is used and recorded; the caller's state is restored on exit, so two such calls
#'   with nothing drawn in between give the same folds.
#' @return An object of class `c("reserve_tree", "reserve_fit")`: a list with the
#'   fitted trees (`fits`, one per development year, `fits[[l + 1]]` for development year
#'   `l`), the chosen complexity parameters (`cp_chosen`, `NA` where no pruning was
#'   applied), training sizes (`n_train`), `prune`, `features`, `feature_levels`, `L`,
#'   `t`, the panel, the control, the RNG information (`rng`), the package versions
#'   (`versions`) and the call. `summary()` returns the same information with a table of
#'   leaves and depths per development year.
#' @seealso [bag_reserve()] for the bagged version, [predict.reserve_tree()].
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' # a small subset for a quick example: the claims of one type
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' tree <- reserve_tree(panel, seed = 1)
#' tree
#' pred <- predict(tree)
#' calendar_year_payments(pred)
#' @export
reserve_tree <- function(panel, control = rpart::rpart.control(cp = 0, xval = 10),
                         prune = c("min_xerror", "none"), seed = NULL) {
  check_panel(panel)
  prune <- match.arg(prune)
  if (prune == "min_xerror" && isTRUE(control$xval == 0))
    stop("prune = \"min_xerror\" needs cross-validation; set xval > 0 in `control` or prune = \"none\"",
         call. = FALSE)
  if (panel$L < 1L) stop("nothing to predict: L must be at least 1", call. = FALSE)

  rng <- begin_rng(seed)
  on.exit(rng$restore(), add = TRUE)

  L <- panel$L
  fits <- vector("list", L)
  cp_chosen <- rep(NA_real_, L)
  n_train <- integer(L)
  for (l in 0:(L - 1L)) {
    rows <- period_rows_train(panel, l)
    n_train[l + 1L] <- length(rows)
    if (length(rows) < 2L)
      stop("period ", l, ": only ", length(rows), " training claim(s)", call. = FALSE)
    frame <- period_frame(panel, rows, l, response = TRUE)
    fit <- fit_period_rpart(frame, l, control)
    if (prune == "min_xerror") {
      cp_chosen[l + 1L] <- cp_min_xerror(fit)
      fit <- prune_min_xerror(fit)
    }
    fits[[l + 1L]] <- slim_rpart(fit)
  }

  structure(list(
    fits = fits, cp_chosen = cp_chosen, n_train = n_train,
    prune = prune, control = control, panel = panel,
    features = panel$features, feature_levels = panel$feature_levels, L = L, t = panel$t,
    rng = rng$info, call = match.call(),
    versions = c(rpart = as.character(utils::packageVersion("rpart")),
                 R = R.version.string)),
    class = c("reserve_tree", "reserve_fit"))
}

#' @export
print.reserve_tree <- function(x, ...) {
  cat("Single-tree reserving fit: ", x$L, " trees, valuation year t = ", x$t, "\n",
      "  (period = development-year index)\n", sep = "")
  cat("  claims: ", format(nrow(x$panel$claims), big.mark = ","), "; features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n", sep = "")
  cat("  control: cp = ", x$control$cp, ", xval = ", x$control$xval, ", minsplit = ",
      x$control$minsplit, "; pruning: ", x$prune, "\n", sep = "")
  leaves <- vapply(x$fits, function(f) sum(f$frame$var == "<leaf>"), 1L)
  print(data.frame(period = 0:(x$L - 1L), n_train = x$n_train,
                   cp = signif(x$cp_chosen, 4), leaves = leaves), row.names = FALSE)
  invisible(x)
}

# RNG bookkeeping shared by reserve_tree() and bag_reserve(): optionally seed, record what
# was used so that a run can be repeated, and always restore the caller's state on exit.
begin_rng <- function(seed) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  if (!is.null(seed)) {
    set.seed(seed)
    info <- list(seed = seed, kind = RNGkind())
  } else {
    if (!had_seed) stats::runif(1)            # initialise the generator so a state exists
    info <- list(seed = NULL, kind = RNGkind(),
                 state = get(".Random.seed", envir = globalenv(), inherits = FALSE))
  }
  list(info = info, restore = function() {
    if (had_seed) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  })
}

#' @rdname reserve_tree
#' @param object A `reserve_tree` fit (for `summary`).
#' @param ... Unused.
#' @export
summary.reserve_tree <- function(object, ...) {
  structure(list(
    L = object$L, t = object$t, n_claims = nrow(object$panel$claims),
    features = object$features, control = object$control, prune = object$prune,
    periods = data.frame(period = 0:(object$L - 1L), n_train = object$n_train,
                         cp = object$cp_chosen,
                         leaves = vapply(object$fits, function(f) sum(f$frame$var == "<leaf>"), 1L),
                         depth = vapply(object$fits, function(f) as.integer(max(rpart_depth(f))), 1L)),
    rng = object$rng, versions = object$versions), class = "summary.reserve_tree")
}

#' @export
print.summary.reserve_tree <- function(x, ...) {
  cat("Single-tree reserving fit: ", x$L, " trees, ", format(x$n_claims, big.mark = ","),
      " claims, t = ", x$t, "\n  features: ", paste(c(x$features, "rep_delay"), collapse = ", "),
      "\n  control: cp = ", x$control$cp, ", xval = ", x$control$xval, ", minsplit = ",
      x$control$minsplit, ", minbucket = ", x$control$minbucket, "; pruning: ", x$prune,
      if (!is.null(x$rng$seed)) paste0("; seed ", x$rng$seed) else "", "\n", sep = "")
  print(x$periods, row.names = FALSE)
  invisible(x)
}

rpart_depth <- function(fit) {
  # depth of every node of an rpart frame from its node numbers
  floor(log2(as.integer(rownames(fit$frame))))
}
