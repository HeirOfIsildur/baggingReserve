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
#' pruned at the smallest cross-validated error (the simplest tree on ties), the pruning rule of
#' \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025), the source article (deliberately not
#' \pkg{rpart}'s customary one-standard-error rule, which prunes more and would turn more of the
#' late, mostly-zero development years into single-leaf trees).
#'
#' `predict()` completes the payment histories of all reported claims recursively one
#' development year ahead and returns a [reserve_prediction] with a single prediction
#' column; see [predict.reserve_tree()].
#'
#' @param panel A [claims_panel] with `L >= 1`.
#' @param control An [rpart::rpart.control()] object; the default is `cp = 0` with
#'   `xval = 10`.
#' @param prune `"min_xerror"` (default) prunes at the smallest cross-validated error
#'   (`xerror` of the complexity table; the simplest tree on ties; requires cross-validation,
#'   `xval >= 2` in `control`, and is an error otherwise);
#'   `"none"` keeps the full tree.
#' @param learner `NULL` (default) fits pruned \pkg{rpart} trees as described above. Otherwise a
#'   list with functions `fit(formula, data)` and `predict(model, newdata)` that replaces them
#'   for every development year (see [bag_reserve()]); `control` and `prune` are then ignored
#'   (with a warning when they are given) and not validated; the fit records `prune = "none"`.
#'   `fit(formula, data)` receives the model frame of one development year (the features,
#'   `rep_delay`, `X0, ..., Xl` and the response `X(l+1)`, one row per training claim) and returns
#'   a model; `predict(model, newdata)` receives the same columns without the response and must
#'   return a numeric vector (or one-column matrix) with one finite value per row, which is checked
#'   for every prediction; an optional `simulate(model, newdata)` with the same contract is used by
#'   `predict(fit, completion = "draw")`. [two_part_learner()] builds one from two models.
#' @param seed Optional integer seed for the cross-validation folds, which are random. It is
#'   set with `set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
#'   sample.kind = "Rejection")`, R's default generator, so that the fit does not depend on the
#'   session's [RNGkind()]; the caller's generator kind and state are restored on exit. With
#'   `NULL` the current generator (of whatever kind) is used, its state recorded in the result
#'   and advanced, as by any random draw in \R, so that repeated calls give different folds.
#'   (The stream allocation of [bag_reserve()] does not apply to single-tree fits.)
#' @return An object of class `c("reserve_tree", "reserve_fit")`: a list with the
#'   fitted trees (`fits`, one per development year, `fits[[l + 1]]` for development year
#'   `l`), the chosen complexity parameters (`cp_chosen`, `NA` where no pruning was
#'   applied), training sizes (`n_train`), `prune`, `features`, `feature_levels`, `L`,
#'   `t`, the panel, the control, the RNG information (`rng`), the package versions
#'   (`versions`) and the call. `summary()` returns the same information with a table of
#'   leaves and depths per development year. With a user-supplied `learner`, `fits` holds the
#'   learner's models, whatever `fit()` returned; `print()` and `summary()` do not inspect them
#'   and report the training sizes per development year (the tree columns and the control
#'   apply to the default learner only).
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
                         prune = c("min_xerror", "none"), seed = NULL, learner = NULL) {
  check_panel(panel)
  check_learner(learner)
  if (!is.null(learner)) warn_ignored_tree_args(missing(control), missing(prune))
  prune <- if (is.null(learner)) match.arg(prune) else "none"
  check_prune_cv(prune, control)
  if (panel$L < 1L) stop("nothing to predict: L must be at least 1", call. = FALSE)

  if (!is.null(seed)) seed <- as_scalar_integer(seed, "seed")
  rng <- begin_rng(seed)
  on.exit(rng$restore(), add = TRUE)

  L <- panel$L
  fits <- vector("list", L)
  leaf_pools <- vector("list", L)
  cp_chosen <- rep(NA_real_, L)
  n_train <- vapply(0:(L - 1L), function(l) length(period_rows_train(panel, l)), 1L)
  if (any(n_train < 2L)) stop_sparse_periods(n_train)
  constant <- logical(L)
  for (l in 0:(L - 1L)) {
    rows <- period_rows_train(panel, l)
    frame <- period_frame(panel, rows, l, response = TRUE)
    constant[l + 1L] <- length(unique(frame[[paste0("X", l + 1L)]])) == 1L
    if (!is.null(learner)) {
      fits[[l + 1L]] <- learner$fit(period_formula(l), frame)
      next
    }
    fit <- fit_period_rpart(frame, l, control)
    if (prune == "min_xerror") {
      cp_chosen[l + 1L] <- cp_min_xerror(fit)
      fit <- prune_min_xerror(fit)
    }
    leaf_pools[[l + 1L]] <- leaf_pool(fit)
    fits[[l + 1L]] <- slim_rpart(fit)
  }

  structure(list(
    fits = fits, leaf_pools = leaf_pools, cp_chosen = cp_chosen, n_train = n_train, constant_response = constant,
    train_levels = period_train_levels(panel), learner = learner,
    prune = prune, control = control, panel = panel,
    features = panel$features, feature_levels = panel$feature_levels, L = L, t = panel$t,
    rng = rng$info, call = match.call(),
    versions = package_versions()),
    class = c("reserve_tree", "reserve_fit"))
}

#' @export
print.reserve_tree <- function(x, ...) {
  user <- !is.null(x$learner)
  cat(if (user) "Reserving fit with a user-supplied learner: " else "Single-tree reserving fit: ",
      x$L, if (user) " models" else " trees", ", valuation year t = ", x$t, "\n",
      "  (period = development-year index)\n", sep = "")
  cat("  claims: ", format(nrow(x$panel$claims), big.mark = ","), "; features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n", sep = "")
  if (user) {
    # the learner's models can be anything its predict() understands (even an atomic value), so
    # nothing is read from them; the rpart control and pruning were not used
    cat("  learner: user-supplied (`control` and `prune` not used)\n")
    print(data.frame(period = 0:(x$L - 1L), n_train = x$n_train), row.names = FALSE)
  } else {
    cat("  control: cp = ", x$control$cp, ", xval = ", x$control$xval, ", minsplit = ",
        x$control$minsplit, "; pruning: ", x$prune, "\n", sep = "")
    print(data.frame(period = 0:(x$L - 1L), n_train = x$n_train,
                     cp = signif(x$cp_chosen, 4), leaves = tree_leaves(x$fits)), row.names = FALSE)
  }
  if (!is.null(x$constant_response)) note_constant_periods(x$constant_response)
  invisible(x)
}

# Leaves and depth of the default learner's rpart trees (NA for a fit without a frame). Only for
# fits made with the default learner: a user-supplied learner's models are not inspected.
tree_leaves <- function(fits)
  vapply(fits, function(f) if (is.null(f$frame)) NA_integer_ else sum(f$frame$var == "<leaf>"), 1L)
tree_depths <- function(fits)
  vapply(fits, function(f) if (is.null(f$frame)) NA_integer_ else as.integer(max(rpart_depth(f))), 1L)

# RNG bookkeeping of reserve_tree() and predict(completion = "draw"): optionally seed, and
# record what was used so that a run can be repeated. A seed is set with R's default generator
# kinds pinned, so that the result does not depend on the session's RNGkind(), and the caller's
# kind and state are restored on exit; without one the draws advance the caller's stream, as any
# random draw does.
begin_rng <- function(seed) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  old_kind <- RNGkind()
  if (!is.null(seed)) {
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    info <- list(seed = seed, kind = RNGkind())
  } else {
    if (!had_seed) stats::runif(1)            # initialise the generator so a state exists
    info <- list(seed = NULL, kind = RNGkind(),
                 state = get(".Random.seed", envir = globalenv(), inherits = FALSE))
  }
  list(info = info, restore = function() {
    if (is.null(seed)) return(invisible(NULL))
    if (had_seed) {
      assign(".Random.seed", old, envir = globalenv())      # encodes the old kind too
    } else {
      do.call(RNGkind, as.list(old_kind))     # sets the kind (and a seed) ...
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())             # ... which is removed again
    }
  })
}

#' @rdname reserve_tree
#' @param object A `reserve_tree` fit (for `summary`).
#' @param ... Not used; an argument given here is an error.
#' @export
summary.reserve_tree <- function(object, ...) {
  check_unused_dots(list(...), "summary")
  user <- !is.null(object$learner)
  periods <- data.frame(period = 0:(object$L - 1L), n_train = object$n_train)
  if (!user) {
    periods$cp <- object$cp_chosen
    periods$leaves <- tree_leaves(object$fits)
    periods$depth <- tree_depths(object$fits)
  }
  structure(list(
    L = object$L, t = object$t, n_claims = nrow(object$panel$claims),
    features = object$features, control = if (!user) object$control, prune = object$prune,
    learner = if (user) "user-supplied" else "rpart (default)",
    periods = periods, rng = object$rng, versions = object$versions), class = "summary.reserve_tree")
}

#' @export
print.summary.reserve_tree <- function(x, ...) {
  user <- identical(x$learner, "user-supplied")
  seed <- if (!is.null(x$rng$seed)) paste0("; seed ", x$rng$seed) else ""
  cat(if (user) "Reserving fit with a user-supplied learner: " else "Single-tree reserving fit: ",
      x$L, if (user) " models, " else " trees, ", format(x$n_claims, big.mark = ","),
      " claims, t = ", x$t, "\n  features: ", paste(c(x$features, "rep_delay"), collapse = ", "), "\n", sep = "")
  if (user) {
    cat("  learner: user-supplied (`control` and `prune` not used)", seed, "\n", sep = "")
  } else {
    cat("  control: cp = ", x$control$cp, ", xval = ", x$control$xval, ", minsplit = ",
        x$control$minsplit, ", minbucket = ", x$control$minbucket, "; pruning: ", x$prune, seed, "\n", sep = "")
  }
  print(x$periods, row.names = FALSE)
  invisible(x)
}

rpart_depth <- function(fit) {
  # depth of every node of an rpart frame from its node numbers
  floor(log2(as.integer(rownames(fit$frame))))
}

# control and prune configure the default rpart trees only; with a user-supplied learner a
# value given for them would be silently ignored, which the package avoids elsewhere
warn_ignored_tree_args <- function(control_missing, prune_missing) {
  given <- c(control = !control_missing, prune = !prune_missing)
  if (any(given))
    warning(paste0("`", names(given)[given], "`", collapse = " and "), " appl", if (sum(given) == 1L) "ies" else "y",
            " to the default rpart trees only and ", if (sum(given) == 1L) "is" else "are",
            " ignored with a user-supplied `learner`", call. = FALSE)
  invisible()
}

# prune = "min_xerror" reads the cross-validated error of the complexity table, which rpart computes
# only with at least two folds (xval = 0 and xval = 1 leave no xerror column, and the tree would stay
# unpruned while recorded as pruned); a vector xval assigns the folds explicitly
check_prune_cv <- function(prune, control) {
  xv <- control$xval
  if (prune == "min_xerror" && (is.null(xv) || (length(xv) == 1L && !isTRUE(xv >= 2))))
    stop("prune = \"min_xerror\" needs cross-validation with at least two folds: set xval >= 2 in `control`, ",
         "or prune = \"none\"", call. = FALSE)
  invisible()
}
