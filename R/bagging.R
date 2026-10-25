#' Fit bootstrap regression trees and complete reserve replicates
#'
#' Bootstrap aggregating of the trees of [reserve_tree()]. For every replicate
#' `b = 1, ..., B` and every development year `l` (its `period`), a bootstrap sample of the
#' training claims of that development year is drawn (with replacement, duplicates kept)
#' and a regression tree is grown on it (by default unpruned, `cp = 0`, no
#' cross-validation). Replicate `b` then completes the payment histories of all reported
#' claims with its `L` trees, one per development year, recursively one development year
#' ahead. The `B` completed replicates are the output: the point estimate is the bagged
#' mean of the completed replicate predictions, and their spread describes the variation
#' under this resampling of the predictions of one set of bootstrap trees (unpruned by
#' default, unlike the pruned trees of [reserve_tree()]). It is not a predictive distribution
#' of the future payments: the numbers of unreported claims are projected once and held
#' fixed, and with the default `completion = "mean"` no payment noise is simulated. With
#' `completion = "draw"` each replicate is completed once by payments simulated from its own
#' trees, so its spread adds process variability to this resampling variation. It is still not a
#' predictive distribution: the estimation part is the variation of one set of (unpruned)
#' bootstrap trees, a noisier estimator than the bagged mean that is reported; the bootstrap
#' samples are drawn independently for each development year, not once per claim, so they do
#' not reproduce how the errors of successive trees are linked through the same claims; and the
#' IBNR counts stay fixed. The prediction records which case applies (`variability`), and its
#' `print()`, `summary()`, `plot()` and [backtest()] output says so.
#'
#' **Completion.** The default `completion = "mean"` is the source article's method, kept so
#' that the package reproduces it. For totals beyond next year we recommend
#' `completion = "draw"`: plugging in means is biased for the later years when the occurrence of
#' a payment depends on the payment history (see the argument `completion`).
#'
#' The default `B = 100` suffices for the bagged mean: its Monte Carlo standard error is the
#' replicate standard deviation divided by `sqrt(B)`, small against the errors back-tests
#' measure. Extreme replicate quantiles and complete out-of-bag coverage need more; the JSS
#' manuscript uses `B = 1000`.
#'
#' A factor level absent from the training claims of a development year gives a warning
#' (see [predict.reserve_tree()]). A rare level can also be missing from the bootstrap
#' sample of one replicate while present in the training claims; that replicate's tree
#' then treats it as missing without a warning, which is part of the resampling variation.
#'
#' The out-of-bag (OOB) score of development year `l` uses, for each training claim, the
#' trees of the replicates in which it was not sampled (its out-of-bag predictions, or
#' "votes"), predicts its observed `X_{l+1}` from its observed history, and compares:
#' `OOB_error = mean((y - OOB)^2)` over the covered claims and
#' `OOB_score = 1 - sum_covered (y - OOB)^2 / sum_all (y - mean(y))^2`, the denominator
#' running over all training claims of the development year (\enc{Janoušek}{Janousek} and
#' \enc{Pešta}{Pesta} 2025). It is a diagnostic of the one-year regressions given observed
#' histories, not a validation of the recursive forecast.
#'
#' Random numbers come from `"L'Ecuyer-CMRG"` streams allocated to (replicate, development
#' year) tasks, so the completed replicates do not depend on `ncores`, on the batching or on
#' the scheduling of the workers under the same software; the out-of-bag sums are
#' accumulated batch by batch and can differ in their last floating-point digits. The
#' caller's generator kind and state are restored on exit (after the one draw that makes the
#' seed when `seed = NULL`). Replicates are streamed: each worker fits
#' the trees of a replicate, records the OOB votes, aggregates the predicted future cells
#' and discards the trees unless `keep_trees = TRUE`.
#'
#' @inheritParams reserve_tree
#' @param B Number of bootstrap replicates (a positive integer; default 100).
#' @param seed Integer seed of the stream allocation. `NULL` draws one from the current
#'   generator (advancing it, so that repeated calls differ) and records it in the result.
#' @param ncores Number of worker processes (forked on Unix, a socket cluster elsewhere), a
#'   positive integer; default 1, which runs serially. Values above the number of cores detected
#'   are reduced to it, with a message; when [parallel::detectCores()] cannot determine that
#'   number (it returns `NA` in some containers), the value is used as given. The value used is
#'   recorded (`ncores` of the result) and printed.
#' @param control [rpart::rpart.control()] for the bootstrap trees (default `cp = 0`,
#'   `xval = 0`).
#' @param prune `"none"` (default) or `"min_xerror"` (needs cross-validation, `xval >= 2` in
#'   `control`, and is an error otherwise). Ignored, and not checked, with a user-supplied `learner`.
#' @param oob Compute the out-of-bag diagnostics (default `TRUE`; with `FALSE` the `oob`
#'   element of the result is `NULL` and [oob_score()] fails).
#' @param keep_trees Keep the `B * L` fitted trees (and the bootstrap indices) in the
#'   result (default `FALSE`), which allows `predict(..., newdata = )` (for
#'   `completion = "mean"` only) and inspection at a large memory cost.
#' @param batch_size Replicates processed per worker task before OOB votes are returned;
#'   `NULL` (default) uses `max(1, ceiling(B / ncores / 2))`. The batching does not change
#'   the completed replicates.
#' @param claim_level Return the claim-level reserve: the mean over the replicates of the
#'   completed claim-by-development-year payment matrices (default `TRUE`; component
#'   `claim_level` of the prediction). It costs one matrix of the panel's size per worker.
#' @param learner `NULL` (default) grows \pkg{rpart} trees as described above. Otherwise a list
#'   with functions `fit(formula, data)`, returning a model for one development year from its
#'   bootstrap sample, and `predict(model, newdata)`, returning numeric predictions; the
#'   bootstrap, the recursion, the out-of-bag diagnostic and the random-number streams are those
#'   of the package. `control` and `prune` apply only to the default learner (given with a
#'   `learner`, they are ignored with a warning and not validated; the fit records `prune = "none"`),
#'   as in [reserve_tree()]. Every prediction of
#'   a learner (out-of-bag votes, plugged-in and simulated completion) is checked: a numeric vector
#'   with one finite value per row of `newdata`; see [reserve_tree()] and [two_part_learner()].
#' @param completion `"mean"` (default, the source article's method) completes each replicate by
#'   plugging in its predicted means; `"draw"` simulates each replicate's completion once, drawing
#'   every future payment from the training responses of the claim's leaf in that replicate's tree
#'   (for a user-supplied learner, from its `simulate()`), on a separate random-number substream,
#'   so the trees are those of `"mean"`. The replicate spread then adds the process variability of
#'   one simulated completion to the resampling variation of one set of bootstrap trees (see
#'   Details for what it still leaves out). For bagged fits the completion is chosen here, when
#'   fitting; [predict.bag_reserve()] has no `completion` argument (unlike
#'   [predict.reserve_tree()]) and refuses one that differs from the fit's; a fit with `"draw"`
#'   cannot predict `newdata` (the leaf responses of its trees are not kept).
#'   Plugging in means is biased for payments beyond next year when the occurrence of a payment
#'   depends on the payment history: in the correctly specified simulation of the JSS manuscript
#'   (Section 4.6) it overstated the total reserve of the reported claims by up to 54% for the
#'   bagged mean, a bias that does not vanish as the portfolio grows. Simulated completion removes
#'   this mechanism and most of the bias; what remained there was at most 1.7% for the bagged mean,
#'   whose unpruned bootstrap trees also overpredict next year's payments by about 1%. Next year's
#'   payments are the same in expectation under both completions. **For totals beyond next year
#'   we recommend `"draw"`.**
#' @param backend `"auto"` forks workers on Unix and uses a socket cluster elsewhere;
#'   `"fork"`, `"psock"` and `"serial"` force one backend. Results are the same for all of
#'   them (up to floating-point summation order in the OOB sums). Socket workers load the
#'   installed package; a version different from the one in use is an error. They are fresh R
#'   sessions, so for a user-supplied `learner` they are given the variables of the global
#'   environment that its functions refer to (found with [codetools::findGlobals()], also inside
#'   the functions among them) and the attached packages those functions use; an object a learner
#'   reaches otherwise (through [get()], a character string, a formula's environment) must be
#'   created inside its functions or carried in their environment, as [two_part_learner()] does,
#'   and a failure on the workers says so.
#' @return An object of class `c("bag_reserve", "reserve_fit")`: a list with the cached
#'   [reserve_prediction] of the fitted portfolio (`prediction`), the OOB table (`oob`,
#'   `NULL` when `oob = FALSE`), the trees and bootstrap indices (`trees`, `inbag`; `NULL`
#'   unless `keep_trees = TRUE`; `trees[[b]][[l + 1]]` is the tree of replicate `b` for
#'   development year `l`), `B`, `L`, `t`, `n_train`, `features`, `feature_levels`, the
#'   `control`, `prune`, `keep_trees`, `ncores`, `backend`, `elapsed_secs`, the RNG
#'   information (`rng`), the package versions (`versions`) and the call. Five replicates,
#'   as in the example, illustrate the interface; they are far too few for stable quantiles.
#' @seealso [predict.bag_reserve()], [oob_score()], [ibnr_reserve()]
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' # a small subset for a quick example: the claims of one type
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' bag <- bag_reserve(panel, B = 5, seed = 2001)
#' bag
#' oob_score(bag)
#' calendar_year_payments(predict(bag), "reported")
#' # simulated completion, recommended for totals beyond next year
#' bag_draw <- bag_reserve(panel, B = 5, seed = 2001, completion = "draw")
#' calendar_year_payments(predict(bag_draw), "reported")[c(1, 11), ]
#' # another learner for the one-year regressions, here a linear model
#' lm_learner <- list(fit = function(formula, data) lm(formula, data = data),
#'                    predict = function(model, newdata) predict(model, newdata = newdata))
#' bag_lm <- bag_reserve(panel, B = 5, seed = 2001, learner = lm_learner)
#' calendar_year_payments(predict(bag_lm), "reported")[c(1, 11), ]
#' @export
bag_reserve <- function(panel, B = 100L, seed = NULL, ncores = 1L,
                        control = rpart::rpart.control(cp = 0, xval = 0),
                        prune = c("none", "min_xerror"), oob = TRUE, keep_trees = FALSE,
                        batch_size = NULL, backend = c("auto", "fork", "psock", "serial"),
                        claim_level = TRUE, learner = NULL, completion = c("mean", "draw")) {
  check_learner(learner)
  if (!is.null(learner)) warn_ignored_tree_args(missing(control), missing(prune))
  completion <- match.arg(completion)
  if (completion == "draw" && !is.null(learner) && !is.function(learner$simulate))
    stop("completion = \"draw\" needs a learner with a `simulate(model, newdata)` function ",
         "(see two_part_learner())", call. = FALSE)
  pred_fun <- learner_predict_fun(learner)
  check_panel(panel)
  # with a user-supplied learner, prune and control are ignored (with the warning above) and not
  # validated, as by reserve_tree()
  prune <- if (is.null(learner)) match.arg(prune) else "none"
  backend <- match.arg(backend)
  B <- as_scalar_integer(B, "B"); if (B < 1L) stop("`B` must be at least 1", call. = FALSE)
  ncores <- as_scalar_integer(ncores, "ncores")
  if (ncores < 1L) stop("`ncores` must be at least 1 (1 runs serially)", call. = FALSE)
  # capped at the number of cores detected; when the number cannot be detected
  # (parallel::detectCores() is NA, as in some containers and sandboxes) the request is used as given
  avail <- detected_cores()
  if (!is.na(avail) && ncores > avail) {
    message("ncores = ", ncores, " is more than the ", avail, " cores detected: using ", avail, " worker process",
            if (avail > 1L) "es")
    ncores <- as.integer(avail)
  }
  check_prune_cv(prune, control)
  if (panel$L < 1L) stop("nothing to predict: L must be at least 1", call. = FALSE)
  t0 <- Sys.time()
  L <- panel$L

  ## RNG: one L'Ecuyer stream per replicate, one substream per period -------------------
  rng <- begin_rng_streams(seed, B)
  on.exit(rng$restore(), add = TRUE)

  ## training frames, shared by all replicates -----------------------------------------
  rows <- lapply(0:(L - 1L), function(l) period_rows_train(panel, l))
  n_train <- lengths(rows)
  if (any(n_train < 2L)) stop_sparse_periods(n_train)
  frames <- lapply(0:(L - 1L), function(l) period_frame(panel, rows[[l + 1L]], l, response = TRUE))
  # the out-of-bag votes are predictions like those of the completion: the learner receives the
  # predictors without the response, the columns of period_frame(response = FALSE) in its order
  x_cols <- lapply(0:(L - 1L), function(l) setdiff(names(frames[[l + 1L]]), paste0("X", l + 1L)))
  responses <- lapply(0:(L - 1L), function(l) frames[[l + 1L]][[paste0("X", l + 1L)]])
  constant <- vapply(responses, function(y) length(unique(y)) == 1L, TRUE)
  cells <- future_cells(panel)
  train_levels <- period_train_levels(panel)
  warn_unseen_levels(panel, train_levels)

  run_batch <- function(bs) {
    P_sum <- NULL                      # claim-level sum over the batch's completed replicates
    if (claim_level) { P_sum <- panel$payments; P_sum[] <- 0 }
    oob_sum <- if (oob) lapply(n_train, function(n) numeric(n)) else NULL
    oob_cnt <- if (oob) lapply(n_train, function(n) integer(n)) else NULL
    reported <- matrix(NA_real_, nrow(cells), length(bs))
    trees <- if (keep_trees) vector("list", length(bs)) else NULL
    inbag <- if (keep_trees) vector("list", length(bs)) else NULL
    for (k in seq_along(bs)) {
      b <- bs[k]
      s <- rng$streams[[b]]
      models <- vector("list", L)
      pools <- if (completion == "draw" && is.null(learner)) vector("list", L) else NULL
      idx_b <- if (keep_trees) vector("list", L) else NULL
      for (l in 0:(L - 1L)) {
        s <- parallel::nextRNGSubStream(s)
        assign(".Random.seed", s, envir = globalenv())
        n <- n_train[l + 1L]
        idx <- sample.int(n, n, replace = TRUE)
        if (is.null(learner)) {
          fit <- fit_period_rpart(frames[[l + 1L]][idx, , drop = FALSE], l, control)
          if (prune == "min_xerror") fit <- prune_min_xerror(fit)
          if (!is.null(pools)) pools[[l + 1L]] <- leaf_pool(fit)
          fit <- slim_rpart(fit, keep_cptable = FALSE)
        } else {
          fit <- learner$fit(period_formula(l), frames[[l + 1L]][idx, , drop = FALSE])
        }
        if (oob) {
          out <- which(tabulate(idx, nbins = n) == 0L)
          if (length(out)) {
            nd <- frames[[l + 1L]][out, x_cols[[l + 1L]], drop = FALSE]
            rownames(nd) <- NULL
            p <- check_learner_output(pred_fun(fit, nd), length(out), l, "out-of-bag prediction")
            oob_sum[[l + 1L]][out] <- oob_sum[[l + 1L]][out] + p
            oob_cnt[[l + 1L]][out] <- oob_cnt[[l + 1L]][out] + 1L
          }
        }
        models[[l + 1L]] <- fit
        if (keep_trees) idx_b[[l + 1L]] <- idx
      }
      if (completion == "mean") {
        P <- complete_payments(panel, models, pred_fun)
      } else {
        # one simulated completion per replicate, on its own substream after the L fitting
        # substreams, so the trees are those of completion = "mean"
        s <- parallel::nextRNGSubStream(s)
        assign(".Random.seed", s, envir = globalenv())
        draw_fun <- if (is.null(learner)) draw_period_rpart(pools) else learner_simulate_fun(learner)
        P <- complete_payments(panel, models, draw_fun, "simulated completion")
      }
      reported[, k] <- aggregate_triangle(panel, P)[cells$index]
      if (claim_level) P_sum <- P_sum + P
      if (keep_trees) { trees[[k]] <- models; inbag[[k]] <- idx_b }
    }
    list(reported = reported, oob_sum = oob_sum, oob_cnt = oob_cnt, trees = trees, inbag = inbag,
         P_sum = if (claim_level) P_sum else NULL)
  }

  if (is.null(batch_size)) batch_size <- max(1L, ceiling(B / max(ncores, 1L) / 2L))
  batch_size <- as_scalar_integer(batch_size, "batch_size")
  if (batch_size < 1L) stop("`batch_size` must be at least 1", call. = FALSE)
  batches <- split(seq_len(B), ceiling(seq_len(B) / batch_size))
  results <- run_parallel(batches, run_batch, ncores, backend, learner = learner)
  ok <- vapply(results, function(r) is.list(r) && is.matrix(r$reported), TRUE)
  if (!all(ok)) stop("worker(s) returned no result for batch(es) ",
                     paste(which(!ok), collapse = ", "), call. = FALSE)

  ## reduce in fixed order -----------------------------------------------------------------
  reported <- do.call(cbind, lapply(results, `[[`, "reported"))
  dimnames(reported) <- list(NULL, paste0("b", seq_len(B)))
  oob_tab <- NULL
  if (oob) {
    oob_sum <- lapply(seq_len(L), function(j) Reduce(`+`, lapply(results, function(r) r$oob_sum[[j]])))
    oob_cnt <- lapply(seq_len(L), function(j) Reduce(`+`, lapply(results, function(r) r$oob_cnt[[j]])))
    oob_tab <- oob_table(responses, oob_sum, oob_cnt, n_train)
  }
  trees <- inbag <- NULL
  if (keep_trees) {
    trees <- do.call(c, lapply(results, `[[`, "trees"))       # trees[[b]][[l + 1]]
    inbag <- do.call(c, lapply(results, `[[`, "inbag"))
  }

  P_mean <- if (claim_level) Reduce(`+`, lapply(results, `[[`, "P_sum")) / B else NULL
  prediction <- new_reserve_prediction(panel, reported,
                                       estimator = if (completion == "draw") "bagging (simulated completion)" else "bagging",
                                       claim_level = P_mean, completion = completion,
                                       provenance = bag_provenance(B, rng$info, control, prune, learner, completion,
                                                                   versions = package_versions()))
  structure(list(
    B = B, prediction = prediction, oob = oob_tab, trees = trees, inbag = inbag,
    n_train = n_train, train_levels = train_levels, learner = learner, completion = completion, constant_response = constant, control = control, prune = prune, keep_trees = keep_trees,
    panel = panel, features = panel$features, feature_levels = panel$feature_levels,
    L = L, t = panel$t, rng = rng$info, ncores = ncores, backend = backend,
    elapsed_secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    call = match.call(),
    versions = package_versions()),
    class = c("bag_reserve", "reserve_fit"))
}

# The provenance of a bagged prediction: the fitting settings, the random numbers and the package
# versions, so that a prediction saved on its own still says how it was made.
bag_provenance <- function(B, rng_info, control, prune, learner, completion, versions, newdata = FALSE) {
  list(B = B, seed = rng_info$seed, rng = "L'Ecuyer-CMRG streams by replicate and period",
       rng_scheme = rng_info$scheme, control = control, prune = prune,
       learner = if (is.null(learner)) "rpart (default)" else "user-supplied",
       versions = versions, completion = completion,
       completion_rng = if (completion == "draw")
         paste0("substream L + 1 of each replicate's stream (seed ", rng_info$seed, ")"),
       newdata = newdata)
}

# The versions recorded by the fits: the package itself, rpart and R.
package_versions <- function() {
  c(baggingReserve = as.character(utils::packageVersion("baggingReserve")),
    rpart = as.character(utils::packageVersion("rpart")), R = R.version.string)
}

# The number of cores, NA when it cannot be determined (a function of its own so that the tests
# can simulate that case).
detected_cores <- function() parallel::detectCores()

oob_table <- function(responses, oob_sum, oob_cnt, n_train) {
  L <- length(responses)
  out <- data.frame(period = 0:(L - 1L), n_train = n_train, n_scored = NA_integer_,
                    coverage = NA_real_, mean_votes = NA_real_, oob_error = NA_real_,
                    oob_score = NA_real_, response_var = NA_real_, oob_score_covered = NA_real_)
  for (j in seq_len(L)) {
    y <- responses[[j]]; cnt <- oob_cnt[[j]]; covered <- cnt > 0L
    out$n_scored[j] <- sum(covered)
    out$coverage[j] <- mean(covered)
    out$mean_votes[j] <- if (any(covered)) mean(cnt[covered]) else NA_real_
    out$response_var[j] <- stats::var(y)
    if (any(covered)) {
      pred <- oob_sum[[j]][covered] / cnt[covered]
      sse <- sum((y[covered] - pred)^2)
      sst <- sum((y - mean(y))^2)
      out$oob_error[j] <- sse / sum(covered)
      out$oob_score[j] <- if (sst > 0) 1 - sse / sst else NA_real_
      yc <- y[covered]; sst_c <- sum((yc - mean(yc))^2)
      out$oob_score_covered[j] <- if (sst_c > 0) 1 - sse / sst_c else NA_real_
    }
  }
  out
}

# Run `fun` over the list `tasks`; each task sets its own RNG state, so the backend does
# not change the result. Forking where available, a socket cluster otherwise. `learner` (a
# user-supplied learner or NULL) is what the socket workers must be able to run: they receive
# the global variables and attached packages its functions refer to (see learner_globals()).
run_parallel <- function(tasks, fun, ncores, backend = "auto", learner = NULL) {
  if (backend == "serial" || ncores <= 1L || length(tasks) == 1L) return(lapply(tasks, fun))
  ncores <- min(ncores, length(tasks))
  if (backend == "auto") backend <- if (.Platform$OS.type == "unix") "fork" else "psock"
  if (backend == "fork" && .Platform$OS.type != "unix")
    stop("forked workers are not available on this platform; use backend = \"psock\"", call. = FALSE)
  if (backend == "fork") {
    res <- parallel::mclapply(tasks, fun, mc.cores = ncores, mc.set.seed = FALSE,
                              mc.preschedule = FALSE)
    err <- vapply(res, inherits, TRUE, "try-error")
    if (any(err)) stop("worker error: ", conditionMessage(attr(res[[which(err)[1L]]], "condition")),
                       call. = FALSE)
    return(res)
  }
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  # the workers are fresh R processes: give them the caller's library paths first, so that a
  # package installed in a non-default library (a temporary one while vignettes are built,
  # a user or project library) is found there too
  parallel::clusterCall(cl, function(paths) { .libPaths(paths); invisible(NULL) }, .libPaths())
  parallel::clusterEvalQ(cl, library(baggingReserve))
  # the workers load the installed package; a development copy loaded in the main process
  # (e.g. with pkgload::load_all()) is not what they run
  v_workers <- unique(unlist(parallel::clusterEvalQ(cl, as.character(utils::packageVersion("baggingReserve")))))
  v_here <- as.character(utils::packageVersion("baggingReserve"))
  # a different version can lack the internal functions this session's tasks call, which then fail
  # with an obscure message; stop with a clear one instead
  if (!identical(v_workers, v_here))
    stop("the socket workers load baggingReserve ", paste(v_workers, collapse = "/"),
         " (the version installed in the library paths) while this session runs ", v_here,
         ": install the version in use there, or use backend = \"fork\" (Unix) or \"serial\"", call. = FALSE)
  if (is.null(learner)) return(parallel::parLapplyLB(cl, tasks, fun))
  # A user-supplied learner's functions travel to the workers with their own environments, but
  # the global environment and the attached packages travel by reference only: on a fresh
  # worker they are empty or absent. Give the workers the global variables and the packages the
  # learner's functions refer to, so that a learner runs there as it does here.
  g <- learner_globals(learner)
  parallel::clusterCall(cl, function(pkgs, vars) {
    for (p in pkgs) suppressPackageStartupMessages(library(p, character.only = TRUE))
    list2env(vars, envir = globalenv())
    invisible(NULL)
  }, g$packages, g$variables)
  tryCatch(parallel::parLapplyLB(cl, tasks, fun), error = function(e)
    stop(conditionMessage(e), "\nThe socket workers are separate R sessions. They received the global ",
         "variables the learner's functions refer to (",
         if (length(g$variables)) paste(names(g$variables), collapse = ", ") else "none", ") and the attached ",
         "packages they use (", if (length(g$packages)) paste(g$packages, collapse = ", ") else "none",
         "); an object the functions reach otherwise (through get(), a character string or a formula's ",
         "environment, say) must be created inside them or in an environment they carry, as in ",
         "two_part_learner(); or use backend = \"fork\" (Unix) or \"serial\"", call. = FALSE))
}

# The free variables of a learner's functions that a socket worker would not find: those that
# resolve, from each function's environment, in the global environment (or in an attached
# environment that is not a package), found by codetools::findGlobals() and followed into the
# functions and lists among them; and the attached packages that supply the others. Variables
# found in a function's own (non-global) environment chain travel with the function and are
# not collected; names that resolve nowhere (a column of a formula, say) are skipped.
learner_globals <- function(learner) {
  vars <- list(); pkgs <- character(); seen <- list()
  visit <- function(obj) {
    if (is.list(obj) && !is.data.frame(obj)) { for (x in obj) visit(x); return(invisible()) }
    if (!is.function(obj) || is.primitive(obj)) return(invisible())
    for (f in seen) if (identical(f, obj)) return(invisible())
    seen[[length(seen) + 1L]] <<- obj
    env <- environment(obj)
    for (nm in codetools::findGlobals(obj)) {
      if (nm %in% c("...", names(vars))) next
      e <- env
      while (!identical(e, emptyenv()) && !exists(nm, envir = e, inherits = FALSE)) e <- parent.env(e)
      if (identical(e, emptyenv()) || identical(e, baseenv()) || isNamespace(e)) next
      en <- environmentName(e)
      if (startsWith(en, "package:")) {
        pkgs <<- union(pkgs, sub("^package:", "", en))
      } else if (identical(e, globalenv()) || en %in% search()) {
        if (identical(en, "Autoloads")) next
        vars[nm] <<- list(get(nm, envir = e, inherits = FALSE))   # list(): a NULL value is kept
        visit(vars[[nm]])
      } else {
        visit(get(nm, envir = e, inherits = FALSE))   # travels with the function; its own globals may not
      }
    }
    invisible()
  }
  visit(learner)
  list(variables = vars, packages = setdiff(pkgs, c("base", "baggingReserve")))
}

# L'Ecuyer-CMRG streams: stream b for replicate b (substreams for the periods are taken
# inside the worker). Restores the caller's RNG kind and state on exit; with seed = NULL the
# restored state is the one after drawing the seed, so the caller's stream advances.
begin_rng_streams <- function(seed, B) {
  if (is.null(seed)) {
    seed <- sample.int(.Machine$integer.max, 1L)       # from the caller's generator
  }
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  old_kind <- RNGkind()
  seed <- as_scalar_integer(seed, "seed")
  set.seed(seed, kind = "L'Ecuyer-CMRG", normal.kind = "Inversion", sample.kind = "Rejection")
  s <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
  streams <- vector("list", B)
  for (b in seq_len(B)) { s <- parallel::nextRNGStream(s); streams[[b]] <- s }
  list(streams = streams,
       info = list(seed = seed, kind = c("L'Ecuyer-CMRG", "Inversion", "Rejection"),
                   scheme = "stream b = nextRNGStream^b(set.seed(seed)); period l = nextRNGSubStream^(l+1)"),
       restore = function() {
         if (had_seed) {
           assign(".Random.seed", old, envir = globalenv())   # encodes the old kind too
         } else {
           do.call(RNGkind, as.list(old_kind))  # sets the kind (and a seed) ...
           if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
             rm(".Random.seed", envir = globalenv())          # ... which is removed again
         }
       })
}

#' @rdname bag_reserve
#' @param object A `bag_reserve` fit (for `summary`).
#' @param ... For `summary`, passed on to [summary.reserve_prediction()] (e.g. `probs`).
#' @export
summary.bag_reserve <- function(object, ...) {
  structure(list(
    B = object$B, L = object$L, t = object$t, n_claims = nrow(object$panel$claims),
    features = object$features, control = object$control, prune = object$prune, learner = object$learner,
    keep_trees = object$keep_trees, rng = object$rng, ncores = object$ncores,
    elapsed_secs = object$elapsed_secs, oob = object$oob,
    prediction = summary(object$prediction, ...)), class = "summary.bag_reserve")
}

#' @export
print.summary.bag_reserve <- function(x, ...) {
  cat("Bagged regression-tree reserving fit: B = ", x$B, ", ", x$L, " periods, ",
      format(x$n_claims, big.mark = ","), " claims, t = ", x$t, "\n  features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n  ", fit_settings_line(x, minbucket = TRUE),
      "; seed ", x$rng$seed, " (", x$rng$kind[1L],
      "); ", round(x$elapsed_secs), " s; worker processes: ", x$ncores, "\n", sep = "")
  if (!is.null(x$oob)) { cat("  out-of-bag diagnostics:\n"); print(x$oob, row.names = FALSE, digits = 4) }
  print(x$prediction)
  invisible(x)
}

# The settings line of the print methods of bagged fits: the rpart control and pruning of the
# default learner, or a note that a user-supplied learner did not use them.
fit_settings_line <- function(x, minbucket = FALSE) {
  if (!is.null(x$learner)) return("learner: user-supplied (`control` and `prune` not used)")
  # each value formatted as cat() prints it
  ctl <- lapply(x$control[c("cp", "xval", "minsplit", "minbucket")],
                function(v) paste(vapply(v, format, ""), collapse = " "))
  paste0("control: cp = ", ctl$cp, ", xval = ", ctl$xval, ", minsplit = ", ctl$minsplit,
         if (minbucket) paste0(", minbucket = ", ctl$minbucket), "; pruning: ", x$prune)
}

#' @export
print.bag_reserve <- function(x, ...) {
  cat("Bagged reserving fit: B = ", x$B, " replicates, ", x$L,
      " trees per replicate (period = development-year index), valuation year t = ", x$t, "\n", sep = "")
  cat("  claims: ", format(nrow(x$panel$claims), big.mark = ","), "; features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n", sep = "")
  cat("  ", fit_settings_line(x), "; trees kept: ", x$keep_trees,
      "; seed ", x$rng$seed, " (L'Ecuyer-CMRG streams); ", round(x$elapsed_secs),
      " s; worker processes: ", x$ncores, "\n", sep = "")
  if (!is.null(x$oob)) {
    cat("  out-of-bag score per period:\n")
    o <- x$oob
    tab <- data.frame(period = o$period, n_train = o$n_train, coverage = round(o$coverage, 3),
                      oob_score = round(o$oob_score, 3))
    # with incomplete coverage (small B) oob_score is biased upwards; show the score over the covered claims
    low <- !is.na(o$coverage) & o$coverage < 0.99
    if (any(low) && !is.null(o$oob_score_covered)) tab$oob_score_covered <- round(o$oob_score_covered, 3)
    print(tab, row.names = FALSE)
    if (any(low))
      cat("  coverage below 0.99: oob_score is biased upwards there; oob_score_covered is the R^2 over the covered claims\n")
  }
  if (!is.null(x$constant_response)) note_constant_periods(x$constant_response)
  print(x$prediction)
  invisible(x)
}

#' Predictions of a bagged fit
#'
#' Returns the completed replicate predictions, or their bagged mean, for the fitted
#' portfolio or for compatible new data. Without `newdata`, the [reserve_prediction]
#' cached at fitting time is returned: one column per bootstrap replicate
#' (`type = "replicates"`) or their mean (`type = "mean"`, which averages the *completed*
#' replicates, never the trees development year by development year). With `newdata`, the
#' stored trees complete another [claims_panel]; this needs `keep_trees = TRUE` and
#' `completion = "mean"` at fitting time (a fit with `completion = "draw"` does not keep the leaf
#' responses its simulation draws from, so it predicts only the fitted portfolio).
#'
#' The completion (plugged-in means or simulated payments) of a bagged fit is chosen when fitting,
#' with `bag_reserve(completion = )`, not here: `completion` is accepted only when it restates the
#' fit's own choice, and any other argument (`R`, `seed`, a misspelt `type`) is an error, so that
#' no call silently returns another estimator than the one asked for.
#'
#' @inheritParams predict.reserve_tree
#' @param object A [bag_reserve] fit.
#' @param ... Only `completion`, equal to the fit's (`object$completion`); anything else is an
#'   error.
#' @return A [reserve_prediction] as documented in [predict.reserve_tree()], with `B`
#'   columns of predicted reported-claim payments and, unless the fit was made with
#'   `claim_level = FALSE`, the claim-level matrix averaged over the replicates
#'   (`claim_level`; its future cells sum to the bagged mean of the reported component). With `type = "mean"` the object holds one column named
#'   `"mean"`, `B = 1` and the estimator label `"bagging (mean of replicates)"`; the
#'   replicate distribution is then no longer available.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' bag <- bag_reserve(panel, B = 5, seed = 2026, keep_trees = TRUE)
#' pred <- predict(bag)                      # five columns, one per replicate
#' pred_mean <- predict(bag, type = "mean")  # one column, the bagged mean
#' calendar_year_payments(pred)[c(1, nrow(calendar_year_payments(pred))), ]
#' # the same trees applied to another panel of the same layout (the claims of type 4)
#' cl4 <- synthetic_claims[synthetic_claims$Type == "4", ]
#' panel4 <- claims_panel(cl4, synthetic_payments[synthetic_payments$Id %in% cl4$Id, ],
#'                        t = 12, L = 11, features = c("Age", "AccWeekday"))
#' predict(bag, newdata = panel4)
#' @export
predict.bag_reserve <- function(object, newdata = NULL, type = c("replicates", "mean"), ...) {
  dots <- list(...)
  fit_completion <- if (is.null(object$completion)) "mean" else object$completion
  if ("completion" %in% names(dots)) {
    # a fitting-time choice for bagged fits; restating the fit's own choice is harmless
    if (!identical(dots$completion, fit_completion))
      stop("for a bagged fit the completion is chosen when fitting: this fit was made with completion = \"",
           fit_completion, "\"; refit with bag_reserve(..., completion = \"",
           if (fit_completion == "mean") "draw" else "mean", "\") (predict() on a reserve_tree takes `completion`)",
           call. = FALSE)
    dots$completion <- NULL
  }
  check_unused_dots(dots, "predict", c("newdata", "type"),
                    hints = c(R = "the number of simulated completions of a bagged fit is B, set when fitting",
                              seed = "the random numbers of a bagged fit are fixed by bag_reserve(seed = )"))
  type <- match.arg(type)
  if (is.null(newdata)) {
    pred <- object$prediction
  } else {
    if (identical(object$completion, "draw"))
      stop("a fit made with completion = \"draw\" cannot predict `newdata`: the leaf responses its simulation ",
           "draws from are not kept; refit with completion = \"mean\" and keep_trees = TRUE, or fit the new panel",
           call. = FALSE)
    if (!object$keep_trees)
      stop("the trees were not kept (keep_trees = FALSE); refit with keep_trees = TRUE to predict new data",
           call. = FALSE)
    panel <- check_newdata(object, newdata)
    warn_unseen_levels(panel, object$train_levels)
    cells <- future_cells(panel)
    P_sum <- panel$payments; P_sum[] <- 0
    pred_fun <- learner_predict_fun(object$learner)
    reported <- vapply(object$trees, function(models) {
      P <- complete_payments(panel, models, pred_fun)
      P_sum <<- P_sum + P
      aggregate_triangle(panel, P)[cells$index]
    }, numeric(nrow(cells)))
    reported <- matrix(reported, nrow = nrow(cells), ncol = object$B,
                       dimnames = list(NULL, paste0("b", seq_len(object$B))))
    pred <- new_reserve_prediction(panel, reported, estimator = "bagging", claim_level = P_sum / object$B,
                                   completion = "mean",
                                   provenance = bag_provenance(object$B, object$rng, object$control, object$prune,
                                                               object$learner, "mean", object$versions,
                                                               newdata = TRUE))
  }
  if (type == "mean") pred <- collapse_mean(pred)
  pred
}

collapse_mean <- function(pred) {
  pred$reported <- matrix(rowMeans(pred$reported), ncol = 1L, dimnames = list(NULL, "mean"))
  if (!is.null(pred$ibnr)) {
    pred$ibnr <- matrix(rowMeans(pred$ibnr), ncol = 1L, dimnames = list(NULL, "mean"))
    if (!is.null(pred$ibnr_details$mu))
      pred$ibnr_details$mu <- matrix(colMeans(pred$ibnr_details$mu), nrow = 1L,
                                     dimnames = list("mean", colnames(pred$ibnr_details$mu)))
  }
  info <- pred_info(pred)
  R <- pred$B
  pred$B <- 1L
  pred$replicate_ids <- "mean"
  pred$estimator <- if (info$kind == "tree" && info$completion == "draw")
    paste0("tree (mean of ", R, " simulated completions)") else paste0(pred$estimator, " (mean of replicates)")
  pred$completion <- info$completion
  pred$variability <- info$variability     # of the columns that were averaged
  pred
}

#' Out-of-bag diagnostics of a bagged fit
#'
#' Extracts the out-of-bag errors, scores and coverage per development year from a bagged
#' fit. For development year `l`, every training claim that was left out of the bootstrap
#' sample of at least one replicate (a *covered* claim) receives the average prediction of
#' those replicates' trees, computed from its observed history. `oob_error` is the mean
#' squared error over the covered claims; `oob_score` is one minus the sum of squared
#' errors over the covered claims divided by the total sum of squares of the response over
#' *all* training claims of the development year (the definition of
#' \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} 2025). With `B` replicates a claim is
#' covered with probability `1 - (1 - (1 - 1/n)^n)^B`, which is also the expected coverage:
#' about 0.99 at `B = 10`, so that coverage is essentially complete for `B` in the hundreds but
#' not for very small `B`; the observed `coverage = n_scored / n_train` reports it, and the
#' score is `NA` when no claim is covered or the response has zero variance. Because its
#' numerator runs over the covered claims only, the score is biased upwards when the
#' coverage is below one (small `B`); `oob_score_covered` is the ordinary coefficient of
#' determination over the covered claims, which does not have that bias. A negative score
#' means that the out-of-bag predictions are worse than the mean of the response, which is
#' common for unpruned bootstrap trees (`cp = 0`) on noisy late development years;
#' a larger `minbucket` or a positive `cp` in `control` smooths them. The score is a
#' diagnostic of the one-year regressions, not a validation of the recursive multi-year
#' forecast or of the IBNR component.
#'
#' @param object A [bag_reserve] fit created with `oob = TRUE` (otherwise an error).
#' @return A data frame with one row per development year and the columns `period` (the
#'   development-year index), `n_train`, `n_scored` (claims with at least one out-of-bag
#'   vote), `coverage`, `mean_votes` (mean number of votes among the covered claims),
#'   `oob_error`, `oob_score`, `response_var` and `oob_score_covered`.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' bag <- bag_reserve(panel, B = 5, seed = 2026)
#' oob_score(bag)[, c("period", "n_train", "coverage", "oob_score")]
#' @export
oob_score <- function(object) UseMethod("oob_score")

#' @export
oob_score.bag_reserve <- function(object) {
  if (is.null(object$oob)) stop("the fit was made with oob = FALSE", call. = FALSE)
  object$oob
}

#' @export
oob_score.default <- function(object) {
  stop("out-of-bag diagnostics exist only for bag_reserve fits", call. = FALSE)
}
