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
#' of single-tree predictions under this resampling. It is not a predictive distribution
#' of the future payments: the numbers of unreported claims are projected once and held
#' fixed and no payment noise is simulated.
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
#' caller's generator state is restored on exit. Replicates are streamed: each worker fits
#' the trees of a replicate, records the OOB votes, aggregates the predicted future cells
#' and discards the trees unless `keep_trees = TRUE`.
#'
#' @inheritParams reserve_tree
#' @param B Number of bootstrap replicates (a positive integer; default 100).
#' @param seed Integer seed of the stream allocation. `NULL` draws one from the current
#'   generator and records it in the result. Because the caller's generator state is
#'   restored on exit, two calls with `seed = NULL` and nothing drawn in between use the
#'   same seed; give explicit seeds for distinct runs.
#' @param ncores Number of worker processes (forked on Unix, a socket cluster elsewhere);
#'   default 1, which runs serially. Values below one count as one.
#' @param control [rpart::rpart.control()] for the bootstrap trees (default `cp = 0`,
#'   `xval = 0`).
#' @param prune `"none"` (default) or `"min_xerror"` (needs `xval > 0` in `control`).
#' @param oob Compute the out-of-bag diagnostics (default `TRUE`; with `FALSE` the `oob`
#'   element of the result is `NULL` and [oob_score()] fails).
#' @param keep_trees Keep the `B * L` fitted trees (and the bootstrap indices) in the
#'   result (default `FALSE`), which allows `predict(..., newdata = )` and inspection at a
#'   large memory cost.
#' @param batch_size Replicates processed per worker task before OOB votes are returned;
#'   `NULL` (default) uses `max(1, ceiling(B / ncores / 2))`. The batching does not change
#'   the completed replicates.
#' @param backend `"auto"` forks workers on Unix and uses a socket cluster elsewhere;
#'   `"fork"`, `"psock"` and `"serial"` force one backend. Results are the same for all of
#'   them (up to floating-point summation order in the OOB sums).
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
#' @export
bag_reserve <- function(panel, B = 100L, seed = NULL, ncores = 1L,
                        control = rpart::rpart.control(cp = 0, xval = 0),
                        prune = c("none", "min_xerror"), oob = TRUE, keep_trees = FALSE,
                        batch_size = NULL, backend = c("auto", "fork", "psock", "serial")) {
  check_panel(panel)
  prune <- match.arg(prune)
  backend <- match.arg(backend)
  B <- as_scalar_integer(B, "B"); if (B < 1L) stop("`B` must be at least 1", call. = FALSE)
  ncores <- as_scalar_integer(ncores, "ncores"); if (ncores < 1L) ncores <- 1L
  if (prune == "min_xerror" && isTRUE(control$xval == 0))
    stop("prune = \"min_xerror\" needs cross-validation; set xval > 0 in `control`", call. = FALSE)
  if (panel$L < 1L) stop("nothing to predict: L must be at least 1", call. = FALSE)
  t0 <- Sys.time()
  L <- panel$L

  ## RNG: one L'Ecuyer stream per replicate, one substream per period -------------------
  rng <- begin_rng_streams(seed, B)
  on.exit(rng$restore(), add = TRUE)

  ## training frames, shared by all replicates -----------------------------------------
  rows <- lapply(0:(L - 1L), function(l) period_rows_train(panel, l))
  n_train <- lengths(rows)
  if (any(n_train < 2L))
    stop("period(s) ", paste(which(n_train < 2L) - 1L, collapse = ", "),
         " have fewer than two training claims", call. = FALSE)
  frames <- lapply(0:(L - 1L), function(l) period_frame(panel, rows[[l + 1L]], l, response = TRUE))
  responses <- lapply(0:(L - 1L), function(l) frames[[l + 1L]][[paste0("X", l + 1L)]])
  cells <- future_cells(panel)

  run_batch <- function(bs) {
    oob_sum <- if (oob) lapply(n_train, function(n) numeric(n)) else NULL
    oob_cnt <- if (oob) lapply(n_train, function(n) integer(n)) else NULL
    reported <- matrix(NA_real_, nrow(cells), length(bs))
    trees <- if (keep_trees) vector("list", length(bs)) else NULL
    inbag <- if (keep_trees) vector("list", length(bs)) else NULL
    for (k in seq_along(bs)) {
      b <- bs[k]
      s <- rng$streams[[b]]
      models <- vector("list", L)
      idx_b <- if (keep_trees) vector("list", L) else NULL
      for (l in 0:(L - 1L)) {
        s <- parallel::nextRNGSubStream(s)
        assign(".Random.seed", s, envir = globalenv())
        n <- n_train[l + 1L]
        idx <- sample.int(n, n, replace = TRUE)
        fit <- fit_period_rpart(frames[[l + 1L]][idx, , drop = FALSE], l, control)
        if (prune == "min_xerror") fit <- prune_min_xerror(fit)
        fit <- slim_rpart(fit)
        if (oob) {
          out <- which(tabulate(idx, nbins = n) == 0L)
          if (length(out)) {
            p <- predict_period_rpart(fit, frames[[l + 1L]][out, , drop = FALSE])
            oob_sum[[l + 1L]][out] <- oob_sum[[l + 1L]][out] + p
            oob_cnt[[l + 1L]][out] <- oob_cnt[[l + 1L]][out] + 1L
          }
        }
        models[[l + 1L]] <- fit
        if (keep_trees) idx_b[[l + 1L]] <- idx
      }
      P <- complete_payments(panel, models, predict_period_rpart)
      reported[, k] <- aggregate_triangle(panel, P)[cells$index]
      if (keep_trees) { trees[[k]] <- models; inbag[[k]] <- idx_b }
    }
    list(reported = reported, oob_sum = oob_sum, oob_cnt = oob_cnt, trees = trees, inbag = inbag)
  }

  if (is.null(batch_size)) batch_size <- max(1L, ceiling(B / max(ncores, 1L) / 2L))
  batch_size <- as_scalar_integer(batch_size, "batch_size")
  if (batch_size < 1L) stop("`batch_size` must be at least 1", call. = FALSE)
  batches <- split(seq_len(B), ceiling(seq_len(B) / batch_size))
  results <- run_parallel(batches, run_batch, ncores, backend)
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

  prediction <- new_reserve_prediction(panel, reported, estimator = "bagging",
                                       provenance = list(B = B, seed = rng$info$seed,
                                                         rng = "L'Ecuyer-CMRG streams by replicate and period"))
  structure(list(
    B = B, prediction = prediction, oob = oob_tab, trees = trees, inbag = inbag,
    n_train = n_train, control = control, prune = prune, keep_trees = keep_trees,
    panel = panel, features = panel$features, feature_levels = panel$feature_levels,
    L = L, t = panel$t, rng = rng$info, ncores = ncores, backend = backend,
    elapsed_secs = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    call = match.call(),
    versions = c(rpart = as.character(utils::packageVersion("rpart")), R = R.version.string)),
    class = c("bag_reserve", "reserve_fit"))
}

oob_table <- function(responses, oob_sum, oob_cnt, n_train) {
  L <- length(responses)
  out <- data.frame(period = 0:(L - 1L), n_train = n_train, n_scored = NA_integer_,
                    coverage = NA_real_, mean_votes = NA_real_, oob_error = NA_real_,
                    oob_score = NA_real_, response_var = NA_real_)
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
    }
  }
  out
}

# Run `fun` over the list `tasks`; each task sets its own RNG state, so the backend does
# not change the result. Forking where available, a socket cluster otherwise.
run_parallel <- function(tasks, fun, ncores, backend = "auto") {
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
  parallel::parLapplyLB(cl, tasks, fun)
}

# L'Ecuyer-CMRG streams: stream b for replicate b (substreams for the periods are taken
# inside the worker). Restores the caller's RNG kind and state on exit.
begin_rng_streams <- function(seed, B) {
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  old_kind <- RNGkind()
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)
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
           RNGkind(old_kind[1L], old_kind[2L], old_kind[3L])  # sets the kind (and a seed) ...
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
    features = object$features, control = object$control, prune = object$prune,
    keep_trees = object$keep_trees, rng = object$rng, ncores = object$ncores,
    elapsed_secs = object$elapsed_secs, oob = object$oob,
    prediction = summary(object$prediction, ...)), class = "summary.bag_reserve")
}

#' @export
print.summary.bag_reserve <- function(x, ...) {
  cat("Bagged regression-tree reserving fit: B = ", x$B, ", ", x$L, " periods, ",
      format(x$n_claims, big.mark = ","), " claims, t = ", x$t, "\n  features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n  control: cp = ", x$control$cp,
      ", xval = ", x$control$xval, ", minsplit = ", x$control$minsplit, ", minbucket = ",
      x$control$minbucket, "; pruning: ", x$prune, "; seed ", x$rng$seed, " (", x$rng$kind[1L],
      "); ", round(x$elapsed_secs), " s; worker processes requested: ", x$ncores, "\n", sep = "")
  if (!is.null(x$oob)) { cat("  out-of-bag diagnostics:\n"); print(x$oob, row.names = FALSE, digits = 4) }
  print(x$prediction)
  invisible(x)
}

#' @export
print.bag_reserve <- function(x, ...) {
  cat("Bagged reserving fit: B = ", x$B, " replicates, ", x$L,
      " trees per replicate (period = development-year index), valuation year t = ", x$t, "\n", sep = "")
  cat("  claims: ", format(nrow(x$panel$claims), big.mark = ","), "; features: ",
      paste(c(x$features, "rep_delay"), collapse = ", "), "\n", sep = "")
  cat("  control: cp = ", x$control$cp, ", xval = ", x$control$xval, ", minsplit = ",
      x$control$minsplit, "; pruning: ", x$prune, "; trees kept: ", x$keep_trees,
      "; seed ", x$rng$seed, " (L'Ecuyer-CMRG streams); ", round(x$elapsed_secs),
      " s; worker processes requested: ", x$ncores, "\n", sep = "")
  if (!is.null(x$oob)) {
    cat("  out-of-bag score per period:\n")
    o <- x$oob
    print(data.frame(period = o$period, n_train = o$n_train, coverage = round(o$coverage, 3),
                     oob_score = round(o$oob_score, 3)), row.names = FALSE)
  }
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
#' stored trees complete another [claims_panel]; this needs `keep_trees = TRUE` at fitting
#' time.
#'
#' @inheritParams predict.reserve_tree
#' @param object A [bag_reserve] fit.
#' @return A [reserve_prediction] as documented in [predict.reserve_tree()], with `B`
#'   columns of predicted reported-claim payments and no claim-level matrix
#'   (`claim_level = NULL`). With `type = "mean"` the object holds one column named
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
  type <- match.arg(type)
  if (is.null(newdata)) {
    pred <- object$prediction
  } else {
    if (!object$keep_trees)
      stop("the trees were not kept (keep_trees = FALSE); refit with keep_trees = TRUE to predict new data",
           call. = FALSE)
    panel <- check_newdata(object, newdata)
    cells <- future_cells(panel)
    reported <- vapply(object$trees, function(models) {
      P <- complete_payments(panel, models, predict_period_rpart)
      aggregate_triangle(panel, P)[cells$index]
    }, numeric(nrow(cells)))
    reported <- matrix(reported, nrow = nrow(cells), ncol = object$B,
                       dimnames = list(NULL, paste0("b", seq_len(object$B))))
    pred <- new_reserve_prediction(panel, reported, estimator = "bagging",
                                   provenance = list(B = object$B, seed = object$rng$seed,
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
  pred$B <- 1L
  pred$replicate_ids <- "mean"
  pred$estimator <- paste0(pred$estimator, " (mean of replicates)")
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
#' covered with probability `1 - (1 - (1 - 1/n)^n)^B` in expectation, close to one for `B`
#' of a few dozen or more; the observed `coverage = n_scored / n_train` reports it, and the
#' score is `NA` when no claim is covered or the response has zero variance. The score is
#' a diagnostic of the one-year regressions, not a validation of the recursive multi-year
#' forecast or of the IBNR component.
#'
#' @param object A [bag_reserve] fit created with `oob = TRUE` (otherwise an error).
#' @return A data frame with one row per development year and the columns `period` (the
#'   development-year index), `n_train`, `n_scored` (claims with at least one out-of-bag
#'   vote), `coverage`, `mean_votes` (mean number of votes among the covered claims),
#'   `oob_error`, `oob_score` and `response_var`.
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
