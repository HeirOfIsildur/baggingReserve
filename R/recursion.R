# Internal recursion engine: complete the payment histories of the reported claims one
# development period ahead, using one fitted model per period.
#
# For l = 0, ..., L-1 the model of period l predicts X_{l+1} for the claims whose cell
# (acc_year, l+1) lies after the valuation date, from the static features, the reporting
# delay and X_0, ..., X_l -- where any of X_0, ..., X_l that lies after the valuation date
# has itself been predicted in an earlier period of the SAME pass (recursive one-period-ahead
# completion). Observed cells are never changed. The observed panel is immutable; every pass
# works on its own copy.
#
# The same engine serves the single pruned tree (one pass) and bagging (one pass per
# replicate, using the b-th bootstrap tree of every period). Averaging the completed passes
# is not the same as one pass with per-period averaged predictions; the package always does
# the former.

# models: list of length L (period l at index l + 1); predict_fun(model, newdata) -> numeric.
complete_payments <- function(panel, models, predict_fun, stage = "completion") {
  L <- panel$L
  if (length(models) != L)
    stop("need one model per development period 0, ..., L - 1", call. = FALSE)
  P <- panel$payments
  for (l in 0:(L - 1L)) {
    rows <- period_rows_predict(panel, l)
    if (!length(rows)) next
    nd <- period_frame(panel, rows, l, payments = P, response = FALSE)
    pred <- if (length(formals(predict_fun)) >= 3L) predict_fun(models[[l + 1L]], nd, l)
            else predict_fun(models[[l + 1L]], nd)
    P[rows, l + 2L] <- check_learner_output(pred, length(rows), l, stage)
  }
  P
}

# The one check of every learner prediction (out-of-bag votes, plugged-in completion, simulated
# completion): numeric, one value per row of newdata, finite. A reserve built from an infinite or
# missing prediction is meaningless, so any such value is an error naming the stage and period.
check_learner_output <- function(pred, n, l, stage) {
  if (is.data.frame(pred) && ncol(pred) == 1L) pred <- pred[[1L]]   # a one-column data frame: its column
  if (!is.numeric(pred) || !is.null(dim(pred)) && length(dim(pred)) > 1L && min(dim(pred)) > 1L)
    stop(stage, ", period ", l, ": the model's predictions must be a numeric vector, not ",
         describe_output(pred), call. = FALSE)
  if (length(pred) != n)
    stop(stage, ", period ", l, ": the model returned ", length(pred), " prediction(s) for ", n,
         " claims; it must return one per row of `newdata`", call. = FALSE)
  bad <- !is.finite(pred)
  if (any(bad))
    stop(stage, ", period ", l, ": the model returned ", sum(bad), " non-finite prediction(s) (",
         paste(unique(format(pred[bad])), collapse = ", "), ") for ", n,
         " claims; predictions must be finite numbers", call. = FALSE)
  as.vector(pred)
}

# The same check for one part of two_part_learner() (occurrence or size model), before the parts are
# combined: numeric, one finite value per row, not a character or a matrix with several columns.
check_part_output <- function(x, n, what) {
  if (is.data.frame(x) && ncol(x) == 1L) x <- x[[1L]]
  if (!is.numeric(x) || !is.null(dim(x)) && length(dim(x)) > 1L && min(dim(x)) > 1L)
    stop("two_part_learner(): ", what, " must return a numeric vector, not ", describe_output(x), call. = FALSE)
  if (length(x) != n)
    stop("two_part_learner(): ", what, " returned ", length(x), " value(s) for ", n,
         " rows; it must return one per row of `newdata`", call. = FALSE)
  bad <- !is.finite(x)
  if (any(bad))
    stop("two_part_learner(): ", what, " returned ", sum(bad), " non-finite value(s) (",
         paste(unique(format(x[bad])), collapse = ", "), "); values must be finite numbers", call. = FALSE)
  as.numeric(x)
}

# What an unusable learner output is, for the messages above.
describe_output <- function(x) {
  if (is.data.frame(x)) return(paste0("a data frame with ", ncol(x), " columns"))
  if (!is.null(dim(x)) && length(dim(x)) > 1L)
    return(paste0(if (is.numeric(x)) "a numeric" else paste("a", typeof(x)), " matrix with ", ncol(x), " column",
                  if (ncol(x) != 1L) "s"))
  class(x)[1L]
}

# Sum a completed claim-level payment matrix into the accident-year x development triangle.
aggregate_triangle <- function(panel, P) {
  ays <- panel$accident_years
  row <- match(panel$claims$acc_year, ays)
  M <- matrix(0, length(ays), panel$L + 1L, dimnames = list(ays, 0:panel$L))
  for (j in seq_len(ncol(P))) {
    s <- rowsum(P[, j], group = row, reorder = FALSE)
    M[as.integer(rownames(s)), j] <- s[, 1L]
  }
  M
}

# Coordinates of the cells after the valuation date, in column-major (development-major)
# order: the order the 2023 code used for its 45 stored cells.
future_cells <- function(panel) {
  ays <- panel$accident_years
  mask <- future_mask(ays, panel$L, panel$t)
  idx <- which(mask)                                   # column-major
  d <- data.frame(acc_year = ays[(idx - 1L) %% length(ays) + 1L],
                  dev = (idx - 1L) %/% length(ays),
                  index = idx)
  d$calendar <- d$acc_year + d$dev
  d[, c("acc_year", "dev", "calendar", "index")]
}

# rpart adapter -------------------------------------------------------------------------

fit_period_rpart <- function(frame, l, control) {
  rpart::rpart(period_formula(l), data = frame, method = "anova", control = control)
}

predict_period_rpart <- function(model, newdata) {
  unname(stats::predict(model, newdata = newdata))
}

# The training responses of every leaf of a (pruned) tree, keyed by the leaf's node number;
# simulated completion draws from them. Needs the tree's `y` and `where`, so it is taken
# before slim_rpart().
leaf_pool <- function(fit) {
  split(unname(fit$y), rownames(fit$frame)[fit$where])
}

# Training responses below an internal node. predict.rpart() stops at an internal node when it
# cannot route a claim further (e.g. a factor level absent from the training sample and no
# surrogate); its prediction there is the mean of the node's training rows, i.e. of the leaves
# below it. Node k's descendants are the numbers whose binary prefix is k.
subtree_pool <- function(pool, k) {
  leaves <- as.numeric(names(pool))
  below <- vapply(leaves, function(n) { while (n > k) n <- n %/% 2; n == k }, TRUE)
  p <- unlist(pool[below], use.names = FALSE)
  if (!length(p)) stop("internal: no training responses below node ", k, call. = FALSE)
  p
}

# A prediction function that draws, for each claim, one training response of the claim's
# leaf (simulated completion). The leaf is found by predicting with the node numbers in place
# of the leaf means.
draw_period_rpart <- function(pools) {
  force(pools)
  function(model, newdata, l) {
    node <- model; node$frame$yval <- as.numeric(rownames(model$frame))
    # integer keys: node numbers of deep trees exceed 1e5, which as.character() of a double prints as "1e+05"
    leaf <- as.character(as.integer(round(unname(stats::predict(node, newdata = newdata)))))
    out <- numeric(length(leaf)); pool <- pools[[l + 1L]]
    # group the claims by leaf once (linear, not one scan per leaf); the groups come in the order
    # of first appearance with ascending indices, as unique() and which() gave them, so the random
    # draws are the same
    keys <- unique(leaf)
    groups <- split(seq_along(leaf), factor(leaf, levels = keys))
    pk <- pool[keys]                             # NULL for an internal node
    for (j in seq_along(keys)) {
      i <- groups[[j]]; p <- pk[[j]]
      if (!length(p)) p <- subtree_pool(pool, as.numeric(keys[j]))
      out[i] <- if (length(p) == 1L) rep(p, length(i)) else p[sample.int(length(p), length(i), replace = TRUE)]
    }
    out
  }
}

# The smallest cross-validated error (the first, i.e. simplest, tree on ties: which.min), as in
# the 2023 code. Returns the fit itself when no cross-validation was run (xval = 0).
prune_min_xerror <- function(fit) {
  best <- cp_min_xerror(fit)
  if (is.na(best)) return(fit)
  rpart::prune(fit, cp = best)
}

# The complexity parameter of the smallest xerror (the first on ties); NA when there is no usable
# cross-validation result (xval = 0, or a constant response with undefined xerror).
cp_min_xerror <- function(fit) {
  cpt <- fit$cptable
  if (is.null(cpt) || !"xerror" %in% colnames(cpt) || nrow(cpt) == 0L) return(NA_real_)
  xe <- cpt[, "xerror"]
  if (all(is.na(xe))) return(NA_real_)
  unname(cpt[which.min(xe), "CP"])
}

slim_rpart <- function(fit, keep_cptable = TRUE) {
  # keep what predict.rpart() needs (frame, splits, csplit, terms, attributes) and drop the
  # copies of the training data, the formula environment and `where` (the leaf of every
  # training row, a named vector as long as the training data that prediction never uses);
  # the bootstrap trees also drop the complexity table, which only the single trees report
  fit$call <- NULL
  fit$y <- NULL
  fit$x <- NULL
  fit$where <- NULL
  if (!keep_cptable) fit$cptable <- NULL
  attr(fit$terms, ".Environment") <- NULL
  fit
}

# Learners other than rpart: a list of fit(formula, data) and predict(model, newdata).
check_learner <- function(learner) {
  if (!is.null(learner) && !(is.list(learner) && is.function(learner$fit) && is.function(learner$predict)))
    stop("`learner` must be NULL or a list with functions `fit(formula, data)` and `predict(model, newdata)`",
         call. = FALSE)
  invisible(learner)
}
# The learner's functions are called as given; their output is checked by check_learner_output()
# (not coerced, so that a character or factor prediction is an error rather than NA or level codes).
learner_predict_fun <- function(learner) {
  if (is.null(learner)) predict_period_rpart else
    function(model, newdata) learner$predict(model, newdata)
}
learner_simulate_fun <- function(learner) {
  function(model, newdata) learner$simulate(model, newdata)
}
