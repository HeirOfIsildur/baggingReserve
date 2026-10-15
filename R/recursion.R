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
complete_payments <- function(panel, models, predict_fun) {
  L <- panel$L
  if (length(models) != L)
    stop("need one model per development period 0, ..., L - 1", call. = FALSE)
  P <- panel$payments
  for (l in 0:(L - 1L)) {
    rows <- period_rows_predict(panel, l)
    if (!length(rows)) next
    nd <- period_frame(panel, rows, l, payments = P, response = FALSE)
    pred <- predict_fun(models[[l + 1L]], nd)
    if (length(pred) != length(rows) || anyNA(pred))
      stop("period ", l, ": the model returned ", length(pred), " prediction(s) with ",
           sum(is.na(pred)), " missing for ", length(rows), " claims", call. = FALSE)
    P[rows, l + 2L] <- pred
  }
  P
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

# First minimum of the cross-validated error, as in the 2023 code. Returns the fit itself
# when no cross-validation was run (xval = 0).
prune_min_xerror <- function(fit) {
  best <- cp_min_xerror(fit)
  if (is.na(best)) return(fit)
  rpart::prune(fit, cp = best)
}

# The complexity parameter at the first minimum of xerror; NA when there is no usable
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
