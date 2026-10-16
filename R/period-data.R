# Internal: which claims train and which are predicted in development period l, and the
# model frames handed to the learner. Period l predicts X_{l+1} from the static features,
# the reporting delay and X_0, ..., X_l.
#
#   training rows   S_l = { r : rep_delay_r <= l  and  acc_year_r + l + 1 <= t }
#   prediction rows F_l = { r : acc_year_r + l + 1 > t }
#
# For reported claims (acc_year + rep_delay <= t) the prediction rows automatically satisfy
# rep_delay <= l, so no delay filter is needed there. Column order of the frames matters for
# rpart's tie-breaking between equally good splits and follows the 2023 code: features in the
# order given, reporting delay, X0 ... Xl, response last.

period_rows_train <- function(panel, l) {
  cl <- panel$claims
  which(cl$rep_delay <= l & cl$acc_year + l + 1L <= panel$t)
}

period_rows_predict <- function(panel, l) {
  cl <- panel$claims
  which(cl$acc_year + l + 1L > panel$t)
}

# `payments` may be a working copy with earlier predictions filled in (recursion).
period_frame <- function(panel, rows, l, payments = panel$payments, response = TRUE) {
  df <- panel$claims[rows, panel$features, drop = FALSE]
  df$rep_delay <- panel$claims$rep_delay[rows]
  X <- payments[rows, seq_len(l + 1L), drop = FALSE]
  df <- cbind(df, as.data.frame(X))
  if (response) df[[paste0("X", l + 1L)]] <- payments[rows, l + 2L]
  rownames(df) <- NULL
  df
}

period_formula <- function(l) {
  stats::as.formula(paste0("X", l + 1L, " ~ ."))
}

# Error for a period with fewer than two training claims, stating the eligibility rule.
stop_sparse_periods <- function(n_train) {
  bad <- which(n_train < 2L) - 1L
  stop("fewer than two training claims in period(s) ", paste(bad, collapse = ", "), " (",
       paste(n_train[bad + 1L], collapse = ", "), "): the tree of period l trains on the ",
       "claims with reporting delay <= l and acc_year + l + 1 <= t. ",
       "Use a smaller L or more accident years", call. = FALSE)
}

# Note printed by the fits for periods whose training response is constant: their tree is a
# root that predicts that constant for every claim completed in the period.
note_constant_periods <- function(constant) {
  if (any(constant))
    cat("  constant training response in period(s) ", paste(which(constant) - 1L, collapse = ", "),
        ": the tree predicts that value for every claim\n", sep = "")
}

period_sizes <- function(panel) {
  L <- panel$L
  data.frame(period = 0:(L - 1L),
             n_train = vapply(0:(L - 1L), function(l) length(period_rows_train(panel, l)), 1L),
             n_predict = vapply(0:(L - 1L), function(l) length(period_rows_predict(panel, l)), 1L))
}

# Levels of each factor feature among the training claims of every period: a list over the
# periods (index l + 1) of named lists of character vectors. A level can exist in the panel
# but be absent from the training claims of a period (it may occur only in the latest
# accident years, which train no tree of the later periods).
period_train_levels <- function(panel) {
  fac <- panel$features[vapply(panel$features, function(f) is.factor(panel$claims[[f]]), TRUE)]
  lapply(0:(panel$L - 1L), function(l) {
    rows <- period_rows_train(panel, l)
    stats::setNames(lapply(fac, function(f) unique(as.character(panel$claims[[f]][rows]))), fac)
  })
}

# Warn when claims completed in a period carry a factor level that the period's training
# claims lack: rpart treats the level as missing at each split on the feature (surrogate
# splits, else the majority direction), so the prediction is not informed by the level.
# Fits made by older versions of the package (no `train_levels`) are not checked.
warn_unseen_levels <- function(panel, train_levels) {
  if (is.null(train_levels)) return(invisible(NULL))
  found <- character()
  for (l in 0:(panel$L - 1L)) {
    rows <- period_rows_predict(panel, l)
    if (!length(rows)) next
    for (f in names(train_levels[[l + 1L]])) {
      lv <- setdiff(unique(as.character(panel$claims[[f]][rows])), train_levels[[l + 1L]][[f]])
      lv <- lv[!is.na(lv)]               # a missing value is not a level (na_features = "surrogate")
      if (length(lv)) found <- c(found, sprintf("`%s` = %s in period %d", f,
                                                paste(lv, collapse = "/"), l))
    }
  }
  if (length(found))
    warning("factor level(s) absent from the training claims of the period they are predicted in: ",
            paste(found, collapse = "; "), ". At each split on that feature the trees treat such ",
            "a claim as missing (surrogate splits, else the majority direction)", call. = FALSE)
  invisible(NULL)
}
