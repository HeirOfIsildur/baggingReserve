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

period_sizes <- function(panel) {
  L <- panel$L
  data.frame(period = 0:(L - 1L),
             n_train = vapply(0:(L - 1L), function(l) length(period_rows_train(panel, l)), 1L),
             n_predict = vapply(0:(L - 1L), function(l) length(period_rows_predict(panel, l)), 1L))
}
