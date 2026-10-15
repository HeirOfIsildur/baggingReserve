#' Summarise a reserve prediction over its replicates
#'
#' Summarises the future payments by component and calendar year across the retained
#' replicate predictions: for every available component (reported-claim payments, IBNR
#' component, total) and every future calendar year plus the total through development
#' year `L`, the mean, standard deviation and empirical quantiles across the replicates.
#' The quantiles are those of single-tree reserve predictions under the bootstrap
#' resampling of [bag_reserve()]; they describe resampling variation and are neither
#' prediction intervals for the future payments nor confidence intervals for the bagged
#' mean.
#'
#' @param object A [reserve_prediction].
#' @param probs Probabilities of the empirical quantiles (numeric in `[0, 1]`; default
#'   `c(0.025, 0.25, 0.5, 0.75, 0.975, 0.995)`).
#' @param type Quantile type passed to [stats::quantile()], an integer from 1 to 9. The
#'   source article used order statistics of 10,000 replicates, which corresponds to
#'   `type = 1`; the default is R's `type = 7`.
#' @param ... Unused.
#' @return An object of class `"summary.reserve_prediction"`: a list with `tables`, a
#'   named list of one matrix per component (rows: future calendar years and `total`;
#'   columns: `mean`, `sd`, `min`, the quantiles, `max`; `sd` is `NA` and only `mean` is
#'   printed when the prediction has a single column), plus `B`, `estimator`, `probs`,
#'   `type`, `t` and `L`.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' res <- ibnr_reserve(predict(bag_reserve(panel, B = 5, seed = 2026)), claim_counts(panel))
#' summary(res, probs = c(0.1, 0.5, 0.9))$tables$total
#' @export
summary.reserve_prediction <- function(object, probs = c(0.025, 0.25, 0.5, 0.75, 0.975, 0.995),
                                       type = 7L, ...) {
  comps <- c("reported", if (!is.null(object$ibnr)) c("ibnr", "total"))
  tabs <- lapply(comps, function(cmp) {
    cy <- calendar_year_payments(object, component = cmp)
    summarise_rows(cy, probs, type)
  })
  names(tabs) <- comps
  structure(list(tables = tabs, B = object$B, estimator = object$estimator, probs = probs,
                 type = type, t = object$t, L = object$L),
            class = "summary.reserve_prediction")
}

summarise_rows <- function(m, probs, type) {
  q <- apply(m, 1L, stats::quantile, probs = probs, type = type, names = FALSE)
  q <- if (length(probs) == 1L) matrix(q, ncol = 1L) else t(matrix(q, nrow = length(probs)))
  out <- cbind(mean = rowMeans(m),
               sd = if (ncol(m) > 1L) apply(m, 1L, stats::sd) else NA_real_,
               min = apply(m, 1L, min), q, max = apply(m, 1L, max))
  qn <- vapply(100 * probs, function(p) format(p, trim = TRUE, drop0trailing = TRUE), "")
  colnames(out) <- c("mean", "sd", "min", paste0("q", qn, "%"), "max")
  rownames(out) <- rownames(m)
  out
}

#' @export
print.summary.reserve_prediction <- function(x, digits = 0L, ...) {
  label <- if (identical(x$estimator, "tree")) "single-tree fit" else
    if (identical(x$estimator, "bagging")) "bagged fit" else x$estimator
  cat("Reserve prediction summary (", label, "), ",
      if (x$B > 1L) paste0("replicates retained: B = ", x$B) else "one prediction column", "\n", sep = "")
  cat("Rows: payments in the future calendar years and their total through development year ",
      x$L, ".\n", sep = "")
  if (x$B > 1L)
    cat("Quantiles (type ", x$type, ") are empirical quantiles over the replicates: they describe\n",
        "resampling variation, not prediction intervals or confidence intervals for the bagged mean.\n", sep = "")
  for (cmp in names(x$tables)) {
    cat("\n", switch(cmp, reported = "Reported-claim payments", ibnr = "IBNR component", total = "All claims"),
        ":\n", sep = "")
    tab <- x$tables[[cmp]]
    if (x$B == 1L) tab <- tab[, "mean", drop = FALSE]
    print(round(tab, digits))
  }
  invisible(x)
}

#' Back-test a reserve prediction against realised payments
#'
#' Compares the predicted future payments with the payments that actually followed,
#' cell by cell, from a table of claims and a table of payments that extend beyond the
#' valuation date (for the shipped data: [synthetic_claims_full] and
#' [synthetic_payments_full]). The truth is split into the component of the claims that
#' were reported by the valuation date (those in the prediction) and the claims reported
#' later (IBNR), and restricted to development years `0, ..., L`; payments beyond `L` are
#' reported separately as the excluded tail. The prediction's replicate range is shown
#' next to the truth; it describes resampling variation, so "truth inside the range" is a
#' descriptive statement, not a coverage probability.
#'
#' The truth tables must describe the same portfolio as the prediction, *including* the
#' claims reported after the valuation date: the IBNR component of the truth consists of the
#' claims in `claims` that are absent from the prediction, so selecting the truth by the
#' identifiers of the reported claims removes the IBNR claims from it, and an unrelated claim
#' in `claims` would be counted as IBNR. Select the population by the same criterion as the
#' panel (all claims of a type, say), not by identifiers. When `claims` carries the reporting
#' year (`rep_year`), the function rejects claims absent from the prediction that were
#' reported by the valuation date; it cannot detect IBNR claims that are missing from the
#' truth tables altogether, so the completeness of the truth population remains the caller's
#' responsibility.
#'
#' @param pred A [reserve_prediction] (with or without an IBNR component).
#' @param claims,payments Claims and payments tables with the columns named below.
#' @param id,acc_year,event_year,paid Column names, as in [claims_panel()].
#' @param probs Two increasing probabilities in `[0, 1]` defining the replicate range shown.
#' @param rep_year Name of the reporting-year column of `claims`, used for the population
#'   checks above when present; ignored otherwise.
#' @param type Quantile algorithm for the replicate range, as in [stats::quantile()]
#'   (default 7, as in [summary.reserve_prediction()]).
#' @return An object of class `"reserve_backtest"`: per component a data frame with one
#'   row per future calendar year plus `total` (truth, predicted mean, error, relative
#'   error, the replicate range and whether the truth lies inside it), the cell-level
#'   truth matrices, and the excluded tail.
#' @examples
#' data("synthetic_claims", "synthetic_payments", "synthetic_claims_full",
#'      "synthetic_payments_full", package = "baggingReserve")
#' # a small subset for a quick example: the claims of one type
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)), claim_counts(panel))
#' # the truth of the same portfolio: all claims of that type, including those reported
#' # after the valuation date (selecting by the reported claims' identifiers would drop them)
#' cl_full <- synthetic_claims_full[synthetic_claims_full$Type == "3", ]
#' pay_full <- synthetic_payments_full[synthetic_payments_full$Id %in% cl_full$Id, ]
#' backtest(res, cl_full, pay_full)
#' @export
backtest <- function(pred, claims, payments, id = c(claims = "Id", payments = "Id"),
                     acc_year = "AccYear", event_year = "EventYear", paid = "Paid",
                     probs = c(0.025, 0.975), rep_year = "RepYear", type = 7L) {
  if (!inherits(pred, "reserve_prediction")) stop("`pred` must be a reserve_prediction", call. = FALSE)
  check_probs_pair(probs)
  if (length(id) == 1L && is.null(names(id))) id <- c(claims = id, payments = id)
  for (nm in c(id[["claims"]], acc_year)) if (!nm %in% names(claims))
    stop("`claims` lacks column ", nm, call. = FALSE)
  for (nm in c(id[["payments"]], event_year, paid)) if (!nm %in% names(payments))
    stop("`payments` lacks column ", nm, call. = FALSE)
  t <- pred$t; L <- pred$L; ays <- pred$accident_years
  cid <- claims[[id[["claims"]]]]
  if (anyNA(cid) || anyDuplicated(cid))
    stop("missing or duplicated claim identifiers in `claims`", call. = FALSE)
  missing_ids <- setdiff(pred$claim_ids, cid)
  if (length(missing_ids))
    stop(length(missing_ids), " claim(s) of the prediction are absent from `claims`", call. = FALSE)
  ay <- as_integer_column(claims[[acc_year]], acc_year, allow_na = FALSE)
  if (!is.null(rep_year) && rep_year %in% names(claims)) {
    ry <- as_integer_column(claims[[rep_year]], rep_year, allow_na = TRUE)
    extra <- !cid %in% pred$claim_ids & ay %in% pred$accident_years
    if (any(ry[extra] <= pred$t, na.rm = TRUE))
      stop(sum(ry[extra] <= pred$t, na.rm = TRUE), " claim(s) in `claims` were reported by t = ", pred$t,
           " but are absent from the prediction: the truth must describe the same portfolio as ",
           "the prediction (select it by the same criterion, not by the reported identifiers)", call. = FALSE)
  }
  # the opposite mistake: a truth restricted to the reported claims has no IBNR claims at all,
  # so the IBNR truth is zero and the total error looks small
  late <- !cid %in% pred$claim_ids & ay %in% pred$accident_years
  if (!is.null(rep_year) && rep_year %in% names(claims)) late <- late & ry > pred$t
  if (!is.null(pred$ibnr) && sum(pred$ibnr) > 0 && !any(late, na.rm = TRUE))
    warning("`claims` holds no claim reported after t = ", pred$t, " in the prediction's accident ",
            "years, while the prediction projects IBNR payments: the realised IBNR payments are ",
            "taken as zero. If the truth was selected by the reported claims' identifiers, select ",
            "it by the criterion that defined the portfolio instead", call. = FALSE)
  pid <- payments[[id[["payments"]]]]
  ey <- as_integer_column(payments[[event_year]], event_year, allow_na = FALSE)
  amt <- payments[[paid]]
  if (!is.numeric(amt) || !all(is.finite(amt)))
    stop("`", paid, "` must be finite numeric", call. = FALSE)
  amt <- as.double(amt)          # rowsum() keeps integer storage and could overflow otherwise
  m <- match(pid, cid)
  if (anyNA(m)) stop(sum(is.na(m)), " payment(s) refer to claims absent from `claims`", call. = FALSE)
  pay_ay <- ay[m]
  dev <- ey - pay_ay
  future <- ey > t & pay_ay %in% ays
  tail <- future & dev > L
  keep <- future & dev <= L
  reported <- cid[m] %in% pred$claim_ids
  truth_cell <- function(sel) {
    M <- matrix(0, length(ays), L + 1L, dimnames = list(ays, 0:L))
    if (any(sel)) {
      s <- rowsum(amt[sel], group = paste(match(pay_ay[sel], ays), dev[sel]), reorder = FALSE)
      ij <- do.call(rbind, strsplit(rownames(s), " ", fixed = TRUE))
      M[cbind(as.integer(ij[, 1L]), as.integer(ij[, 2L]) + 1L)] <- s[, 1L]
    }
    M
  }
  truth <- list(reported = truth_cell(keep & reported), ibnr = truth_cell(keep & !reported))
  truth$total <- truth$reported + truth$ibnr
  comps <- c("reported", if (!is.null(pred$ibnr)) c("ibnr", "total"))
  tables <- lapply(comps, function(cmp) {
    pr <- calendar_year_payments(pred, component = cmp)
    tr_cells <- truth[[cmp]][pred$cells$index]
    years <- sort(unique(pred$cells$calendar))
    tr <- rowsum(tr_cells, group = factor(pred$cells$calendar, levels = years), reorder = TRUE)[, 1L]
    tr <- c(tr, total = sum(tr))
    mean_pred <- rowMeans(pr)
    q <- t(apply(pr, 1L, stats::quantile, probs = probs, names = FALSE, type = type))
    if (nrow(pr) == 1L) q <- matrix(q, 1L)
    data.frame(calendar = rownames(pr), truth = unname(tr), predicted = unname(mean_pred),
               error = unname(mean_pred - tr),
               rel_error = unname(ifelse(tr != 0, (mean_pred - tr) / tr, NA_real_)),
               lower = q[, 1L], upper = q[, 2L],
               inside_range = unname(tr >= q[, 1L] & tr <= q[, 2L]),
               stringsAsFactors = FALSE, row.names = NULL)
  })
  names(tables) <- comps
  structure(list(tables = tables, truth_cells = truth,
                 tail = list(n = sum(tail), amount = sum(amt[tail]),
                             max_dev = if (any(tail)) max(dev[tail]) else NA_integer_),
                 n_claims_future = length(unique(cid[m][keep])),
                 B = pred$B, estimator = pred$estimator, probs = probs, type = type, t = t, L = L),
            class = "reserve_backtest")
}

#' @export
print.reserve_backtest <- function(x, digits = 0L, ...) {
  label <- if (identical(x$estimator, "tree")) "single-tree fit" else
    if (identical(x$estimator, "bagging")) "bagged fit" else x$estimator
  cat("Back-test of the ", label, " (B = ", x$B, ") against realised payments through development year ",
      x$L, "\n", "predicted = ", if (x$B > 1L) "bagged mean" else "point prediction",
      "; error = predicted - realised",
      if (x$B > 1L) paste0("; lower/upper = replicate quantiles ", x$probs[1L], " and ", x$probs[2L],
                           " (resampling variation; `inside` is descriptive only)") else "", "\n", sep = "")
  for (cmp in names(x$tables)) {
    cat("\n", switch(cmp, reported = "Reported-claim payments", ibnr = "IBNR component", total = "All claims"),
        ":\n", sep = "")
    tab <- x$tables[[cmp]]
    show <- data.frame(calendar = tab$calendar, truth = round(tab$truth, digits),
                       predicted = round(tab$predicted, digits),
                       error = round(tab$error, digits),
                       rel_error = round(100 * tab$rel_error, 1), check.names = FALSE)
    names(show)[5L] <- "rel_error_%"
    if (x$B > 1L) {
      show$lower <- round(tab$lower, digits); show$upper <- round(tab$upper, digits)
      show$inside <- tab$inside_range
    }
    print(show, row.names = FALSE)
  }
  if (x$tail$n > 0L)
    cat("\nExcluded future payments beyond development year ", x$L, ": ", x$tail$n, " payment(s), amount ",
        format(round(x$tail$amount), big.mark = ","), " (max development ", x$tail$max_dev, ")\n",
        sep = "")
  invisible(x)
}
