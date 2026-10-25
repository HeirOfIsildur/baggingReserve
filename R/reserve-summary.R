#' Summarise a reserve prediction over its replicates
#'
#' Summarises the future payments by component and calendar year across the retained
#' replicate predictions: for every available component (reported-claim payments, IBNR
#' component, total) and every future calendar year plus the total through development
#' year `L`, the mean, standard deviation and empirical quantiles across the replicates.
#' What the quantiles describe depends on the prediction, whose `variability` records it (see
#' [predict.reserve_tree()]): for the replicates of [bag_reserve()], each made by the trees of one
#' bootstrap sample (unpruned by default, unlike the pruned trees of [reserve_tree()]), the
#' resampling variation of the bagged trees; for the simulated completions of a single-tree fit
#' (`predict(fit, completion = "draw")`), the process variability of payments simulated from that
#' one fitted model, without estimation variability; for a bagged fit with
#' `completion = "draw"`, the resampling variation of one set of bootstrap trees (samples drawn
#' per development year) together with one simulated completion from each. They are in no case
#' prediction intervals for the future payments
#' (the IBNR claim counts are held fixed and the model is taken as given) nor confidence
#' intervals for the mean. The printed summary states which case applies.
#'
#' @param object A [reserve_prediction].
#' @param probs Probabilities of the empirical quantiles (numeric in `[0, 1]`; default
#'   `c(0.025, 0.25, 0.5, 0.75, 0.975, 0.995)`). The `q99.5%` column is the 99.5% quantile of the
#'   replicate (or simulated) predictions, as described above, not the 99.5% value at risk of the
#'   Solvency II capital requirement nor an upper prediction bound for the payments; `print()`
#'   says so for every probability of 0.99 or more.
#' @param type Quantile type passed to [stats::quantile()], an integer from 1 to 9. The
#'   source article used order statistics of 10,000 replicates, which corresponds to
#'   `type = 1`; the default is R's `type = 7`.
#' @param ... Not used; an argument given here (e.g. a misspelt `probs`) is an error.
#' @return An object of class `"summary.reserve_prediction"`: a list with `tables`, a
#'   named list of one matrix per component (rows: future calendar years and `total`;
#'   columns: `mean`, `sd`, `min`, the quantiles, `max`; `sd` is `NA` and only `mean` is
#'   printed when the prediction has a single column), plus `B`, `estimator`, `completion`,
#'   `variability`, `probs`, `type`, `t` and `L`.
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
  check_unused_dots(list(...), "summary", c("probs", "type"))
  check_quantile_type(type)
  comps <- c("reported", if (!is.null(object$ibnr)) c("ibnr", "total"))
  tabs <- lapply(comps, function(cmp) {
    cy <- calendar_year_payments(object, component = cmp)
    summarise_rows(cy, probs, type)
  })
  names(tabs) <- comps
  info <- pred_info(object)
  structure(list(tables = tabs, B = object$B, estimator = object$estimator,
                 completion = info$completion, variability = info$variability, probs = probs,
                 type = type, t = object$t, L = object$L, info = info),
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
  info <- if (is.null(x$info)) pred_info(x) else x$info
  cat("Reserve prediction summary (", info$fit, "), ",
      if (x$B > 1L) paste0(info$columns, " retained: ", if (info$kind == "tree") "R" else "B", " = ", x$B)
      else if (info$collapsed) info$point else "one prediction column", "\n", sep = "")
  cat("Rows: payments in the future calendar years and their total through development year ",
      x$L, ".\n", sep = "")
  if (x$B > 1L)
    cat(paste(strwrap(paste0("Quantiles (type ", x$type, ") are empirical quantiles over the ", info$columns,
                             ": they describe ", info$spread, ". They are not prediction intervals for the ",
                             "payments (the IBNR claim counts are held fixed and the model is taken as given) ",
                             "nor confidence intervals for the mean."), width = 90), collapse = "\n"),
        "\n", sep = "")
  hi <- x$probs[x$probs >= 0.99]
  if (x$B > 1L && length(hi)) {
    lab <- paste0("q", vapply(100 * hi, function(p) format(p, trim = TRUE, drop0trailing = TRUE), ""), "%")
    cat(paste(strwrap(paste0(paste(lab, collapse = ", "), if (length(hi) > 1L) " are " else " is ", "the ",
                             if (length(hi) > 1L) "corresponding " else "", sub("s$", "", info$quantiles),
                             if (length(hi) > 1L) "s" else "", " of the predictions, not a capital requirement, a value at ",
                             "risk or an upper prediction bound for the payments."), width = 90), collapse = "\n"),
        "\n", sep = "")
  }
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
#' next to the truth; it describes the variability recorded in the prediction (resampling
#' variation of bagged trees, simulated payments of one fitted model, or both; see
#' [summary.reserve_prediction()]), so "truth inside the range" is a descriptive statement, not a
#' coverage probability.
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
#' responsibility. The claims of the prediction must carry in `claims` the accident years and,
#' when the reporting year is known, the reporting years they were predicted with (the reporting
#' delay is a covariate of the fit); a contradiction is an error. (A prediction saved before the
#' reporting delays were stored in it, `claim_rep_delays`, is only checked for a reporting year
#' by `t`.) Claims whose accident year is not among the prediction's accident years
#' (for example accident years after the valuation date), and their payments, are not scored;
#' their numbers and amount are reported in `not_scored`. Claims that the panel set aside with
#' `claims_panel(beyond_L = "drop")` (reported by `t` with a reporting delay above `L`, recorded in
#' the prediction as `beyond_L_ids`) belong to the portfolio but not to the prediction: they are
#' not counted as missing from it, and their payments after `t`, which all fall beyond development
#' year `L`, are part of the excluded tail, also when their accident year lies outside the
#' prediction's accident years (the panel's default range is determined after setting them aside);
#' `not_scored` records their number and the number and amount of those payments, and does not count
#' them among the claims outside the accident years.
#'
#' **Chronology.** The truth is held to the data contract of [claims_panel()]: when `claims`
#' carries the reporting year, a claim reported before its accident year, and a payment dated
#' before the reporting year of its claim (which the model defines as zero, and which the
#' constructor refuses), are errors naming an example; move such a payment to the reporting
#' year, or correct the reporting year, as for the constructor. A claim of the prediction's
#' accident years without a reporting year is treated, as by the constructor, as not reported by
#' `t`: its payments after `t` count as IBNR truth, with a warning. Without a reporting-year column
#' (see `rep_year`) none of these checks, nor the population checks above, can be made.
#'
#' **Column names.** The defaults of `id`, `acc_year`, `event_year`, `paid` and `rep_year` are the
#' column names of the input tables of the panel the prediction was made from (stored in the
#' prediction as `mapping`), so that a portfolio with another layout is back-tested with the
#' names given once to [claims_panel()]; a name passed here overrides it. The reporting year
#' follows the rule of [claims_panel()]: when the panel was built from a reporting-delay column
#' (`rep_delay`) and `claims` has that column, the reporting year is the accident year plus the
#' delay, whether or not `claims` also has a reporting-year column, which must then agree with it
#' wherever both are known; otherwise it is the reporting-year column. A `rep_year` passed
#' explicitly is used instead. For a prediction without a mapping (made by an earlier
#' version) the defaults are the names shown in the usage.
#'
#' **Follow-up.** Every truth cell up to `observed_to` starts at zero and receives the payments
#' dated in it, so a calendar year without any payment counts as a realised zero. By default
#' (`observed_to = NULL`) the payments are taken to cover every calendar year of the prediction;
#' if the extract ends earlier, pass the last fully observed calendar year as `observed_to`:
#' the cells after it are unobserved (`NA` in `truth_cells`), their rows have `NA` truth, error
#' and range check, and the `total` rows (truth, prediction, range and reference alike) sum the
#' calendar years up to `observed_to` only (`total_years` of the result). The function does not
#' infer the end of the follow-up from the data, because a fully observed year can have no
#' payment; it warns when `observed_to` is `NULL` and the payments hold no transaction in the
#' last calendar years of the prediction. Payments dated after `observed_to` are not scored.
#'
#' **Reference.** A `reference` is scored against the truth of the population it predicts:
#' `reference_component = "total"` (the default) for a prediction of all claims, such as a
#' chain ladder on the panel's paid triangle, which needs a prediction with an IBNR component
#' (attach it with [ibnr_reserve()]); `"reported"` for a named vector predicting the payments of
#' the reported claims only. A [chain_ladder] reference always predicts all claims, and it must
#' have been fitted on the paid triangle of the prediction's panel: the same accident years,
#' development years `0, ..., L`, valuation date and observed payments (a chain ladder of
#' another triangle can be passed as the named vector [calendar_year_payments()] of it). A
#' named vector must hold every future calendar year of the prediction and no other year, plus
#' optionally a `total`, which must equal the sum over those years; the `total` row of the
#' reference is that sum (over the observed years only when `observed_to` cuts the follow-up).
#'
#' **IBNR claim counts.** When `pred` has an IBNR component and `claims` carries the reporting
#' year, the result holds `counts`, the projected and the realised numbers of IBNR claims by
#' accident year: projected, the chain-ladder count projection stored by [ibnr_reserve()]
#' (`ibnr_details$counts_projected`) summed over the cells after the valuation date; realised,
#' the claims of `claims` absent from the prediction and reported after `t` with a reporting
#' delay of at most `L` (and by `observed_to`, when given). The count error is a part of the
#' IBNR error that the pooled mean payment does not explain.
#'
#' @param pred A [reserve_prediction] (with or without an IBNR component).
#' @param claims,payments Claims and payments tables with the columns named below.
#' @param id,acc_year,event_year,paid Column names, as in [claims_panel()]; by default those the
#'   prediction's panel was built with (see the section on column names).
#' @param probs Two increasing probabilities in `[0, 1]` defining the replicate range shown.
#' @param rep_year Name of the reporting-year column of `claims` (by default the one the panel was
#'   built with), used for the population and chronology checks above and the IBNR claim counts.
#'   When `claims` has no such column these are skipped with a warning; `rep_year = NULL` skips
#'   them silently.
#' @param type Quantile algorithm for the replicate range, as in [stats::quantile()]
#'   (an integer from 1 to 9; default 7, as in [summary.reserve_prediction()]).
#' @param reference Optional prediction of another method, scored against the same truth (see
#'   the section on the reference): a [chain_ladder] object fitted on the paid triangle of the
#'   prediction's panel, or a named numeric vector of payments by future calendar year, with an
#'   optional `total` element (as returned by [calendar_year_payments()]). It adds the columns
#'   `reference`, `reference_error` and `reference_rel_error` to the table of the component
#'   given by `reference_component`.
#' @param reference_component The population the `reference` predicts: `"total"` (default; all
#'   claims) or `"reported"` (the claims reported by `t`; named vectors only).
#' @param observed_to Last fully observed calendar year of `payments`, after the valuation date
#'   `t`; `NULL` (default) takes every calendar year of the prediction as observed. See the
#'   section on the follow-up.
#' @return An object of class `"reserve_backtest"`: a list with `tables`, per component a data
#'   frame with one row per future calendar year plus `total` (truth, predicted mean,
#'   `error = predicted - truth`, `rel_error = error / truth`, `NA` where the truth is zero; where
#'   the realised amount is negative, a calendar year of net recoveries, `rel_error` has the sign
#'   opposite to that of `error`, so an overprediction shows a negative relative error and `error`
#'   gives the direction; `reference_rel_error` is defined alike; then
#'   the replicate range `lower`, `upper` and whether the truth lies inside it,
#'   `inside_range`; the range columns are `NA` for a prediction with a single column, such as a
#'   single-tree fit, whose range is degenerate); the cell-level truth matrices (`truth_cells`);
#'   the excluded tail (`tail`); `counts` (the IBNR claim counts described above, or `NULL`);
#'   `total_years`, the calendar years summed in the `total` rows; `observed_to`;
#'   `reference_component` (`NULL` without a reference); `not_scored`, the claims and the
#'   payments after `t` outside the prediction's accident years, the payments after
#'   `observed_to` and the claims set aside by `beyond_L = "drop"` with their payments after `t`
#'   (`n_claims_beyond_L`, `n_payments_beyond_L`, `amount_beyond_L`); the column names used (`columns`); and the settings.
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
#' backtest(res, cl_full, pay_full, reference = chain_ladder(as_triangle(panel)))
#' # an extract that ends three years after the valuation date
#' short <- pay_full[pay_full$EventYear <= 15, ]
#' backtest(res, cl_full, short, observed_to = 15)$tables$total
#' @export
backtest <- function(pred, claims, payments, id = c(claims = "Id", payments = "Id"),
                     acc_year = "AccYear", event_year = "EventYear", paid = "Paid",
                     probs = c(0.025, 0.975), rep_year = "RepYear", type = 7L, reference = NULL,
                     reference_component = c("total", "reported"), observed_to = NULL) {
  if (!inherits(pred, "reserve_prediction")) stop("`pred` must be a reserve_prediction", call. = FALSE)
  check_probs_pair(probs)
  check_quantile_type(type)
  reference_component <- match.arg(reference_component)
  # the column names default to those the prediction's panel was built with
  mp <- pred$mapping
  rep_year_given <- !missing(rep_year)
  if (!is.null(mp)) {
    if (missing(id)) id <- mp$id
    if (missing(acc_year)) acc_year <- mp$acc_year
    if (missing(event_year)) event_year <- mp$event_year
    if (missing(paid)) paid <- mp$paid
    if (!rep_year_given) rep_year <- mp$rep_year
  }
  if (length(id) == 1L && is.null(names(id))) id <- c(claims = id, payments = id)
  for (nm in c(id[["claims"]], acc_year)) if (!nm %in% names(claims))
    stop("`claims` lacks column ", nm, call. = FALSE)
  for (nm in c(id[["payments"]], event_year, paid)) if (!nm %in% names(payments))
    stop("`payments` lacks column ", nm, call. = FALSE)
  t <- pred$t; L <- pred$L; ays <- pred$accident_years
  years <- sort(unique(pred$cells$calendar))
  last_year <- if (length(years)) max(years) else t
  if (!is.null(observed_to)) {
    observed_to <- as_scalar_integer(observed_to, "observed_to")
    if (observed_to <= t)
      stop("`observed_to` must be a calendar year after the valuation date t = ", t, call. = FALSE)
  }
  obs_end <- if (is.null(observed_to)) Inf else observed_to
  cid <- claims[[id[["claims"]]]]
  if (anyNA(cid) || anyDuplicated(cid))
    stop("missing or duplicated claim identifiers in `claims`", call. = FALSE)
  missing_ids <- setdiff(pred$claim_ids, cid)
  if (length(missing_ids))
    stop(length(missing_ids), " claim(s) of the prediction are absent from `claims`", call. = FALSE)
  ay <- as_integer_column(claims[[acc_year]], acc_year, allow_na = FALSE)
  # the claims of the prediction must keep the accident years they were predicted with (the
  # truth is filtered and placed in cells by accident year)
  if (!is.null(pred$claim_acc_years)) {
    ay_pred <- ay[match(pred$claim_ids, cid)]
    changed <- ay_pred != pred$claim_acc_years
    if (any(changed))
      stop(sum(changed), " claim(s) of the prediction have another accident year in `claims` (e.g. claim ",
           format(pred$claim_ids[changed][1L]), ": ", ay_pred[changed][1L], " in `claims`, ",
           pred$claim_acc_years[changed][1L], " in the prediction): the truth must describe the same claims",
           call. = FALSE)
  }
  # the reporting year, by the rule of claims_panel(): from the reporting-delay column when the
  # panel was built from one (a reporting-year column, if also present, must agree with it where
  # both are known), else from the reporting-year column; a rep_year named here takes precedence
  ry <- NULL; ry_label <- rep_year
  delay_col <- if (!rep_year_given && !is.null(mp$rep_delay) && mp$rep_delay %in% names(claims)) mp$rep_delay
  if (!is.null(delay_col)) {
    ry <- ay + as_integer_column(claims[[delay_col]], delay_col, allow_na = TRUE)
    ry_label <- paste0(acc_year, " + ", delay_col)
    if (!is.null(rep_year) && rep_year %in% names(claims)) {
      ry_col <- as_integer_column(claims[[rep_year]], rep_year, allow_na = TRUE)
      differ <- !is.na(ry) & !is.na(ry_col) & ry != ry_col
      if (any(differ))
        stop("`", rep_year, "` and `", acc_year, " + ", delay_col, "` disagree for ", sum(differ),
             " claim(s) in `claims` (e.g. claim ", format(cid[differ][1L]), ": ", ry_col[differ][1L], " and ",
             ry[differ][1L], "), which claims_panel() refuses", call. = FALSE)
    }
  } else if (!is.null(rep_year) && rep_year %in% names(claims)) {
    ry <- as_integer_column(claims[[rep_year]], rep_year, allow_na = TRUE)
  } else if (!is.null(rep_year)) {
    warning("`claims` has no reporting-year column `", rep_year, "`: the checks that the truth describes the ",
            "prediction's population and the data contract's chronology, and the IBNR claim counts, are ",
            "skipped. Name the column with rep_year = (rep_year = NULL skips them without this warning)",
            call. = FALSE)
  }
  has_ry <- !is.null(ry)
  set_aside <- if (is.null(pred$beyond_L_ids)) cid[0L] else pred$beyond_L_ids
  in_aside <- cid %in% set_aside
  if (has_ry) {
    # the chronology of the data contract (claims_panel()): reported on or after the accident year
    before_acc <- !is.na(ry) & ry < ay
    if (any(before_acc))
      stop(sum(before_acc), " claim(s) in `claims` are reported before their accident year (e.g. claim ",
           format(cid[before_acc][1L]), ": accident year ", ay[before_acc][1L], ", reported ", ry[before_acc][1L],
           "), which claims_panel() refuses", call. = FALSE)
    no_ry <- is.na(ry) & !cid %in% pred$claim_ids & ay %in% pred$accident_years
    if (any(no_ry))
      warning(sum(no_ry), " claim(s) of the prediction's accident years have no reporting year in `claims`: as ",
              "by claims_panel(), they are treated as not reported by t = ", pred$t, ", so their payments after t ",
              "count as IBNR truth", call. = FALSE)
    # the claims of the prediction keep the reporting years they were predicted with (the
    # reporting delay is a covariate of the fit); predictions made before 0.2.0's
    # claim_rep_delays was stored are checked for a reporting year by t only
    if (!is.null(pred$claim_rep_delays)) {
      d_truth <- ry[match(pred$claim_ids, cid)] - ay[match(pred$claim_ids, cid)]
      changed <- is.na(d_truth) | d_truth != pred$claim_rep_delays
      if (any(changed)) {
        k <- which(changed)[1L]
        stop(sum(changed), " claim(s) of the prediction have another reporting year in `claims` (e.g. claim ",
             format(pred$claim_ids[k]), ": ", if (is.na(d_truth[k])) "none" else pred$claim_acc_years[k] + d_truth[k],
             " according to `", ry_label, "`, ", pred$claim_acc_years[k] + pred$claim_rep_delays[k],
             " in the prediction): the truth must describe the same claims", call. = FALSE)
      }
    }
    # the claims of the prediction were reported by t
    late_pred <- ry[match(pred$claim_ids, cid)] > pred$t
    if (any(late_pred, na.rm = TRUE))
      stop(sum(late_pred, na.rm = TRUE), " claim(s) of the prediction are recorded in `claims` as reported after ",
           "t = ", pred$t, ", although the prediction treats them as reported by t: the truth must describe the ",
           "same claims (check the reporting-year column `", rep_year, "`)", call. = FALSE)
    # claims set aside by claims_panel(beyond_L = "drop") belong to the portfolio but not to the prediction
    extra <- !cid %in% pred$claim_ids & !in_aside & ay %in% pred$accident_years
    if (any(ry[extra] <= pred$t, na.rm = TRUE))
      stop(sum(ry[extra] <= pred$t, na.rm = TRUE), " claim(s) in `claims` were reported by t = ", pred$t,
           " but are absent from the prediction: the truth must describe the same portfolio as ",
           "the prediction (select it by the same criterion, not by the reported identifiers)", call. = FALSE)
    aside_short <- in_aside & !is.na(ry) & ry - ay <= pred$L
    if (any(aside_short))
      stop(sum(aside_short), " claim(s) that the panel set aside with beyond_L = \"drop\" (a reporting delay above ",
           "L = ", pred$L, ") have a reporting delay of at most L in `claims`: the truth must describe the same claims",
           call. = FALSE)
  }
  # the opposite mistake: a truth restricted to the reported claims has no IBNR claims at all,
  # so the IBNR truth is zero and the total error looks small
  late <- !cid %in% pred$claim_ids & ay %in% pred$accident_years
  if (has_ry) late <- late & ry > pred$t
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
  if (has_ry) {
    # a payment before the reporting year is zero by the model's definition, and claims_panel() refuses it
    early <- !is.na(ry[m]) & ey < ry[m]
    if (any(early)) {
      k <- which(early)[1L]
      stop(sum(early), " payment(s) in `payments` are dated before the reporting year of their claim (e.g. claim ",
           format(pid[k]), ": paid in ", ey[k], ", reported in ", ry[m][k], " according to `", ry_label, "`). ",
           "The model defines payments before reporting as zero and claims_panel() refuses them; move such a ",
           "payment to the reporting year, or correct the reporting year", call. = FALSE)
    }
  }
  if (!any(ey > pred$t)) {
    warning("`payments` holds no transaction after t = ", pred$t, ": the realised future payments ",
            "are taken as zero. Pass the payments that followed the valuation date", call. = FALSE)
  } else if (is.null(observed_to) && max(ey) < last_year) {
    warning("`payments` holds no transaction after calendar year ", max(ey), ", while the prediction ",
            "runs to ", last_year, ": the later calendar years are scored as realised zero payments. ",
            "If the extract ends before ", last_year, ", pass observed_to = the last fully observed ",
            "calendar year (at most ", max(ey), ")", call. = FALSE)
  }
  pay_ay <- ay[m]
  dev <- ey - pay_ay
  future <- ey > t & pay_ay %in% ays & ey <= obs_end
  # the payments after t of the claims set aside by claims_panel(beyond_L = "drop"), all beyond
  # development year L, belong to the excluded tail whatever their accident year: the panel's
  # accident-year range is determined after setting them aside and need not include theirs
  aside_pay <- in_aside[m] & ey > t & ey <= obs_end
  tail <- (future & dev > L) | aside_pay
  keep <- future & dev <= L
  reported <- cid[m] %in% pred$claim_ids
  cal_mat <- outer(ays, 0:L, `+`)
  truth_cell <- function(sel) {
    M <- matrix(0, length(ays), L + 1L, dimnames = list(ays, 0:L))
    if (any(sel)) {
      s <- rowsum(amt[sel], group = paste(match(pay_ay[sel], ays), dev[sel]), reorder = FALSE)
      ij <- do.call(rbind, strsplit(rownames(s), " ", fixed = TRUE))
      M[cbind(as.integer(ij[, 1L]), as.integer(ij[, 2L]) + 1L)] <- s[, 1L]
    }
    M[cal_mat > obs_end] <- NA_real_        # not observed: no truth
    M
  }
  truth <- list(reported = truth_cell(keep & reported), ibnr = truth_cell(keep & !reported))
  truth$total <- truth$reported + truth$ibnr
  partial <- any(years > obs_end)
  total_years <- years[years <= obs_end]
  comps <- c("reported", if (!is.null(pred$ibnr)) c("ibnr", "total"))
  tables <- lapply(comps, function(cmp) {
    pr <- calendar_year_payments(pred, component = cmp)
    tr_cells <- truth[[cmp]][pred$cells$index]
    tr <- rowsum(tr_cells, group = factor(pred$cells$calendar, levels = years), reorder = TRUE)[, 1L]
    if (partial) {
      # the total rows cover the observed calendar years only, for truth and prediction alike
      tr <- c(tr, total = sum(tr[years <= obs_end]))
      pr["total", ] <- colSums(pr[as.character(total_years), , drop = FALSE])
    } else {
      tr <- c(tr, total = sum(tr))
    }
    mean_pred <- rowMeans(pr)
    if (ncol(pr) > 1L) {
      q <- t(apply(pr, 1L, stats::quantile, probs = probs, names = FALSE, type = type))
      if (nrow(pr) == 1L) q <- matrix(q, 1L)
    } else {
      q <- matrix(NA_real_, nrow(pr), 2L)    # one prediction column: no replicate range
    }
    data.frame(calendar = rownames(pr), truth = unname(tr), predicted = unname(mean_pred),
               error = unname(mean_pred - tr),
               rel_error = unname(ifelse(tr != 0, (mean_pred - tr) / tr, NA_real_)),
               lower = q[, 1L], upper = q[, 2L],
               inside_range = unname(tr >= q[, 1L] & tr <= q[, 2L]),
               stringsAsFactors = FALSE, row.names = NULL)
  })
  names(tables) <- comps
  if (!is.null(reference)) {
    cmp <- reference_component
    if (inherits(reference, "chain_ladder")) {
      if (cmp != "total")
        stop("a chain_ladder `reference` predicts the payments of all claims; it is scored with ",
             "reference_component = \"total\" only", call. = FALSE)
      check_reference_chain_ladder(reference, pred)
      ref <- calendar_year_payments(reference)
    } else {
      ref <- reference
    }
    if (!cmp %in% names(tables))
      stop("`reference` is scored against the truth of all claims (reference_component = \"total\"), ",
           "but `pred` has no IBNR component, so it predicts the reported claims only: attach the ",
           "IBNR component with ibnr_reserve() first, or pass reference_component = \"reported\" ",
           "for a reference that predicts the reported claims only", call. = FALSE)
    tab <- tables[[cmp]]
    yrs <- as.character(years)
    if (!is.numeric(ref) || is.null(names(ref)) || !all(tab$calendar[tab$calendar != "total"] %in% names(ref)))
      stop("`reference` must be a chain_ladder object or a named numeric vector with the calendar years ",
           paste(tab$calendar, collapse = ", "), call. = FALSE)
    other <- setdiff(names(ref), c(yrs, "total"))
    if (length(other))
      stop("`reference` has calendar year(s) the prediction does not have (", paste(other, collapse = ", "),
           "): it describes another horizon or portfolio", call. = FALSE)
    if (anyDuplicated(names(ref))) stop("`reference` has duplicated names", call. = FALSE)
    vals <- unname(ref[yrs])
    if (any(!is.finite(vals[years <= obs_end])) || ("total" %in% names(ref) && !is.finite(ref[["total"]])))
      stop("`reference` must hold finite payments for every scored calendar year (and a finite `total`)",
           call. = FALSE)
    if ("total" %in% names(ref) && !isTRUE(all.equal(ref[["total"]], sum(vals))))
      stop("the `total` of `reference` (", format(ref[["total"]], big.mark = ","), ") is not the sum of its ",
           "calendar years ", paste(range(years), collapse = "-"), " (", format(sum(vals), big.mark = ","),
           "): the total must cover the calendar years of the prediction", call. = FALSE)
    ref_total <- if (!partial && "total" %in% names(ref)) ref[["total"]] else sum(vals[years <= obs_end])
    tab$reference <- c(vals, ref_total)
    tab$reference_error <- tab$reference - tab$truth
    tab$reference_rel_error <- ifelse(tab$truth != 0, tab$reference_error / tab$truth, NA_real_)
    tables[[cmp]] <- tab
  }
  counts <- NULL
  if (!is.null(pred$ibnr) && has_ry && !is.null(pred$ibnr_details$counts_projected)) {
    Nf <- bare_triangle(pred$ibnr_details$counts_projected)
    Nf[!future_mask(ays, L, t) | cal_mat > obs_end] <- 0     # claims still to be reported, observed
    projected <- rowSums(Nf)
    in_horizon <- !cid %in% pred$claim_ids & ay %in% ays & !is.na(ry) & ry > t & ry - ay <= L &
      ry <= obs_end
    realised <- tabulate(match(ay[in_horizon], ays), nbins = length(ays))
    counts <- data.frame(acc_year = c(as.character(ays), "total"),
                         projected = c(unname(projected), sum(projected)),
                         realised = c(realised, sum(realised)), stringsAsFactors = FALSE)
    counts$error <- counts$projected - counts$realised
    counts$rel_error <- ifelse(counts$realised != 0, counts$error / counts$realised, NA_real_)
  }
  outside <- !ay %in% ays & !in_aside
  pay_outside <- ey > t & !pay_ay %in% ays & !in_aside[m]
  after_end <- ey > obs_end & (pay_ay %in% ays | in_aside[m])
  structure(list(tables = tables, truth_cells = truth,
                 tail = list(n = sum(tail), amount = sum(amt[tail]),
                             max_dev = if (any(tail)) max(dev[tail]) else NA_integer_),
                 n_claims_future = length(unique(cid[m][keep])),
                 counts = counts, total_years = total_years, observed_to = observed_to,
                 reference_component = if (!is.null(reference)) reference_component,
                 not_scored = list(n_claims_outside = sum(outside),
                                   n_payments_outside = sum(pay_outside),
                                   amount_outside = sum(amt[pay_outside]),
                                   n_payments_after_observed_to = sum(after_end),
                                   amount_after_observed_to = sum(amt[after_end]),
                                   n_claims_beyond_L = sum(in_aside),
                                   n_payments_beyond_L = sum(aside_pay),
                                   amount_beyond_L = sum(amt[aside_pay])),
                 columns = list(id = id, acc_year = acc_year, event_year = event_year, paid = paid,
                                rep_year = if (has_ry) ry_label),
                 B = pred$B, estimator = pred$estimator, completion = pred_info(pred)$completion,
                 variability = pred_info(pred)$variability, info = pred_info(pred),
                 probs = probs, type = type, t = t, L = L),
            class = "reserve_backtest")
}

# A chain_ladder reference must describe the prediction's paid triangle: the same accident
# years, development years, valuation date (observation mask) and observed payments.
check_reference_chain_ladder <- function(cl, pred) {
  ays <- as.character(pred$accident_years)
  ays_cl <- rownames(cl$incremental)
  if (is.null(ays_cl) || !identical(ays_cl, ays))
    stop("the chain_ladder `reference` has accident years ",
         if (is.null(ays_cl)) "without names" else paste(range(as.integer(ays_cl)), collapse = "-"),
         ", the prediction ", paste(range(pred$accident_years), collapse = "-"),
         ": it was fitted on another triangle", call. = FALSE)
  if (!identical(colnames(cl$incremental), as.character(0:pred$L)))
    stop("the chain_ladder `reference` has development years ",
         paste(range(as.integer(colnames(cl$incremental))), collapse = "-"), ", the prediction 0-", pred$L,
         ": it was fitted on another triangle", call. = FALSE)
  cl_t <- attr(cl$input, "t")                  # set by as_triangle()
  if (is.numeric(cl_t) && length(cl_t) == 1L && !identical(as.integer(cl_t), as.integer(pred$t)))
    stop("the chain_ladder `reference` was fitted at valuation date t = ", cl_t, ", the prediction at t = ",
         pred$t, call. = FALSE)
  obs <- !is.na(bare_triangle(pred$observed))
  if (!identical(unname(cl$observed), unname(obs)))
    stop("the chain_ladder `reference` has another observation mask than the prediction (another ",
         "valuation date?)", call. = FALSE)
  if (!isTRUE(all.equal(unname(cl$incremental[obs]), unname(bare_triangle(pred$observed)[obs]))))
    stop("the chain_ladder `reference` was fitted on other observed payments than the prediction's panel ",
         "(another portfolio?); to score a chain ladder of another triangle, pass calendar_year_payments() ",
         "of it", call. = FALSE)
  invisible(cl)
}

check_quantile_type <- function(type) {
  if (length(type) != 1L || !is.numeric(type) || !is.finite(type) || !type %in% 1:9)
    stop("`type` must be a quantile type, an integer from 1 to 9", call. = FALSE)
  invisible(type)
}

#' @export
print.reserve_backtest <- function(x, digits = 0L, ...) {
  info <- if (is.null(x$info)) pred_info(x) else x$info
  cat("Back-test of the ", info$fit, " (", if (info$kind == "tree" && info$completion == "draw") "R" else "B",
      " = ", x$B, ") against realised payments through development year ",
      x$L, "\n", "predicted = ", if (x$B > 1L || info$collapsed) info$point else "point prediction",
      "; error = predicted - realised\n", sep = "")
  if (x$B > 1L)
    cat(paste(strwrap(paste0("lower/upper = ", info$quantiles, " ", x$probs[1L], " and ", x$probs[2L], " over the ",
                             info$columns, ": ", info$spread, "; `inside` is descriptive, not a coverage ",
                             "probability"), width = 90), collapse = "\n"), "\n", sep = "")
  if (!is.null(x$observed_to) && length(x$total_years) &&
      any(as.integer(setdiff(x$tables[[1L]]$calendar, "total")) > x$observed_to))
    cat("Payments observed through calendar year ", x$observed_to, ": later years have no truth (NA), ",
        "and `total` covers calendar years ", min(x$total_years), "-", max(x$total_years), " only\n", sep = "")
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
    if (!is.null(tab$reference)) {
      show$reference <- round(tab$reference, digits)
      show$`ref_error_%` <- round(100 * tab$reference_rel_error, 1)
    }
    print(show, row.names = FALSE)
  }
  if (any(vapply(x$tables, function(tb) any(tb$truth < 0, na.rm = TRUE), TRUE)))
    cat("\nrel_error_% = error / truth: where the realised amount is negative (net recoveries) its sign is the ",
        "opposite of the error's\n", sep = "")
  if (!is.null(x$counts)) {
    cat("\nNumbers of IBNR claims (reported after t = ", x$t, " with delay <= ", x$L,
        if (!is.null(x$observed_to)) paste0(", by ", x$observed_to), "), projected by the chain ladder ",
        "on counts and realised:\n", sep = "")
    cn <- x$counts
    print(data.frame(acc_year = cn$acc_year, projected = round(cn$projected, 1), realised = cn$realised,
                     error = round(cn$error, 1), `rel_error_%` = round(100 * cn$rel_error, 1),
                     check.names = FALSE), row.names = FALSE)
  }
  if (x$tail$n > 0L)
    cat("\nExcluded future payments beyond development year ", x$L, ": ", x$tail$n, " payment(s), amount ",
        format(round(x$tail$amount), big.mark = ","), " (max development ", x$tail$max_dev, ")\n",
        sep = "")
  ns <- x$not_scored
  if (!is.null(ns) && ns$n_claims_outside > 0L)
    cat("Not scored: ", ns$n_claims_outside, " claim(s) outside the prediction's accident years, with ",
        ns$n_payments_outside, " payment(s) after t, amount ", format(round(ns$amount_outside), big.mark = ","),
        "\n", sep = "")
  if (isTRUE(ns$n_claims_beyond_L > 0L))
    cat("Set aside by claims_panel(beyond_L = \"drop\"): ", ns$n_claims_beyond_L, " claim(s) reported by t with a ",
        "reporting delay above L; their ", ns$n_payments_beyond_L, " payment(s) after t (amount ",
        format(round(ns$amount_beyond_L), big.mark = ","), ") are part of the excluded tail\n", sep = "")
  if (!is.null(ns) && ns$n_payments_after_observed_to > 0L)
    cat("Not scored: ", ns$n_payments_after_observed_to, " payment(s) dated after observed_to = ",
        x$observed_to, ", amount ", format(round(ns$amount_after_observed_to), big.mark = ","), "\n", sep = "")
  invisible(x)
}
