#' Build a claim-by-development panel from individual claims and payments
#'
#' Turns two long tables, one row per claim and one row per payment, into the
#' observation-masked panel on which the reserving method works: one row per claim
#' reported by the valuation date `t`, static features, the reporting delay, and the
#' incremental payments in development years `0, ..., L`. Cells that lie after the
#' valuation date are `NA`; observed cells without payments are `0`.
#'
#' The constructor enforces the valuation date on both tables: claims are kept only if
#' reported by `t` (reporting year `<= t`), payments only if made by `t`. It preserves
#' recoveries (negative payments), keeps reported claims without any payment, and
#' refuses input it cannot interpret: duplicated or missing identifiers, payments on
#' claims absent from `claims`, payments before the accident or reporting year,
#' reporting delays or observed payments beyond `L`, and payments after `t` unless
#' `future_payments = "drop"` is given explicitly (the dropped number and amount are
#' recorded in the diagnostics).
#'
#' @param claims Data frame with one row per claim.
#' @param payments Data frame with one row per payment (several payments of one claim
#'   in the same year are summed).
#' @param t Valuation (reserving) date, an integer year on the same grid as the accident
#'   and event years. Claims reported after `t` and payments after `t` are not part of
#'   the panel.
#' @param L Maximal development year; payments have columns `X0, ..., XL`. It is not
#'   inferred from the data.
#' @param id Name of the claim identifier column, either one name used in both tables
#'   or `c(claims = , payments = )`.
#' @param acc_year Name of the accident-year column in `claims`.
#' @param rep_year Name of the reporting-year column in `claims`. Ignored if `rep_delay`
#'   is given and absent from `claims`; if both are present they must agree.
#' @param rep_delay Optional name of a reporting-delay column (`rep_year - acc_year`).
#' @param event_year Name of the payment-year column in `payments`.
#' @param paid Name of the amount column in `payments`.
#' @param features Character vector of static feature columns of `claims` to carry into
#'   the panel as predictors. Character columns become factors with levels in sorted
#'   order; factors keep their levels. The reporting delay is always a predictor and
#'   need not be listed. No feature is ever picked up automatically.
#' @param accident_years Optional integer vector of accident years defining the rows of
#'   the aggregate triangles (default: the sorted accident years present).
#' @param na_features `"fail"` (default) refuses missing feature values; `"surrogate"`
#'   keeps them for \pkg{rpart}'s surrogate-split handling.
#' @param future_payments `"error"` (default) refuses payments dated after `t`;
#'   `"drop"` discards them and records how many and how much.
#'
#' @return An object of class `"claims_panel"`: a list with elements
#' \describe{
#'   \item{claims}{data frame with `id`, `acc_year`, `rep_delay` and the feature columns}
#'   \item{payments}{numeric matrix, one row per claim, columns `X0, ..., XL`; `NA` after `t`}
#'   \item{observed}{logical matrix of the same shape, `TRUE` where `acc_year + l <= t`}
#'   \item{t, L, accident_years, I}{valuation date, horizon, accident years, their number}
#'   \item{features, feature_levels}{the feature names and the factor levels}
#'   \item{mapping, diagnostics, call}{column mapping, construction counts, the call}
#' }
#' @seealso [as_triangle()], [claim_counts()], [reserve_tree()], [bag_reserve()]
#' @examples
#' claims <- data.frame(Id = c("a", "b", "c"), AccYear = c(1, 1, 2), RepYear = c(1, 2, 2),
#'                      Type = c("x", "y", "x"))
#' payments <- data.frame(Id = c("a", "a", "a", "b"), EventYear = c(1, 1, 2, 2),
#'                        Paid = c(10, 5, -2, 7))
#' panel <- claims_panel(claims, payments, t = 3, L = 2, features = "Type")
#' panel
#' panel$payments
#' as_triangle(panel)
#' @export
claims_panel <- function(claims, payments, t, L,
                         id = c(claims = "Id", payments = "Id"),
                         acc_year = "AccYear", rep_year = "RepYear", rep_delay = NULL,
                         event_year = "EventYear", paid = "Paid",
                         features = character(), accident_years = NULL,
                         na_features = c("fail", "surrogate"),
                         future_payments = c("error", "drop")) {
  na_features <- match.arg(na_features)
  future_payments <- match.arg(future_payments)
  if (!is.data.frame(claims)) stop("`claims` must be a data frame", call. = FALSE)
  if (!is.data.frame(payments)) stop("`payments` must be a data frame", call. = FALSE)
  t <- as_scalar_integer(t, "t")
  L <- as_scalar_integer(L, "L")
  if (L < 0L) stop("`L` must be a non-negative integer", call. = FALSE)
  if (length(id) == 1L && is.null(names(id))) id <- c(claims = id, payments = id)
  if (!all(c("claims", "payments") %in% names(id)))
    stop("`id` must be one column name or c(claims = , payments = )", call. = FALSE)
  features <- as.character(features)
  if (anyDuplicated(features)) stop("duplicated `features`", call. = FALSE)
  reserved <- intersect(features, c("id", "acc_year", "rep_delay", paste0("X", 0:L)))
  if (length(reserved))
    stop("feature name(s) clash with panel columns: ", paste(reserved, collapse = ", "),
         call. = FALSE)

  delay_col <- if (is.null(rep_delay)) rep_year else rep_delay
  need_c <- c(id[["claims"]], acc_year, delay_col, features)
  miss_c <- setdiff(need_c, names(claims))
  if (length(miss_c))
    stop("`claims` lacks column(s): ", paste(miss_c, collapse = ", "), call. = FALSE)
  need_p <- c(id[["payments"]], event_year, paid)
  miss_p <- setdiff(need_p, names(payments))
  if (length(miss_p))
    stop("`payments` lacks column(s): ", paste(miss_p, collapse = ", "), call. = FALSE)

  ## claims ----------------------------------------------------------------------------
  cid <- claims[[id[["claims"]]]]
  if (anyNA(cid)) stop("missing claim identifiers in `claims`", call. = FALSE)
  if (anyDuplicated(cid))
    stop("duplicated claim identifiers in `claims`, e.g. ",
         format(cid[anyDuplicated(cid)]), call. = FALSE)
  ay <- as_integer_column(claims[[acc_year]], acc_year, allow_na = FALSE)
  if (any(ay > t))
    stop(sum(ay > t), " claim(s) have an accident year after t = ", t, call. = FALSE)
  if (is.null(rep_delay)) {
    ry <- as_integer_column(claims[[rep_year]], rep_year, allow_na = TRUE)
    d <- ry - ay
  } else {
    d <- as_integer_column(claims[[rep_delay]], rep_delay, allow_na = TRUE)
    if (!is.null(rep_year) && rep_year %in% names(claims)) {
      ry <- as_integer_column(claims[[rep_year]], rep_year, allow_na = TRUE)
      both <- !is.na(ry) & !is.na(d)
      if (any(ry[both] - ay[both] != d[both]))
        stop("`", rep_year, "` and `", rep_delay, "` disagree for ",
             sum(ry[both] - ay[both] != d[both]), " claim(s)", call. = FALSE)
    }
  }
  if (any(d < 0, na.rm = TRUE))
    stop(sum(d < 0, na.rm = TRUE), " claim(s) are reported before their accident year",
         call. = FALSE)
  keep <- !is.na(d) & (ay + d <= t)          # reported by the valuation date
  n_unreported <- sum(!keep)
  if (!any(keep)) stop("no claim is reported by t = ", t, call. = FALSE)
  if (any(d[keep] > L))
    stop(sum(d[keep] > L), " reported claim(s) have a reporting delay above L = ", L,
         "; choose a larger L", call. = FALSE)

  feat <- claims[keep, features, drop = FALSE]
  rownames(feat) <- NULL
  for (f in features) {
    x <- feat[[f]]
    # sorted levels in the C locale, so that the order does not depend on the session
    if (is.character(x)) x <- factor(x, levels = sort(unique(x[!is.na(x)]), method = "radix"))
    if (!(is.numeric(x) || is.factor(x) || is.logical(x)))
      stop("feature `", f, "` must be numeric, factor, character or logical", call. = FALSE)
    if (is.numeric(x) && any(is.infinite(x)))
      stop("infinite values in feature `", f, "`", call. = FALSE)
    if (anyNA(x) && na_features == "fail")
      stop("missing values in feature `", f, "`; use na_features = \"surrogate\" to keep them",
           call. = FALSE)
    feat[[f]] <- x
  }

  ## payments ---------------------------------------------------------------------------
  pid <- payments[[id[["payments"]]]]
  if (anyNA(pid)) stop("missing claim identifiers in `payments`", call. = FALSE)
  ey <- as_integer_column(payments[[event_year]], event_year, allow_na = FALSE)
  amt <- payments[[paid]]
  if (!is.numeric(amt) || !all(is.finite(amt)))
    stop("`", paid, "` must be finite numeric", call. = FALSE)
  amt <- as.double(amt)          # rowsum() keeps integer storage and could overflow otherwise
  if (!all(pid %in% cid))
    stop(sum(!pid %in% cid), " payment(s) refer to claims absent from `claims`", call. = FALSE)
  kept_ids <- cid[keep]
  on_kept <- pid %in% kept_ids
  # a payment dated by t on a claim not reported by t contradicts the claims table
  if (any(!on_kept & ey <= t))
    stop(sum(!on_kept & ey <= t), " payment(s) dated by t = ", t,
         " belong to claims not reported by t", call. = FALSE)
  n_pay_unreported <- sum(!on_kept)
  pid <- pid[on_kept]; ey <- ey[on_kept]; amt <- amt[on_kept]
  after_t <- ey > t
  dropped_future <- list(n = 0L, amount = 0)
  if (any(after_t)) {
    if (future_payments == "error")
      stop(sum(after_t), " payment(s) are dated after t = ", t,
           "; pass future_payments = \"drop\" to discard them", call. = FALSE)
    dropped_future <- list(n = sum(after_t), amount = sum(amt[after_t]))
    pid <- pid[!after_t]; ey <- ey[!after_t]; amt <- amt[!after_t]
  }
  row <- match(pid, kept_ids)
  dev <- ey - ay[keep][row]
  if (any(dev < 0))
    stop(sum(dev < 0), " payment(s) are dated before the accident year", call. = FALSE)
  if (any(dev < d[keep][row]))
    stop(sum(dev < d[keep][row]), " payment(s) are dated before the reporting year",
         call. = FALSE)
  if (any(dev > L))
    stop(sum(dev > L), " observed payment(s) fall beyond development year L = ", L,
         "; choose a larger L", call. = FALSE)

  n <- sum(keep)
  X <- matrix(0, n, L + 1L, dimnames = list(NULL, paste0("X", 0:L)))
  if (length(row)) {
    # rowsum() accumulates in row order from zero: identical arithmetic to adding the
    # payments one by one, which is what the original 2023 code did
    lin <- row + n * dev
    sums <- rowsum(amt, group = lin, reorder = FALSE)
    X[as.numeric(rownames(sums))] <- sums[, 1L]
  }
  obs <- outer(ay[keep], 0:L, function(a, l) a + l <= t)
  dimnames(obs) <- dimnames(X)
  X[!obs] <- NA_real_

  if (is.null(accident_years)) {
    accident_years <- sort(unique(ay[keep]))
  } else {
    accident_years <- sort(unique(as_integer_column(accident_years, "accident_years",
                                                    allow_na = FALSE)))
    if (!all(ay[keep] %in% accident_years))
      stop("`accident_years` does not cover all accident years present", call. = FALSE)
    if (any(accident_years > t))
      stop("`accident_years` must not exceed the valuation date t", call. = FALSE)
  }

  cl <- data.frame(id = cid[keep], acc_year = ay[keep], rep_delay = d[keep],
                   stringsAsFactors = FALSE)
  cl <- cbind(cl, feat)
  rownames(cl) <- NULL

  structure(list(
    claims = cl, payments = X, observed = obs,
    t = t, L = L, accident_years = accident_years, I = length(accident_years),
    features = features,
    feature_levels = lapply(feat[features], function(x) if (is.factor(x)) levels(x) else NULL),
    mapping = list(id = id, acc_year = acc_year, rep_year = rep_year, rep_delay = rep_delay,
                   event_year = event_year, paid = paid),
    diagnostics = list(
      n_claims_in = nrow(claims), n_reported = n, n_unreported_dropped = n_unreported,
      n_payments_in = nrow(payments), n_payments_on_unreported = n_pay_unreported,
      n_future_payments_dropped = dropped_future$n,
      future_amount_dropped = dropped_future$amount,
      n_recoveries = sum(amt < 0),
      n_claims_without_payment = sum(!seq_len(n) %in% row)),
    call = match.call()),
    class = "claims_panel")
}

#' @export
print.claims_panel <- function(x, ...) {
  ays <- x$accident_years
  cat("Claims panel: ", format(nrow(x$claims), big.mark = ","), " claims reported by t = ",
      x$t, "\n", sep = "")
  cat("  accident years ", ays[1L], "-", ays[length(ays)], " (I = ", x$I, "), development years 0-",
      x$L, "\n", sep = "")
  cat("  features: ", if (length(x$features)) paste(x$features, collapse = ", ") else "none",
      " (+ reporting delay)\n", sep = "")
  cat("  observed cells: ", format(sum(x$observed), big.mark = ","), " of ",
      format(length(x$observed), big.mark = ","), "; recoveries: ",
      x$diagnostics$n_recoveries, "; claims without payment: ",
      x$diagnostics$n_claims_without_payment, "\n", sep = "")
  if (x$diagnostics$n_unreported_dropped > 0L || x$diagnostics$n_future_payments_dropped > 0L)
    cat("  dropped: ", x$diagnostics$n_unreported_dropped, " claim(s) not reported by t, ",
        x$diagnostics$n_future_payments_dropped, " payment(s) after t\n", sep = "")
  invisible(x)
}

#' @rdname claims_panel
#' @param object A `claims_panel` (for `summary`).
#' @param x A `claims_panel` (for `as.data.frame`).
#' @param ... Unused.
#' @export
summary.claims_panel <- function(object, ...) {
  structure(list(
    n = nrow(object$claims), t = object$t, L = object$L, I = object$I,
    accident_years = object$accident_years, features = object$features,
    counts = as_triangle(object, what = "counts"),
    paid = as_triangle(object, what = "paid"),
    diagnostics = object$diagnostics), class = "summary.claims_panel")
}

#' @export
print.summary.claims_panel <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Claims panel summary: ", format(x$n, big.mark = ","), " reported claims, t = ", x$t,
      ", L = ", x$L, "\n\n", sep = "")
  cat("Reported claims by accident year x reporting delay (NA = not yet observable):\n")
  print(bare_triangle(x$counts))
  cat("\nIncremental payments by accident year x development year:\n")
  print(round(bare_triangle(x$paid), digits))
  d <- x$diagnostics
  cat("\nConstruction: ", d$n_claims_in, " claims in, ", d$n_unreported_dropped,
      " not reported by t; ", d$n_payments_in, " payments in, ", d$n_payments_on_unreported,
      " on unreported claims, ", d$n_future_payments_dropped, " after t dropped (",
      format(d$future_amount_dropped, big.mark = ","), "); ", d$n_recoveries,
      " recoveries; ", d$n_claims_without_payment, " claims without payment.\n", sep = "")
  invisible(x)
}

#' @rdname claims_panel
#' @export
as.data.frame.claims_panel <- function(x, ...) {
  cbind(x$claims, as.data.frame(x$payments))
}

## ---- internal helpers -----------------------------------------------------------------

as_scalar_integer <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || !is.finite(x) || x != round(x) ||
      abs(x) > .Machine$integer.max)
    stop("`", name, "` must be a single integer (finite, within the integer range)", call. = FALSE)
  as.integer(x)
}

# Two increasing probabilities in [0, 1], for replicate ranges.
check_probs_pair <- function(probs) {
  if (!is.numeric(probs) || length(probs) != 2L || any(!is.finite(probs)) ||
      any(probs < 0 | probs > 1) || probs[1L] >= probs[2L])
    stop("`probs` must be two increasing probabilities in [0, 1]", call. = FALSE)
  invisible(probs)
}

as_integer_column <- function(x, name, allow_na) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    was_na <- is.na(x) | !nzchar(trimws(x))
    x <- suppressWarnings(as.numeric(x))
    if (any(is.na(x) & !was_na))
      stop("`", name, "` has values that are not integers, e.g. ",
           format(x[is.na(x) & !was_na][1L]), call. = FALSE)
  }
  if (!is.numeric(x)) stop("`", name, "` must be integer-valued", call. = FALSE)
  if (!allow_na && anyNA(x)) stop("missing values in `", name, "`", call. = FALSE)
  finite <- !is.na(x)
  if (any(!is.finite(x[finite])) || any(abs(x[finite]) > .Machine$integer.max))
    stop("`", name, "` has infinite or out-of-range values", call. = FALSE)
  if (any(x[finite] != round(x[finite]))) stop("`", name, "` must be integer-valued", call. = FALSE)
  as.integer(round(x))
}

is_claims_panel <- function(x) inherits(x, "claims_panel")

check_panel <- function(x, arg = "panel") {
  if (!is_claims_panel(x)) stop("`", arg, "` must be a claims_panel", call. = FALSE)
  invisible(x)
}
