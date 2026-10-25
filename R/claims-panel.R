#' Build a claim-by-development panel from individual claims and payments
#'
#' Turns two long tables, one row per claim and one row per payment, into the
#' observation-masked panel on which the reserving method works: one row per claim
#' reported by the valuation date `t`, static features, the reporting delay, and the
#' incremental payments in development years `0, ..., L`. Cells that lie after the
#' valuation date are `NA`; observed cells without payments are `0`.
#'
#' The constructor enforces the valuation date on both tables: claims are kept only if
#' reported by `t` (reporting year `<= t`), payments only if made by `t`. Claims reported after
#' `t` are dropped, and so are claims whose accident year is after `t` (they cannot be reported
#' by `t`); both are counted in `n_unreported_dropped`. A claim without a reporting year (or
#' delay) is treated as not reported by `t` and dropped, with a warning, since a missing date is
#' more often a data defect than a late report. It preserves
#' recoveries (negative payments), keeps reported claims without any payment, and
#' refuses input it cannot interpret: duplicated or missing identifiers, payments on
#' claims absent from `claims`, payments before the accident or reporting year,
#' reporting delays or observed payments beyond `L` unless `beyond_L = "drop"` is given, and
#' payments after `t` unless `future_payments = "drop"` is given explicitly (the dropped numbers
#' and amounts are recorded in the diagnostics). Payments dated before the recorded reporting year (advance
#' payments, or reporting dates corrected after a first payment) are refused: move such a
#' payment to the reporting year, or correct the reporting year, before calling the
#' constructor.
#'
#' @param claims Data frame with one row per claim.
#' @param payments Data frame with one row per payment (several payments of one claim
#'   in the same year are summed).
#' @param t Valuation (reserving) date, an integer year on the same grid as the accident
#'   and event years. Claims reported after `t` and payments after `t` are not part of
#'   the panel. The grid need not be yearly: any integer time unit works (quarters or months
#'   numbered consecutively, say), and "development year" then means development period.
#' @param L Maximal development year; payments have columns `X0, ..., XL`. It is not
#'   inferred from the data. The fitting functions need training claims for the model of every
#'   development year up to `L - 1`, i.e. claims with `acc_year + L <= t`; when the latest accident
#'   year is `t`, `L` is therefore at most the number of accident years less one ([reserve_tree()]
#'   and [bag_reserve()] stop otherwise, naming the development years without training claims).
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
#'   the aggregate triangles. The default runs from the earliest to the latest accident
#'   year with a reported claim, gaps included. Accident years after the latest one are
#'   not added, because a portfolio in run-off has none; when that latest year is before `t`,
#'   a warning says so, since otherwise the most recent accident years, whose claims may all be
#'   reported later (a long-tail line, a new line of business), would silently have neither a row
#'   nor an IBNR component. To include them, pass them, e.g. `accident_years = first:t`: their rows
#'   then exist (empty), but the chain ladder projects no IBNR claims from an empty row, so
#'   [ibnr_reserve()] warns again and their IBNR payments must be estimated outside the package
#'   (see [ibnr_reserve()]). For a portfolio that wrote no business in those years, pass
#'   `accident_years = first:last` to state this and silence the warning.
#' @param na_features `"fail"` (default) refuses missing feature values; `"surrogate"`
#'   keeps them for \pkg{rpart}'s handling of missing values: a claim whose value of the split
#'   feature is missing is sent down by the first surrogate split it has a value for, and by the
#'   majority direction of the split when no surrogate resolves it (with rpart's defaults
#'   `usesurrogate = 2`, `maxsurrogate = 5`; a user-supplied `control` changes this). Claims
#'   with a missing value do not count when a split on that feature is chosen.
#' @param future_payments `"error"` (default) refuses payments dated after `t`;
#'   `"drop"` discards them and records how many and how much.
#' @param beyond_L `"error"` (default) refuses reported claims with a reporting delay above `L`
#'   and observed payments in development years after `L`, which are outside the model; `"drop"`
#'   discards them (such claims together with all their payments) and records how many and how
#'   much (`n_claims_beyond_L_dropped`, `n_payments_beyond_L_dropped`,
#'   `amount_beyond_L_dropped`), for a long history whose oldest accident years have developed
#'   beyond `L`. The identifiers of the claims set aside are kept (`beyond_L_ids`) and passed on to
#'   the predictions made from the panel, so that [backtest()] accepts the complete claims and
#'   payments tables: it does not count these claims as missing from the prediction, and their
#'   payments after `t`, all beyond development year `L`, are part of the excluded tail it reports.
#'
#' @return An object of class `"claims_panel"`: a list with elements
#' \describe{
#'   \item{claims}{data frame with `id`, `acc_year`, `rep_delay` and the feature columns}
#'   \item{payments}{numeric matrix, one row per claim, columns `X0, ..., XL`; `NA` after `t`}
#'   \item{observed}{logical matrix of the same shape, `TRUE` where `acc_year + l <= t`}
#'   \item{t, L, accident_years, I}{valuation date, horizon, accident years, their number}
#'   \item{features, feature_levels}{the feature names and the factor levels}
#'   \item{mapping, diagnostics, call}{column mapping (carried into the predictions, where
#'     [backtest()] uses it as its default column names), construction counts, the call}
#'   \item{beyond_L_ids}{identifiers of the reported claims set aside by `beyond_L = "drop"`
#'     (empty otherwise)}
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
                         future_payments = c("error", "drop"), beyond_L = c("error", "drop")) {
  na_features <- match.arg(na_features)
  future_payments <- match.arg(future_payments)
  beyond_L <- match.arg(beyond_L)
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
  if (anyNA(d))
    warning(sum(is.na(d)), " claim(s) have no ", if (is.null(rep_delay)) "reporting year" else "reporting delay",
            " and are treated as not reported by t = ", t, " (dropped)", call. = FALSE)
  keep <- !is.na(d) & (ay + d <= t)          # reported by the valuation date (so ay <= t)
  n_unreported <- sum(!keep)
  if (!any(keep)) stop("no claim is reported by t = ", t, call. = FALSE)
  long_delay <- keep & d > L
  if (any(long_delay)) {
    if (beyond_L == "error")
      stop(sum(long_delay), " reported claim(s) have a reporting delay above L = ", L,
           "; choose a larger L, or pass beyond_L = \"drop\" to discard them", call. = FALSE)
    keep <- keep & !long_delay
    if (!any(keep)) stop("no claim reported by t = ", t, " has a reporting delay of at most L = ", L, call. = FALSE)
  }

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
  on_long <- pid %in% cid[long_delay]           # claims dropped for a delay above L (beyond_L = "drop")
  # a payment dated by t on a claim not reported by t contradicts the claims table
  if (any(!on_kept & !on_long & ey <= t))
    stop(sum(!on_kept & !on_long & ey <= t), " payment(s) dated by t = ", t,
         " belong to claims not reported by t", call. = FALSE)
  n_pay_unreported <- sum(!on_kept & !on_long)
  dropped_beyond <- list(n_claims = sum(long_delay), n = sum(on_long), amount = sum(amt[on_long]))
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
  # data extracted before the end of year t would turn the unobserved last diagonal into zeros
  if (length(ey) && !any(ey == t))
    warning("no payment of the reported claims is dated in year t = ", t, ": if the data end ",
            "before year t, its cells are not observed zeros; choose t as the last complete year",
            call. = FALSE)
  row <- match(pid, kept_ids)
  dev <- ey - ay[keep][row]
  if (any(dev < 0))
    stop(sum(dev < 0), " payment(s) are dated before the accident year", call. = FALSE)
  if (any(dev < d[keep][row]))
    stop(sum(dev < d[keep][row]), " payment(s) are dated before the reporting year",
         call. = FALSE)
  if (any(dev > L)) {
    if (beyond_L == "error")
      stop(sum(dev > L), " observed payment(s) fall beyond development year L = ", L,
           "; choose a larger L, or pass beyond_L = \"drop\" to discard them", call. = FALSE)
    late <- dev > L
    dropped_beyond$n <- dropped_beyond$n + sum(late)
    dropped_beyond$amount <- dropped_beyond$amount + sum(amt[late])
    pid <- pid[!late]; ey <- ey[!late]; amt <- amt[!late]; row <- row[!late]; dev <- dev[!late]
  }

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
    accident_years <- seq.int(min(ay[keep]), max(ay[keep]))
    last <- max(accident_years)
    if (last < t) {
      later <- sum(ay > last & ay <= t)        # claims of the later years in `claims`, none reported by t
      warning("the latest accident year with a claim reported by t = ", t, " is ", last, ": accident year(s) ",
              last + 1L, if (t > last + 1L) paste0("-", t), " have no row in the panel and no IBNR component",
              if (later) paste0(" (`claims` holds ", later, " claim(s) of these years, none reported by t)"),
              ". If the portfolio has claims of these years, pass accident_years = ", min(accident_years), ":", t,
              " (their IBNR payments must then be estimated outside the package; see ?ibnr_reserve); if it has ",
              "none, pass accident_years = ", min(accident_years), ":", last, " to say so", call. = FALSE)
    }
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
    beyond_L_ids = cid[long_delay],
    diagnostics = list(
      n_claims_in = nrow(claims), n_reported = n, n_unreported_dropped = n_unreported,
      n_payments_in = nrow(payments), n_payments_on_unreported = n_pay_unreported,
      n_future_payments_dropped = dropped_future$n,
      future_amount_dropped = dropped_future$amount,
      n_claims_beyond_L_dropped = dropped_beyond$n_claims,
      n_payments_beyond_L_dropped = dropped_beyond$n,
      amount_beyond_L_dropped = dropped_beyond$amount,
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
  dg <- x$diagnostics
  n_on_unrep <- if (is.null(dg$n_payments_on_unreported)) 0L else dg$n_payments_on_unreported
  if (dg$n_unreported_dropped > 0L || dg$n_future_payments_dropped > 0L)
    cat("  dropped: ", dg$n_unreported_dropped, " claim(s) not reported by t",
        if (n_on_unrep > 0L) paste0(", with ", n_on_unrep, " payment(s) after t"), ";\n           ",
        dg$n_future_payments_dropped, " payment(s) after t of the reported claims\n", sep = "")
  if (isTRUE(dg$n_claims_beyond_L_dropped > 0L || dg$n_payments_beyond_L_dropped > 0L))
    cat("  beyond L: ", dg$n_claims_beyond_L_dropped, " claim(s) with a reporting delay above L and ",
        dg$n_payments_beyond_L_dropped, " payment(s) dropped (amount ", format(dg$amount_beyond_L_dropped, big.mark = ","),
        ")\n", sep = "")
  invisible(x)
}

#' @rdname claims_panel
#' @param object A `claims_panel` (for `summary`).
#' @param x A `claims_panel` (for `as.data.frame`).
#' @param ... For `summary`, not used: an argument given there is an error. Ignored by `as.data.frame`.
#' @export
summary.claims_panel <- function(object, ...) {
  check_unused_dots(list(...), "summary")
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
  if (inherits(x, c("Date", "POSIXt")))
    stop("`", name, "` holds dates; times must be integer years (or periods numbered consecutively): ",
         "convert the dates first, e.g. with as.integer(format(", name, ", \"%Y\"))", call. = FALSE)
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    was_na <- is.na(x) | !nzchar(trimws(x))
    num <- suppressWarnings(as.numeric(x))
    bad <- is.na(num) & !was_na
    if (any(bad))
      stop("`", name, "` has values that are not integers, e.g. \"", x[bad][1L], "\"", call. = FALSE)
    x <- num
  }
  if (!is.numeric(x)) stop("`", name, "` must be integer-valued", call. = FALSE)
  if (!allow_na && anyNA(x)) stop("missing values in `", name, "`", call. = FALSE)
  finite <- !is.na(x)
  if (any(!is.finite(x[finite])) || any(abs(x[finite]) > .Machine$integer.max))
    stop("`", name, "` has infinite or out-of-range values", call. = FALSE)
  if (any(x[finite] != round(x[finite]))) stop("`", name, "` must be integer-valued", call. = FALSE)
  as.integer(round(x))
}

# S3 methods with `...` (needed for the generic) would silently ignore a misspelt or misplaced
# argument -- predict(bag, completion = "draw") would return the plug-in replicates. They call
# this instead: any argument in `...` is an error, with a hint at the intended argument.
check_unused_dots <- function(dots, fun, own = character(), hints = NULL) {
  if (!length(dots)) return(invisible())
  nm <- names(dots); if (is.null(nm)) nm <- rep("", length(dots))
  lab <- ifelse(nzchar(nm), paste0("`", nm, "`"), "an unnamed argument")
  hint <- vapply(nm, function(b) {
    if (!is.null(hints) && b %in% names(hints)) return(paste0(" (", hints[[b]], ")"))
    if (!nzchar(b) || !length(own)) return("")
    d <- utils::adist(b, own); if (min(d) <= 2) paste0(" (did you mean `", own[which.min(d)], "`?)") else ""
  }, "")
  stop("unused argument", if (length(dots) > 1L) "s", " to ", fun, "(): ", paste0(lab, hint, collapse = ", "),
       call. = FALSE)
}

is_claims_panel <- function(x) inherits(x, "claims_panel")

check_panel <- function(x, arg = "panel") {
  if (!is_claims_panel(x)) stop("`", arg, "` must be a claims_panel", call. = FALSE)
  invisible(x)
}
