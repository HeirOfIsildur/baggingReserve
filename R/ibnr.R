#' Payments of the claims incurred but not reported
#'
#' Adds the IBNR component to a [reserve_prediction], following Theorem 1 and
#' Section 3.6 of \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025): the numbers of claims still to be
#' reported are projected from the reporting-count triangle with a chain-ladder-type
#' method and held fixed across replicates; the payment of such a claim in development
#' year `l` is estimated by the pooled average payment of the reported claims in
#' development year `l`,
#' \deqn{\hat\mu_l = \frac{\sum_i \hat R_{i,l}}{\sum_i \sum_{d \le l,\; i + d \le t} N_{i,d}},}
#' where \eqn{\hat R_{i,l}} are the observed plus predicted payments of the reported
#' claims (a replicate of the prediction) and \eqn{N_{i,d}} the observed numbers of
#' claims of accident year \eqn{i} reported with delay \eqn{d}. The IBNR payment in cell
#' \eqn{(i, l)} is the projected number of claims of accident year \eqn{i} reported
#' after the valuation date with delay \eqn{\le l}, times \eqn{\hat\mu_l}.
#'
#' The estimator assumes that the pooled payment per development year of the reported
#' claims applies to the claims reported late; a payment pattern that depends on the
#' reporting delay or a business mix that differs between early and late reporters
#' invalidates it (see the JSS manuscript, Section 2.4).
#'
#' An accident year without any reported claim at the valuation date has an all-zero row in
#' the count triangle, and the chain ladder projects no late-reported claims from it: such
#' years get no IBNR payments, with a warning. The function has no argument for an external
#' count projection; when this matters, estimate the IBNR payments of such years outside the
#' package (for example, as in the formula above, externally projected numbers of late
#' claims with delay `<= l` times the pooled mean payments `ibnr_details$mu`), or leave those
#' accident years out of the panel (argument `accident_years` of [claims_panel()]).
#'
#' In the last development years \eqn{\hat\mu_l} rests on little information: only the oldest
#' accident years are observed there, and the other accident years contribute the predictions of
#' the late trees, which are trained on those few accident years. A few recoveries can then make
#' \eqn{\hat\mu_l} negative in every replicate (on the claims of type 2 of the shipped portfolio,
#' for example, \eqn{\hat\mu_L} is negative, and so is the IBNR payment of calendar year 23);
#' inspect `ibnr_details$mu`, and `ibnr_details$divisor`, the reported claims behind it, before
#' relying on the late cells.
#'
#' The projected numbers of claims are the same in every replicate, so the spread of the IBNR
#' component over the replicates comes from the pooled mean payments \eqn{\hat\mu_l} only; it
#' contains no uncertainty of the count projection.
#'
#' With `projection = "incremental_ratio"` and `round = TRUE` the count projection uses
#' the source article's incremental-ratio factors and rounding convention: undefined
#' (`NA`/`NaN`) projected counts become zero, an infinite projected count is an error, and
#' the completed count matrix is rounded once. This applies the source article's
#' computational conventions; it does not reproduce its numerical results on the shipped
#' portfolio. The default is the standard chain ladder on cumulative counts without
#' rounding.
#'
#' @param pred A [reserve_prediction] (from `predict()` on a fit). An existing IBNR
#'   component is replaced.
#' @param counts The *incremental* reporting-count triangle of the panel the prediction was
#'   made for. `NULL` (default) uses the triangle stored in the prediction
#'   ([claim_counts()] of that panel). A supplied triangle must agree with the stored one in
#'   its observed cells (it can only differ in how it is passed, e.g. without attributes);
#'   for a prediction made by an older version of the package, which stores no counts, it
#'   must have the same dimensions, observation mask, accident years and valuation date as
#'   the prediction, with finite non-negative integer observed cells.
#' @param projection Count projection, `"chain_ladder"` (default) or `"incremental_ratio"`;
#'   see [chain_ladder()].
#' @param round Round the projected numbers of claims to integers (default `FALSE`).
#' @return `pred` with the components `ibnr` (matrix, future cells by replicates) and
#'   `ibnr_details` added: the projected count triangle (`counts_projected`), the
#'   projected numbers of late-reported claims per cell (`counts_future_cumulative`), the
#'   development `factors`, the `divisor` (observed reported-claim counts per development
#'   year), `mu` (replicates by development years), and the projection settings.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' # a small subset for a quick example: the claims of one type
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' tree <- reserve_tree(panel, seed = 1)
#' res <- ibnr_reserve(predict(tree), claim_counts(panel))
#' calendar_year_payments(res, "total")
#' @export
ibnr_reserve <- function(pred, counts = NULL, projection = c("chain_ladder", "incremental_ratio"),
                         round = FALSE) {
  if (!inherits(pred, "reserve_prediction"))
    stop("`pred` must be a reserve_prediction", call. = FALSE)
  projection <- match.arg(projection)
  stored <- pred$counts
  if (is.null(counts)) {
    if (is.null(stored))
      stop("the prediction stores no count triangle (made by an older version); ",
           "supply `counts = claim_counts(panel)`", call. = FALSE)
    counts <- stored
  }
  if (isTRUE(attr(counts, "cumulative")))
    stop("`counts` must be the incremental count triangle (claim_counts(panel), not cumulative = TRUE)",
         call. = FALSE)
  N <- as.matrix(counts)
  if (!identical(dim(N), dim(pred$observed)))
    stop("`counts` must have the shape of the prediction's triangle (", nrow(pred$observed),
         " x ", ncol(pred$observed), ")", call. = FALSE)
  obs <- !is.na(N)
  if (!identical(unname(obs), unname(!is.na(pred$observed))))
    stop("`counts` and the prediction have different observation masks (different t?)",
         call. = FALSE)
  if (!is.null(rownames(N)) && !identical(rownames(N), as.character(pred$accident_years)))
    stop("`counts` has other accident years than the prediction", call. = FALSE)
  if (!is.null(attr(N, "t")) && !identical(as.integer(attr(N, "t")), as.integer(pred$t)))
    stop("`counts` was built for another valuation date than the prediction", call. = FALSE)
  if (any(!is.finite(N[obs])) || any(N[obs] < 0) || any(N[obs] != round(N[obs])))
    stop("observed counts must be finite non-negative integers", call. = FALSE)
  if (!is.null(stored) && !isTRUE(all.equal(unname(N[obs]), unname(as.matrix(stored)[obs]),
                                            check.attributes = FALSE)))
    stop("`counts` differs from the reporting counts of the panel the prediction was made for ",
         "(another portfolio, or cumulative counts?); omit `counts` to use the stored triangle",
         call. = FALSE)
  empty <- rowSums(N * obs, na.rm = TRUE) == 0
  if (any(empty))
    warning("no reported claims in accident year(s) ",
            paste(if (is.null(rownames(N))) which(empty) else rownames(N)[empty], collapse = ", "),
            ": the chain ladder projects no IBNR claims for them", call. = FALSE)

  ## projected numbers of claims still to be reported, fixed across replicates ------------
  cl <- chain_ladder(N, input = "incremental", projection = projection)
  N_hat <- cl$incremental
  n_undefined <- 0L
  if (projection == "incremental_ratio") {
    # the 2023 code set NA/NaN projected counts to zero and left Inf untouched
    n_undefined <- sum(is.na(N_hat))
    N_hat[is.na(N_hat)] <- 0
    if (any(is.infinite(N_hat)))
      stop("infinite projected counts (positive count over a zero column sum); ",
           "the incremental-ratio projection cannot represent this case", call. = FALSE)
  }
  if (round) N_hat <- base::round(N_hat)
  N_future <- N_hat
  N_future[obs] <- 0
  U <- t(apply(N_future, 1L, cumsum))            # future claims of AY i with delay <= l
  if (nrow(N) == 1L) U <- matrix(U, 1L)
  dimnames(U) <- dimnames(N)

  ## pooled average payment per reported claim and development year, per replicate ---------
  N_obs <- N; N_obs[!obs] <- 0
  Ncum <- t(apply(N_obs, 1L, cumsum)); if (nrow(N) == 1L) Ncum <- matrix(Ncum, 1L)
  D <- colSums(Ncum)                             # reported claims with delay <= l, all AYs
  cells <- pred$cells
  B <- pred$B
  mu <- matrix(NA_real_, B, ncol(N), dimnames = list(colnames(pred$reported), colnames(N)))
  ibnr <- matrix(0, nrow(cells), B, dimnames = dimnames(pred$reported))
  base_tri <- pred$observed; base_tri[is.na(base_tri)] <- 0
  exposure <- colSums(U)
  # IBNR claims expected in a development year without any reported claim to learn the
  # payment from. A fitted panel always has reported claims at delay 0 (period 0 needs
  # training claims), so D is positive; guard against hand-made inputs.
  if (any(D == 0 & exposure > 0))
    stop("IBNR severity unidentified: no reported claims in development year(s) ",
         paste(colnames(N)[D == 0 & exposure > 0], collapse = ", "), call. = FALSE)
  for (b in seq_len(B)) {
    R <- base_tri
    R[cells$index] <- pred$reported[, b]
    cs <- colSums(R)
    m <- cs / D
    m[D == 0] <- 0                              # no reported claims and no exposure
    mu[b, ] <- m
    ib <- sweep(U, 2L, m, `*`)
    ibnr[, b] <- ib[cells$index]
  }
  pred$ibnr <- ibnr
  pred$ibnr_details <- list(projection = projection, round = round, factors = cl$factors,
                            counts_projected = N_hat, counts_future_cumulative = U,
                            divisor = D, mu = mu, n_undefined_set_to_zero = n_undefined)
  pred
}
