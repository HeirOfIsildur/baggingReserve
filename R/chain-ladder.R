#' Chain-ladder projection of a run-off triangle
#'
#' Completes a run-off triangle (accident years in rows, development years in columns,
#' `NA` for the cells after the valuation date) with development factors estimated from
#' the observed cells. Two projections are available:
#' \describe{
#'   \item{`"chain_ladder"`}{the standard chain ladder: factors are ratios of
#'     successive column sums of the *cumulative* triangle over the accident years
#'     observed in both columns, and each row is projected from its latest observed
#'     cumulative value. Point predictions only, no tail, no uncertainty measure.}
#'   \item{`"incremental_ratio"`}{the projection used for the baselines of \enc{Janoušek}{Janousek} and
#'     \enc{Pešta}{Pesta} (2025), the source article: ratios of successive column sums of the
#'     *incremental* triangle, applied multiplicatively to the incremental cells. Provided
#'     to apply the source article's convention, not as a reproduction of its results; a
#'     factor with a zero denominator is `NaN` (zero numerator) or `Inf` (positive
#'     numerator), as in the source article's code, and the projected cells that are not
#'     finite are listed in `undefined`.}
#' }
#'
#' @param triangle Numeric matrix with `NA` in the unobserved cells; each row must be
#'   observed in a leading block of columns. Typically [as_triangle()] output.
#' @param input Whether `triangle` holds `"incremental"` or `"cumulative"` amounts. `NULL`
#'   (default) takes it from the triangle's `"cumulative"` attribute, which [as_triangle()]
#'   sets, and assumes incremental amounts when the attribute is absent. An explicit value
#'   that contradicts the attribute is an error.
#' @param projection `"chain_ladder"` (default) or `"incremental_ratio"`, as described
#'   above.
#' @return An object of class `"chain_ladder"`: a list with `factors` (one per
#'   development year `1, ..., L`, with the number of rows used), the completed
#'   `incremental` and `cumulative` triangles, `latest`, `ultimate` and `outstanding`
#'   per accident year, `outstanding_total`, the observation mask and the method.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' panel <- claims_panel(synthetic_claims, synthetic_payments, t = 12, L = 11,
#'                       features = c("Type", "Age", "AccWeekday"))
#' cl <- chain_ladder(as_triangle(panel))
#' cl
#' round(cl$outstanding)
#' @export
chain_ladder <- function(triangle, input = NULL,
                         projection = c("chain_ladder", "incremental_ratio")) {
  flag <- attr(triangle, "cumulative")
  flag <- if (is.logical(flag) && length(flag) == 1L && !is.na(flag)) flag else NULL
  if (is.null(input)) {
    input <- if (isTRUE(flag)) "cumulative" else "incremental"
  } else {
    input <- match.arg(input, c("incremental", "cumulative"))
    if (!is.null(flag) && flag != (input == "cumulative"))
      stop("`input = \"", input, "\"` contradicts the triangle, which is ",
           if (flag) "cumulative" else "incremental", " (attribute \"cumulative\")", call. = FALSE)
  }
  projection <- match.arg(projection)
  M <- as.matrix(triangle)
  if (!is.numeric(M) || ncol(M) < 2L)
    stop("`triangle` must be a numeric matrix with at least two columns", call. = FALSE)
  obs <- !is.na(M)
  if (any(!is.finite(M[obs])))
    stop("`triangle` has ", sum(!is.finite(M[obs])), " infinite observed cell(s); observed cells must be ",
         "finite (NA marks the unobserved cells)", call. = FALSE)
  if (any(!obs[, 1L])) stop("every row must have at least its first cell observed", call. = FALSE)
  if (!all(apply(obs, 1L, function(o) all(diff(o) <= 0))))
    stop("every row of `triangle` must be observed in a leading block of columns", call. = FALSE)
  I <- nrow(M); K <- ncol(M)
  rn <- rownames(M); if (is.null(rn)) rn <- seq_len(I)
  cn <- colnames(M); if (is.null(cn)) cn <- seq_len(K) - 1L

  A <- if (input == "incremental") M
       else cbind(M[, 1L, drop = FALSE], M[, -1L, drop = FALSE] - M[, -K, drop = FALSE])
  C <- t(apply(M, 1L, function(r) { r[is.na(r)] <- 0; cumsum(r) }))
  if (I == 1L) C <- matrix(C, 1L)
  C[!obs] <- NA_real_
  if (input == "cumulative") C[obs] <- M[obs]
  last_obs <- apply(obs, 1L, function(o) max(which(o)))

  basis <- if (projection == "chain_ladder") C else A
  factors <- rep(NA_real_, K - 1L); used <- integer(K - 1L)
  for (j in 2:K) {
    E <- obs[, j] & obs[, j - 1L]
    used[j - 1L] <- sum(E)
    if (any(E)) {
      # plain division as in the 2023 code: 0/0 is NaN, x/0 is Inf
      factors[j - 1L] <- sum(basis[E, j]) / sum(basis[E, j - 1L])
    }
  }
  need <- vapply(2:K, function(j) any(last_obs < j), TRUE)   # is factor j needed?
  if (projection == "chain_ladder" && any(need & !is.finite(factors)))
    stop("development factor(s) undefined for development year(s) ",
         paste(cn[-1L][need & !is.finite(factors)], collapse = ", "),
         " (no rows observed or zero denominator)", call. = FALSE)

  full <- basis
  for (i in seq_len(I)) {
    k <- last_obs[i]
    if (k < K) for (j in (k + 1L):K) full[i, j] <- full[i, j - 1L] * factors[j - 1L]
  }
  undefined <- which(!is.finite(full) & !obs, arr.ind = TRUE)
  if (projection == "chain_ladder") {
    Cfull <- full
    Afull <- cbind(Cfull[, 1L, drop = FALSE], Cfull[, -1L, drop = FALSE] - Cfull[, -K, drop = FALSE])
  } else {
    Afull <- full
    Cfull <- t(apply(Afull, 1L, cumsum)); if (I == 1L) Cfull <- matrix(Cfull, 1L)
  }
  dimnames(Afull) <- dimnames(Cfull) <- list(rn, cn)
  latest <- C[cbind(seq_len(I), last_obs)]
  ultimate <- Cfull[, K]
  structure(list(
    input = M, input_type = input, projection = projection, observed = obs,
    factors = stats::setNames(factors, cn[-1L]), rows_used = stats::setNames(used, cn[-1L]),
    incremental = Afull, cumulative = Cfull,
    latest = stats::setNames(latest, rn), ultimate = stats::setNames(ultimate, rn),
    outstanding = stats::setNames(ultimate - latest, rn),
    outstanding_total = sum(ultimate - latest),
    undefined = undefined, tail = FALSE), class = "chain_ladder")
}

#' @export
print.chain_ladder <- function(x, digits = 4L, ...) {
  cat("Chain-ladder projection (",
      if (x$projection == "chain_ladder") "cumulative development factors"
      else "incremental-ratio factors, the source article's convention",
      ", no tail)\n", sep = "")
  cat("Development factors:\n"); print(round(x$factors, digits))
  cat("\nOutstanding by accident year:\n"); print(round(x$outstanding))
  cat("Total outstanding: ", format(round(x$outstanding_total), big.mark = ","), "\n", sep = "")
  if (nrow(x$undefined))
    cat("Non-finite projected cells: ", nrow(x$undefined), "\n", sep = "")
  invisible(x)
}

#' Future payments of a chain-ladder projection by calendar year
#'
#' `calendar_year_payments()` on a [chain_ladder] object sums the projected incremental cells
#' after the valuation date by calendar year (accident year plus development year).
#'
#' @param x A [chain_ladder] object whose columns are development years starting at 0 and
#'   whose row names are the accident years.
#' @param ... Not used; any argument given here is an error.
#' @return Named vector of projected payments per future calendar year and a `total`.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11)
#' calendar_year_payments(chain_ladder(as_triangle(panel)))
#' @export
calendar_year_payments.chain_ladder <- function(x, ...) {
  check_unused_dots(list(...), "calendar_year_payments",
                    hints = c(component = "a chain ladder projects all claims; it has no components"))
  cl_calendar(x)
}

#' Deprecated: use calendar_year_payments()
#'
#' `chain_ladder_calendar()` was exported in version 0.1.1 and is deprecated; it returns
#' `calendar_year_payments()` of the [chain_ladder] object, with a warning.
#' @param x A [chain_ladder] object.
#' @return As [calendar_year_payments.chain_ladder()].
#' @keywords internal
#' @export
chain_ladder_calendar <- function(x) {
  .Deprecated("calendar_year_payments", package = "baggingReserve")
  cl_calendar(x)
}

cl_calendar <- function(x) {
  if (!inherits(x, "chain_ladder")) stop("`x` must be a chain_ladder object", call. = FALSE)
  ays <- as.integer(rownames(x$incremental)); devs <- as.integer(colnames(x$incremental))
  cal <- outer(ays, devs, `+`)
  fut <- !x$observed
  out <- tapply(x$incremental[fut], cal[fut], sum)
  c(out, total = sum(out))
}

#' @rdname as_triangle
#' @param type For a [chain_ladder] object: the completed `"incremental"` (default) or
#'   `"cumulative"` triangle.
#' @export
as_triangle.chain_ladder <- function(x, type = c("incremental", "cumulative"), ...) {
  check_unused_dots(list(...), "as_triangle", "type")
  type <- match.arg(type)
  M <- x[[type]]
  attr(M, "cumulative") <- type == "cumulative"
  M
}
