#' Run-off triangles of a claims panel or a reserve prediction
#'
#' `as_triangle()` aggregates a [claims_panel] into an observed payment or reporting-count
#' triangle, or extracts a completed payment triangle from a [reserve_prediction]. For a
#' panel, `what = "paid"` gives the accident-year by development-year triangle of
#' incremental (or cumulative) payments and `what = "counts"` the accident-year by
#' reporting-delay triangle of numbers of reported claims; cells that are not observable
#' at the valuation date (`acc_year + l > t`) are `NA` and observed cells without claims or
#' payments are `0`. For a prediction, the future cells are filled with the predicted
#' payments of one replicate or of the mean over replicates: the observed cells are kept
#' for `"reported"` and `"total"` and are zero for `"ibnr"`, and no cell is `NA`.
#' `claim_counts()` returns the incremental count triangle of a panel.
#'
#' @param x A [claims_panel] (for `claim_counts()` and the panel method), a
#'   [reserve_prediction] or a [chain_ladder] object; for `print`, a triangle.
#' @param what `"paid"` (default) or `"counts"`; panels only.
#' @param cumulative Logical (default `FALSE`): cumulate along development (or reporting
#'   delay) within each accident year; panels only.
#' @param ... Passed on by the generic to its methods and by `print` to the matrix
#'   printing method; in the methods, an argument that is not one of theirs (a misspelt one, say)
#'   is an error.
#' @return A numeric matrix of class `"reserve_triangle"` with the accident years as row
#'   names and the development years (or reporting delays) `0, ..., L` as column names,
#'   carrying the attributes `"t"`, `"what"` and `"cumulative"`. It is an ordinary matrix in
#'   every computation; only its `print` method hides the attributes behind a heading.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' panel <- claims_panel(synthetic_claims, synthetic_payments, t = 12, L = 11,
#'                       features = c("Type", "Age", "AccWeekday"))
#' round(as_triangle(panel) / 1e6, 2)
#' claim_counts(panel)
#' # a completed triangle from a prediction on a small subset: all claims of one type
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' small <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' res <- ibnr_reserve(predict(reserve_tree(small, seed = 1)), claim_counts(small))
#' round(as_triangle(res, "total") / 1e6, 2)
#' @export
as_triangle <- function(x, ...) UseMethod("as_triangle")

#' @rdname as_triangle
#' @export
as_triangle.claims_panel <- function(x, what = c("paid", "counts"), cumulative = FALSE, ...) {
  check_unused_dots(list(...), "as_triangle", c("what", "cumulative"))
  what <- match.arg(what)
  ays <- x$accident_years
  L <- x$L
  row <- match(x$claims$acc_year, ays)
  M <- matrix(0, length(ays), L + 1L, dimnames = list(ays, 0:L))
  if (what == "paid") {
    P <- x$payments
    P[is.na(P)] <- 0
    for (j in seq_len(L + 1L)) {
      s <- rowsum(P[, j], group = row, reorder = FALSE)
      M[as.integer(rownames(s)), j] <- s[, 1L]
    }
  } else {
    tab <- table(factor(row, levels = seq_along(ays)), factor(x$claims$rep_delay, levels = 0:L))
    M[] <- as.numeric(tab)
  }
  unobs <- outer(ays, 0:L, function(a, l) a + l > x$t)
  M[unobs] <- NA_real_
  if (cumulative && L > 0L) {
    M <- t(apply(M, 1L, cumsum))
    if (length(ays) == 1L) M <- matrix(M, 1L, dimnames = list(ays, 0:L))
  }
  new_triangle(M, x$t, what, cumulative)
}

#' @rdname as_triangle
#' @param digits Round the cells before printing; `NULL` prints them as they are.
#' @export
print.reserve_triangle <- function(x, digits = NULL, ...) {
  what <- attr(x, "what")
  if (is.null(what)) what <- "paid"
  label <- switch(what,
    counts = "Numbers of reported claims (rows: accident years, columns: reporting delays)",
    paid = paste(if (isTRUE(attr(x, "cumulative"))) "Cumulative" else "Incremental",
                 "payments (rows: accident years, columns: development years)"),
    reported = "Payments of reported claims, observed and predicted (rows: accident years,\ncolumns: development years)",
    ibnr = "Predicted payments of unreported claims (rows: accident years,\ncolumns: development years)",
    total = "Payments of all claims, observed and predicted (rows: accident years,\ncolumns: development years)",
    what)
  cat(label, "\n", sep = "")
  if (anyNA(x)) cat("NA: not observable at t = ", attr(x, "t"), "\n", sep = "")
  m <- bare_triangle(x)
  if (!is.null(digits)) m <- round(m, digits)
  print(m, ...)
  invisible(x)
}

# Internal constructor: a numeric matrix with the triangle's provenance as attributes.
new_triangle <- function(M, t, what, cumulative = FALSE) {
  attr(M, "t") <- t
  attr(M, "what") <- what
  attr(M, "cumulative") <- cumulative
  class(M) <- c("reserve_triangle", "matrix", "array")
  M
}

# Internal: the plain matrix (dim and dimnames only).
bare_triangle <- function(x) {
  m <- unclass(x)
  for (a in c("t", "what", "cumulative")) attr(m, a) <- NULL
  m
}

#' @rdname as_triangle
#' @export
claim_counts <- function(x) {
  check_panel(x, "x")
  as_triangle(x, what = "counts", cumulative = FALSE)
}

# Internal: logical mask of triangle cells after the valuation date (acc_year + l > t).
# In column-major order this reproduces the 45 hard-coded cell indices of the 2023 code
# for the 10 x 10 case.
future_mask <- function(accident_years, L, t) {
  outer(accident_years, 0:L, function(a, l) a + l > t)
}
