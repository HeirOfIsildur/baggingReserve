#' A two-part learner: whether a claim pays, and how much
#'
#' Builds a `learner` for [reserve_tree()] and [bag_reserve()] from two models: one for the
#' probability that a claim makes a non-zero payment in the development year, one for the
#' amount of a non-zero payment. Most payments of the later development years are exactly
#' zero; a single smooth model then predicts a small positive amount for every claim, and the
#' recursive completion carries these forward. Separating occurrence from size, and simulating
#' the occurrence (`predict(fit, completion = "draw")`), avoids this.
#'
#' The prediction is `p * s`, the probability of a payment times the predicted amount; the
#' simulation, used by `completion = "draw"`, draws the payment indicator and returns `s` when
#' it is 1, else 0. Only the occurrence is then random: the amount of a simulated payment is the
#' size model's predicted mean, so the simulated completions contain no variation of the payment
#' size given its predictors, unless `size` has a `simulate(model, newdata)` function, which is
#' then used for the amount of every simulated payment.
#'
#' @param occurrence A list with `fit(formula, data)` and `predict(model, newdata)`; it is
#'   fitted to a 0/1 response (1 for a non-zero payment) and must predict probabilities.
#'
#'   The outputs of both models are checked separately, before they are combined, against the
#'   learner contract of [reserve_tree()]: a numeric vector (or one-column matrix) with one finite
#'   value per row of `newdata` (per simulated payment for `size$simulate()`), and for the
#'   occurrence model values in `[0, 1]`. A violation is an error naming the model, so that R's
#'   recycling cannot turn, say, a single number returned for a whole batch into a prediction.
#' @param size A list with `fit(formula, data)` and `predict(model, newdata)`, fitted to the
#'   non-zero payments only, and optionally `simulate(model, newdata)`, returning one simulated
#'   non-zero amount per row (used by `completion = "draw"`; without it the predicted amount is
#'   used).
#' @param min_nonzero When fewer non-zero payments are available in a development year, the
#'   size model is replaced by their mean (default 20).
#' @return A list with functions `fit`, `predict` and `simulate`, to pass as `learner`.
#' @examples
#' data("synthetic_claims", "synthetic_payments", package = "baggingReserve")
#' cl <- synthetic_claims[synthetic_claims$Type == "3", ]
#' pay <- synthetic_payments[synthetic_payments$Id %in% cl$Id, ]
#' panel <- claims_panel(cl, pay, t = 12, L = 11, features = c("Age", "AccWeekday"))
#' logistic <- list(
#'   fit = function(formula, data) stats::glm(formula, data = data, family = stats::binomial()),
#'   predict = function(model, newdata) stats::predict(model, newdata, type = "response"))
#' linear <- list(
#'   fit = function(formula, data) stats::lm(formula, data = data),
#'   predict = function(model, newdata) stats::predict(model, newdata))
#' fit <- reserve_tree(panel, learner = two_part_learner(logistic, linear))
#' calendar_year_payments(predict(fit))["total", ]
#' calendar_year_payments(predict(fit, completion = "draw", R = 20, seed = 1))["total", ]
#' @export
two_part_learner <- function(occurrence, size, min_nonzero = 20L) {
  for (nm in c("occurrence", "size")) {
    l <- get(nm)
    if (!(is.list(l) && is.function(l$fit) && is.function(l$predict)))
      stop("`", nm, "` must be a list with functions `fit(formula, data)` and `predict(model, newdata)`",
           call. = FALSE)
  }
  if (!is.null(size$simulate) && !is.function(size$simulate))
    stop("`size$simulate` must be a function(model, newdata) or absent", call. = FALSE)
  min_nonzero <- as_scalar_integer(min_nonzero, "min_nonzero")
  # each part is checked on its own, before it is combined: the learner contract (a numeric vector
  # with one finite value per row of newdata) would otherwise apply only to the product p * s, where
  # R's recycling could turn a scalar or a vector of the wrong length into a plausible prediction
  parts <- function(model, newdata) {
    n <- nrow(newdata)
    p <- check_part_output(occurrence$predict(model$occurrence, newdata), n, "the occurrence model's predict()")
    if (any(p < 0 | p > 1))
      stop("two_part_learner(): the occurrence model must predict probabilities in [0, 1]; it returned ",
           sum(p < 0 | p > 1), " value(s) outside, e.g. ", format(p[p < 0 | p > 1][1L]), call. = FALSE)
    s <- if (is.null(model$size)) rep(model$mean_nonzero, n)
         else check_part_output(size$predict(model$size, newdata), n, "the size model's predict()")
    list(p = p, s = s)
  }
  list(
    fit = function(formula, data) {
      y <- data[[all.vars(formula)[1L]]]
      nz <- y != 0
      occ_data <- data; occ_data[[all.vars(formula)[1L]]] <- as.numeric(nz)
      list(occurrence = occurrence$fit(formula, occ_data),
           size = if (sum(nz) >= min_nonzero) size$fit(formula, data[nz, , drop = FALSE]),
           mean_nonzero = if (any(nz)) mean(y[nz]) else 0)
    },
    predict = function(model, newdata) { x <- parts(model, newdata); x$p * x$s },
    simulate = function(model, newdata) {
      x <- parts(model, newdata)
      ind <- stats::rbinom(length(x$p), 1L, x$p)
      # the amounts: the predicted sizes, or draws from the size model when it can simulate (only
      # for the claims that pay, after the indicators, so that the indicator draws are unchanged)
      if (!is.null(model$size) && is.function(size$simulate) && any(ind == 1L)) {
        pay <- which(ind == 1L)
        x$s[pay] <- check_part_output(size$simulate(model$size, newdata[pay, , drop = FALSE]), length(pay),
                                      "the size model's simulate()")
      }
      ind * x$s
    })
}
