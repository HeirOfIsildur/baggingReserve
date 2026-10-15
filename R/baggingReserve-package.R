#' baggingReserve: Bagging and Regression Trees for Individual Claims Reserving
#'
#' Claim-by-claim loss reserving in non-life insurance with regression trees and
#' bootstrap aggregating (bagging), following \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025).
#'
#' The method works on a claim \eqn{\times} development-year panel. For every
#' development year \eqn{l = 0, \dots, L-1} one regression tree is calibrated to
#' predict the payment of the next development year, \eqn{X_{l+1}}, from the claim's
#' static features (type, age of the insured, accident weekday, reporting delay) and its
#' payment history \eqn{X_0, \dots, X_l}, using the claims reported by development year
#' \eqn{l} whose payment in development year \eqn{l + 1} is observed at the valuation
#' date. In the package's arguments, printed output and tables, the development-year
#' index \eqn{l} of a tree is called its `period`. The lower triangle of outstanding
#' payments of the reported claims (closed claims included; the status is not a feature
#' of the source article's specification, but may be named as one) is then filled
#' recursively, one development year ahead, feeding earlier predictions into later
#' features. Bagging replaces each tree by a bootstrap ensemble: a replicate completes all
#' histories with one bootstrap tree per development year, the point estimate is the
#' bagged mean of the completed replicate predictions, and the replicate quantiles
#' describe the variation of single-tree predictions under the resampling (they are
#' neither prediction intervals for the payments nor confidence intervals for the bagged
#' mean). An out-of-bag score per development year is a diagnostic of the one-year
#' regressions. Payments for incurred-but-not-reported (IBNR) claims, the IBNR component,
#' are predicted from chain-ladder claim counts and the average predicted payment per
#' development year (Theorem 1 of the source article).
#'
#' The package ships a synthetic individual-claims portfolio generated with the simulator
#' used in the source article ([synthetic_claims], [synthetic_payments]; a fresh portfolio
#' with its own seed and twelve accident years, not the source article's data) together
#' with the generated future ([synthetic_claims_full], [synthetic_payments_full]) for
#' back-testing.
#'
#' @references
#' \enc{Janoušek}{Janousek} J, \enc{Pešta}{Pesta} M (2025). Bagging and regression trees in individual claims
#' reserving. *Statistical Papers*, 66(4), 89. \doi{10.1007/s00362-025-01715-9}
#'
#' Breiman L (1996). Bagging predictors. *Machine Learning*, 24(2), 123–140.
#' \doi{10.1007/BF00058655}
#'
#' \enc{Wüthrich}{Wuthrich} MV (2018). Machine learning in individual claims reserving.
#' *Scandinavian Actuarial Journal*, 2018(6), 465–480.
#' \doi{10.1080/03461238.2018.1428681}
#'
#' Wang M, \enc{Wüthrich}{Wuthrich} MV (2022). Individual claims generator for claims reserving
#' studies: data simulation.R. SSRN 4127073. \doi{10.2139/ssrn.4127073}
#'
#' @keywords internal
"_PACKAGE"
