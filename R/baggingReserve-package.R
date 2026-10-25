#' baggingReserve: Bagging and Regression Trees for Individual Claims Reserving
#'
#' Software for claim-level (claim-by-claim) loss reserving in non-life insurance and for
#' checking claim-level reserves against the payments that followed. It has three parts:
#'
#' * a **data contract**: [claims_panel()] turns a table of claims and a table of payment
#'   transactions, with user-specified column names, into a claim \eqn{\times} development-year
#'   panel at a valuation date `t`. It enforces the valuation date on both tables, distinguishes
#'   observed zeros from future cells, keeps recoveries and claims without payments, and refuses
#'   input it cannot interpret;
#' * a **recursion engine**, shared by the single-tree fit ([reserve_tree()]), the bagged ensemble
#'   ([bag_reserve()]) and user-supplied learners (argument `learner`, [two_part_learner()]), that
#'   completes the payment history of every reported claim one development year ahead, either by
#'   plugging in predicted means (`completion = "mean"`) or by simulating the payments
#'   (`completion = "draw"`), with random-number streams that make bagged results independent of
#'   the number of worker processes; [ibnr_reserve()] adds a component for the claims incurred but
#'   not reported (IBNR) from chain-ladder projections of the reporting counts;
#' * **back-testing**: [backtest()] scores a prediction, and a reference method such as
#'   [chain_ladder()], against realised payments by calendar year and separately for the reported
#'   and the IBNR claims; valuing a claims history at an earlier date turns it into such a test.
#'
#' The default estimator, a documented baseline that any learner can be measured against, is the
#' method of \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025), the source article. For every
#' development year \eqn{l = 0, \dots, L-1} one regression tree predicts the payment of the next
#' development year, \eqn{X_{l+1}}, from the claim's static features, its reporting delay and its
#' payment history \eqn{X_0, \dots, X_l}, using the claims reported by development year \eqn{l}
#' whose payment in development year \eqn{l + 1} is observed at the valuation date. In the
#' package's arguments, printed output and tables, the development-year index \eqn{l} of a tree
#' is called its `period`. The lower triangle of outstanding payments of the reported claims
#' (closed claims included; the status is not a feature of the source article's specification) is
#' filled recursively, feeding earlier predictions into later features. Bagging replaces each tree
#' by a bootstrap ensemble: a replicate completes all histories with one bootstrap tree per
#' development year, the point estimate is the bagged mean of the completed replicates, and the
#' replicate quantiles describe the variation of the bootstrap-tree predictions under the
#' resampling (they are neither prediction intervals for the payments nor confidence intervals for
#' the bagged mean). An out-of-bag score per development year is a diagnostic of the one-year
#' regressions. The IBNR component is the projected number of late-reported claims times the
#' average predicted payment per development year (Theorem 1 of the source article).
#'
#' The default `completion = "mean"` reproduces the source article. Plugging in means is biased
#' for the payments after next year when the occurrence of a payment depends on the payment
#' history, so for totals beyond next year we recommend `completion = "draw"` (see
#' [bag_reserve()]). On the portfolios studied in the JSS manuscript the standard chain ladder is
#' more accurate than the default trees on most of them; the package is meant for finding out, with
#' [backtest()], whether claim-level information helps on a user's own data.
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
