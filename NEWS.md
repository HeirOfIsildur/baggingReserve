# baggingReserve 0.1.1

Changes after an independent software review of 0.1.0. The results of 0.1.0 are unchanged:
fits with explicit seeds give identical predictions, replicates and IBNR components.

## Guards against silent misuse

* `ibnr_reserve()`: every prediction now stores the reporting-count triangle of its panel, and
  `counts` defaults to it. Supplied counts must agree with the stored ones, so counts of
  another portfolio, or cumulative counts stripped of their attribute, are an error instead
  of a wrong IBNR component.
* `chain_ladder()`: `input` now defaults to the triangle's `"cumulative"` attribute set by
  `as_triangle()`; an explicit `input` that contradicts it is an error (a cumulative triangle
  read as incremental gave a 26-fold outstanding).
* `backtest()` warns when the truth holds no claim reported after `t` while IBNR payments are
  predicted, the trace of a truth selected by the reported claims' identifiers.
* `claims_panel()` keeps accident years without reported claims between the first and the
  last one by default, and `ibnr_reserve()` warns about accident years without reported
  claims, for which the chain ladder projects no IBNR claims. The documentation explains how
  to include recent accident years that have no reported claim yet.
* A factor level that is absent from the training claims of the development year in which a
  claim is completed now gives a warning (`predict()`, `bag_reserve()`), as rpart treats it as
  missing at each split on that feature.
* `predict(newdata = )` refuses a logical feature where the fit had a numeric one.
* `claims_panel()` warns when no payment is dated in year `t` (data extracted before the end
  of year `t`); `backtest()` warns when the payments hold no transaction after `t` (a censored
  truth); missing feature values are no longer reported as unseen factor levels.

## Behaviour

* `seed = NULL` in `reserve_tree()` and `bag_reserve()` now advances the caller's random
  number stream, as any random draw in R, so repeated calls give different fits. Explicit
  seeds still leave the caller's state untouched. `reserve_tree()` validates `seed`.
* Stored trees (`keep_trees = TRUE`, and the trees of `reserve_tree()`) no longer keep rpart's
  per-training-row `where` vector, and the bootstrap trees drop their complexity table:
  about a third of the memory, with identical predictions.
* `bag_reserve()` caps `ncores` at the number of cores detected and records the value used.
* Errors for development years with fewer than two training claims state the eligibility
  rule; fits report development years whose training response is constant.
* `oob_score()` gains `oob_score_covered`, the coefficient of determination over the claims
  with out-of-bag votes; the help page explains negative scores.
* Socket workers warn when the installed package differs from the version in use.

## Interface

* `bag_reserve()` returns the claim-level reserve: the prediction's `claim_level` is the mean
  over the replicates of the completed claim-by-development-year payment matrices
  (`claim_level = TRUE`, the default; also for `predict(newdata = )`). The replicates are
  unchanged.
* `backtest()` gains `reference`, which scores another method's prediction, for example a
  `chain_ladder()` fit, against the same realised payments.

* `calendar_year_payments()` is generic, with a method for `chain_ladder` objects
  (`chain_ladder_calendar()` remains); `as_triangle()` gains a `chain_ladder` method.
* `as.data.frame()` methods for predictions and back-tests.
* `backtest()` gains the quantile `type` argument.
* `plot()` on a single-tree fit validates `period`; plots speak of development years.
* Examples for the plot methods, `chain_ladder_calendar()` and the four datasets; the
  prediction's `ibnr_details` and `counts` are documented.

# baggingReserve 0.1.0

First release, implementing the method of the source article, Janoušek & Pešta
(2025), *Bagging and regression trees in individual claims reserving*, Statistical Papers 66(4).

* `claims_panel()`: claims and payment transactions with user-named columns to an
  observation-masked claim-by-development panel; enforces the valuation date, keeps recoveries
  and claims without payments, refuses input it cannot interpret. `as_triangle()`,
  `claim_counts()`.
* `reserve_tree()`: one pruned `rpart` tree per development year; `predict()` completes every
  reported claim's payment history recursively one year ahead.
* `bag_reserve()`: bootstrap trees per development year and replicate with the package's own
  bootstrap, L'Ecuyer-CMRG streams per (replicate, development year) task, streamed
  replicates, the bagged mean as point estimate, out-of-bag scores (`oob_score()`), forked or
  socket workers.
* `ibnr_reserve()`: the source article's IBNR rule (Theorem 1) with the standard chain ladder
  on counts by default and the source article's incremental-ratio projection with rounding by
  argument.
* `chain_ladder()`: standard chain ladder (point predictions, no tail) and the source article's
  incremental-ratio baseline; cross-checked against `ChainLadder::MackChainLadder()`.
* `summary()`, `calendar_year_payments()`, `plot()` and `backtest()` for predictions.
* Socket-cluster workers (`backend = "psock"`, the default on Windows) receive the caller's
  library paths before loading the package, so a package installed in a non-default library
  (including the temporary library used while vignettes are built) is found on the workers.
* Triangles returned by `as_triangle()` and `claim_counts()` carry the class
  `"reserve_triangle"`: ordinary matrices with the attributes `t`, `what` and `cumulative`,
  printed under a heading instead of with their attributes.
* `backtest()` rejects, when the claims table carries a reporting year, claims that were
  reported by the valuation date but are absent from the prediction; the completeness of the
  truth population (its IBNR claims in particular) remains the caller's responsibility, and
  the example selects all claims of a type, IBNR included. `ibnr_reserve()` refuses cumulative count
  triangles; `t` and `L` must be finite integers; `probs` of `backtest()` and `plot()` must be
  two increasing probabilities; the calendar plot scales to the displayed years.
* Datasets `synthetic_claims`, `synthetic_payments`, `synthetic_claims_full`,
  `synthetic_payments_full`: a fresh portfolio from the vendored Wang & Wüthrich (2022)
  generator (seed 2026, twelve accident years, valuation at t = 12, SynthETIC 1.1.2), not the
  article's data; the observed view carries only what is observable at the valuation date.
