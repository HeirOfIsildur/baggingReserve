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
