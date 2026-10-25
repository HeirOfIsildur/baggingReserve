# baggingReserve 0.2.0

* Restoring the caller's random-number generator works with any number of generator kinds
  reported by `RNGkind()` (newer R versions also report the binomial kind).

* New vignette `baggingReserve-jss`: the manuscript submitted to the *Journal of Statistical
  Software*, shipped as a precompiled PDF (`vignette("baggingReserve-jss")`) because it reads
  deposited results computed over hours; its knitr source is in `inst/manuscript/`.

Fits with explicit seeds under R's default random-number generator give the same trees,
predictions, replicates and IBNR components as 0.1.1; back-tests of predictions with several
columns are unchanged.

## Socket workers, reporting dates and smaller guards

* `print()` and `summary()` of a fit made with a user-supplied learner no longer inspect the
  learner's models, which may be anything its `predict()` understands (an atomic training mean,
  say, made them fail): they report the training sizes per development year and say that `control`
  and `prune` were not used; the leaves, depths and complexity parameters are reported for the
  default rpart learner only. The print methods of bagged fits print the learner line likewise.
* `bag_reserve()` with a user-supplied learner ignores `prune` (with the warning) without
  validating it against `control`, as `reserve_tree()` does; such fits record `prune = "none"`.
* `prune = "min_xerror"` needs at least two cross-validation folds: `xval = 1`, which computes no
  cross-validated error and left the trees unpruned while recording them as pruned, is now an
  error in `reserve_tree()` and `bag_reserve()`, as `xval = 0` was.
* `backtest()` counts the payments after `t` of claims set aside by `claims_panel(beyond_L =
  "drop")` in the excluded tail and in the set-aside diagnostics also when their accident year
  lies outside the panel's accident-year range (determined after setting them aside); they were
  counted among the payments outside the prediction's accident years. Scored tables are unchanged.
* Socket workers (`backend = "psock"`, the default on Windows) run a user-supplied learner whose
  functions refer to variables of the global environment, such as a leaf size `mb <- 50` set at
  top level: the workers receive the global variables those functions refer to (found with
  `codetools::findGlobals()`, also inside the functions among them) and load the attached packages
  they use, so that all backends give the same replicates. A learner that reaches an object in a
  way that cannot be detected (`get()`, a string) fails on the workers with a message that says
  what to do. `codetools` is a new import.
* `backtest()` takes the reporting year by the rule of `claims_panel()`: from the reporting-delay
  column when the panel was built from one (a reporting-year column present as well must agree
  with it), so that a panel built with `rep_delay` and a missing or partly missing reporting-year
  column is scored, IBNR claim counts included, as the constructor saw it. Predictions store the
  reporting delays of their claims (`claim_rep_delays`), and `backtest()` refuses a truth that
  gives a predicted claim another reporting year, also one before `t`; a prediction saved without
  them is checked for a reporting year by `t` only, as before.
* `chain_ladder()` refuses infinite observed cells, and `backtest()` a `reference` with non-finite
  payments in a scored calendar year.
* `reserve_tree()` and `bag_reserve()` warn when `control` or `prune` is given together with a
  user-supplied `learner`, which ignores them; a learner's `predict()` may return a one-column data
  frame, and an unusable output is described correctly in the error.
* `print()` of a summary says that its 99.5% (and any other quantile at 0.99 or above) is a
  replicate quantile, not a capital requirement, a value at risk or a prediction bound; the help
  of `backtest()` defines `rel_error = error / truth` and its sign where the realised amount is
  negative, and `print()` of a back-test notes it when such a year occurs.
* Accident, reporting or payment years given as dates are refused with a hint at the conversion.
  `?ibnr_reserve` notes that the pooled mean payment of the last development years rests on little
  information and can be negative. The help pages say "the smallest cross-validated error (the
  simplest tree on ties)" for the pruning rule, which is what `which.min()` on `xerror` does.

## Back-testing with the panel's layout and contract

* Predictions carry the column mapping of their panel (`mapping`), and `backtest()` uses it as the
  default for `id`, `acc_year`, `event_year`, `paid` and `rep_year`, so a portfolio with another
  layout is back-tested with the names given once to `claims_panel()` (a reporting-delay column is
  turned into the reporting year). Without a reporting-year column `backtest()` now warns that its
  population and chronology checks and the IBNR claim counts are skipped (they used to switch off
  silently); `rep_year = NULL` skips them without the warning.
* `claims_panel(beyond_L = "drop")` keeps the identifiers of the claims it sets aside
  (`beyond_L_ids`), the predictions carry them, and `backtest()` accepts the complete truth tables:
  it no longer stops because these claims are "absent from the prediction", their payments after `t`
  (all beyond `L`) are part of the excluded tail, and `not_scored` and `print()` count them.
* `backtest()` holds the truth to the chronology of the data contract when the reporting year is
  known: a claim reported before its accident year and a payment dated before its claim's reporting
  year are errors, as in `claims_panel()`; claims without a reporting year are treated as not
  reported by `t`, with a warning. (None of the deposited back-tests has such records.)

## Learners and completion

* `two_part_learner()` checks the outputs of its occurrence and size models (and of the size
  model's `simulate()`) separately, before combining them: a numeric vector with one finite value
  per row, probabilities in `[0, 1]`. A scalar, a character value or a vector of the wrong length
  used to be recycled into an apparently valid prediction.
* `completion = "draw"` and the `learner` interface are no longer labelled experimental. The help
  pages of `bag_reserve()`, `predict.reserve_tree()` and the package, and the `Description`,
  recommend `completion = "draw"` for totals beyond next year (the default stays the source
  article's plug-in completion), state what the replicates of a bagged fit with simulated
  completion contain (one set of bootstrap trees per replicate, samples drawn per development
  year, one simulated completion, fixed IBNR counts), and no longer call simulated completion
  unbiased. `print()` of a plug-in prediction with several future years says so in one line.
* `predict()` on a single tree refuses `R` and `seed` with `completion = "mean"`, which ignored them.
* `predict(newdata = )` warns when `newdata` is valued before the fit and shares claims with it (the
  trees then learned from payments after its valuation date); a bagged fit with
  `completion = "draw"` cannot predict `newdata`, which the help pages now say.
* `claim_reserves()` and the `claim_level` element document that per-claim values are consistent
  with the aggregate but not a reliable allocation between open and closed claims.

## Smaller changes

* `bag_reserve()` says in a message when it reduces `ncores` to the number of cores detected;
  `print()` shows `oob_score_covered` when the out-of-bag coverage is below 0.99; socket workers
  that load another version of the package are an error instead of a warning followed by an
  obscure failure.
* The provenance of a bagged prediction records the control settings, pruning, learner and package
  versions, as that of a single-tree prediction does; `versions` includes the package's own.
* `as_triangle(<prediction>, component = "total")` warns when it falls back to the reported claims.
* The tour vignette shows simulated completion and describes the replicate quantiles correctly
  (unpruned bootstrap trees, not single-tree predictions).

## Learner contract, labels of variability, ignored arguments

* Out-of-bag prediction passes a learner the predictors without the response, the frame that
  forecasting passes (it used to receive the training rows with the response column, so a learner
  that checks its input failed with the default `oob = TRUE`, and one that uses every column of
  `newdata` could see the response). One check now applies to every prediction a learner makes
  (out-of-bag votes, plugged-in completion, simulated completion): a numeric vector (or one-column
  matrix) with one finite value per row; `Inf`, `NA`, a wrong length or a non-numeric result is an
  error naming the stage and the development year. The default trees are unaffected: their
  out-of-bag scores and predictions are unchanged.
* Predictions record their completion mode and the sources of variability among their columns
  (`completion`, `variability`), and `print()`, `summary()`, `plot()` and `backtest()` describe
  them accordingly: the simulated completions of a single-tree fit (`predict(fit, completion =
  "draw")`) as process variability given the one fitted model, without estimation variability
  (they used to be described as resampling variation, and plotted as a "bagged mean"); the
  replicates of `bag_reserve()` as bootstrap resampling variation; with `completion = "draw"` as
  both. `?bag_reserve` states that no payment noise is simulated for the default completion only.
* The provenance of a simulated prediction records the random-number information of the
  simulation (`completion_rng`: its seed, or the generator state it started from) next to that of
  the fit (`rng`).
* `predict()` on a bagged fit refuses `completion` other than the fit's (a fitting-time choice for
  bagging, `bag_reserve(completion = )`), `R`, `seed` and any misspelt argument, which used to be
  ignored silently, returning the plug-in replicates. The other methods with `...` (`predict()` on
  a single tree, `summary()` of predictions, fits and panels, `calendar_year_payments()`,
  `as_triangle()`) likewise reject arguments that are not theirs, suggesting the intended one.
* `claims_panel()` warns when the latest accident year with a reported claim is before `t`: the
  later accident years used to drop out of the panel, and so out of the IBNR component, without a
  message. The warning and `?claims_panel` say how to include them (`accident_years = first:t`) or
  to state that there are none.
* `bag_reserve()` uses the requested `ncores` when the number of cores cannot be detected
  (`parallel::detectCores()` is `NA`); `ncores` below one is an error instead of counting as one.
  The test of the cap no longer fails where detection is unavailable.

## Earlier changes in this version

* `backtest()` rejects claims of the prediction whose accident year in `claims` differs from the
  one they were predicted with, or that `claims` records as reported after `t`.
* `claims_panel(beyond_L = "drop")` discards reported claims with a reporting delay above `L` and
  observed payments beyond development year `L` (recorded in the diagnostics), so that a long
  history can be used without editing it; the default still refuses them.
* `print()` of a panel counts the payments of the claims dropped as not reported by `t`.
* `two_part_learner()`: the documentation states that simulated completion draws the occurrence
  only, the amount being the size model's mean; a size model with a `simulate()` function is used
  for the amounts.
* `plot()` methods: graphical arguments such as `ylab`, `ylim` and `main` override the defaults
  (they used to fail as duplicated arguments, and `main` was rejected by the tree and out-of-bag
  plots).
* `as_triangle(<prediction>, replicate = )` validates the replicate.
* The `Description` no longer calls the replicate spread the resampling variation of
  single-tree predictions; the bootstrap trees are unpruned. `?bag_reserve` says what the default
  `B = 100` is enough for, and `?oob_score` states the coverage at `B = 10` as the manuscript does.

## Back-testing

* `backtest()` scores a `reference` against the truth of the population it predicts. The new
  argument `reference_component` is `"total"` (default; all claims, e.g. a chain ladder) or
  `"reported"` (a named vector predicting the reported claims only). An all-claims reference
  for a prediction without IBNR component is now an error, suggesting `ibnr_reserve()`; it used
  to be scored against the reported-claim truth only.
* `backtest()` checks a `chain_ladder()` reference against the prediction: the same accident
  years, development years, valuation date and observed payments (a chain ladder of another
  portfolio was accepted silently). A named-vector reference must hold exactly the prediction's
  calendar years, and its `total`, which is now optional, must equal their sum.
* `backtest(observed_to = )`: the last fully observed calendar year of the payments. Later cells
  are unobserved (`NA` truth, error and range check), and the `total` rows of truth,
  prediction, range and reference cover the observed years only (`total_years` of the result;
  stated by `print()`). Without it, every predicted calendar year counts as observed, as before,
  and a warning suggests `observed_to` when the payments end before the prediction's last
  calendar year. Payments after `observed_to` are not scored.
* `backtest()` scores the IBNR claim counts: with an IBNR component and a reporting-year column,
  `counts` holds the projected (chain ladder on counts) and realised numbers of claims reported
  after `t` by accident year, also printed. On the shipped portfolio the projection is 2,935
  claims against 2,588 realised.
* `backtest()` returns `NA` for `lower`, `upper` and `inside_range` of a prediction with a
  single column (a single-tree fit), whose replicate range was degenerate and gave
  `inside_range = FALSE`.
* `backtest()` reports in `not_scored` (and prints) the claims and payments outside the
  prediction's accident years, which it ignores, and validates `type`.

## Reproducibility

* `reserve_tree(seed = )` and `predict(completion = "draw", seed = )` set the seed with R's
  default generator kinds pinned (`"Mersenne-Twister"`, `"Inversion"`, `"Rejection"`) and
  restore the caller's kind and state on exit, so a fit no longer depends on the session's
  `RNGkind()`. Under R's default generator the results are identical to 0.1.1; `seed = NULL`
  still uses the session's generator as it is.

## Data contract

* `claims_panel()` drops claims whose accident year is after `t`, like claims reported after
  `t`, and counts both in `n_unreported_dropped`; they used to be an error. A missing reporting
  year (or delay) now gives a warning; such claims are treated as not reported by `t`, as before.
* The error for a non-integer year shows the offending value instead of `NA`.

## Learners, completion and claim-level reserves

* `bag_reserve(learner = )`: any learner given as `fit(formula, data)` and
  `predict(model, newdata)` functions replaces the rpart trees; the bootstrap, recursion,
  out-of-bag diagnostic and random-number streams stay the package's. The default is unchanged.
* `predict(<reserve_tree>, completion = "draw", R, seed)`: simulated instead of
  plugged-in completion; each future payment is drawn from the training responses of the claim's
  leaf (or from a learner's `simulate()`), and `R` completions are averaged. Single-tree fits
  now keep these leaf responses (`leaf_pools`).
* `bag_reserve(completion = "draw")`: each replicate's completion is simulated
  once from the leaves of its own trees, on a separate random-number substream, so the trees
  are those of `completion = "mean"`. Simulated completion groups the claims by leaf once
  instead of scanning them per leaf (about three times faster per draw on an unpruned tree with
  4,000 leaves), with the same draws for the same seed.
* `two_part_learner(occurrence, size)`: a learner from a model for whether a claim pays and a
  model for the amount of a non-zero payment, with `predict` (`p * s`) and `simulate`.
* `claim_reserves()`: the reserve of every reported claim (its completed payments after the
  valuation date), replacing the `outer(...) > t` mask used in the article.
* `chain_ladder_calendar()` is deprecated (a warning): use `calendar_year_payments()` on the
  `chain_ladder()` result, which returns the same vector. It will be removed in a later version.
* `plot()` methods reject unnamed extra arguments and names that are neither their own
  arguments nor graphical parameters, suggesting the intended argument for a misspelling.

## Other

* `plot.reserve_prediction()` gains the quantile `type` (named), as `summary()` and
  `backtest()`, and checks the length of `truth`.
* Documentation: the IBNR replicate spread contains no count uncertainty; no external count
  projection is supported (the help page suggested one); missing feature values under
  `na_features = "surrogate"`; a valuation-date status should not be a feature; the replicate
  quantiles describe unpruned bootstrap trees; the out-of-bag coverage probability.
* Tests no longer emit unhandled warnings.

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
