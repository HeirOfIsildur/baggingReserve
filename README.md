# baggingReserve

<!-- badges: start -->
[![R-CMD-check](https://github.com/HeirOfIsildur/baggingReserve/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HeirOfIsildur/baggingReserve/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package for claim-level (individual) loss reserving and for checking claim-level reserves
against the payments that followed. It has three parts:

- a **data contract**: `claims_panel()` turns a table of claims and a table of payment
  transactions, with your column names, into a claim-by-development-year panel at a valuation
  date; it enforces the valuation date on both tables, distinguishes observed zeros from future
  cells, keeps recoveries and claims without payments, and refuses input it cannot interpret;
- a **recursion engine**, shared by the single-tree fit (`reserve_tree()`), the bagged ensemble
  (`bag_reserve()`) and user-supplied learners (`learner = `, `two_part_learner()`), that completes
  the payment history of every reported claim one development year ahead, with plugged-in means
  (`completion = "mean"`) or simulated payments (`completion = "draw"`); `ibnr_reserve()` adds the
  claims incurred but not reported (IBNR) from chain-ladder projections of the reporting counts;
- **back-testing**: `backtest()` scores a prediction, and a reference method such as the chain
  ladder, against realised payments by calendar year and separately for reported and IBNR claims.

The default estimator, a documented baseline that other learners can be measured against, is the
method of *Bagging and regression trees in individual claims reserving* (J. Janoušek and M. Pešta,
Statistical Papers 66(4), 2025, [doi:10.1007/s00362-025-01715-9](https://doi.org/10.1007/s00362-025-01715-9)).
A manuscript describing the package has been submitted to the *Journal of Statistical Software*;
it ships as the vignette `baggingReserve-jss`. On the portfolios studied there, the standard chain
ladder is more accurate than the default trees on most of them: the package is meant for finding
out, with `backtest()`, whether claim-level information helps on your own data.

## What the default method does

- Each reported claim carries static features (type, age of the insured, accident weekday,
  reporting delay `d`) and its yearly payment history `X_0, …, X_l`.
- For every development year `l = 0, …, L−1` (called `period` in the package) one
  **regression tree** (`rpart`, pruned at the smallest cross-validated error)
  predicts `X_{l+1}` from the features and `X_0, …, X_l`, trained on the claims reported by
  development year `l` whose payment in year `l+1` is already observed.
- The unobserved cells of every reported claim (closed claims included; status is not a
  predictor in the source article's specification) are filled **recursively one development
  year ahead**, earlier predictions feeding later features.
- **Bagging** replaces each tree by a bootstrap ensemble. A replicate takes one bootstrap tree
  from every development year and completes all histories; the point estimate is the bagged
  mean of the completed replicates, and the replicate quantiles describe the variation, under
  this resampling, of the reserve predicted by one set of unpruned bootstrap trees (they are
  neither prediction intervals nor confidence intervals for the bagged mean). The
  **out-of-bag score** per development year is a diagnostic of the one-year regressions.
- The **IBNR component** follows Theorem 1 of the source article: projected numbers of
  not-yet-reported claims (chain ladder on the reporting-count triangle) times the mean
  predicted payment per development year of the reported claims.

The default `completion = "mean"` plugs each predicted mean into the history used by the next
tree, as the source article does. Where the occurrence of a payment depends on the payment
history this biases the payments after next year, so **for totals beyond next year use
`completion = "draw"`**, which simulates the payments instead.

The per-claim reserves (`claim_reserves()`) add up to the aggregate, but they are not a reliable
allocation between open and closed claims: the trees do not see the settlement status.

## Data

The shipped data are synthetic and fully reproducible: the Wang & Wüthrich (2022)
[Individual Claims Simulator](https://github.com/actuarial-data-science/PackageIndividualClaimsSimulator)
(GPL-3, built on SynthETIC) run on a yearly grid with seed 2026 — six claim types, twelve accident
years, 62,904 closed + 8,213 RBNS = 71,117 reported claims and 2,588 IBNR claims at `t = 12`.
This is a fresh portfolio from the same simulator family as the article's, not the article's data
(which used another seed, ten accident years and valuation at `t = 10`); the manuscript's numbers
are therefore not those of the article and are not meant to reproduce them.

| Object | Rows | Meaning |
|---|---|---|
| `synthetic_claims` | 71,117 | reported claims as the insurer sees them at `t = 12` |
| `synthetic_payments` | — | their payments up to year 12 (recoveries are negative) |
| `synthetic_claims_full` | 73,705 | every generated claim, incl. IBNR, uncensored, with the generated ultimate and payment count |
| `synthetic_payments_full` | — | every generated payment, past and future |

`synthetic_claims` holds only what is observable at `t = 12`; the generated ultimate claim
amount and payment count live in `synthetic_claims_full` alone. `data-raw/make_synthetic_claims.R`
in the source repository regenerates the four `.rda` files from the vendored generator and records
the call and versions in `data-raw/GENERATION.txt`.

## Installation

```r
# from CRAN (once released; version 0.1.1 is under review)
install.packages("baggingReserve")

# development version
# install.packages("remotes")
remotes::install_github("HeirOfIsildur/baggingReserve", build_vignettes = TRUE)
```

## Interface

```r
library("baggingReserve")
panel <- claims_panel(synthetic_claims, synthetic_payments, t = 12, L = 11,
                      features = c("Type", "Age", "AccWeekday"))
tree  <- reserve_tree(panel, seed = 1)                     # one pruned rpart per period l
bag   <- bag_reserve(panel, B = 100, seed = 2026, ncores = 4)
oob_score(bag)                                             # per-period OOB score, coverage, n
pred  <- predict(bag)                                      # future-cell payments, one column per replicate
res   <- ibnr_reserve(pred, claim_counts(panel))           # adds the IBNR component (Theorem 1)
summary(res)                                               # mean, sd, replicate quantiles per calendar year
calendar_year_payments(res)                                # next year ... development L, per replicate
plot(res, "next_year"); plot(res, "calendar")
cl    <- chain_ladder(as_triangle(panel))                  # aggregate baseline (standard chain ladder)
backtest(res, synthetic_claims_full, synthetic_payments_full, reference = cl)
# simulated completion, recommended for totals beyond next year
bag_draw <- bag_reserve(panel, B = 100, seed = 2026, ncores = 4, completion = "draw")
```

The article's variants are reachable by argument: `ibnr_reserve(..., projection = "incremental_ratio",
round = TRUE)` and `chain_ladder(..., projection = "incremental_ratio")` use its incremental-ratio
count projection and chain-ladder baseline; `summary(..., type = 1)` gives order-statistic
quantiles.

## Vignettes

- `vignette("baggingReserve")`: a short tour of the workflow on a part of the shipped portfolio.
- `vignette("baggingReserve-jss")`: the manuscript submitted to the *Journal of Statistical
  Software* (precompiled PDF).

## Related repositories

- [`article-bagging-regression-trees`](https://github.com/HeirOfIsildur/article-bagging-regression-trees) — the Statistical Papers article (PDFs).
- [`master-thesis-bagging-regression-trees`](https://github.com/HeirOfIsildur/master-thesis-bagging-regression-trees) — the thesis the article grew from; its R script (`rpart` + `ipred::bagging`) is the ancestor of this package.

## Citation

```r
citation("baggingReserve")
```

## Licence

GPL (≥ 3). The vendored generator in `data-raw/IndividualClaimsSimulator/` of the source
repository is GPL-3 by Melantha Wang and Mario V. Wüthrich and keeps its own licence file.
