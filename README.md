# baggingReserve

<!-- badges: start -->
[![R-CMD-check](https://github.com/HeirOfIsildur/baggingReserve/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HeirOfIsildur/baggingReserve/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R package implementing the reserving method of
*Bagging and regression trees in individual claims reserving* (J. Janoušek and M. Pešta,
Statistical Papers 66(4), 2025, [doi:10.1007/s00362-025-01715-9](https://doi.org/10.1007/s00362-025-01715-9)).

Version 0.1.1 (0.1.0 is under review at CRAN). A manuscript describing the package is being prepared for the *Journal of
Statistical Software*.

## What the method does

Claims reserving predicts the payments an insurer still owes on claims that have already
occurred: claims reported by the valuation date and claims incurred but not yet reported (IBNR).
Chain ladder works on aggregated run-off triangles; the source article (Janoušek & Pešta 2025)
predicts the payments claim by claim and keeps an aggregate count projection for the IBNR
component.

- Each reported claim carries static features (type, age of the insured, accident weekday,
  reporting delay `d`) and its yearly payment history `X_0, …, X_l`.
- For every development year `l = 0, …, L−1` (called `period` in the package) one
  **regression tree** (`rpart`, pruned at the first minimum of the cross-validated error)
  predicts `X_{l+1}` from the features and `X_0, …, X_l`, trained on the claims reported by
  development year `l` whose payment in year `l+1` is already observed.
- The unobserved cells of every reported claim (closed claims included; status is not a
  predictor in the source article's specification, but may be named as a feature) are filled
  **recursively one development year ahead**, earlier predictions feeding later features.
- **Bagging** replaces each tree by a bootstrap ensemble. A replicate takes one bootstrap tree
  from every development year and completes all histories; the point estimate is the bagged
  mean of the completed replicates, and the replicate quantiles describe the variation of
  single-tree predictions under this resampling (they are neither prediction intervals nor
  confidence intervals for the bagged mean). The **out-of-bag score** per development year is
  a diagnostic of the one-year regressions.
- The **IBNR component** follows Theorem 1 of the source article: projected numbers of
  not-yet-reported claims (chain ladder on the reporting-count triangle) times the mean
  predicted payment per development year of the reported claims.

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
regenerates the four `.rda` files from the vendored generator (`data-raw/IndividualClaimsSimulator/`,
attribution in `SOURCE.md`) and records the call and versions in `data-raw/GENERATION.txt`;
`tests/testthat/test-data.R` pins the counts above.

## Installation

```r
# install.packages("remotes")
remotes::install_github("HeirOfIsildur/baggingReserve")
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
chain_ladder(as_triangle(panel))                           # aggregate baseline (standard chain ladder)
backtest(res, synthetic_claims_full, synthetic_payments_full)
```

The article's variants are reachable by argument: `ibnr_reserve(..., projection = "incremental_ratio",
round = TRUE)` and `chain_ladder(..., projection = "incremental_ratio")` use its incremental-ratio
count projection and chain-ladder baseline; `summary(..., type = 1)` gives order-statistic
quantiles.

## Related repositories

- [`article-bagging-regression-trees`](https://github.com/HeirOfIsildur/article-bagging-regression-trees) — the Statistical Papers article (PDFs).
- [`master-thesis-bagging-regression-trees`](https://github.com/HeirOfIsildur/master-thesis-bagging-regression-trees) — the thesis the article grew from; its R script (`rpart` + `ipred::bagging`) is the ancestor of this package.

## Citation

```r
citation("baggingReserve")
```

## Licence

GPL (≥ 3). The vendored generator in `data-raw/IndividualClaimsSimulator/` is GPL-3 by
Melantha Wang and Mario V. Wüthrich and keeps its own licence file.
