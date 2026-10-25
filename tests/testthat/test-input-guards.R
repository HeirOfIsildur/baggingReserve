# Guards against silent misuse found in the software review of version 0.1.0: inconsistent
# inputs that used to return wrong numbers without a message, the random-number contract
# with seed = NULL, the memory of stored trees, and smaller interface points.

toy_fit <- function() {
  toy <- make_toy_portfolio()
  p <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  list(toy = toy, panel = p, pred = predict(reserve_tree(p, seed = 1)))
}

test_that("ibnr_reserve() uses the stored counts and refuses counts of another portfolio", {
  f <- toy_fit()
  default <- ibnr_reserve(f$pred)
  explicit <- ibnr_reserve(f$pred, claim_counts(f$panel))
  expect_equal(default$ibnr, explicit$ibnr)
  # the same counts without attributes are accepted
  expect_equal(ibnr_reserve(f$pred, unclass(claim_counts(f$panel)))$ibnr, default$ibnr)
  # counts of another portfolio, or cumulative counts stripped of their attribute, are refused
  other <- make_toy_portfolio(seed = 99)
  p2 <- claims_panel(other$claims, other$payments, t = 5, L = 4, features = "Type")
  expect_error(ibnr_reserve(f$pred, claim_counts(p2)), "differs from the reporting counts")
  cum <- unclass(as_triangle(f$panel, what = "counts", cumulative = TRUE))
  attr(cum, "cumulative") <- NULL
  expect_error(ibnr_reserve(f$pred, cum), "differs from the reporting counts")
  # a prediction without stored counts (older version) needs them supplied
  old <- f$pred; old$counts <- NULL
  expect_error(ibnr_reserve(old), "stores no count triangle")
  expect_equal(ibnr_reserve(old, claim_counts(f$panel))$ibnr, default$ibnr)
})

test_that("chain_ladder() reads the cumulative attribute and refuses a contradiction", {
  f <- toy_fit()
  inc <- chain_ladder(as_triangle(f$panel))
  cum_tri <- as_triangle(f$panel, cumulative = TRUE)
  expect_equal(chain_ladder(cum_tri)$outstanding_total, inc$outstanding_total)
  expect_equal(chain_ladder(cum_tri, input = "cumulative")$outstanding_total, inc$outstanding_total)
  expect_error(chain_ladder(cum_tri, input = "incremental"), "contradicts the triangle")
  expect_error(chain_ladder(as_triangle(f$panel), input = "cumulative"), "contradicts the triangle")
  # plain matrices: incremental unless stated
  expect_equal(chain_ladder(unclass(as_triangle(f$panel)))$outstanding_total, inc$outstanding_total)
  # the calendar-year method and the triangle extractor
  expect_warning(old <- chain_ladder_calendar(inc), "deprecated")
  expect_equal(calendar_year_payments(inc), old)
  expect_equal(unname(as_triangle(inc, "cumulative")[, ncol(inc$cumulative)]), unname(inc$ultimate))
})

test_that("backtest() warns when the truth has no claim reported after t", {
  f <- toy_fit()
  res <- ibnr_reserve(f$pred)
  expect_gt(sum(res$ibnr), 0)
  reported_only <- f$toy$claims[f$toy$claims$Id %in% res$claim_ids, ]
  # (observed_to states that the follow-up is complete, which the reported claims alone do not show)
  expect_warning(backtest(res, reported_only,
                          f$toy$payments_full[f$toy$payments_full$Id %in% reported_only$Id, ],
                          observed_to = 9),
                 "no claim reported after t")
  expect_no_warning(backtest(res, f$toy$claims, f$toy$payments_full))
  # the quantile type is an argument and is recorded
  expect_equal(backtest(res, f$toy$claims, f$toy$payments_full, type = 1)$type, 1)
})

test_that("stored trees drop the training-row leaf vector and predict identically", {
  toy <- make_toy_portfolio()
  p <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  bag <- bag_reserve(p, B = 3, seed = 5, keep_trees = TRUE)
  tr <- bag$trees[[1]][[1]]
  expect_null(tr$where)
  expect_null(tr$cptable)
  expect_null(reserve_tree(p, seed = 1)$fits[[1]]$where)
  # the trees still complete the panel exactly as at fitting time
  expect_equal(predict(bag, newdata = p)$reported, predict(bag)$reported)
})

test_that("seed = NULL advances the caller's stream; an explicit seed restores it", {
  toy <- make_toy_portfolio()
  p <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  set.seed(1)
  b1 <- bag_reserve(p, B = 2)
  b2 <- bag_reserve(p, B = 2)
  expect_false(identical(b1$rng$seed, b2$rng$seed))
  set.seed(1); before <- .Random.seed
  reserve_tree(p)
  expect_false(identical(.Random.seed, before))
  set.seed(1); before <- .Random.seed
  reserve_tree(p, seed = 3); bag_reserve(p, B = 2, seed = 3)
  expect_identical(.Random.seed, before)
  expect_error(reserve_tree(p, seed = "a"), "seed")
})

test_that("a factor level absent from a period's training claims gives a warning", {
  toy <- make_toy_portfolio()
  cl <- toy$claims
  cl$Grp <- factor(ifelse(cl$AccYear == 5, "z", ifelse(cl$Type == "A", "a", "b")))
  p <- claims_panel(cl, toy$payments, t = 5, L = 4, features = c("Type", "Grp"))
  expect_warning(fit <- reserve_tree(p, seed = 1), NA)
  expect_warning(predict(fit), "absent from the training claims.*`Grp` = z in period 0")
  expect_warning(bag_reserve(p, B = 2, seed = 1), "`Grp` = z")
})

test_that("predict(newdata =) refuses a logical feature where the fit had a numeric one", {
  toy <- make_toy_portfolio()
  cl <- toy$claims; cl$Big <- as.numeric(cl$Type == "B")
  p <- claims_panel(cl, toy$payments, t = 5, L = 4, features = "Big")
  fit <- reserve_tree(p, seed = 1)
  cl2 <- cl; cl2$Big <- cl2$Big == 1
  p2 <- claims_panel(cl2, toy$payments, t = 5, L = 4, features = "Big")
  expect_error(predict(fit, newdata = p2), "logical in `newdata` but was numeric")
})

test_that("accident years: gaps are kept by default, empty rows warn in ibnr_reserve()", {
  toy <- make_toy_portfolio()
  keep <- toy$claims$AccYear != 3
  cl <- toy$claims[keep, ]; pay <- toy$payments[toy$payments$Id %in% cl$Id, ]
  p <- claims_panel(cl, pay, t = 5, L = 4, features = "Type")
  expect_equal(p$accident_years, 1:5)
  pred <- predict(reserve_tree(p, seed = 1))
  expect_warning(ibnr_reserve(pred), "no reported claims in accident year\\(s\\) 3")
  # a recent accident year without reported claims is kept when passed explicitly
  cl4 <- toy$claims[toy$claims$AccYear <= 4, ]
  pay4 <- toy$payments[toy$payments$Id %in% cl4$Id, ]
  expect_warning(p4 <- claims_panel(cl4, pay4, t = 5, L = 4, features = "Type"),
                 "latest accident year with a claim reported by t = 5 is 4: accident year\\(s\\) 5 have no row")
  expect_equal(p4$accident_years, 1:4)
  expect_no_warning(claims_panel(cl4, pay4, t = 5, L = 4, features = "Type", accident_years = 1:4))
  p5 <- claims_panel(cl4, pay4, t = 5, L = 4, features = "Type", accident_years = 1:5)
  expect_warning(ibnr_reserve(predict(reserve_tree(p5, seed = 1))), "accident year\\(s\\) 5")
})

test_that("sparse periods are explained and constant periods are reported", {
  cl <- data.frame(Id = 1:4, AccYear = c(1, 1, 2, 2), RepYear = c(1, 1, 2, 2))
  pay <- data.frame(Id = 1:4, EventYear = c(1, 1, 2, 2), Paid = 10)
  p <- claims_panel(cl, pay, t = 2, L = 2)
  expect_error(reserve_tree(p, seed = 1), "reporting delay <= l and acc_year \\+ l \\+ 1 <= t")
  toy <- make_toy_portfolio()
  pz <- toy$payments[!(toy$payments$EventYear - toy$claims$AccYear[toy$payments$Id] == 3), ]
  p2 <- claims_panel(toy$claims, pz, t = 5, L = 4, features = "Type")
  fit <- reserve_tree(p2, seed = 1)
  expect_true(fit$constant_response[3])
  expect_output(print(fit), "constant training response in period\\(s\\) 2")
})

test_that("interface details: plot periods, ncores, OOB columns, data frames", {
  f <- toy_fit()
  fit <- reserve_tree(f$panel, seed = 1)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(fit, period = 4), "between 0 and L - 1 = 3")
  expect_error(plot(fit, period = -1), "between 0 and L - 1")
  expect_invisible(plot(fit, period = 0))
  bag <- suppressMessages(bag_reserve(f$panel, B = 3, seed = 1, ncores = 10000L, backend = "serial"))
  avail <- parallel::detectCores()
  expect_equal(bag$ncores, if (is.na(avail)) 10000L else min(10000L, as.integer(avail)))
  o <- oob_score(bag)
  expect_true("oob_score_covered" %in% names(o))
  covered <- o$coverage == 1 & !is.na(o$oob_score)
  expect_equal(o$oob_score[covered], o$oob_score_covered[covered])
  res <- ibnr_reserve(f$pred)
  d <- as.data.frame(res)
  expect_equal(nrow(d), nrow(res$cells))
  expect_equal(sum(d$total), unname(calendar_year_payments(res)["total", 1]))
  bt <- backtest(res, f$toy$claims, f$toy$payments_full)
  expect_setequal(unique(as.data.frame(bt)$component), c("reported", "ibnr", "total"))
  expect_error(calendar_year_payments(1:3), "reserve_prediction or a chain_ladder")
})

test_that("a bagged fit returns the claim-level mean of its completed replicates", {
  f <- toy_fit()
  bag <- bag_reserve(f$panel, B = 4, seed = 3, keep_trees = TRUE)
  pr <- predict(bag)
  P <- pr$claim_level
  expect_equal(dim(P), dim(f$panel$payments))
  # observed cells unchanged, future cells sum to the bagged mean of the reported component
  obs <- !is.na(f$panel$payments)
  expect_equal(P[obs], f$panel$payments[obs])
  agg <- baggingReserve:::aggregate_triangle(f$panel, P)[pr$cells$index]
  expect_equal(agg, rowMeans(pr$reported))
  # identical replicates whether or not the claim-level mean is kept, and with newdata
  expect_identical(predict(bag_reserve(f$panel, B = 4, seed = 3, claim_level = FALSE))$reported, pr$reported)
  expect_null(predict(bag_reserve(f$panel, B = 4, seed = 3, claim_level = FALSE))$claim_level)
  expect_equal(predict(bag, newdata = f$panel)$claim_level, P)
  # forked workers give the same claim-level mean
  skip_on_cran()
  expect_equal(predict(bag_reserve(f$panel, B = 4, seed = 3, ncores = 2L, batch_size = 1L))$claim_level, P)
})

test_that("backtest() scores a reference prediction against the same truth", {
  f <- toy_fit()
  res <- ibnr_reserve(f$pred)
  cl <- chain_ladder(as_triangle(f$panel))
  bt <- backtest(res, f$toy$claims, f$toy$payments_full, reference = cl)
  tot <- bt$tables$total
  ref <- calendar_year_payments(cl)
  expect_equal(tot$reference, unname(ref[tot$calendar]))
  expect_equal(tot$reference_error, tot$reference - tot$truth)
  expect_null(bt$tables$reported$reference)
  expect_output(print(bt), "reference")
  d <- as.data.frame(bt)
  expect_true(all(is.na(d$reference[d$component != "total"])))
  expect_error(backtest(res, f$toy$claims, f$toy$payments_full, reference = c(a = 1)), "calendar years")
  # without the reference the back-test is unchanged
  expect_identical(backtest(res, f$toy$claims, f$toy$payments_full)$tables$total,
                   tot[setdiff(names(tot), c("reference", "reference_error", "reference_rel_error"))])
})

test_that("silent traps of the second review now warn", {
  toy <- make_toy_portfolio()
  # a valuation date beyond the data
  expect_warning(claims_panel(toy$claims[toy$claims$AccYear <= 4, ], toy$payments[toy$payments$EventYear <= 4 &
                   toy$payments$Id %in% toy$claims$Id[toy$claims$AccYear <= 4], ], t = 5, L = 4, features = "Type",
                   accident_years = 1:4),
                 "no payment of the reported claims is dated in year t = 5")
  # a censored truth
  f <- toy_fit()
  res <- ibnr_reserve(f$pred)
  expect_warning(backtest(res, f$toy$claims, f$toy$payments), "no transaction after t")
  # missing feature values are not reported as unseen levels
  cl <- toy$claims; cl$Grp <- factor(ifelse(cl$Type == "A", "a", "b")); cl$Grp[c(1, 5)] <- NA
  p <- claims_panel(cl, toy$payments, t = 5, L = 4, features = c("Type", "Grp"), na_features = "surrogate")
  w <- character()
  withCallingHandlers(predict(reserve_tree(p, seed = 1)), warning = function(c) { w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
  expect_false(any(grepl("`Grp` = NA", w)))
})

test_that("bag_reserve() accepts another learner; the default is unchanged", {
  f <- toy_fit()
  lm_learner <- list(fit = function(formula, data) stats::lm(formula, data = data),
                     predict = function(model, newdata) stats::predict(model, newdata = newdata))
  bag_lm <- bag_reserve(f$panel, B = 3, seed = 4, learner = lm_learner, keep_trees = TRUE)
  expect_s3_class(bag_lm$trees[[1]][[1]], "lm")
  expect_equal(dim(predict(bag_lm)$reported), c(nrow(f$pred$cells), 3L))
  expect_true(all(is.finite(oob_score(bag_lm)$oob_error)))
  expect_equal(predict(bag_lm, newdata = f$panel)$reported, predict(bag_lm)$reported)
  # the same seed gives the same bootstrap samples, so the default path is untouched
  expect_identical(predict(bag_reserve(f$panel, B = 3, seed = 4))$reported,
                   predict(bag_reserve(f$panel, B = 3, seed = 4, learner = NULL))$reported)
  expect_error(bag_reserve(f$panel, B = 2, learner = list(fit = 1)), "learner")
})

test_that("reserve_tree() accepts another learner; the default is unchanged", {
  f <- toy_fit()
  lm_learner <- list(fit = function(formula, data) stats::lm(formula, data = data),
                     predict = function(model, newdata) stats::predict(model, newdata = newdata))
  fit_lm <- reserve_tree(f$panel, seed = 1, learner = lm_learner)
  expect_s3_class(fit_lm$fits[[1]], "lm")
  pr <- predict(fit_lm)
  expect_equal(dim(pr$claim_level), dim(f$panel$payments))
  expect_output(print(fit_lm), "user-supplied")
  expect_true(all(is.na(summary(fit_lm)$periods$leaves)))
  expect_equal(predict(fit_lm, newdata = f$panel)$reported, pr$reported)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(fit_lm), "another learner")
  expect_identical(predict(reserve_tree(f$panel, seed = 1))$reported, f$pred$reported)
})

test_that("chain_ladder() and backtest(reference = ) refuse non-finite values", {
  expect_error(chain_ladder(matrix(c(1, Inf, 2, 3), 2, byrow = TRUE)), "infinite observed cell")
  expect_error(chain_ladder(matrix(c(1, -Inf, 2, NA), 2, byrow = TRUE)), "infinite observed cell")
  expect_no_error(chain_ladder(matrix(c(1, 2, 2, NA), 2, byrow = TRUE)))   # NA marks the unobserved cell
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  res <- predict(reserve_tree(panel, seed = 1))
  yrs <- as.character(sort(unique(res$cells$calendar)))
  ref <- stats::setNames(rep(1, length(yrs)), yrs)
  expect_no_error(backtest(res, toy$claims, toy$payments_full, reference = ref, reference_component = "reported"))
  ref[1] <- Inf
  expect_error(backtest(res, toy$claims, toy$payments_full, reference = ref, reference_component = "reported"),
               "finite payments")
})

test_that("control and prune given together with a learner are ignored with a warning", {
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  lrn <- list(fit = function(formula, data) stats::lm(formula, data = data),
              predict = function(model, newdata) stats::predict(model, newdata))
  expect_warning(reserve_tree(panel, learner = lrn, prune = "min_xerror"), "`prune` applies to the default rpart")
  expect_warning(bag_reserve(panel, B = 2, seed = 1, learner = lrn, control = rpart::rpart.control(cp = 0.5)),
                 "`control` applies to the default rpart")
  expect_warning(reserve_tree(panel, learner = lrn, control = rpart::rpart.control(), prune = "none"),
                 "`control` and `prune` apply")
  expect_no_warning(reserve_tree(panel, learner = lrn))
})

test_that("a date given as a year is refused with a hint at the conversion", {
  claims <- data.frame(Id = 1:2, AccYear = as.Date(c("2001-03-01", "2002-05-01")), RepYear = c(2001, 2002))
  payments <- data.frame(Id = 1:2, EventYear = c(2001, 2002), Paid = c(1, 2))
  expect_error(claims_panel(claims, payments, t = 2002, L = 1), "holds dates.*as.integer\\(format\\(AccYear")
})
