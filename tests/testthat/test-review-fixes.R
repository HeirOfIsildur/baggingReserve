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
  expect_equal(calendar_year_payments(inc), chain_ladder_calendar(inc))
  expect_equal(unname(as_triangle(inc, "cumulative")[, ncol(inc$cumulative)]), unname(inc$ultimate))
})

test_that("backtest() warns when the truth has no claim reported after t", {
  f <- toy_fit()
  res <- ibnr_reserve(f$pred)
  expect_gt(sum(res$ibnr), 0)
  reported_only <- f$toy$claims[f$toy$claims$Id %in% res$claim_ids, ]
  expect_warning(backtest(res, reported_only,
                          f$toy$payments_full[f$toy$payments_full$Id %in% reported_only$Id, ]),
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
  expect_equal(claims_panel(cl4, pay4, t = 5, L = 4, features = "Type")$accident_years, 1:4)
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
  bag <- bag_reserve(f$panel, B = 3, seed = 1, ncores = 10000L, backend = "serial")
  expect_lte(bag$ncores, max(1L, parallel::detectCores(), na.rm = TRUE))
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
