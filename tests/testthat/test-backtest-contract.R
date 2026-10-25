# The back-test against the panel's contract: claims set aside by beyond_L = "drop", the
# chronology of reporting and payments, and the column mapping carried by the prediction; the
# checks of the two-part learner's parts; smaller guards of predict() and bag_reserve().

toy_with_long_delay <- function() {
  toy <- make_toy_portfolio()
  # a claim of accident year 1 reported in year 5 (delay 4 > L = 3), paying after t as well
  toy$claims <- rbind(toy$claims, data.frame(Id = 10000L, AccYear = 1L, RepYear = 5L, Type = "A"))
  extra <- data.frame(Id = 10000L, EventYear = c(5L, 6L), Paid = c(10, 25))
  toy$payments_full <- rbind(toy$payments_full, extra)
  toy$payments <- rbind(toy$payments, extra[extra$EventYear <= 5, ])
  toy
}

test_that("claims_panel(beyond_L = \"drop\") composes with backtest() on the full truth tables", {
  toy <- toy_with_long_delay()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 3, features = "Type", beyond_L = "drop")
  expect_identical(panel$beyond_L_ids, 10000L)
  res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)))
  expect_identical(res$beyond_L_ids, 10000L)
  bt <- backtest(res, toy$claims, toy$payments_full)
  expect_equal(bt$not_scored$n_claims_beyond_L, 1L)
  expect_equal(bt$not_scored$n_payments_beyond_L, 1L)
  expect_equal(bt$not_scored$amount_beyond_L, 25)
  # the claim's payment after t is in the excluded tail, not in any scored component
  without <- backtest(res, toy$claims[toy$claims$Id != 10000L, ],
                      toy$payments_full[toy$payments_full$Id != 10000L, ])
  expect_equal(bt$tables, without$tables)
  expect_equal(bt$tail$amount, without$tail$amount + 25)
  expect_output(print(bt), "Set aside by claims_panel\\(beyond_L = \"drop\"\\): 1 claim")
  # the same truth against a panel that did not set the claim aside is still an error
  panel0 <- claims_panel(toy$claims[toy$claims$Id != 10000L, ], toy$payments[toy$payments$Id != 10000L, ],
                         t = 5, L = 3, features = "Type", beyond_L = "drop")
  expect_length(panel0$beyond_L_ids, 0L)
  res0 <- ibnr_reserve(predict(reserve_tree(panel0, seed = 1)))
  expect_error(backtest(res0, toy$claims, toy$payments_full), "reported by t = 5 but are absent from the prediction")
  # a set-aside claim whose delay in the truth is within L contradicts the panel
  cl <- toy$claims; cl$RepYear[cl$Id == 10000L] <- 3L
  pay <- toy$payments_full; pay$EventYear[pay$Id == 10000L & pay$EventYear == 5L] <- 3L
  expect_error(backtest(res, cl, pay), "set aside with beyond_L = \"drop\"")
})

test_that("backtest() holds the truth to the chronology of the data contract", {
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)))
  cl <- toy$claims; pay <- toy$payments_full
  # an IBNR claim paid in year 6 but reported in year 7: the model defines that payment as zero
  cl_bad <- rbind(cl, data.frame(Id = 20000L, AccYear = 5L, RepYear = 7L, Type = "A"))
  pay_bad <- rbind(pay, data.frame(Id = 20000L, EventYear = 6L, Paid = 1e6))
  expect_error(backtest(res, cl_bad, pay_bad),
               "1 payment\\(s\\) in `payments` are dated before the reporting year.*claim 20000: paid in 6, reported in 7")
  # a claim reported before its accident year
  cl_acc <- rbind(cl, data.frame(Id = 20000L, AccYear = 5L, RepYear = 4L, Type = "A"))
  expect_error(backtest(res, cl_acc, pay), "reported before their accident year")
  # a missing reporting year: treated as not reported by t, with a warning
  cl_na <- rbind(cl, data.frame(Id = 20000L, AccYear = 5L, RepYear = NA_integer_, Type = "A"))
  pay_na <- rbind(pay, data.frame(Id = 20000L, EventYear = 6L, Paid = 7))
  expect_warning(bt_na <- backtest(res, cl_na, pay_na), "have no reporting year in `claims`")
  bt <- backtest(res, cl, pay)
  expect_equal(bt_na$tables$ibnr$truth[1], bt$tables$ibnr$truth[1] + 7)
  # the deposited data obey the contract: the shipped truth passes without an error
  data("synthetic_claims_full", "synthetic_payments_full", package = "baggingReserve", envir = environment())
  m <- match(synthetic_payments_full$Id, synthetic_claims_full$Id)
  expect_false(any(synthetic_payments_full$EventYear < synthetic_claims_full$RepYear[m]))
})

test_that("backtest() takes its column names from the prediction's panel and warns without a reporting year", {
  toy <- make_toy_portfolio()
  ren <- function(d, map) { names(d)[match(names(map), names(d))] <- map; d }
  cmap <- c(Id = "claim", AccYear = "occ", RepYear = "notif")
  pmap <- c(Id = "claim", EventYear = "year", Paid = "amount")
  cl <- ren(toy$claims, cmap); pay <- ren(toy$payments, pmap); pay_full <- ren(toy$payments_full, pmap)
  panel <- claims_panel(cl, pay, t = 5, L = 4, id = "claim", acc_year = "occ", rep_year = "notif",
                        event_year = "year", paid = "amount", features = "Type")
  res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)))
  expect_equal(res$mapping$rep_year, "notif")
  bt <- backtest(res, cl, pay_full)
  expect_false(is.null(bt$counts))
  expect_equal(bt$columns$rep_year, "notif")
  ref <- ibnr_reserve(predict(reserve_tree(claims_panel(toy$claims, toy$payments, t = 5, L = 4,
                                                         features = "Type"), seed = 1)))
  expect_equal(bt$tables, backtest(ref, toy$claims, toy$payments_full)$tables)
  # the population checks run under the mapped name: a truth selected by the reported identifiers
  w <- character()
  withCallingHandlers(backtest(res, cl[cl$claim %in% res$claim_ids, ], pay_full[pay_full$claim %in% res$claim_ids, ]),
                      warning = function(cnd) { w <<- c(w, conditionMessage(cnd)); invokeRestart("muffleWarning") })
  expect_true(any(grepl("holds no claim reported after t", w)))
  # a reporting delay given to the panel is turned into the reporting year
  cl_d <- cl; cl_d$delay <- cl_d$notif - cl_d$occ; cl_d$notif <- NULL
  panel_d <- claims_panel(cl_d, pay, t = 5, L = 4, id = "claim", acc_year = "occ", rep_delay = "delay",
                          event_year = "year", paid = "amount", features = "Type")
  bt_d <- backtest(ibnr_reserve(predict(reserve_tree(panel_d, seed = 1))), cl_d, pay_full)
  expect_equal(bt_d$counts, bt$counts)
  # no reporting-year column: the checks and the counts are skipped, with a warning (silently with NULL)
  res_old <- res; res_old$mapping <- NULL          # a prediction without a mapping: the usage defaults
  expect_warning(b0 <- backtest(res, cl_d, pay_full, rep_year = "notif"), "no reporting-year column `notif`")
  expect_null(b0$counts)
  expect_silent(b1 <- backtest(res, cl_d, pay_full, rep_year = NULL))
  expect_null(b1$counts)
  expect_error(backtest(res_old, cl, pay_full), "`claims` lacks column Id")
})

test_that("two_part_learner() validates its occurrence and size models before combining them", {
  f <- list(toy = make_toy_portfolio())
  f$panel <- claims_panel(f$toy$claims, f$toy$payments, t = 5, L = 4, features = "Type")
  out <- function(fun) list(fit = function(formula, data) list(k = 1), predict = function(model, newdata) fun(nrow(newdata)))
  ok_occ <- out(function(n) rep(1, n)); ok_size <- out(function(n) rep(100, n))
  fit_with <- function(occ, size) reserve_tree(f$panel, learner = two_part_learner(occ, size, min_nonzero = 1))
  expect_no_error(predict(fit_with(ok_occ, ok_size)))
  bad <- list(scalar = function(n) 100, character = function(n) rep("100", n), short = function(n) rep(100, n - 1),
              matrix = function(n) matrix(100, n, 2), nonfinite = function(n) c(Inf, rep(100, n - 1)))
  msg <- c(scalar = "returned 1 value\\(s\\) for", character = "must return a numeric vector, not character",
           short = "returned [0-9]+ value\\(s\\) for", matrix = "not a numeric matrix with 2 columns",
           nonfinite = "non-finite value")
  for (k in names(bad)) {
    expect_error(predict(fit_with(ok_occ, out(bad[[k]]))), "size model's predict\\(\\)", info = k)
    expect_error(predict(fit_with(ok_occ, out(bad[[k]]))), msg[[k]], info = k)
    expect_error(predict(fit_with(out(bad[[k]]), ok_size)), "occurrence model's predict\\(\\)", info = k)
  }
  expect_error(predict(fit_with(out(function(n) rep(1.5, n)), ok_size)), "probabilities in \\[0, 1\\]")
  # a one-column matrix or data frame is a valid output; a data frame with more columns is named as such
  expect_no_error(predict(fit_with(ok_occ, out(function(n) matrix(100, n, 1)))))
  expect_no_error(predict(fit_with(ok_occ, out(function(n) data.frame(p = rep(100, n))))))
  expect_error(predict(fit_with(ok_occ, out(function(n) data.frame(a = rep(1, n), b = 2)))), "a data frame with 2 columns")
  df_learner <- list(fit = function(formula, data) stats::lm(formula, data = data),
                     predict = function(model, newdata) data.frame(fit = stats::predict(model, newdata)))
  expect_equal(predict(reserve_tree(f$panel, learner = df_learner))$reported,
               predict(reserve_tree(f$panel, learner = list(fit = df_learner$fit,
                 predict = function(model, newdata) stats::predict(model, newdata))))$reported)
  # the size model's simulate() is checked as well
  sim_bad <- c(ok_size, list(simulate = function(model, newdata) 70))
  fit_sim <- fit_with(ok_occ, sim_bad)
  expect_error(predict(fit_sim, completion = "draw", R = 1, seed = 1), "size model's simulate\\(\\) returned 1 value")
})

test_that("predict() refuses R and seed with plugged-in completion and warns about an earlier panel", {
  f <- list(toy = make_toy_portfolio())
  panel <- claims_panel(f$toy$claims, f$toy$payments, t = 5, L = 3, features = "Type", beyond_L = "drop")
  tree <- reserve_tree(panel, seed = 1)
  expect_error(predict(tree, R = 7), "`R` given with completion = \"mean\": it applies to completion = \"draw\" only")
  expect_error(predict(tree, R = 7, seed = 3), "`R` and `seed` given with completion = \"mean\": they apply")
  expect_no_error(predict(tree, completion = "draw", R = 2, seed = 3))
  # the same claims valued a year earlier: the trees have seen their later payments
  cl4 <- f$toy$claims[f$toy$claims$AccYear <= 4 & f$toy$claims$RepYear <= 4, ]
  p4 <- claims_panel(cl4, f$toy$payments[f$toy$payments$Id %in% cl4$Id & f$toy$payments$EventYear <= 4, ],
                     t = 4, L = 3, features = "Type", beyond_L = "drop")
  expect_warning(pr <- predict(tree, newdata = p4), "valued at t = 4, before the fit's t = 5")
  expect_equal(pr$t, 4L)
  expect_equal(min(pr$cells$calendar), 5L)
})

test_that("bag_reserve() reports a reduced ncores; print() shows the covered score at low coverage", {
  f <- list(toy = make_toy_portfolio())
  panel <- claims_panel(f$toy$claims, f$toy$payments, t = 5, L = 4, features = "Type")
  local_mocked_bindings(detected_cores = function() 2L)
  expect_message(bag <- bag_reserve(panel, B = 1, seed = 1, ncores = 7L, backend = "serial"),
                 "ncores = 7 is more than the 2 cores detected")
  expect_equal(bag$ncores, 2L)
  expect_output(print(bag), "oob_score_covered")
  expect_false(is.null(predict(bag)$provenance$control))
  expect_equal(names(predict(bag)$provenance$versions), c("baggingReserve", "rpart", "R"))
  expect_output(print(predict(bag)), "plug-in completion")
  pred <- predict(reserve_tree(panel, seed = 1))
  expect_warning(as_triangle(pred, component = "total"), "no IBNR component")
})

test_that("backtest() takes the reporting year by the constructor's rule and checks it against the prediction", {
  fx <- make_identity_fixture()
  cl <- fx$claims; cl$Delay <- cl$RepYear - cl$AccYear
  ref_panel <- claims_panel(cl, fx$payments, t = 3, L = 2, features = "Type")
  ref <- backtest(ibnr_reserve(predict(reserve_tree(ref_panel, seed = 1))), cl, fx$payments_full)
  expect_equal(ref$counts$realised[ref$counts$acc_year == "total"], 2)
  fit_bt <- function(claims) {
    panel <- claims_panel(claims, fx$payments[fx$payments$Id %in% claims$Id[claims$AccYear + claims$Delay <= 3], ],
                          t = 3, L = 2, rep_delay = "Delay", features = "Type")
    backtest(ibnr_reserve(predict(reserve_tree(panel, seed = 1))), claims, fx$payments_full)
  }
  # the panel was built from the delay: a complete, a partly missing and a missing reporting-year
  # column give the same back-test, monetary truth and realised IBNR counts alike
  partly <- cl; partly$RepYear[c(1, 12, 36)] <- NA
  none <- cl; none$RepYear <- NA_integer_
  for (claims in list(cl, partly, none)) {
    bt <- fit_bt(claims)
    expect_equal(bt$tables, ref$tables)
    expect_equal(bt$counts, ref$counts)
  }
  # a reporting-year column that contradicts the delay is an error, as in claims_panel()
  bad <- cl; bad$RepYear[2] <- bad$RepYear[2] + 1L
  expect_error(fit_bt(bad), "disagree for 1 claim")
  # a reported claim whose reporting year changes in the truth, still before t, is refused
  res <- ibnr_reserve(predict(reserve_tree(ref_panel, seed = 1)))
  moved <- cl; k <- which(moved$AccYear == 1 & moved$RepYear == 2)[1]; moved$RepYear[k] <- 1L
  expect_error(backtest(res, moved, fx$payments_full), "another reporting year")
  # a prediction saved without the reporting delays (an earlier version) is checked for t only
  res_old <- res; res_old$claim_rep_delays <- NULL
  expect_no_error(backtest(res_old, moved, fx$payments_full))
})
