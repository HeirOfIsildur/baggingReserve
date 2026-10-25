# Summaries over replicates, plots (smoke tests on a null device) and the back-test.

toy <- make_toy_portfolio()
toy_panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")

test_that("summary gives mean, sd and quantiles per calendar year and component", {
  bag <- bag_reserve(toy_panel, B = 6, seed = 71)
  res <- ibnr_reserve(predict(bag), claim_counts(toy_panel))
  s <- summary(res)
  expect_s3_class(s, "summary.reserve_prediction")
  expect_equal(names(s$tables), c("reported", "ibnr", "total"))
  tab <- s$tables$total
  expect_equal(rownames(tab), c("6", "7", "8", "9", "total"))
  expect_equal(colnames(tab)[1:3], c("mean", "sd", "min"))
  cy <- calendar_year_payments(res, "total")
  expect_equal(unname(tab[, "mean"]), unname(rowMeans(cy)))
  expect_equal(unname(tab[, "q50%"]), unname(apply(cy, 1, stats::median)))
  expect_true(all(tab[, "min"] <= tab[, "mean"] & tab[, "mean"] <= tab[, "max"]))
  expect_output(print(s), "Quantiles \\(type 7\\)")
  # order-statistic quantiles as in the article
  s1 <- summary(res, probs = c(0.5), type = 1)
  expect_equal(unname(s1$tables$total[, "q50%"]), unname(apply(cy, 1, stats::quantile, 0.5, type = 1)))
  # a single tree has no sd
  s_tree <- summary(predict(reserve_tree(toy_panel, seed = 1)))
  expect_true(all(is.na(s_tree$tables$reported[, "sd"])))
  expect_output(print(s_tree), "one prediction column")
})

test_that("backtest reconciles the truth by component and reports the excluded tail", {
  bag <- bag_reserve(toy_panel, B = 6, seed = 81)
  res <- ibnr_reserve(predict(bag), claim_counts(toy_panel))
  bt <- backtest(res, toy$claims, toy$payments_full)
  expect_s3_class(bt, "reserve_backtest")
  expect_equal(names(bt$tables), c("reported", "ibnr", "total"))
  # truth by hand: payments after t = 5 with development <= 4, split by reporting status
  full <- toy$payments_full
  ay <- toy$claims$AccYear[match(full$Id, toy$claims$Id)]
  dev <- full$EventYear - ay
  fut <- full$EventYear > 5 & dev <= 4
  rep_ids <- toy_panel$claims$id
  truth_total <- tapply(full$Paid[fut], full$EventYear[fut], sum)
  tab <- bt$tables$total
  expect_equal(tab$truth[match(names(truth_total), tab$calendar)], as.vector(truth_total))
  expect_equal(tab$truth[tab$calendar == "total"], sum(full$Paid[fut]))
  expect_equal(bt$tables$reported$truth[nrow(tab)], sum(full$Paid[fut & full$Id %in% rep_ids]))
  expect_equal(bt$tables$ibnr$truth[nrow(tab)], sum(full$Paid[fut & !full$Id %in% rep_ids]))
  expect_equal(tab$error, tab$predicted - tab$truth)
  expect_equal(bt$tail$n, sum(full$EventYear > 5 & dev > 4))
  expect_true(all(tab$lower <= tab$upper))
  expect_type(tab$inside_range, "logical")
  expect_output(print(bt), "Back-test")
  # the toy portfolio pays through development 4 only: no tail
  expect_equal(bt$tail$n, 0L)
})

test_that("backtest on the identity fixture has zero error", {
  fx <- make_identity_fixture()
  panel <- claims_panel(fx$claims, fx$payments, t = 3, L = 2, features = "Type")
  res <- ibnr_reserve(predict(reserve_tree(panel, seed = 1)), claim_counts(panel))
  bt <- backtest(res, fx$claims, fx$payments_full)
  expect_equal(bt$tables$total$error, rep(0, 3))
  expect_equal(bt$tables$ibnr$truth[3], 320)
  expect_equal(bt$tables$reported$error, rep(0, 3))
})

test_that("plots run on a null device for every display and B", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  tree_pred <- predict(reserve_tree(toy_panel, seed = 1))
  bag <- bag_reserve(toy_panel, B = 25, seed = 91)
  res <- ibnr_reserve(predict(bag), claim_counts(toy_panel))
  small <- ibnr_reserve(predict(bag_reserve(toy_panel, B = 3, seed = 92)), claim_counts(toy_panel))
  bt <- backtest(res, toy$claims, toy$payments_full)
  truth_cal <- stats::setNames(bt$tables$total$truth, bt$tables$total$calendar)
  expect_invisible(plot(tree_pred, "calendar"))
  expect_invisible(plot(tree_pred, "next_year"))
  expect_invisible(plot(res, "calendar", truth = truth_cal))
  expect_invisible(plot(res, "next_year", truth = truth_cal[[1]], reference = c(`chain ladder` = 1000)))
  expect_invisible(plot(res, "ultimate", component = "ibnr"))
  expect_invisible(plot(small, "ultimate"))
  expect_invisible(plot(bag))
  fit <- reserve_tree(toy_panel, seed = 1)
  expect_invisible(plot(fit, period = 0))
  expect_invisible(plot(fit, period = 3))
})

test_that("backtest validates the truth tables", {
  res <- predict(reserve_tree(toy_panel, seed = 1))
  dup <- rbind(toy$claims, toy$claims[1, ])
  expect_error(backtest(res, dup, toy$payments_full), "duplicated")
  expect_error(backtest(res, toy$claims[-1, ], toy$payments_full[toy$payments_full$Id != 1, ]),
               "absent from")
  bad_amt <- toy$payments_full; bad_amt$Paid[1] <- NA
  expect_error(backtest(res, toy$claims, bad_amt), "finite numeric")
})

test_that("backtest refuses a truth population that omits reported claims or misstates it", {
  fx <- make_toy_portfolio()
  sub <- fx$claims[fx$claims$Type == "A", ]
  p <- claims_panel(sub, fx$payments[fx$payments$Id %in% sub$Id, ], t = 5, L = 4, features = "Type")
  res <- ibnr_reserve(predict(reserve_tree(p, seed = 1, control = rpart::rpart.control(cp = 0, xval = 2))),
                      claim_counts(p))
  # the same population selected by the same criterion: fine
  expect_s3_class(backtest(res, fx$claims[fx$claims$Type == "A", ],
                           fx$payments_full[fx$payments_full$Id %in% sub$Id, ]), "reserve_backtest")
  # a claim of the other type, reported by t, sneaks into the truth: caught via RepYear
  wrong <- rbind(sub, fx$claims[fx$claims$Type == "B", ][1, ])
  expect_error(backtest(res, wrong, fx$payments_full[fx$payments_full$Id %in% wrong$Id, ]),
               "same portfolio")
  # without a reporting-year column the check is skipped, with a warning, and the claim counts as IBNR
  wrong2 <- wrong; wrong2$RepYear <- NULL
  expect_warning(b2 <- backtest(res, wrong2, fx$payments_full[fx$payments_full$Id %in% wrong2$Id, ]),
                 "no reporting-year column `RepYear`")
  expect_s3_class(b2, "reserve_backtest")
  expect_error(backtest(res, sub, fx$payments_full[fx$payments_full$Id %in% sub$Id, ], probs = c(0.9, 0.1)),
               "increasing")
  expect_error(plot(res, "next_year", probs = c(0.5)), "two increasing")
})

test_that("ibnr_reserve refuses a cumulative count triangle", {
  fx <- make_toy_portfolio()
  p <- claims_panel(fx$claims, fx$payments, t = 5, L = 4, features = "Type")
  pred <- predict(reserve_tree(p, seed = 1, control = rpart::rpart.control(cp = 0, xval = 2)))
  expect_error(ibnr_reserve(pred, as_triangle(p, what = "counts", cumulative = TRUE)), "incremental")
  expect_error(claims_panel(fx$claims, fx$payments, t = Inf, L = 4, features = "Type"), "single integer")
})

test_that("claim_reserves() returns per-claim reserves that add up to the reported total", {
  tree_pred <- predict(reserve_tree(toy_panel, seed = 1))
  cr <- claim_reserves(tree_pred)
  expect_equal(names(cr), c("id", "acc_year", "reserve"))
  expect_equal(nrow(cr), nrow(toy_panel$claims))
  cy <- calendar_year_payments(tree_pred, "reported")
  expect_equal(sum(cr$reserve), unname(cy["total", 1]))
  expect_true(all(cr$reserve[cr$acc_year <= 5 - 4] == 0))   # fully developed through L
  bag_pred <- ibnr_reserve(predict(bag_reserve(toy_panel, B = 4, seed = 3)))
  expect_equal(sum(claim_reserves(bag_pred)$reserve),
               mean(calendar_year_payments(bag_pred, "reported")["total", ]))
  expect_error(claim_reserves(predict(bag_reserve(toy_panel, B = 2, seed = 3, claim_level = FALSE))),
               "claim-level")
  expect_error(claim_reserves(1), "reserve_prediction")
})

test_that("plot() rejects unknown and misspelt arguments", {
  res <- predict(bag_reserve(toy_panel, B = 3, seed = 5))
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_error(plot(res, compnent = "reported"), "did you mean `component`")
  expect_error(plot(res, "calendar", "reported", c(0.1, 0.9), NULL, NULL, NULL, 99), "unnamed")
  expect_error(plot(reserve_tree(toy_panel, seed = 1), perod = 1), "did you mean `period`")
  expect_silent(plot(res, cex.axis = 0.8, col = "red"))
})

## ---- back-test contract (0.2.0) ------------------------------------------------------------

bt_fixture <- function() {
  pred <- predict(reserve_tree(toy_panel, seed = 1))
  list(pred = pred, res = ibnr_reserve(pred), cl = chain_ladder(as_triangle(toy_panel)))
}

test_that("a reference is scored against the truth of the population it predicts", {
  f <- bt_fixture()
  # an all-claims reference needs the prediction's IBNR component
  expect_error(backtest(f$pred, toy$claims, toy$payments_full, reference = f$cl), "ibnr_reserve\\(\\)")
  bt <- backtest(f$res, toy$claims, toy$payments_full, reference = f$cl)
  tot <- bt$tables$total
  expect_gt(bt$tables$ibnr$truth[nrow(tot)], 0)               # realised IBNR payments exist ...
  expect_equal(tot$reference_error, tot$reference - tot$truth)  # ... and are part of the target
  expect_equal(bt$reference_component, "total")
  # a reference of the reported claims only, as a named vector, against the reported truth
  ref_rep <- calendar_year_payments(f$pred, "reported")[, 1]
  bt_r <- backtest(f$pred, toy$claims, toy$payments_full, reference = ref_rep, reference_component = "reported")
  expect_equal(bt_r$tables$reported$reference_error, rep(0, nrow(tot)) + bt_r$tables$reported$error)
  bt_r2 <- backtest(f$res, toy$claims, toy$payments_full, reference = ref_rep, reference_component = "reported")
  expect_null(bt_r2$tables$total$reference)
  expect_equal(bt_r2$tables$reported$reference, unname(ref_rep))
  # a chain ladder predicts all claims
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = f$cl, reference_component = "reported"),
               "all claims")
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = f$cl, reference_component = "x"))
})

test_that("a chain-ladder reference must describe the prediction's triangle", {
  f <- bt_fixture()
  other <- make_toy_portfolio(seed = 99)
  p2 <- claims_panel(other$claims, other$payments, t = 5, L = 4, features = "Type")
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = chain_ladder(as_triangle(p2))),
               "other observed payments")
  tri <- as_triangle(toy_panel)
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = chain_ladder(tri[-5, ])),
               "accident years")
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = chain_ladder(tri[, 1:4])),
               "development years")
  masked <- tri; masked[2, 4] <- NA
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = chain_ladder(masked)),
               "observation mask")
  shifted <- tri; attr(shifted, "t") <- 6L
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = chain_ladder(shifted)),
               "valuation date")
  # a cumulative triangle of the same panel is the same reference
  expect_equal(backtest(f$res, toy$claims, toy$payments_full,
                        reference = chain_ladder(as_triangle(toy_panel, cumulative = TRUE)))$tables$total$reference,
               backtest(f$res, toy$claims, toy$payments_full, reference = f$cl)$tables$total$reference)
})

test_that("a named-vector reference must cover exactly the prediction's calendar years", {
  f <- bt_fixture()
  ref <- calendar_year_payments(f$cl)
  expect_equal(backtest(f$res, toy$claims, toy$payments_full, reference = ref[names(ref) != "total"])$tables$total$reference,
               unname(ref))
  bad_total <- ref; bad_total[["total"]] <- bad_total[["total"]] + 1000
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = bad_total), "is not the sum")
  expect_error(backtest(f$res, toy$claims, toy$payments_full, reference = c(ref, `10` = 5)), "another horizon")
})

test_that("observed_to leaves the unobserved calendar years unscored", {
  f <- bt_fixture()
  full <- backtest(f$res, toy$claims, toy$payments_full)
  short <- toy$payments_full[toy$payments_full$EventYear <= 7, ]
  # without observed_to the missing years are zeros, with a warning
  expect_warning(backtest(f$res, toy$claims, short), "pass observed_to")
  bt <- backtest(f$res, toy$claims, short, observed_to = 7)
  expect_equal(bt$total_years, 6:7)
  for (cmp in c("reported", "ibnr", "total")) {
    tab <- bt$tables[[cmp]]; ref <- full$tables[[cmp]]
    expect_equal(tab$truth[1:2], ref$truth[1:2])
    expect_true(all(is.na(tab$truth[3:4]) & is.na(tab$error[3:4]) & is.na(tab$inside_range[3:4])))
    expect_equal(tab$predicted[1:4], ref$predicted[1:4])           # the prediction is still shown
    expect_equal(tab$truth[5], sum(ref$truth[1:2]))                # total over the observed years
    expect_equal(tab$predicted[5], sum(ref$predicted[1:2]))
  }
  expect_true(all(is.na(bt$truth_cells$total[outer(1:5, 0:4, `+`) > 7])))
  expect_output(print(bt), "observed through calendar year 7")
  # a reference total covers the same years
  btr <- backtest(f$res, toy$claims, short, observed_to = 7, reference = f$cl)
  expect_equal(btr$tables$total$reference[5], sum(calendar_year_payments(f$cl)[1:2]))
  # payments after observed_to are not scored but counted
  bt_cut <- backtest(f$res, toy$claims, toy$payments_full, observed_to = 7)
  expect_equal(bt_cut$tables$total$truth, bt$tables$total$truth)
  expect_gt(bt_cut$not_scored$n_payments_after_observed_to, 0L)
  # a complete follow-up with a genuinely empty year: observed_to makes it a realised zero
  no9 <- toy$payments_full[toy$payments_full$EventYear != 9, ]
  expect_warning(backtest(f$res, toy$claims, no9), "observed_to")
  bt9 <- expect_no_warning(backtest(f$res, toy$claims, no9, observed_to = 9))
  expect_equal(bt9$tables$total$truth[4], 0)
  # observed_to covering the horizon is the default
  expect_identical(backtest(f$res, toy$claims, toy$payments_full, observed_to = 12)$tables, full$tables)
  expect_error(backtest(f$res, toy$claims, short, observed_to = 5), "after the valuation date")
})

test_that("a single prediction column has no replicate range", {
  f <- bt_fixture()
  bt <- backtest(f$res, toy$claims, toy$payments_full)
  tab <- bt$tables$total
  expect_true(all(is.na(tab$lower) & is.na(tab$upper) & is.na(tab$inside_range)))
  expect_output(print(bt), "point prediction")
  expect_false(any(grepl("lower", capture.output(print(bt)))))
  expect_true(all(is.na(as.data.frame(bt)$inside_range)))
  # a bagged fit keeps its range
  res_b <- ibnr_reserve(predict(bag_reserve(toy_panel, B = 4, seed = 2)))
  expect_false(anyNA(backtest(res_b, toy$claims, toy$payments_full)$tables$total$lower))
})

test_that("the IBNR claim counts are scored by accident year", {
  fx <- make_identity_fixture()
  panel <- claims_panel(fx$claims, fx$payments, t = 3, L = 2, features = "Type")
  bt <- backtest(ibnr_reserve(predict(reserve_tree(panel, seed = 1))), fx$claims, fx$payments_full)
  expect_equal(bt$counts$acc_year, c("1", "2", "3", "total"))
  expect_equal(bt$counts$projected, c(0, 0, 2, 2))
  expect_equal(bt$counts$realised, c(0, 0, 2, 2))
  expect_output(print(bt), "Numbers of IBNR claims")
  # by hand on the toy portfolio
  f <- bt_fixture()
  bt2 <- backtest(f$res, toy$claims, toy$payments_full)
  late <- toy$claims$RepYear > 5
  expect_equal(bt2$counts$realised[1:5], as.vector(table(factor(toy$claims$AccYear[late], levels = 1:5))))
  U <- f$res$ibnr_details$counts_future_cumulative
  expect_equal(bt2$counts$projected[1:5], unname(U[, ncol(U)]))
  # no counts without an IBNR component or a reporting year
  expect_null(backtest(f$pred, toy$claims, toy$payments_full)$counts)
  no_ry <- toy$claims; no_ry$RepYear <- NULL
  expect_null(backtest(f$res, no_ry, toy$payments_full, rep_year = NULL)$counts)
})

test_that("claims outside the prediction's accident years are reported as not scored", {
  f <- bt_fixture()
  cl <- rbind(toy$claims, data.frame(Id = 9999, AccYear = 6, RepYear = 6, Type = "A"))
  pay <- rbind(toy$payments_full, data.frame(Id = 9999, EventYear = 6:7, Paid = c(1e6, 1e6)))
  bt <- backtest(f$res, cl, pay)
  expect_equal(bt$tables, backtest(f$res, toy$claims, toy$payments_full)$tables)
  expect_equal(bt$not_scored$n_claims_outside, 1L)
  expect_equal(bt$not_scored$amount_outside, 2e6)
  expect_output(print(bt), "Not scored: 1 claim")
  expect_error(backtest(f$res, toy$claims, toy$payments_full, type = 12), "quantile type")
})

test_that("plot() takes the quantile type and checks the truth", {
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  res <- predict(bag_reserve(toy_panel, B = 5, seed = 4))
  expect_invisible(plot(res, type = 1))
  expect_invisible(plot(res, "next_year", type = 1))
  expect_error(plot(res, type = 12), "quantile type")
  expect_error(plot(res, truth = 1:3), "one value per future calendar year")
  expect_invisible(plot(res, truth = 1:4))
  expect_error(plot(res, truth = c(`6` = 1)), "lacks the calendar year")
  expect_error(plot(res, "ultimate", truth = 1:2), "single number")
})
