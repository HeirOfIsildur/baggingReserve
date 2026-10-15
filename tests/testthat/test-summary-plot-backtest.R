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
  # without a reporting-year column the check is skipped and the claim counts as IBNR
  wrong2 <- wrong; wrong2$RepYear <- NULL
  expect_s3_class(backtest(res, wrong2, fx$payments_full[fx$payments_full$Id %in% wrong2$Id, ]),
                  "reserve_backtest")
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
