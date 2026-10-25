# The learner contract (every prediction a learner makes gets the forecasting frame and one
# check), the description of the variability of a prediction in print/summary/plot/backtest,
# arguments that used to be ignored silently, and smaller guards.

contract_fixture <- function() {
  toy <- make_toy_portfolio()
  list(toy = toy, panel = claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type"))
}

# A learner that obeys the documented interface strictly: its model records the predictor names
# of the training frame (all columns but the response) and its predict() refuses any other frame.
strict_learner <- function(value = function(n) rep(1, n), sim = NULL) {
  l <- list(
    fit = function(formula, data) {
      resp <- all.vars(formula)[1L]
      list(predictors = setdiff(names(data), resp), mean = mean(data[[resp]]))
    },
    predict = function(model, newdata) {
      if (!identical(names(newdata), model$predictors))
        stop("schema violation: got ", paste(names(newdata), collapse = ","))
      value(nrow(newdata))
    })
  if (!is.null(sim)) l$simulate <- function(model, newdata) {
    if (!identical(names(newdata), model$predictors)) stop("schema violation in simulate()")
    sim(nrow(newdata))
  }
  l
}

test_that("out-of-bag prediction passes the learner the response-free forecasting frame", {
  f <- contract_fixture()
  lrn <- strict_learner()
  expect_no_error(bag <- bag_reserve(f$panel, B = 3, seed = 1, learner = lrn, oob = TRUE))
  expect_false(is.null(bag$oob))
  expect_no_error(bag_reserve(f$panel, B = 3, seed = 1, learner = lrn, oob = FALSE))
  expect_no_error(predict(reserve_tree(f$panel, learner = lrn)))
  sim <- strict_learner(sim = function(n) rep(2, n))
  expect_no_error(bag_reserve(f$panel, B = 2, seed = 1, learner = sim, completion = "draw"))
  expect_no_error(predict(reserve_tree(f$panel, learner = sim), completion = "draw", R = 2, seed = 1))
})

test_that("the default trees give the same out-of-bag scores with the response-free frame", {
  # rpart ignores columns that are not in its formula, so the scores are unchanged; recomputed by
  # hand from the kept trees and bootstrap indices
  f <- contract_fixture()
  bag <- bag_reserve(f$panel, B = 4, seed = 3, keep_trees = TRUE)
  l <- 1L
  rows <- baggingReserve:::period_rows_train(f$panel, l)
  fr <- baggingReserve:::period_frame(f$panel, rows, l, response = TRUE)
  s <- numeric(length(rows)); k <- integer(length(rows))
  for (b in 1:4) {
    out <- which(tabulate(bag$inbag[[b]][[l + 1L]], nbins = length(rows)) == 0L)
    s[out] <- s[out] + unname(stats::predict(bag$trees[[b]][[l + 1L]], fr[out, ]))
    k[out] <- k[out] + 1L
  }
  y <- fr$X2; cov <- k > 0
  expect_equal(oob_score(bag)$oob_error[l + 1L], mean((y[cov] - s[cov] / k[cov])^2))
})

test_that("malformed learner predictions are errors at every stage", {
  f <- contract_fixture()
  bad <- list(inf = function(n) rep(Inf, n), na = function(n) rep(NA_real_, n),
              short = function(n) rep(1, max(0L, n - 1L)), chr = function(n) rep("1", n),
              matrix = function(n) matrix(1, n, 2))
  msg <- c(inf = "non-finite prediction", na = "non-finite prediction", short = "prediction\\(s\\) for",
           chr = "numeric vector", matrix = "numeric vector")
  for (k in names(bad)) {
    lrn <- strict_learner(value = bad[[k]])
    expect_error(bag_reserve(f$panel, B = 2, seed = 1, learner = lrn), paste0("out-of-bag prediction, period 0: .*", msg[[k]]))
    expect_error(bag_reserve(f$panel, B = 2, seed = 1, learner = lrn, oob = FALSE), paste0("^completion, period 0: .*", msg[[k]]))
    expect_error(predict(reserve_tree(f$panel, learner = lrn)), paste0("^completion, period 0: .*", msg[[k]]))
    sim <- strict_learner(sim = bad[[k]])
    expect_error(predict(reserve_tree(f$panel, learner = sim), completion = "draw", R = 2, seed = 1),
                 paste0("simulated completion, period 0: .*", msg[[k]]))
    expect_error(bag_reserve(f$panel, B = 2, seed = 1, learner = sim, completion = "draw"),
                 paste0("simulated completion, period 0: .*", msg[[k]]))
  }
  # a one-column matrix and an integer vector are accepted as numeric vectors
  expect_no_error(predict(reserve_tree(f$panel, learner = strict_learner(value = function(n) matrix(1, n, 1)))))
  expect_no_error(predict(reserve_tree(f$panel, learner = strict_learner(value = function(n) rep(1L, n)))))
})

test_that("predictions record their completion, sources of variability and simulation seed", {
  f <- contract_fixture()
  tree <- reserve_tree(f$panel, seed = 1)
  p_mean <- predict(tree)
  expect_equal(p_mean$completion, "mean")
  expect_length(p_mean$variability, 0L)
  p_draw <- predict(tree, completion = "draw", R = 3, seed = 123)
  expect_equal(p_draw$completion, "draw")
  expect_equal(p_draw$variability, "simulated payments")
  expect_equal(p_draw$provenance$rng$seed, 1L)                 # the fitting generator
  expect_equal(p_draw$provenance$completion_rng$seed, 123L)    # the simulation's
  p_null <- predict(tree, completion = "draw", R = 2)
  expect_null(p_null$provenance$completion_rng$seed)
  expect_true(is.integer(p_null$provenance$completion_rng$state))
  bag <- bag_reserve(f$panel, B = 3, seed = 2)
  expect_equal(predict(bag)$variability, "bootstrap resampling")
  bag_d <- bag_reserve(f$panel, B = 3, seed = 2, completion = "draw")
  expect_equal(predict(bag_d)$variability, c("bootstrap resampling", "simulated payments"))
  expect_match(predict(bag_d)$provenance$completion_rng, "substream L \\+ 1")
  # the mean of the simulated completions keeps what was averaged
  m <- predict(tree, completion = "draw", R = 3, seed = 123, type = "mean")
  expect_equal(m$estimator, "tree (mean of 3 simulated completions)")
  expect_equal(m$variability, "simulated payments")
})

test_that("print, summary, backtest and plot describe the three kinds of columns", {
  f <- contract_fixture()
  res_tree_draw <- ibnr_reserve(predict(reserve_tree(f$panel, seed = 1), completion = "draw", R = 3, seed = 123))
  res_bag <- ibnr_reserve(predict(bag_reserve(f$panel, B = 3, seed = 2)))
  res_bag_draw <- ibnr_reserve(predict(bag_reserve(f$panel, B = 3, seed = 2, completion = "draw")))
  out <- function(x) paste(capture.output(print(x)), collapse = " ")
  # simulated completions of one tree: process variability, never resampling or a bagged mean
  s <- out(summary(res_tree_draw))
  expect_match(s, "simulated completions retained: R = 3")
  expect_match(s, "process variability of payments simulated from one fitted model")
  expect_match(s, "no estimation variability")
  expect_false(grepl("resampling", s))
  b <- out(suppressWarnings(backtest(res_tree_draw, f$toy$claims, f$toy$payments_full)))
  expect_match(b, "predicted = mean of the simulated completions")
  expect_match(b, "simulation quantiles")
  expect_false(grepl("bagged|resampling", b))
  expect_match(out(res_tree_draw), "spread of the simulated completions: process variability")
  # bagging, plugged-in means: bootstrap resampling only
  s <- out(summary(res_bag))
  expect_match(s, "replicates retained: B = 3")
  expect_match(s, "bootstrap resampling variation of the bagged trees \\(no payment noise is simulated\\)")
  b <- out(suppressWarnings(backtest(res_bag, f$toy$claims, f$toy$payments_full)))
  expect_match(b, "predicted = bagged mean;")
  expect_match(b, "replicate quantiles")
  # bagging, simulated completion: both
  s <- out(summary(res_bag_draw))
  expect_match(s, "bagged fit, simulated completion")
  expect_match(s, "one set of bootstrap trees \\(samples drawn per development year\\) and the payments of one completion")
  expect_false(grepl("no payment noise", s))
  b <- out(suppressWarnings(backtest(res_bag_draw, f$toy$claims, f$toy$payments_full)))
  expect_match(b, "predicted = bagged mean \\(simulated completion\\)")
  # the plots' legends
  legends <- list()
  local_mocked_bindings(draw_legend = function(...) { legends[[length(legends) + 1L]] <<- list(...)$legend; invisible(NULL) })
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  for (w in c("calendar", "ultimate")) plot(res_tree_draw, w)
  for (w in c("calendar", "ultimate")) plot(res_bag, w)
  for (w in c("calendar", "ultimate")) plot(res_bag_draw, w)
  plot(ibnr_reserve(predict(reserve_tree(f$panel, seed = 1))), "calendar")
  plot(collapse <- predict(reserve_tree(f$panel, seed = 1), completion = "draw", R = 3, seed = 1, type = "mean"))
  lab <- vapply(legends, function(l) l[1L], "")
  expect_equal(lab, c(rep("mean of the simulated completions", 2), rep("bagged mean", 2),
                      rep("bagged mean (simulated completion)", 2), "single-tree prediction",
                      "mean of the simulated completions"))
  expect_true(any(grepl("^simulation quantiles", unlist(legends[1:2]))))
  expect_false(any(grepl("replicate", unlist(legends[1:2]))))
  expect_true(any(grepl("^replicate quantiles", unlist(legends[3:6]))))
})

test_that("arguments that would change the estimator are not ignored", {
  f <- contract_fixture()
  bag <- bag_reserve(f$panel, B = 2, seed = 1)
  expect_error(predict(bag, completion = "draw"), "chosen when fitting: this fit was made with completion = \"mean\"")
  expect_identical(predict(bag, completion = "mean"), predict(bag))     # restating the fit's choice is fine
  expect_error(predict(bag, tpye = "mean"), "unused argument to predict\\(\\): `tpye` \\(did you mean `type`\\?\\)")
  expect_error(predict(bag, R = 10), "number of simulated completions of a bagged fit is B")
  expect_error(predict(bag, seed = 1), "bag_reserve\\(seed = \\)")
  bag_d <- bag_reserve(f$panel, B = 2, seed = 1, completion = "draw")
  expect_error(predict(bag_d, completion = "mean"), "made with completion = \"draw\"")
  tree <- reserve_tree(f$panel, seed = 1)
  expect_error(predict(tree, compeltion = "draw"), "`compeltion` \\(did you mean `completion`\\?\\)")
  expect_error(predict(tree, f$panel, "mean", "draw", 10, 1, "extra"), "an unnamed argument")
  pred <- predict(tree)
  expect_error(summary(pred, prbs = 0.5), "`prbs` \\(did you mean `probs`\\?\\)")
  expect_error(summary(tree, digits = 2), "unused argument to summary")
  expect_error(summary(f$panel, x = 1), "unused argument to summary")
  expect_error(calendar_year_payments(pred, compnent = "ibnr"), "did you mean `component`")
  cl <- chain_ladder(as_triangle(f$panel))
  expect_error(calendar_year_payments(cl, component = "reported"), "has no components")
  expect_error(calendar_year_payments(cl, "reported"), "an unnamed argument")
  expect_error(as_triangle(pred, replicte = 1), "did you mean `replicate`")
  expect_error(as_triangle(f$panel, cumulatve = TRUE), "unused argument")
  expect_error(as_triangle(cl, tpye = "cumulative"), "unused argument")
  expect_error(summary(bag, prbs = 0.5), "did you mean `probs`")
})

test_that("as_triangle() validates the replicate", {
  f <- contract_fixture()
  pred <- predict(reserve_tree(f$panel, seed = 1))
  expect_error(as_triangle(pred, replicate = 3), "column number from 1 to 1")
  expect_error(as_triangle(pred, replicate = "b9"), "one of the column names \\(b1\\)")
  expect_equal(as_triangle(pred, replicate = 1), as_triangle(pred, replicate = "b1"))
})

test_that("an undetectable number of cores keeps the request; ncores below one is an error", {
  f <- contract_fixture()
  local_mocked_bindings(detected_cores = function() NA_integer_)
  bag <- bag_reserve(f$panel, B = 2, seed = 1, ncores = 7L, backend = "serial")
  expect_equal(bag$ncores, 7L)
  local_mocked_bindings(detected_cores = function() 2L)
  expect_message(b2 <- bag_reserve(f$panel, B = 2, seed = 1, ncores = 7L, backend = "serial"), "using 2 worker processes")
  expect_equal(b2$ncores, 2L)
  expect_error(bag_reserve(f$panel, B = 2, seed = 1, ncores = 0L), "`ncores` must be at least 1")
})

test_that("the latest accident year without a reported claim is reported by claims_panel()", {
  toy <- make_toy_portfolio()
  # all claims of accident year 5 reported after t = 5 (an observed view at t = 5 lacks them)
  cl <- toy$claims; cl$RepYear[cl$AccYear == 5] <- 6
  pay <- toy$payments[toy$payments$Id %in% cl$Id[cl$AccYear <= 4], ]
  expect_warning(p <- claims_panel(cl, pay, t = 5, L = 4, features = "Type"),
                 "latest accident year with a claim reported by t = 5 is 4.*holds [0-9]+ claim\\(s\\) of these years")
  expect_equal(p$accident_years, 1:4)
  # including it gives an empty row, for which ibnr_reserve() warns that it projects no IBNR claims
  p5 <- claims_panel(cl, pay, t = 5, L = 4, features = "Type", accident_years = 1:5)
  expect_warning(ibnr_reserve(predict(reserve_tree(p5, seed = 1))), "no reported claims in accident year\\(s\\) 5")
})

test_that("backtest() rejects a truth that contradicts the prediction's claims", {
  f <- contract_fixture()
  res <- ibnr_reserve(predict(reserve_tree(f$panel, seed = 1)))
  cl <- f$toy$claims
  id <- f$panel$claims$id[f$panel$claims$acc_year == 2][1]
  cl2 <- cl; cl2$AccYear[cl2$Id == id] <- 1; cl2$RepYear[cl2$Id == id] <- 1
  expect_error(backtest(res, cl2, f$toy$payments_full), "another accident year in `claims`")
  cl3 <- cl; cl3$RepYear[cl3$Id == id] <- 9
  expect_error(backtest(res, cl3, f$toy$payments_full), "another reporting year in `claims`.*9 according to `RepYear`")
  res_old <- res; res_old$claim_rep_delays <- NULL     # saved without the reporting delays
  expect_error(backtest(res_old, cl3, f$toy$payments_full), "recorded in `claims` as reported after t = 5")
  expect_no_error(backtest(res, cl, f$toy$payments_full))
})

test_that("claims_panel(beyond_L = \"drop\") discards what lies beyond L and records it", {
  toy <- make_toy_portfolio()
  expect_error(claims_panel(toy$claims, toy$payments, t = 5, L = 2, features = "Type"),
               "beyond development year L = 2.*beyond_L = \"drop\"")
  p <- claims_panel(toy$claims, toy$payments, t = 5, L = 2, features = "Type", beyond_L = "drop")
  dev <- toy$payments$EventYear - toy$claims$AccYear[match(toy$payments$Id, toy$claims$Id)]
  expect_equal(p$diagnostics$n_payments_beyond_L_dropped, sum(dev > 2))
  expect_equal(p$diagnostics$amount_beyond_L_dropped, sum(toy$payments$Paid[dev > 2]))
  expect_equal(sum(p$payments, na.rm = TRUE), sum(toy$payments$Paid[dev <= 2]))
  expect_output(print(p), "beyond L: 0 claim\\(s\\)")
  # a claim reported with a delay above L is dropped with all its payments
  cl <- data.frame(Id = 1:4, AccYear = c(1, 1, 2, 3), RepYear = c(1, 4, 2, 3))
  pay <- data.frame(Id = c(1, 1, 1, 1, 2, 3, 3, 4), EventYear = c(1, 2, 3, 4, 4, 2, 3, 4),
                    Paid = c(10, 20, 30, 40, 50, 60, 70, 80))
  expect_error(claims_panel(cl, pay, t = 4, L = 2, accident_years = 1:3), "1 reported claim\\(s\\) have a reporting delay above L = 2")
  p2 <- claims_panel(cl, pay, t = 4, L = 2, beyond_L = "drop", accident_years = 1:3)
  expect_equal(p2$claims$id, c(1, 3, 4))
  expect_equal(p2$diagnostics$n_claims_beyond_L_dropped, 1L)
  expect_equal(p2$diagnostics$n_payments_beyond_L_dropped, 2L)      # claim 2's payment, claim 1's at l = 3
  expect_equal(p2$diagnostics$amount_beyond_L_dropped, 90)
  expect_equal(unname(p2$payments[1, ]), c(10, 20, 30))
  # without anything beyond L the option changes nothing
  q <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  q2 <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type", beyond_L = "drop")
  expect_identical(q$payments, q2$payments)
})

test_that("the printed panel counts the payments of the claims it drops as unreported", {
  claims <- data.frame(Id = c("a", "b", "c", "d", "e"), AccYear = c(1, 1, 2, 3, 3), RepYear = c(1, 2, 2, 4, 3))
  payments <- data.frame(Id = c("a", "a", "a", "b", "c", "d"), EventYear = c(1, 1, 2, 2, 3, 4),
                         Paid = c(10, 5, -2, 7, 3, 1))
  expect_output(print(claims_panel(claims, payments, t = 3, L = 2)),
                "dropped: 1 claim\\(s\\) not reported by t, with 1 payment\\(s\\) after t;\n +0 payment\\(s\\) after t of the reported claims")
})

test_that("plot() lets graphical arguments override its defaults", {
  f <- contract_fixture()
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  tree <- reserve_tree(f$panel, seed = 1)
  pred <- predict(tree)
  expect_no_error(plot(pred, ylab = "Custom", ylim = c(0, 1e5), xlab = "Year"))
  expect_no_error(plot(predict(tree, completion = "draw", R = 25, seed = 1), "ultimate", xlab = "X", col = "red"))
  expect_no_error(plot(tree, period = 0, main = "Title"))
  bag <- bag_reserve(f$panel, B = 3, seed = 1)
  expect_no_error(plot(bag, ylim = c(-1, 1), main = "OOB"))
  expect_error(plot(bag, mian = "x"), "unknown argument")
})

test_that("two_part_learner() uses the size model's simulate() for the amounts when it has one", {
  f <- contract_fixture()
  const <- function(v) list(fit = function(formula, data) v, predict = function(model, newdata) rep(model, nrow(newdata)))
  occ <- const(1)                                  # every claim pays
  size_det <- const(50)
  size_sim <- c(const(50), list(simulate = function(model, newdata) rep(70, nrow(newdata))))
  fit_det <- reserve_tree(f$panel, learner = two_part_learner(occ, size_det, min_nonzero = 1))
  fit_sim <- reserve_tree(f$panel, learner = two_part_learner(occ, size_sim, min_nonzero = 1))
  cy <- function(fit, ...) calendar_year_payments(predict(fit, ...), "reported")["total", 1]
  expect_equal(cy(fit_det, completion = "draw", R = 1, seed = 1), cy(fit_det))   # amounts are the means
  expect_gt(cy(fit_sim, completion = "draw", R = 1, seed = 1), cy(fit_sim))      # amounts drawn: 70
  expect_equal(cy(fit_sim), cy(fit_det))                                          # predict() unchanged
  expect_error(two_part_learner(occ, c(size_det, list(simulate = 1))), "size\\$simulate")
})
