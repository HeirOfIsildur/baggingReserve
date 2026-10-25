# Single tree per period and the recursive completion.

toy <- make_toy_portfolio()
toy_panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")

test_that("period selectors follow the two eligibility conditions", {
  p <- toy_panel
  for (l in 0:3) {
    tr <- baggingReserve:::period_rows_train(p, l)
    pr <- baggingReserve:::period_rows_predict(p, l)
    expect_true(all(p$claims$rep_delay[tr] <= l))
    expect_true(all(p$claims$acc_year[tr] + l + 1 <= p$t))
    expect_true(all(p$claims$acc_year[pr] + l + 1 > p$t))
    expect_length(intersect(tr, pr), 0)
    fr <- baggingReserve:::period_frame(p, tr, l)
    expect_equal(names(fr), c("Type", "rep_delay", paste0("X", 0:(l + 1))))
    expect_false(anyNA(fr))
  }
})

test_that("reserve_tree fits one tree per period and predict completes the panel", {
  fit <- reserve_tree(toy_panel, seed = 1)
  expect_s3_class(fit, "reserve_tree")
  expect_length(fit$fits, 4)
  expect_true(all(vapply(fit$fits, inherits, TRUE, "rpart")))
  expect_output(print(fit), "4 trees")
  pred <- predict(fit)
  expect_s3_class(pred, "reserve_prediction")
  expect_equal(pred$B, 1L)
  # observed cells untouched, future cells filled, no NA left
  P <- pred$claim_level
  expect_equal(P[toy_panel$observed], toy_panel$payments[toy_panel$observed])
  expect_false(anyNA(P))
  expect_equal(nrow(pred$cells), sum(outer(1:5, 0:4, function(a, l) a + l > 5)))
  tri <- as_triangle(pred, replicate = 1)
  expect_equal(tri[!is.na(pred$observed)], pred$observed[!is.na(pred$observed)])
  cy <- calendar_year_payments(pred)
  expect_equal(rownames(cy), c("6", "7", "8", "9", "total"))
  expect_equal(unname(cy["total", 1]), sum(cy[1:4, 1]))
  expect_equal(unname(cy["total", 1]), sum(pred$reported[, 1]))
  expect_output(print(pred), "one prediction column")
})

test_that("the recursion feeds earlier predictions into later periods", {
  fit <- reserve_tree(toy_panel, seed = 1)
  pred <- predict(fit)
  # redo period 1 by hand with the completed X1 and check it equals the engine's X2
  P <- pred$claim_level
  rows <- baggingReserve:::period_rows_predict(toy_panel, 1)
  nd <- baggingReserve:::period_frame(toy_panel, rows, 1, payments = P, response = FALSE)
  expect_equal(unname(P[rows, "X2"]), unname(predict(fit$fits[[2]], nd)))
  # and that it differs from what the observed panel alone (future X1 set to 0) would give
  nd0 <- nd
  nd0$X1 <- ifelse(toy_panel$observed[rows, "X1"], nd0$X1, 0)
  if (any(!toy_panel$observed[rows, "X1"]))
    expect_false(isTRUE(all.equal(unname(P[rows, "X2"]), unname(predict(fit$fits[[2]], nd0)))))
})

test_that("seeding gives repeatable fits and restores the caller's RNG state", {
  set.seed(99); before <- .Random.seed
  f1 <- reserve_tree(toy_panel, seed = 3)
  expect_identical(.Random.seed, before)
  f2 <- reserve_tree(toy_panel, seed = 3)
  expect_equal(f1$cp_chosen, f2$cp_chosen)
  expect_equal(predict(f1)$reported, predict(f2)$reported)
  expect_equal(f1$rng$seed, 3)
})

test_that("prune = 'none' keeps the full tree; min_xerror needs xval", {
  f <- reserve_tree(toy_panel, prune = "none", seed = 1,
                    control = rpart::rpart.control(cp = 0, xval = 0))
  expect_true(all(is.na(f$cp_chosen)))
  expect_error(reserve_tree(toy_panel, control = rpart::rpart.control(cp = 0, xval = 0)),
               "needs cross-validation")
})

test_that("predict on newdata checks the contract", {
  fit <- reserve_tree(toy_panel, seed = 1)
  other <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  expect_equal(predict(fit, newdata = other)$reported, predict(fit)$reported)
  bad_L <- claims_panel(toy$claims, toy$payments, t = 5, L = 5, features = "Type")
  expect_error(predict(fit, newdata = bad_L), "L = 5")
  no_feat <- claims_panel(toy$claims, toy$payments, t = 5, L = 4)
  expect_error(predict(fit, newdata = no_feat), "same features")
  expect_error(predict(fit, newdata = toy$claims), "claims_panel")
})

test_that("the engine recurses on earlier predictions and averaging trajectories differs from averaging predictors", {
  # hand-built period models: a linear rule X_{l+1} = a * X_l applied by the engine
  cl <- data.frame(Id = 1:2, AccYear = c(1L, 3L), RepYear = c(1L, 3L))
  pay <- data.frame(Id = c(1L, 1L, 1L, 2L), EventYear = c(1L, 2L, 3L, 3L), Paid = c(1, 2, 4, 1))
  p <- claims_panel(cl, pay, t = 3, L = 2)
  rule <- function(a) list(a = a)
  pf <- function(model, nd) model$a * nd[[ncol(nd)]]   # the last column of the frame is X_l
  # claim 2 (AY 3) has X0 = 1 observed, X1 and X2 in the future
  eng <- baggingReserve:::complete_payments
  PA <- eng(p, list(rule(2), rule(2)), pf)           # doubles each year: 1, 2, 4
  PB <- eng(p, list(rule(0.5), rule(0.5)), pf)       # halves each year: 1, 0.5, 0.25
  expect_equal(unname(PA[2, ]), c(1, 2, 4))
  expect_equal(unname(PB[2, ]), c(1, 0.5, 0.25))
  expect_equal(unname(PA[1, ]), c(1, 2, 4))          # observed claim untouched
  mean_traj <- (PA[2, "X2"] + PB[2, "X2"]) / 2       # 2.125
  Pavg <- eng(p, list(rule(1.25), rule(1.25)), pf)   # the averaged predictor recursed
  expect_equal(unname(mean_traj), 2.125)
  expect_equal(unname(Pavg[2, "X2"]), 1.5625)
  expect_false(isTRUE(all.equal(unname(mean_traj), unname(Pavg[2, "X2"]))))
})

test_that("newdata with a subset of the fit's factor levels is re-levelled; unseen levels are refused", {
  fit <- reserve_tree(toy_panel, seed = 1)
  only_a <- toy$claims[toy$claims$Type == "A", ]
  pay_a <- toy$payments[toy$payments$Id %in% only_a$Id, ]
  sub_panel <- claims_panel(only_a, pay_a, t = 5, L = 4, features = "Type")
  expect_equal(levels(sub_panel$claims$Type), "A")
  pred_sub <- predict(fit, newdata = sub_panel)
  expect_s3_class(pred_sub, "reserve_prediction")
  # the same claims predicted inside the full panel give the same completed cells
  full_pred <- predict(fit)
  rows <- match(sub_panel$claims$id, toy_panel$claims$id)
  expect_equal(unname(sub_panel$payments[sub_panel$observed]), unname(toy_panel$payments[rows, ][sub_panel$observed]))
  P_full <- full_pred$claim_level[rows, ]
  expect_equal(unname(pred_sub$claim_level), unname(P_full))
  # an unseen level is an error, a numeric feature where a factor was fitted too
  new_lv <- toy$claims; new_lv$Type[1] <- "Z"
  bad <- claims_panel(new_lv, toy$payments, t = 5, L = 4, features = "Type")
  expect_error(predict(fit, newdata = bad), "unknown to the fit")
  num <- toy$claims; num$Type <- as.numeric(factor(num$Type))
  bad2 <- claims_panel(num, toy$payments, t = 5, L = 4, features = "Type")
  expect_error(predict(fit, newdata = bad2), "must be a factor")
})

test_that("surrogate handling: fits and predicts with missing feature values", {
  na_claims <- toy$claims; na_claims$Type[c(3, 7, 11)] <- NA
  p <- claims_panel(na_claims, toy$payments, t = 5, L = 4, features = "Type",
                    na_features = "surrogate")
  fit <- reserve_tree(p, seed = 1)
  pred <- predict(fit)
  expect_false(anyNA(pred$claim_level))
  bag <- bag_reserve(p, B = 2, seed = 1)
  expect_false(anyNA(predict(bag)$reported))
})

test_that("simulated completion: leaf draws average near the plug-in, are reproducible, and need pools", {
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  fit <- reserve_tree(panel, seed = 1)
  expect_length(fit$leaf_pools, 4L)
  plug <- calendar_year_payments(predict(fit), "reported")
  d1 <- predict(fit, completion = "draw", R = 30, seed = 9)
  d2 <- predict(fit, completion = "draw", R = 30, seed = 9)
  expect_identical(d1$reported, d2$reported)
  expect_match(d1$estimator, "simulated completions")
  # the first future step uses observed payments only, so its expectation equals the plug-in
  drw <- calendar_year_payments(d1, "reported")
  expect_equal(ncol(drw), 30L)                       # one column per simulated completion
  expect_equal(unname(mean(drw[1, ])), unname(plug[1, 1]), tolerance = 0.1)
  dm <- predict(fit, completion = "draw", R = 30, seed = 9, type = "mean")
  expect_equal(unname(calendar_year_payments(dm, "reported")[, 1]), unname(rowMeans(drw)))
  # observed cells are never changed
  obs <- !is.na(panel$payments)
  expect_equal(d1$claim_level[obs], fit$panel$payments[obs])
  fit_old <- fit; fit_old$leaf_pools <- NULL
  expect_error(predict(fit_old, completion = "draw"), "leaf responses")
  expect_error(predict(fit, completion = "draw", R = 0), "at least 1")
})

test_that("two_part_learner fits occurrence and size, predicts p * s and simulates", {
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  logistic <- list(fit = function(formula, data) suppressWarnings(stats::glm(formula, data = data, family = stats::binomial())),
                   predict = function(model, newdata) stats::predict(model, newdata, type = "response"))
  linear <- list(fit = function(formula, data) stats::lm(formula, data = data),
                 predict = function(model, newdata) stats::predict(model, newdata))
  tp <- two_part_learner(logistic, linear)
  expect_true(all(c("fit", "predict", "simulate") %in% names(tp)))
  fit <- suppressWarnings(reserve_tree(panel, seed = 1, learner = tp))
  m <- suppressWarnings(calendar_year_payments(predict(fit), "reported"))
  s <- suppressWarnings(calendar_year_payments(predict(fit, completion = "draw", R = 5, seed = 2), "reported"))
  expect_true(all(is.finite(m)) && all(is.finite(s)))
  expect_error(predict(reserve_tree(panel, seed = 1, learner = linear), completion = "draw"), "simulate")
  expect_error(two_part_learner(list(fit = 1), linear), "occurrence")
})

test_that("draws at an internal node use the training responses below it", {
  pool <- list(`4` = 1, `10` = 2, `11` = 3, `3` = 4, `100000` = 5)
  expect_equal(baggingReserve:::subtree_pool(pool, 5), c(2, 3))
  expect_equal(baggingReserve:::subtree_pool(pool, 2), c(1, 2, 3))
  expect_error(baggingReserve:::subtree_pool(pool, 7), "no training responses")
})

test_that("reserve_tree(seed = ) does not depend on the session's RNG kind and restores it", {
  p <- claims_panel(make_toy_portfolio()$claims, make_toy_portfolio()$payments, t = 5, L = 4, features = "Type")
  ref <- reserve_tree(p, seed = 1)
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Box-Muller")
  set.seed(5); state <- .Random.seed
  fit <- reserve_tree(p, seed = 1)
  expect_identical(fit$cp_chosen, ref$cp_chosen)
  expect_identical(predict(fit)$reported, predict(ref)$reported)
  # the three kinds the package pins; newer R versions also report a binomial kind as a fourth element
  expect_identical(fit$rng$kind[1:3], c("Mersenne-Twister", "Inversion", "Rejection"))
  expect_identical(RNGkind()[1:3], c("L'Ecuyer-CMRG", "Box-Muller", old[3L]))
  expect_identical(.Random.seed, state)
  # simulated completions too
  d1 <- predict(fit, completion = "draw", R = 3, seed = 9)$reported
  do.call(RNGkind, as.list(old))
  expect_identical(predict(ref, completion = "draw", R = 3, seed = 9)$reported, d1)
  # seed = NULL uses the session's generator as it is
  RNGkind("L'Ecuyer-CMRG"); set.seed(2)
  a <- reserve_tree(p)
  expect_identical(a$rng$kind[1L], "L'Ecuyer-CMRG")
  expect_false(identical(.Random.seed, a$rng$state))
})
