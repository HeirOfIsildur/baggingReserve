# Inspection of fits made with any valid learner, the tree-only arguments with a user-supplied
# learner, the pruning guard, and the back-test classification of claims set aside by
# claims_panel(beyond_L = "drop").

atomic_fixture <- function() {
  cl <- data.frame(Id = 1:60, AccYear = rep(1:3, each = 20), RepYear = rep(1:3, each = 20))
  pay <- data.frame(Id = 1:60, EventYear = cl$AccYear, Paid = 1)
  claims_panel(cl, pay, t = 3, L = 2)
}

# a valid learner whose model is an atomic value (the training mean)
mean_learner <- list(
  fit = function(formula, data) mean(data[[all.vars(formula)[1L]]]),
  predict = function(model, newdata) rep(model, nrow(newdata)))

# a valid learner whose model is a list with an unrelated component named `frame`
frame_learner <- list(
  fit = function(formula, data) list(frame = "not an rpart frame", mean = mean(data[[all.vars(formula)[1L]]])),
  predict = function(model, newdata) rep(model$mean, nrow(newdata)))

test_that("print() and summary() of a fit with a user-supplied learner do not inspect its models", {
  p <- atomic_fixture()
  for (lrn in list(mean_learner, frame_learner)) {
    f <- reserve_tree(p, learner = lrn, seed = 1)
    expect_no_error(calendar_year_payments(predict(f)))
    out <- capture.output(print(f))
    expect_true(any(grepl("user-supplied learner", out)))
    expect_true(any(grepl("`control` and `prune` not used", out, fixed = TRUE)))
    expect_false(any(grepl("control: cp", out, fixed = TRUE)))
    s <- summary(f)
    expect_identical(names(s$periods), c("period", "n_train"))
    expect_identical(s$periods$n_train, f$n_train)
    expect_identical(s$learner, "user-supplied")
    expect_output(print(s), "user-supplied")
    b <- bag_reserve(p, B = 2, seed = 1, learner = lrn)
    expect_output(print(b), "learner: user-supplied")
    expect_no_error(capture.output(print(summary(b))))
    expect_no_error(capture.output(print(predict(b))))
  }
  # the default learner keeps its tree diagnostics
  d <- summary(reserve_tree(p, seed = 1))
  expect_identical(names(d$periods), c("period", "n_train", "cp", "leaves", "depth"))
  expect_output(print(reserve_tree(p, seed = 1)), "control: cp = 0, xval = 10")
})

test_that("bag_reserve() ignores prune and control with a user-supplied learner, as reserve_tree() does", {
  p <- atomic_fixture()
  expect_warning(b <- bag_reserve(p, B = 2, seed = 1, learner = mean_learner, prune = "min_xerror"),
                 "`prune` applies to the default rpart trees only")
  expect_identical(b$prune, "none")
  expect_warning(f <- reserve_tree(p, seed = 1, learner = mean_learner, prune = "min_xerror"),
                 "`prune` applies to the default rpart trees only")
  expect_identical(f$prune, "none")
  expect_warning(bag_reserve(p, B = 2, seed = 1, learner = mean_learner, prune = "min_xerror",
                             control = rpart::rpart.control(xval = 1)),
                 "`control` and `prune` apply to the default rpart trees only")
})

test_that("prune = \"min_xerror\" needs at least two cross-validation folds", {
  p <- atomic_fixture()
  for (xv in c(0, 1))
    expect_error(reserve_tree(p, seed = 1, control = rpart::rpart.control(cp = 0, xval = xv)),
                 "at least two folds")
  expect_error(bag_reserve(p, B = 2, seed = 1, prune = "min_xerror",
                           control = rpart::rpart.control(cp = 0, xval = 1)), "at least two folds")
  expect_no_error(reserve_tree(p, seed = 1, control = rpart::rpart.control(cp = 0, xval = 2)))
  expect_no_error(reserve_tree(p, seed = 1, prune = "none", control = rpart::rpart.control(cp = 0, xval = 1)))
  expect_no_error(bag_reserve(p, B = 2, seed = 1, prune = "min_xerror",
                              control = rpart::rpart.control(cp = 0, xval = 2)))
})

test_that("payments of a set-aside claim outside the panel's accident years are part of the excluded tail", {
  # the claim of accident year 1 (reported in year 4, delay 3 > L = 2) is set aside; the default
  # accident-year range, determined after setting it aside, is 3-5, so year 1 is not in it
  cl <- data.frame(Id = 1:5, AccYear = c(1, 3, 3, 4, 5), RepYear = c(4, 3, 3, 4, 5))
  pay <- data.frame(Id = c(2L, 3L, 4L, 5L, 1L, 5L, 5L), EventYear = c(3, 4, 4, 5, 6, 6, 7),
                    Paid = c(10, 20, 30, 40, 100, 50, 60))
  panel <- claims_panel(cl, pay[pay$EventYear <= 5, ], t = 5, L = 2, beyond_L = "drop")
  expect_identical(panel$accident_years, 3:5)
  expect_identical(panel$beyond_L_ids, 1L)
  pred <- predict(reserve_tree(panel, seed = 1, prune = "none", control = rpart::rpart.control(cp = 0, xval = 0)))
  bt <- backtest(pred, cl, pay)
  expect_equal(bt$tail$n, 1L)
  expect_equal(bt$tail$amount, 100)
  expect_equal(bt$not_scored$n_claims_beyond_L, 1L)
  expect_equal(bt$not_scored$n_payments_beyond_L, 1L)
  expect_equal(bt$not_scored$amount_beyond_L, 100)
  expect_equal(bt$not_scored$n_claims_outside, 0L)
  expect_equal(bt$not_scored$n_payments_outside, 0L)
  expect_equal(bt$not_scored$amount_outside, 0)
  expect_output(print(bt), "their 1 payment\\(s\\) after t \\(amount 100\\) are part of the excluded tail")
  # the scored tables do not depend on that payment
  bt0 <- backtest(pred, cl, pay[pay$Id != 1L | pay$EventYear <= 5, ])
  expect_identical(bt$tables, bt0$tables)
})
