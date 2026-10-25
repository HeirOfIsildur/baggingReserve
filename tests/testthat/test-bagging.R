# Bagging: own bootstrap, streamed replicates, OOB, RNG streams, parallel backend.

toy <- make_toy_portfolio()
toy_panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")

test_that("bag_reserve returns B completed replicates and OOB diagnostics", {
  bag <- bag_reserve(toy_panel, B = 3, seed = 11)
  expect_s3_class(bag, "bag_reserve")
  expect_equal(bag$B, 3L)
  expect_null(bag$trees)
  pred <- predict(bag)
  expect_s3_class(pred, "reserve_prediction")
  expect_equal(dim(pred$reported), c(nrow(pred$cells), 3L))
  expect_false(anyNA(pred$reported))
  expect_equal(colnames(pred$reported), c("b1", "b2", "b3"))
  # observed cells are untouched in every replicate's triangle
  for (b in 1:3) {
    tri <- as_triangle(pred, replicate = b)
    expect_equal(tri[!is.na(pred$observed)], pred$observed[!is.na(pred$observed)])
  }
  oob <- oob_score(bag)
  expect_s3_class(oob, "data.frame")
  expect_equal(oob$period, 0:3)
  expect_equal(oob$n_train, bag$n_train)
  expect_true(all(oob$coverage > 0 & oob$coverage <= 1))
  expect_true(all(oob$oob_score <= 1, na.rm = TRUE))
  expect_true(all(oob$n_scored <= oob$n_train))
  expect_output(print(bag), "B = 3 replicates")
  mean_pred <- predict(bag, type = "mean")
  expect_equal(mean_pred$B, 1L)
  expect_equal(unname(mean_pred$reported[, 1]), unname(rowMeans(pred$reported)))
  expect_error(oob_score(reserve_tree(toy_panel, seed = 1)), "only for bag_reserve")
  expect_error(oob_score(bag_reserve(toy_panel, B = 2, seed = 1, oob = FALSE)), "oob = FALSE")
})

test_that("the same seed reproduces the fit; the caller's RNG state is untouched", {
  set.seed(5); before <- .Random.seed; kind <- RNGkind()
  b1 <- bag_reserve(toy_panel, B = 3, seed = 11)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
  b2 <- bag_reserve(toy_panel, B = 3, seed = 11)
  expect_equal(predict(b1)$reported, predict(b2)$reported)
  expect_equal(oob_score(b1), oob_score(b2))
  b3 <- bag_reserve(toy_panel, B = 3, seed = 12)
  expect_false(isTRUE(all.equal(predict(b1)$reported, predict(b3)$reported)))
  # a NULL seed is drawn and recorded so the run can be repeated
  b4 <- bag_reserve(toy_panel, B = 2)
  expect_true(is.integer(b4$rng$seed))
  b5 <- bag_reserve(toy_panel, B = 2, seed = b4$rng$seed)
  expect_equal(predict(b4)$reported, predict(b5)$reported)
})

test_that("serial, two-worker and different batch sizes give identical results", {
  skip_on_cran()
  serial <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 1)
  par2 <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 2)
  batched <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 1, batch_size = 1)
  expect_equal(predict(serial)$reported, predict(par2)$reported)
  expect_equal(oob_score(serial), oob_score(par2))
  expect_equal(predict(serial)$reported, predict(batched)$reported)
  expect_equal(oob_score(serial), oob_score(batched))
})

test_that("replicate b uses the b-th bootstrap tree of every period (hand recomputation)", {
  bag <- bag_reserve(toy_panel, B = 3, seed = 31, keep_trees = TRUE)
  expect_length(bag$trees, 3)
  expect_length(bag$trees[[2]], 4)
  # replicate 2 by hand from the stored trees
  P <- baggingReserve:::complete_payments(toy_panel, bag$trees[[2]], baggingReserve:::predict_period_rpart)
  cells <- baggingReserve:::future_cells(toy_panel)
  expect_equal(unname(baggingReserve:::aggregate_triangle(toy_panel, P)[cells$index]),
               unname(predict(bag)$reported[, 2]))
  # the bootstrap of replicate 2, period 0 is a with-replacement sample of the training rows
  idx <- bag$inbag[[2]][[1]]
  expect_length(idx, bag$n_train[1])
  expect_true(any(duplicated(idx)))
  # OOB by hand for period 0
  rows <- baggingReserve:::period_rows_train(toy_panel, 0)
  fr <- baggingReserve:::period_frame(toy_panel, rows, 0)
  y <- fr$X1; n <- length(y)
  s <- numeric(n); k <- integer(n)
  for (b in 1:3) {
    out <- which(tabulate(bag$inbag[[b]][[1]], nbins = n) == 0)
    s[out] <- s[out] + unname(predict(bag$trees[[b]][[1]], fr[out, ]))
    k[out] <- k[out] + 1L
  }
  cov <- k > 0
  sse <- sum((y[cov] - s[cov] / k[cov])^2)
  o <- oob_score(bag)
  expect_equal(o$n_scored[1], sum(cov))
  expect_equal(o$oob_error[1], sse / sum(cov))
  expect_equal(o$oob_score[1], 1 - sse / sum((y - mean(y))^2))
  expect_equal(o$mean_votes[1], mean(k[cov]))
  expect_equal(unname(predict(bag, type = "mean")$reported[, 1]), unname(rowMeans(predict(bag)$reported)))
})

test_that("newdata needs the trees and honours the contract", {
  bag <- bag_reserve(toy_panel, B = 2, seed = 41)
  expect_error(predict(bag, newdata = toy_panel), "keep_trees = TRUE")
  bagk <- bag_reserve(toy_panel, B = 2, seed = 41, keep_trees = TRUE)
  expect_equal(predict(bagk, newdata = toy_panel)$reported, predict(bagk)$reported)
  no_feat <- claims_panel(toy$claims, toy$payments, t = 5, L = 4)
  expect_error(predict(bagk, newdata = no_feat), "same features")
})

test_that("B = 1 still draws a bootstrap sample; pruning inside bagging needs xval", {
  b1 <- bag_reserve(toy_panel, B = 1, seed = 51, keep_trees = TRUE)
  expect_true(any(duplicated(b1$inbag[[1]][[1]])))
  expect_error(bag_reserve(toy_panel, B = 2, prune = "min_xerror"), "needs cross-validation")
  bp <- bag_reserve(toy_panel, B = 2, seed = 52, prune = "min_xerror",
                    control = rpart::rpart.control(cp = 0, xval = 5))
  expect_equal(bp$prune, "min_xerror")
  expect_false(anyNA(predict(bp)$reported))
})

test_that("IBNR attaches to every replicate", {
  bag <- bag_reserve(toy_panel, B = 3, seed = 61)
  res <- ibnr_reserve(predict(bag), claim_counts(toy_panel))
  expect_equal(dim(res$ibnr), dim(res$reported))
  expect_equal(nrow(res$ibnr_details$mu), 3L)
  tot <- calendar_year_payments(res, "total")
  expect_equal(unname(tot["total", ]), unname(colSums(res$reported) + colSums(res$ibnr)))
})

test_that("RNG kind and seed are restored even when no seed existed before", {
  if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
  kind <- RNGkind()
  b <- bag_reserve(toy_panel, B = 2, seed = 3)
  expect_false(exists(".Random.seed", envir = globalenv()))
  expect_identical(RNGkind(), kind)
  set.seed(1); u1 <- runif(1)
  set.seed(1); f <- reserve_tree(toy_panel, seed = 4); u2 <- runif(1)
  expect_identical(u1, u2)                                 # subsequent draws are unaffected
})

test_that("batch_size is validated and fit summaries print", {
  expect_error(bag_reserve(toy_panel, B = 2, seed = 1, batch_size = 0), "at least 1")
  expect_error(bag_reserve(toy_panel, B = 2, seed = 1, batch_size = 1.5), "single integer")
  bag <- bag_reserve(toy_panel, B = 3, seed = 5)
  s <- summary(bag)
  expect_s3_class(s, "summary.bag_reserve")
  expect_output(print(s), "out-of-bag diagnostics")
  st <- summary(reserve_tree(toy_panel, seed = 1))
  expect_s3_class(st, "summary.reserve_tree")
  expect_equal(nrow(st$periods), 4L)
  expect_output(print(st), "pruning: min_xerror")
  # without OOB nothing is accumulated
  expect_null(bag_reserve(toy_panel, B = 2, seed = 1, oob = FALSE)$oob)
})

test_that("backends agree: serial, forked and socket workers", {
  skip_on_cran()
  serial <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 1, backend = "serial")
  expect_equal(serial$backend, "serial")
  if (.Platform$OS.type == "unix") {
    forked <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 2, backend = "fork")
    expect_equal(predict(forked)$reported, predict(serial)$reported)
  }
  skip_if_not(requireNamespace("baggingReserve", quietly = TRUE) &&
                "baggingReserve" %in% rownames(utils::installed.packages()),
              "socket workers need the installed package")
  psock <- bag_reserve(toy_panel, B = 4, seed = 21, ncores = 2, backend = "psock")
  expect_equal(predict(psock)$reported, predict(serial)$reported)
  expect_equal(oob_score(psock)$n_scored, oob_score(serial)$n_scored)
})

test_that("bag_reserve(completion = 'draw') simulates per replicate, reproducibly and batch-invariantly", {
  toy <- make_toy_portfolio()
  panel <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  m <- bag_reserve(panel, B = 6, seed = 11)
  d1 <- bag_reserve(panel, B = 6, seed = 11, completion = "draw")
  d2 <- bag_reserve(panel, B = 6, seed = 11, completion = "draw", batch_size = 2)
  expect_identical(d1$prediction$reported, d2$prediction$reported)
  expect_identical(d1$oob, m$oob)                     # same trees as completion = "mean"
  expect_match(d1$prediction$estimator, "simulated completion")
  expect_false(identical(d1$prediction$reported, m$prediction$reported))
  # the first future step uses observed payments only: its mean is close to the plug-in
  expect_equal(mean(calendar_year_payments(d1$prediction, "reported")[1, ]),
               mean(calendar_year_payments(m$prediction, "reported")[1, ]), tolerance = 0.15)
  expect_error(predict(bag_reserve(panel, B = 2, seed = 1, completion = "draw", keep_trees = TRUE),
                       newdata = panel), "completion = \"mean\"")
  lm_learner <- list(fit = function(formula, data) stats::lm(formula, data = data),
                     predict = function(model, newdata) stats::predict(model, newdata))
  expect_error(bag_reserve(panel, B = 2, completion = "draw", learner = lm_learner), "simulate")
})

test_that("socket workers run a learner that refers to global variables, helpers and packages", {
  skip_on_cran()
  skip_if_not(requireNamespace("baggingReserve", quietly = TRUE) &&
                "baggingReserve" %in% rownames(utils::installed.packages()),
              "socket workers need the installed package")
  # a learner written at top level: its functions' environment is the global environment, which
  # socket workers do not share; the minimum leaf size and a helper function live there
  ge <- globalenv()
  assign("br_test_minbucket", 30, envir = ge)
  assign("br_test_fit", function(formula, data)
    rpart::rpart(formula, data = data, method = "anova",
                 control = rpart::rpart.control(cp = 0, xval = 0, minbucket = br_test_minbucket)), envir = ge)
  environment(ge$br_test_fit) <- ge
  on.exit(rm(list = c("br_test_minbucket", "br_test_fit"), envir = ge), add = TRUE)
  lrn <- list(fit = function(formula, data) br_test_fit(formula, data),
              predict = function(model, newdata) unname(stats::predict(model, newdata)))
  environment(lrn$fit) <- ge; environment(lrn$predict) <- ge
  g <- learner_globals(lrn)
  expect_setequal(names(g$variables), c("br_test_fit", "br_test_minbucket"))
  serial <- bag_reserve(toy_panel, B = 4, seed = 5, learner = lrn, backend = "serial")
  psock <- bag_reserve(toy_panel, B = 4, seed = 5, learner = lrn, ncores = 2, backend = "psock")
  expect_identical(predict(psock)$reported, predict(serial)$reported)
  expect_equal(oob_score(psock)$oob_score, oob_score(serial)$oob_score)
  # an object reached by name only cannot be detected: the failure says what to do
  lrn_get <- list(fit = function(formula, data) get("br_test_fit")(formula, data), predict = lrn$predict)
  environment(lrn_get$fit) <- ge
  expect_identical(predict(bag_reserve(toy_panel, B = 2, seed = 5, learner = lrn_get, backend = "serial"))$reported,
                   predict(bag_reserve(toy_panel, B = 2, seed = 5, learner = lrn, backend = "serial"))$reported)
  expect_error(bag_reserve(toy_panel, B = 2, seed = 5, learner = lrn_get, ncores = 2, backend = "psock"),
               "separate R sessions")
})
