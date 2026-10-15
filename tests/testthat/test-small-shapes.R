# Small and degenerate shapes: no future cell, one future cell, one accident year, failures.

test_that("a fully developed panel has no future cell and every method copes", {
  cl <- data.frame(Id = 1:6, AccYear = rep(1:3, each = 2), RepYear = rep(1:3, each = 2),
                   Type = rep(c("a", "b"), 3), stringsAsFactors = FALSE)
  pay <- data.frame(Id = rep(1:6, each = 3), EventYear = rep(1:3, each = 2)[rep(1:6, each = 3)] + rep(0:2, 6),
                    Paid = rep(c(10, 5, 2), 6))
  p <- claims_panel(cl, pay, t = 10, L = 2, features = "Type")
  expect_true(all(p$observed))
  expect_equal(nrow(baggingReserve:::future_cells(p)), 0L)
  fit <- reserve_tree(p, seed = 1)
  pred <- predict(fit)
  expect_equal(dim(pred$reported), c(0L, 1L))
  cy <- calendar_year_payments(pred)
  expect_equal(rownames(cy), "total")
  expect_equal(unname(cy["total", 1]), 0)
  s <- summary(pred)
  expect_equal(rownames(s$tables$reported), "total")
  res <- ibnr_reserve(pred, claim_counts(p))
  expect_equal(dim(res$ibnr), c(0L, 1L))
  bag <- bag_reserve(p, B = 2, seed = 1, keep_trees = TRUE)
  expect_equal(dim(predict(bag)$reported), c(0L, 2L))
  expect_equal(dim(predict(bag, newdata = p)$reported), c(0L, 2L))
  bt <- backtest(res, cl, pay)
  expect_equal(bt$tables$total$truth, 0)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_error(plot(res, "next_year"), "no future cell")
  expect_error(plot(res, "calendar"), "no future cell")
})

test_that("one future cell", {
  cl <- data.frame(Id = 1:40, AccYear = rep(1:2, 20), RepYear = rep(1:2, 20),
                   Type = rep(c("a", "b"), each = 20), stringsAsFactors = FALSE)
  pay <- rbind(data.frame(Id = 1:40, EventYear = rep(1:2, 20), Paid = ifelse(cl$Type == "a", 100, 300)),
               data.frame(Id = 1:40, EventYear = rep(1:2, 20) + 1, Paid = ifelse(cl$Type == "a", 50, 150)))
  p <- claims_panel(cl, pay[pay$EventYear <= 2, ], t = 2, L = 1, features = "Type")
  expect_equal(nrow(baggingReserve:::future_cells(p)), 1L)
  fit <- reserve_tree(p, seed = 1)
  pred <- predict(fit)
  expect_equal(dim(pred$reported), c(1L, 1L))
  cy <- calendar_year_payments(pred)
  expect_equal(rownames(cy), c("3", "total"))
  expect_equal(unname(cy["3", 1]), unname(cy["total", 1]))
  res <- ibnr_reserve(pred, claim_counts(p))
  expect_equal(dim(res$ibnr), c(1L, 1L))
  bag <- bag_reserve(p, B = 3, seed = 2)
  expect_equal(dim(predict(bag)$reported), c(1L, 3L))
  s <- summary(predict(bag))
  expect_equal(rownames(s$tables$reported), c("3", "total"))
  bt <- backtest(ibnr_reserve(predict(bag), claim_counts(p)), cl, pay)
  expect_equal(nrow(bt$tables$total), 2L)
  expect_equal(bt$tables$total$truth[1], 10 * 50 + 10 * 150)   # the 20 claims of accident year 2
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(res, "calendar"))
  expect_invisible(plot(predict(bag), "ultimate"))
})

test_that("a single accident year cannot train and says so", {
  cl <- data.frame(Id = 1:5, AccYear = 5L, RepYear = 5L, Type = "a", stringsAsFactors = FALSE)
  pay <- data.frame(Id = 1:5, EventYear = 5L, Paid = 1:5)
  p <- claims_panel(cl, pay, t = 5, L = 2, features = "Type")
  expect_equal(p$I, 1L)
  expect_error(reserve_tree(p, seed = 1), "training claim")
  expect_error(bag_reserve(p, B = 2, seed = 1), "fewer than two training claims")
  tri <- as_triangle(p, cumulative = TRUE)
  expect_equal(dim(tri), c(1L, 3L))
})

test_that("a failure inside bag_reserve still restores the caller's RNG state", {
  cl <- data.frame(Id = 1:5, AccYear = c(1L, 2L, 2L, 3L, 3L), RepYear = c(1L, 2L, 2L, 3L, 3L),
                   Type = "a", stringsAsFactors = FALSE)
  pay <- data.frame(Id = 1:5, EventYear = c(1L, 2L, 2L, 3L, 3L), Paid = 1:5)
  p <- claims_panel(cl, pay, t = 3, L = 2, features = "Type")   # period 1 trains on AY 1 alone: 1 claim
  set.seed(7); before <- .Random.seed; kind <- RNGkind()
  expect_error(bag_reserve(p, B = 2, seed = 1), "fewer than two")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), kind)
})
