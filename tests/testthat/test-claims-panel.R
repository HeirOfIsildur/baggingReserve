# The data contract: a toy portfolio exercising every rule, then the shipped data.

toy_claims <- data.frame(
  Id = c("a", "b", "c", "d", "e"),
  AccYear = c(1, 1, 2, 3, 3),
  RepYear = c(1, 2, 2, 4, 3),           # d is reported after t = 3
  Type = c("x", "y", "x", "y", "x"),
  Age = c(30, 40, 50, 60, 70),
  stringsAsFactors = FALSE)
toy_payments <- data.frame(
  Id = c("a", "a", "a", "b", "c", "d"),
  EventYear = c(1, 1, 2, 2, 3, 4),
  Paid = c(10, 5, -2, 7, 3, 1))

test_that("toy panel: valuation date, observed zeros, future NA, recoveries, no-payment claims", {
  p <- claims_panel(toy_claims, toy_payments, t = 3, L = 2, features = c("Type", "Age"))
  expect_s3_class(p, "claims_panel")
  expect_equal(p$claims$id, c("a", "b", "c", "e"))            # d not reported by t
  expect_equal(p$claims$acc_year, c(1L, 1L, 2L, 3L))
  expect_equal(p$claims$rep_delay, c(0L, 1L, 0L, 0L))
  expect_s3_class(p$claims$Type, "factor")
  expect_equal(levels(p$claims$Type), c("x", "y"))
  expect_equal(unname(p$payments[1, ]), c(15, -2, 0))        # two payments summed, recovery kept
  expect_equal(unname(p$payments[2, ]), c(0, 7, 0))          # pre-reporting observed zero
  expect_equal(unname(p$payments[3, ]), c(0, 3, NA))         # 2 + 2 > 3 is future
  expect_equal(unname(p$payments[4, ]), c(0, NA, NA))        # claim e: reported, no payment yet
  expect_equal(unname(p$observed), outer(c(1, 1, 2, 3), 0:2, function(a, l) a + l <= 3))
  expect_equal(colnames(p$observed), c("X0", "X1", "X2"))
  expect_equal(p$accident_years, 1:3)
  expect_equal(p$I, 3L)
  d <- p$diagnostics
  expect_equal(d$n_unreported_dropped, 1L)
  expect_equal(d$n_payments_on_unreported, 1L)
  expect_equal(d$n_future_payments_dropped, 0L)
  expect_equal(d$n_recoveries, 1L)
  expect_equal(d$n_claims_without_payment, 1L)
  expect_output(print(p), "4 claims reported by t = 3")
  expect_s3_class(summary(p), "summary.claims_panel")
  expect_output(print(summary(p)), "Construction")
  expect_equal(ncol(as.data.frame(p)), 3 + 2 + 3)
})

test_that("toy panel: triangles", {
  p <- claims_panel(toy_claims, toy_payments, t = 3, L = 2, features = "Type")
  paid <- as_triangle(p)
  expect_equal(dim(paid), c(3L, 3L))
  expect_equal(unname(paid[1, ]), c(15, 5, 0))
  expect_equal(unname(paid[2, ]), c(0, 3, NA))
  expect_equal(unname(paid[3, ]), c(0, NA, NA))
  cum <- as_triangle(p, cumulative = TRUE)
  expect_equal(unname(cum[1, ]), c(15, 20, 20))
  expect_equal(unname(cum[2, ]), c(0, 3, NA))
  cnt <- claim_counts(p)
  expect_equal(unname(cnt[1, ]), c(1, 1, 0))
  expect_equal(unname(cnt[2, ]), c(1, 0, NA))
  expect_equal(unname(cnt[3, ]), c(1, NA, NA))
  expect_equal(attr(cnt, "t"), 3L)
})

test_that("toy panel: rejected input", {
  cp <- function(...) claims_panel(toy_claims, toy_payments, t = 3, L = 2, ...)
  expect_error(claims_panel(toy_claims[c(1, 1), ], toy_payments, t = 3, L = 2), "duplicated")
  expect_error(cp(features = "Missing"), "lacks column")
  expect_error(cp(features = "id"), "clash")
  expect_error(claims_panel(toy_claims, toy_payments, t = 3, L = 0), "reporting delay above L")
  expect_error(claims_panel(toy_claims, rbind(toy_payments, data.frame(Id = "z", EventYear = 1, Paid = 1)),
                            t = 3, L = 2), "absent from")
  expect_error(claims_panel(toy_claims, rbind(toy_payments, data.frame(Id = "a", EventYear = 4, Paid = 1)),
                            t = 3, L = 2), "after t")
  p <- claims_panel(toy_claims, rbind(toy_payments, data.frame(Id = "a", EventYear = 4, Paid = 9)),
                    t = 3, L = 2, future_payments = "drop")
  expect_equal(p$diagnostics$n_future_payments_dropped, 1L)
  expect_equal(p$diagnostics$future_amount_dropped, 9)
  expect_error(claims_panel(toy_claims, rbind(toy_payments, data.frame(Id = "b", EventYear = 1, Paid = 1)),
                            t = 3, L = 2), "before the reporting year")
  expect_error(claims_panel(toy_claims, transform(toy_payments, Paid = c(NA, 5, -2, 7, 3, 1)),
                            t = 3, L = 2), "finite numeric")
  na_claims <- toy_claims; na_claims$Age[2] <- NA
  expect_error(claims_panel(na_claims, toy_payments, t = 3, L = 2, features = "Age"), "missing values")
  expect_silent(claims_panel(na_claims, toy_payments, t = 3, L = 2, features = "Age",
                             na_features = "surrogate"))
  expect_error(cp(accident_years = 1:2), "does not cover")
  expect_error(claims_panel(toy_claims, toy_payments, t = 2.5, L = 2), "single integer")
  expect_error(claims_panel(toy_claims, toy_payments, t = 3, L = -1), "non-negative")
})

test_that("toy panel: alternative column mapping and explicit reporting delay", {
  cl <- data.frame(claim = 1:2, ay = c(1L, 2L), dly = c(0L, 0L), kind = factor(c("u", "v")))
  pay <- data.frame(cid = c(1L, 2L), yr = c(1L, 2L), amount = c(100, 200))
  p <- claims_panel(cl, pay, t = 2, L = 1, id = c(claims = "claim", payments = "cid"),
                    acc_year = "ay", rep_delay = "dly", event_year = "yr", paid = "amount",
                    features = "kind")
  expect_equal(unname(p$payments[, "X0"]), c(100, 200))
  expect_equal(unname(p$payments[, "X1"]), c(0, NA))
  expect_equal(levels(p$claims$kind), c("u", "v"))
  expect_equal(p$diagnostics$n_claims_without_payment, 0L)
})

test_that("shipped data: the panel of the manuscript", {
  skip_if_not(exists("synthetic_claims") && exists("synthetic_payments"))
  p <- claims_panel(synthetic_claims, synthetic_payments, t = 12, L = 11,
                    features = c("Type", "Age", "AccWeekday"))
  expect_equal(nrow(p$claims), 71117L)
  expect_equal(p$I, 12L)
  expect_equal(p$diagnostics$n_unreported_dropped, 0L)
  expect_equal(p$diagnostics$n_future_payments_dropped, 0L)
  # observed cells reconcile with the paid-to-date column of the claims table
  tot <- rowSums(p$payments, na.rm = TRUE)
  expect_equal(unname(tot[match(synthetic_claims$Id, p$claims$id)]), synthetic_claims$CumPaid,
               tolerance = 1e-8)
  cnt <- claim_counts(p)
  expect_equal(sum(cnt, na.rm = TRUE), 71117)
  expect_true(all(is.na(cnt[outer(1:12, 0:11, function(a, l) a + l > 12)])))
  expect_true(all(cnt[, 10:12] == 0, na.rm = TRUE))          # yearly reporting delays run 0..8
  expect_true(all(p$claims$rep_delay %in% 0:8))
  # Type is a factor with the six generator levels; weekday keeps its Mon..Sun order
  expect_equal(levels(p$claims$Type), as.character(1:6))
  expect_equal(levels(p$claims$AccWeekday), c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
})

test_that("integer payments do not overflow and observed cells stay finite", {
  cl <- data.frame(Id = 1:2, AccYear = c(1L, 1L), RepYear = c(1L, 1L))
  pay_int <- data.frame(Id = c(1L, 1L, 2L), EventYear = c(1L, 1L, 1L),
                        Paid = c(1500000000L, 1500000000L, 7L))
  pay_dbl <- transform(pay_int, Paid = as.numeric(Paid))
  p_int <- claims_panel(cl, pay_int, t = 1, L = 0)
  p_dbl <- claims_panel(cl, pay_dbl, t = 1, L = 0)
  expect_equal(p_int$payments, p_dbl$payments)
  expect_equal(unname(p_int$payments[1, 1]), 3e9)
  expect_true(all(is.finite(p_int$payments[p_int$observed])))
  expect_equal(unname(as_triangle(p_int)[1, 1]), 3e9 + 7)
  expect_equal(dim(as_triangle(p_int, cumulative = TRUE)), c(1L, 1L))   # L = 0 keeps its shape
  cl3 <- data.frame(Id = 1:3, AccYear = 1:3, RepYear = 1:3)
  pay3 <- data.frame(Id = 1:3, EventYear = 1:3, Paid = c(1, 2, 3))
  expect_equal(dim(as_triangle(claims_panel(cl3, pay3, t = 3, L = 0), cumulative = TRUE)), c(3L, 1L))
})

test_that("malformed years, infinite features, future accident years and impossible payments are rejected", {
  bad <- toy_claims; bad$RepYear <- as.character(bad$RepYear); bad$RepYear[2] <- "2o22"
  expect_error(claims_panel(bad, toy_payments, t = 3, L = 2), "not integers, e.g. \"2o22\"")
  inf <- toy_claims; inf$Age[1] <- Inf
  expect_error(claims_panel(inf, toy_payments, t = 3, L = 2, features = "Age"), "infinite")
  expect_error(claims_panel(toy_claims, toy_payments, t = 3, L = 2, accident_years = 1:4),
               "must not exceed")
  # claim d is reported in year 4 > t; a payment on it dated by t contradicts the claims table
  bad_pay <- rbind(toy_payments, data.frame(Id = "d", EventYear = 3, Paid = 5))
  expect_error(claims_panel(toy_claims, bad_pay, t = 3, L = 2), "not reported by t")
  # character levels sort in the C locale
  p <- claims_panel(transform(toy_claims, Type = c("b", "A", "a", "B", "c")), toy_payments,
                    t = 3, L = 2, features = "Type")
  expect_equal(levels(p$claims$Type), c("A", "a", "b", "c"))
})

test_that("triangles are classed matrices that print without their attributes", {
  fx <- make_toy_portfolio()
  p <- claims_panel(fx$claims, fx$payments, t = 5, L = 4, features = "Type")
  tri <- as_triangle(p)
  expect_s3_class(tri, "reserve_triangle")
  expect_true(is.matrix(tri) && is.numeric(tri))
  expect_equal(attr(tri, "t"), p$t)
  out <- capture.output(print(tri))
  expect_false(any(grepl("attr(", out, fixed = TRUE)))
  expect_match(out[1], "Incremental payments")
  expect_match(paste(out, collapse = " "), "NA: not observable")
  out_c <- capture.output(print(claim_counts(p)))
  expect_match(out_c[1], "reported claims")
  # arithmetic keeps the class; subsetting gives a plain matrix
  expect_s3_class(tri / 2, "reserve_triangle")
  expect_false(inherits(tri[1:2, ], "reserve_triangle"))
  # prediction triangles: no NA, no observability note, component in the heading
  fit <- reserve_tree(p, seed = 1, control = rpart::rpart.control(cp = 0, xval = 2))
  pt <- as_triangle(predict(fit))
  expect_s3_class(pt, "reserve_triangle")
  expect_false(anyNA(pt))
  out_p <- capture.output(print(pt))
  expect_match(out_p[1], "reported claims")
  expect_false(any(grepl("not observable", out_p)))
})

test_that("claims after the valuation date are dropped alike, and missing reporting years warn", {
  # an accident year after t is dropped like a claim reported after t, and counted
  late_acc <- rbind(toy_claims, data.frame(Id = "f", AccYear = 4, RepYear = 4, Type = "x", Age = 20))
  pays <- rbind(toy_payments, data.frame(Id = "f", EventYear = 4, Paid = 3))
  p0 <- claims_panel(toy_claims, toy_payments, t = 3, L = 2, features = "Type", future_payments = "drop")
  p <- claims_panel(late_acc, pays, t = 3, L = 2, features = "Type", future_payments = "drop")
  expect_identical(p$payments, p0$payments)
  expect_identical(p$claims, p0$claims)
  expect_equal(p$diagnostics$n_unreported_dropped, p0$diagnostics$n_unreported_dropped + 1L)
  # its payment dated by t would contradict the claims table, as for a late-reported claim
  expect_error(claims_panel(late_acc, rbind(pays, data.frame(Id = "f", EventYear = 3, Paid = 1)),
                            t = 3, L = 2, future_payments = "drop"), "not reported by t")
  # a missing reporting year is treated as not reported, with a warning
  na_rep <- toy_claims; na_rep$RepYear[2] <- NA
  expect_warning(p_na <- claims_panel(na_rep, toy_payments[toy_payments$Id != "b", ], t = 3, L = 2,
                                      future_payments = "drop"), "1 claim\\(s\\) have no reporting year")
  expect_false("b" %in% p_na$claims$id)
  na_del <- toy_claims; na_del$RepYear <- NULL; na_del$Delay <- c(0, NA, 0, 1, 0)
  expect_warning(claims_panel(na_del, toy_payments[toy_payments$Id != "b", ], t = 3, L = 2, rep_delay = "Delay",
                              future_payments = "drop"), "no reporting delay")
})
