# Chain ladder (standard and the 2023 incremental-ratio variant) and the IBNR rule.

tri <- matrix(c(10, 5, 2,
                20, 8, NA,
                30, NA, NA), 3, byrow = TRUE, dimnames = list(1:3, 0:2))

test_that("standard chain ladder on a hand-calculated triangle", {
  cl <- chain_ladder(tri)
  expect_s3_class(cl, "chain_ladder")
  expect_equal(unname(cl$factors), c(43 / 30, 17 / 15))
  expect_equal(unname(cl$rows_used), c(2L, 1L))
  expect_equal(unname(cl$cumulative[2, 3]), 28 * 17 / 15)
  expect_equal(unname(cl$cumulative[3, ]), c(30, 43, 43 * 17 / 15))
  expect_equal(unname(cl$incremental[3, ]), c(30, 13, 43 * 17 / 15 - 43))
  expect_equal(unname(cl$latest), c(17, 28, 30))
  expect_equal(unname(cl$outstanding), c(0, 28 * 17 / 15 - 28, 43 * 17 / 15 - 30))
  expect_equal(cl$outstanding_total, sum(cl$outstanding))
  # observed cells are untouched and cumulative input gives the same answer
  expect_equal(cl$incremental[!is.na(tri)], tri[!is.na(tri)])
  cum <- t(apply(tri, 1, function(r) { r[is.na(r)] <- 0; cumsum(r) })); cum[is.na(tri)] <- NA
  expect_equal(chain_ladder(cum, input = "cumulative")$cumulative, cl$cumulative)
  cal <- chain_ladder_calendar(cl)
  expect_equal(names(cal), c("4", "5", "total"))
  expect_equal(unname(cal["4"]), unname(cl$incremental[2, 3] + cl$incremental[3, 2]))
  expect_output(print(cl), "Development factors")
})

test_that("incremental-ratio projection matches the 2023 arithmetic", {
  cl <- chain_ladder(tri, projection = "incremental_ratio")
  expect_equal(unname(cl$factors), c(13 / 30, 2 / 5))
  expect_equal(unname(cl$incremental[2, 3]), 8 * 0.4)
  expect_equal(unname(cl$incremental[3, ]), c(30, 13, 13 * 0.4))
  # a zero denominator gives NaN cells in this mode (reported in `undefined`) ...
  z <- matrix(c(5, 0, 0, 6, 0, NA, 7, NA, NA), 3, byrow = TRUE)
  clz <- chain_ladder(z, projection = "incremental_ratio")
  expect_true(is.nan(clz$factors[2]))
  expect_equal(nrow(clz$undefined), 2L)             # (2,3) and (3,3): both multiply by NaN
  # ... while the standard projection is fine (cumulative counts stay constant)
  expect_equal(unname(chain_ladder(z)$factors), c(1, 1))
})

test_that("chain ladder rejects malformed triangles", {
  expect_error(chain_ladder(matrix(c(1, NA, 2), 1)), "leading block")
  expect_error(chain_ladder(matrix(c(NA, 1, 2, 3), 2)), "first cell")
  expect_error(chain_ladder(matrix(c(0, 0, 1, NA), 2, byrow = TRUE)), "undefined")
})

test_that("standard chain ladder agrees with ChainLadder::MackChainLadder", {
  skip_if_not_installed("ChainLadder")
  panel <- claims_panel(synthetic_claims, synthetic_payments, t = 12, L = 11,
                        features = c("Type", "Age", "AccWeekday"))
  cum <- as_triangle(panel, cumulative = TRUE)
  ours <- chain_ladder(cum, input = "cumulative")
  m <- unclass(cum); attr(m, "t") <- NULL; attr(m, "what") <- NULL; attr(m, "cumulative") <- NULL
  mack <- ChainLadder::MackChainLadder(m, weights = 1, alpha = 1, tail = FALSE, est.sigma = "Mack")
  expect_equal(unname(ours$factors), unname(mack$f[1:11]), tolerance = 1e-10)
  expect_equal(unname(ours$cumulative), unname(unclass(mack$FullTriangle)), tolerance = 1e-10)
  expect_equal(unname(ours$outstanding), unname(summary(mack)$ByOrigin$IBNR), tolerance = 1e-8)
})

test_that("IBNR identity: exact counts and constant payments reproduce the withheld truth", {
  fx <- make_identity_fixture()
  panel <- claims_panel(fx$claims, fx$payments, t = 3, L = 2, features = "Type")
  expect_equal(nrow(panel$claims), 36 - 2)                 # the two AY-3 delay-1 claims are IBNR
  fit <- reserve_tree(panel, seed = 1)                      # constant responses: root-only trees
  pred <- predict(fit)
  res <- ibnr_reserve(pred, claim_counts(panel))
  expect_equal(unname(res$ibnr_details$factors), c(1.2, 1))
  expect_equal(unname(res$ibnr_details$counts_future_cumulative[3, ]), c(0, 2, 2))
  expect_equal(unname(res$ibnr_details$mu[1, ]), c(0, 100, 60))
  ib <- as_triangle(res, "ibnr", replicate = 1)
  expect_equal(unname(ib[3, ]), c(0, 200, 120))
  expect_equal(sum(ib), 320)
  tot <- calendar_year_payments(res, "total")
  # truth: reported AY-3 claims pay 10 * 100 in year 4 and 10 * 60 in year 5, AY-2 claims 12 * 60
  # in year 4; IBNR pays 200 in year 4 and 120 in year 5
  expect_equal(unname(tot["4", 1]), 10 * 100 + 12 * 60 + 200)
  expect_equal(unname(tot["5", 1]), 10 * 60 + 120)
  expect_equal(unname(tot["total", 1]), sum(tot[1:2, 1]))
  expect_output(print(res), "IBNR component")
  # the incremental-ratio variant gives the same counts here (no zero denominators)
  res2 <- ibnr_reserve(pred, claim_counts(panel), projection = "incremental_ratio", round = TRUE)
  expect_equal(res2$ibnr, res$ibnr)
  expect_error(ibnr_reserve(pred, claim_counts(panel)[1:2, ]), "shape")
})

test_that("the 2023 division policy is reproduced: 0/0 is NaN and set to zero, x/0 is Inf and refused", {
  # incremental counts: column 2 sums to zero in the observed rows of both columns 1 and 2
  z <- matrix(c(5, 0, 0, 6, 0, NA, 7, NA, NA), 3, byrow = TRUE)
  expect_true(is.nan(chain_ladder(z, projection = "incremental_ratio")$factors[2]))
  # positive numerator over a zero denominator is Inf, as in num / den
  w <- matrix(c(0, 4, 1, 0, 3, NA, 2, NA, NA), 3, byrow = TRUE)
  clw <- chain_ladder(w, projection = "incremental_ratio")
  expect_true(is.infinite(clw$factors[1]))
  expect_true(nrow(clw$undefined) >= 1)
  toy <- make_toy_portfolio()
  p <- claims_panel(toy$claims, toy$payments, t = 5, L = 4, features = "Type")
  pred <- predict(reserve_tree(p, seed = 1))
  # factor for delay 2 is positive-over-zero (Inf) and multiplies a nonzero observed cell of AY 4;
  # hand-made counts are refused against the stored triangle, so drop it (as in a prediction of an
  # older version) to reach the projection
  cnt <- claim_counts(p); cnt[1:3, 2] <- 0; cnt[1:3, 3] <- 1; cnt[4, 2] <- 1
  expect_error(ibnr_reserve(pred, cnt), "differs from the reporting counts")
  pred_old <- pred; pred_old$counts <- NULL
  expect_error(ibnr_reserve(pred_old, cnt, projection = "incremental_ratio"), "infinite projected counts")
  # counts from another valuation date or with other accident years are refused
  cl4 <- toy$claims[toy$claims$AccYear <= 4, ]
  pay4 <- toy$payments[toy$payments$Id %in% cl4$Id & toy$payments$EventYear <= 4, ]
  other <- claim_counts(claims_panel(cl4, pay4, t = 4, L = 4, features = "Type"))
  expect_error(ibnr_reserve(pred, other), "shape|observation masks|valuation date|accident years")
  expect_error(ibnr_reserve(pred_old, other), "shape|observation masks|valuation date|accident years")
})
