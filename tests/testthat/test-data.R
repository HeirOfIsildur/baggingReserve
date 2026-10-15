# The shipped portfolio: Wang & Wüthrich generator, seed 2026, twelve accident years,
# valuation at t = 12; 62,904 closed + 8,213 RBNS = 71,117 reported claims, 2,588 IBNR.
# (Not the article's data, which used seed 1000 and ten accident years.)

test_that("synthetic_claims is the observed view of the shipped portfolio", {
  expect_equal(nrow(synthetic_claims), 71117)
  expect_equal(as.vector(table(synthetic_claims$Status)[c("Closed", "RBNS")]), c(62904L, 8213L))
  expect_setequal(unique(synthetic_claims$AccYear), 1:12)
  expect_true(all(synthetic_claims$RepYear <= 12))
  expect_equal(synthetic_claims$RepDel, synthetic_claims$RepYear - synthetic_claims$AccYear)
  # on the yearly grid the generator's reporting delay runs to several years (8 here)
  expect_true(all(synthetic_claims$RepDel %in% 0:8))
  expect_equal(max(synthetic_claims$RepDel), 8L)
  expect_true(all(is.na(synthetic_claims$SetYear[synthetic_claims$Status == "RBNS"])))
  expect_named(synthetic_claims,
               c("Id", "Type", "Age", "AccDate", "AccYear", "AccWeekday", "RepDate", "RepYear",
                 "RepDelDays", "SetYear", "SetDel", "Status", "CumPaid", "RepDel"),
               ignore.order = TRUE)
  # nothing unobservable at t = 12 may sit in the observed view
  expect_false(any(c("Ultimate", "PayCount", "ReopenInd") %in% names(synthetic_claims)))
  expect_true(all(c("Ultimate", "PayCount") %in% names(synthetic_claims_full)))
})

test_that("synthetic_payments are censored at the reserving date and tie to the claims", {
  expect_named(synthetic_payments, c("Id", "EventId", "EventYear", "Paid", "PayInd", "OpenInd"))
  expect_true(all(synthetic_payments$Paid[synthetic_payments$PayInd == 0] <= 0))
  expect_true(all(synthetic_payments$EventYear <= 12))
  expect_true(all(synthetic_payments$Id %in% synthetic_claims$Id))
  expect_true(all(is.finite(synthetic_payments$Paid)))
  # a reported claim without any payment yet has CumPaid 0, not NA
  paid <- tapply(synthetic_payments$Paid, synthetic_payments$Id, sum)
  total <- as.vector(paid[as.character(synthetic_claims$Id)])
  total[is.na(total)] <- 0
  expect_equal(total, synthetic_claims$CumPaid, tolerance = 1e-8)
})

test_that("the full data add exactly the IBNR claims and the future payments", {
  expect_equal(nrow(synthetic_claims_full), 71117 + 2588)
  expect_true(all(synthetic_claims$Id %in% synthetic_claims_full$Id))
  ibnr <- synthetic_claims_full[!synthetic_claims_full$Id %in% synthetic_claims$Id, ]
  expect_equal(nrow(ibnr), 2588)
  expect_true(all(ibnr$RepYear > 12))
  expect_true(all(synthetic_payments$Id %in% synthetic_payments_full$Id))
  expect_gte(nrow(synthetic_payments_full), nrow(synthetic_payments))
  expect_true(any(synthetic_payments_full$EventYear > 12))
  expect_true(all(is.finite(synthetic_payments_full$Paid)))
})
