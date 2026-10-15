# Fixtures shared by the test files (testthat sources helper-*.R before the tests).

# A small synthetic portfolio with a real signal so that trees have something to split on:
# two claim types with different sizes, geometric payment decay, reporting delay 0 or 1.
make_toy_portfolio <- function(n = 400, t = 5, seed = 7) {
  set.seed(seed)
  acc <- sample(1:t, n, replace = TRUE)
  d <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))
  type <- sample(c("A", "B"), n, replace = TRUE)
  size <- ifelse(type == "A", 100, 300)
  claims <- data.frame(Id = seq_len(n), AccYear = acc, RepYear = acc + d, Type = type,
                       stringsAsFactors = FALSE)
  pays <- do.call(rbind, lapply(seq_len(n), function(i) {
    devs <- d[i]:min(d[i] + 3, 4)
    data.frame(Id = i, EventYear = acc[i] + devs,
               Paid = round(size[i] * 0.5^(devs - d[i]) * stats::runif(length(devs), 0.8, 1.2)))
  }))
  list(claims = claims, payments_full = pays,
       payments = pays[pays$EventYear <= t, ])
}

# An exactly identifiable IBNR fixture: every claim pays 100 in development year 1 and 60
# in development year 2 whatever its reporting delay (0 or 1); the accident-year-3 claims
# reported with delay 1 are IBNR at t = 3 and their number is exactly what the chain ladder
# on counts projects (10, 10, 10 delay-0 claims and 2, 2, 2 delay-1 claims).
make_identity_fixture <- function() {
  ay <- rep(1:3, each = 12); d <- rep(c(rep(0, 10), rep(1, 2)), 3)
  claims <- data.frame(Id = seq_along(ay), AccYear = ay, RepYear = ay + d,
                       Type = rep(c("u", "v"), 18), stringsAsFactors = FALSE)
  pays <- rbind(data.frame(Id = claims$Id, EventYear = ay + 1, Paid = 100),
                data.frame(Id = claims$Id, EventYear = ay + 2, Paid = 60))
  list(claims = claims, payments_full = pays, payments = pays[pays$EventYear <= 3, ])
}

# Drop the attributes as_triangle() adds (t, what, cumulative) and dimnames, for comparing
# with the plain matrices deposited by the reference script.
plain <- function(m) { m <- unclass(m); attributes(m) <- list(dim = dim(m)); m }

