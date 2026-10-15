# Regenerates the package datasets with the Wang & Wüthrich (2022) individual claims
# generator (vendored in data-raw/IndividualClaimsSimulator/, GPL-3) on a *yearly* grid:
# seed 2026, twelve accident years, valuation at the end of year 12. This is a fresh portfolio
# from the same simulator family as the one of Janoušek & Pešta (2025), not that article's data
# (which used seed 1000 and ten accident years). On this grid the reporting delay
# RepYear - AccYear runs to several years (the generator's three-year cap acts on a day count
# that is scaled by 1/12 under time_unit = 1).
#
# Run from the package root:  Rscript data-raw/make_synthetic_claims.R
# Takes a minute or two. Needs SynthETIC (>= 1.0.0), plyr, dplyr, locfit, actuar, tidyr.
# Writes data/*.rda and data-raw/GENERATION.txt (versions used, for the manuscript).
#
# The generator names its time columns *Month because its default grid is monthly
# (time_unit = 1/12); with time_unit = 1 they hold years, so they are renamed here.

suppressPackageStartupMessages({
  library(SynthETIC)
  library(plyr)
  library(dplyr)
  library(locfit)
  library(actuar)
  library(tidyr)
})

ref_claim <- 1        # monetary unit
time_unit <- 1        # yearly development grid (generator default: 1/12)
set_parameters(ref_claim = ref_claim, time_unit = time_unit)
years <- 12           # accident years 1..12; the insurer values at the end of year 12
I <- years / time_unit

# The generator sources its helpers relative to the working directory ("./Tools/reopening.r"),
# so it is run from inside the vendored folder.
generator_dir <- file.path("data-raw", "IndividualClaimsSimulator")
source(file.path(generator_dir, "functions simulation.r"))
owd <- setwd(generator_dir)
claims_list <- data.generation(seed = 2026, future_info = TRUE)
setwd(owd)

month_to_year <- c(
  AccMonth = "AccYear", RepMonth = "RepYear", SetMonth = "SetYear",
  SetDelMonths = "SetDel", EventMonth = "EventYear", PayMonth = "PayYear"
)
rename_years <- function(df) {
  hit <- intersect(names(month_to_year), names(df))
  names(df)[match(hit, names(df))] <- month_to_year[hit]
  if (all(c("AccYear", "RepYear") %in% names(df))) df$RepDel <- df$RepYear - df$AccYear
  rownames(df) <- NULL
  df
}

# What the insurer observes at t = 12: reported claims only, payments up to year 12.
synthetic_claims   <- rename_years(claims_list$claims)
synthetic_claims   <- synthetic_claims[synthetic_claims$Status != "IBNR", ]
synthetic_claims$Status <- droplevels(synthetic_claims$Status)
# Observable view only: the generator's ultimate claim amount and total payment count are
# unknown to the insurer at t = 12. They stay in synthetic_claims_full for back-testing.
synthetic_claims$Ultimate <- NULL
synthetic_claims$PayCount <- NULL
# Claim type is categorical (six unlabelled types): a factor, as in the 2023 analysis.
synthetic_claims$Type <- factor(synthetic_claims$Type, levels = 1:6)
synthetic_payments <- rename_years(claims_list$paid)
synthetic_payments <- synthetic_payments[synthetic_payments$Id %in% synthetic_claims$Id, ]

# The generated truth: every claim (incl. IBNR at t = 12) and every payment, past and future.
synthetic_claims_full   <- rename_years(claims_list$full_claims)
synthetic_payments_full <- rename_years(claims_list$full_paid)
synthetic_claims_full$Type <- factor(synthetic_claims_full$Type, levels = 1:6)
# The generator can leave a payment event without an amount far in the future (beyond its
# simulated horizon); such rows carry no information and are dropped, their number recorded.
n_na_full <- sum(is.na(synthetic_payments_full$Paid))
synthetic_payments_full <- synthetic_payments_full[!is.na(synthetic_payments_full$Paid), ]
cat(sprintf("generated payment events without an amount, dropped from the full data: %d\n", n_na_full))

stopifnot(
  !anyNA(synthetic_claims$RepYear),
  all(synthetic_payments$EventYear <= I),
  all(synthetic_payments$Id %in% synthetic_claims$Id),
  all(synthetic_claims$Id %in% synthetic_claims_full$Id)
)

cat(sprintf("reported claims: %d (%s)\n", nrow(synthetic_claims),
            paste(names(table(synthetic_claims$Status)), table(synthetic_claims$Status),
                  sep = " = ", collapse = ", ")))
cat(sprintf("all generated claims: %d, of which IBNR at t = %d: %d\n",
            nrow(synthetic_claims_full), I,
            sum(!synthetic_claims_full$Id %in% synthetic_claims$Id)))

# Facts the manuscript must report: payments beyond development year L = 11 (the method's
# payment window on this twelve-year portfolio; later payments are outside the model) and recoveries.
dev_full <- synthetic_payments_full$EventYear -
  synthetic_claims_full$AccYear[match(synthetic_payments_full$Id, synthetic_claims_full$Id)]
cat(sprintf("payments beyond development year 11 in the full data: %d rows, amount %.2f (%.4f%% of all)\n",
            sum(dev_full > 11), sum(synthetic_payments_full$Paid[dev_full > 11]),
            100 * sum(synthetic_payments_full$Paid[dev_full > 11]) / sum(synthetic_payments_full$Paid)))
cat(sprintf("recoveries (negative payments): %d observed at t = %d, %d in the full data\n",
            sum(synthetic_payments$Paid < 0), I, sum(synthetic_payments_full$Paid < 0)))
cat(sprintf("reported claims without any payment by t = %d: %d\n", I,
            sum(!synthetic_claims$Id %in% synthetic_payments$Id)))

# Same as usethis::use_data(..., compress = "xz") without the usethis dependency.
dir.create("data", showWarnings = FALSE)
for (nm in c("synthetic_claims", "synthetic_payments",
             "synthetic_claims_full", "synthetic_payments_full")) {
  save(list = nm, file = file.path("data", paste0(nm, ".rda")), compress = "xz", version = 3)
  cat(sprintf("data/%s.rda: %.0f kB\n", nm, file.size(file.path("data", paste0(nm, ".rda"))) / 1024))
}

# Generation provenance for the manuscript's computational details.
writeLines(c(
  sprintf("generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  R.version.string,
  sprintf("SynthETIC %s", packageVersion("SynthETIC")),
  "generator: JSchelldorfer/IndividualClaimsSimulator @ 4991e882929a0e3f0bc10cc9dc00d7cedbf2a5cf (GPL-3)",
  "call: set_parameters(ref_claim = 1, time_unit = 1); years = 12; data.generation(seed = 2026, future_info = TRUE)",
  sprintf("payment events without an amount dropped from the full data: %d", n_na_full)
), file.path("data-raw", "GENERATION.txt"))
