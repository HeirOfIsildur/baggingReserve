#' Synthetic individual claims observed at the reserving date
#'
#' A synthetic individual claims portfolio generated with the Wang and
#' \enc{Wüthrich}{Wuthrich} (2022) individual claims generator (built on \pkg{SynthETIC})
#' on a yearly grid with seed 2026: six claim types, twelve accident years, reporting
#' delays of up to eight years. It is a fresh portfolio from the same simulator family as the
#' one analysed by \enc{Janoušek}{Janousek} and \enc{Pešta}{Pesta} (2025), not that article's
#' data (which used another seed and ten accident years). `synthetic_claims`
#' holds what the insurer sees at the reserving date \eqn{t = 12}: one row per
#' *reported* claim (closed or RBNS), with the settlement information censored at
#' \eqn{t = 12}. The claims that are still IBNR at that date are absent here and
#' present only in [synthetic_claims_full]. Only quantities observable at \eqn{t = 12}
#' are kept: the generated ultimate claim amount and total number of payments are in
#' [synthetic_claims_full] alone, so that nothing unobservable can leak into a fit.
#'
#' A word on the time grid. The generator was written for a monthly grid; run with
#' `time_unit = 1` its yearly indices `AccYear`, `RepYear`, `SetYear`
#' and `EventYear` are the development grid the method uses, and the reporting delay
#' `RepDel = RepYear - AccYear` runs from 0 to 8 years. The generator's calendar
#' conversion, however, treats one time unit as a month, so `RepDelDays` is the reporting
#' delay in days divided by twelve and the three-year cap the generator
#' places on it does not bind. Both columns are kept as generated; the method only uses
#' the yearly grid.
#'
#' Regenerate with `data-raw/make_synthetic_claims.R`; `data-raw/GENERATION.txt` records the
#' generator call and the software versions used.
#'
#' @format A data frame with one row per reported claim and the columns
#' \describe{
#'   \item{Id}{claim identifier, integer, consistent across the four datasets}
#'   \item{Type}{claim type, factor with levels `1`–`6`}
#'   \item{Age}{age of the insured at the accident, years}
#'   \item{AccDate}{accident time, continuous, in years since the start of year 1}
#'   \item{AccYear}{accident year \eqn{i}, `1`–`12`}
#'   \item{AccWeekday}{weekday of the accident, factor}
#'   \item{RepDate}{reporting time, continuous, in years}
#'   \item{RepYear}{reporting year \eqn{i + d}}
#'   \item{RepDelDays}{reporting delay in the generator's day count (see above; not used)}
#'   \item{RepDel}{reporting delay in years, \eqn{d = } `RepYear - AccYear`, `0`–`8`}
#'   \item{SetYear}{settlement year; `NA` if the claim is open (RBNS) at \eqn{t = 12}}
#'   \item{SetDel}{settlement delay in years, `SetYear - RepYear`; `NA` if open}
#'   \item{Status}{factor, `Closed` or `RBNS` at \eqn{t = 12}}
#'   \item{CumPaid}{cumulative amount paid up to \eqn{t = 12}}
#' }
#' @source Wang M, \enc{Wüthrich}{Wuthrich} MV (2022). Individual claims generator for claims reserving
#'   studies: data simulation.R. SSRN 4127073. \doi{10.2139/ssrn.4127073}. Code:
#'   <https://github.com/actuarial-data-science/PackageIndividualClaimsSimulator> (GPL-3;
#'   formerly JSchelldorfer/IndividualClaimsSimulator, the location the vendored copy was taken from).
#' @seealso [synthetic_payments], [synthetic_claims_full], [synthetic_payments_full]
#' @family synthetic data
#' @examples
#' data("synthetic_claims", package = "baggingReserve")
#' str(synthetic_claims)
#' table(synthetic_claims$AccYear)
"synthetic_claims"

#' Synthetic claim payments observed at the reserving date
#'
#' One row per payment event up to and including year 12 on the claims in
#' [synthetic_claims]. Events are yearly-gridded; several events of one claim can fall
#' into the same year, negative amounts (recoveries) occur, and a few events carry no
#' payment. Summing `Paid` per claim and year gives the incremental payments the method
#' works with.
#'
#' @format A data frame with the columns
#' \describe{
#'   \item{Id}{claim identifier, matches [synthetic_claims]}
#'   \item{EventId}{running number of the event within the claim}
#'   \item{EventYear}{calendar year of the event, \eqn{i + d + k}}
#'   \item{Paid}{amount paid (negative for recoveries, zero for non-payment events)}
#'   \item{PayInd}{`1` for a payment, `0` for a recovery or a zero-amount event, as generated}
#'   \item{OpenInd}{`1` if the claim is still open after the event, as generated}
#' }
#' @inherit synthetic_claims source
#' @family synthetic data
#' @examples
#' data("synthetic_payments", package = "baggingReserve")
#' head(synthetic_payments)
#' tapply(synthetic_payments$Paid, synthetic_payments$EventYear, sum)
"synthetic_payments"

#' Synthetic individual claims: the generated truth
#'
#' Every generated claim, including those that are IBNR at \eqn{t = 12}, with the
#' uncensored settlement information. Used to back-test predictions of the
#' outstanding liabilities. IBNR claims at \eqn{t = 12} are those whose `Id` is not
#' in [synthetic_claims].
#'
#' @format A data frame with one row per generated claim and the columns
#' \describe{
#'   \item{Id, Type, Age, AccDate, AccYear, AccWeekday, RepDate, RepYear, RepDelDays, RepDel}{as
#'     in [synthetic_claims]; here the reporting information is uncensored, so IBNR claims
#'     have `RepYear > 12`}
#'   \item{SetYear}{settlement year, uncensored}
#'   \item{SetDel}{settlement delay in years, `SetYear - RepYear`, uncensored}
#'   \item{Ultimate}{ultimate claim amount as generated (not observable in practice)}
#'   \item{PayCount}{total number of payments as generated (not observable in practice)}
#'   \item{ReopenInd}{`1` if the claim was reopened after its first settlement}
#' }
#' @inherit synthetic_claims source
#' @family synthetic data
#' @examples
#' data("synthetic_claims", "synthetic_claims_full", package = "baggingReserve")
#' # claims reported after the valuation date t = 12 (the IBNR claims)
#' sum(!synthetic_claims_full$Id %in% synthetic_claims$Id)
"synthetic_claims_full"

#' Synthetic claim payments: the generated truth
#'
#' Every generated payment, past and future, for the claims in
#' [synthetic_claims_full]. Same columns as [synthetic_payments]. One generated event far
#' beyond the simulated horizon carried no amount and is dropped (the count is recorded in
#' `data-raw/GENERATION.txt`).
#'
#' @format A data frame with the columns of [synthetic_payments].
#' @inherit synthetic_claims source
#' @family synthetic data
#' @examples
#' data("synthetic_payments_full", package = "baggingReserve")
#' # realised payments after the valuation date t = 12, by calendar year
#' fut <- synthetic_payments_full[synthetic_payments_full$EventYear > 12, ]
#' tapply(fut$Paid, fut$EventYear, sum)
"synthetic_payments_full"
