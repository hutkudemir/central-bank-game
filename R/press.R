# R/press.R
# Module: Generate an educational, realistic press statement for each policy decision

#' Generate a press statement title and body
#'
#' @param lastDecision   One of "increase", "decrease", or "maintain"
#' @param lastRateChange Numeric change in rate (basis points)
#' @param infl           Current inflation percentage
#' @param unemp          Current unemployment percentage
#' @param has_shock      Logical flag: was there an active shock this period?
#' @param shock          Optional: list describing current shock (with headline, description, type, severity)
#' @param month          Integer: current month index (0-based)
#' @param language       "tr" or "en"
#' @return A list with elements $title and $statement
#' @examples
#' ps <- econ_generate_press(
#'   lastDecision = "increase",
#'   lastRateChange = 50,
#'   infl = 7.2,
#'   unemp = 6.5,
#'   has_shock = TRUE,
#'   shock = list(headline = "Energy Crisis", description = "Oil prices surged"),
#'   month = 5,
#'   language = "en"
#' )
econ_generate_press <- function(lastDecision,
                                lastRateChange,
                                infl,
                                unemp,
                                has_shock,
                                shock = NULL,
                                month,
                                language) {
  # Policy benchmarks
  pi_star <- 5.0   # inflation target
  u_star  <- 8.0   # unemployment target
  
  # Compute deviations
  infl_dev   <- infl - pi_star
  unemp_dev  <- unemp - u_star
  
  # Prepare messages by language
  if (language == "tr") {
    meeting_no <- month + 1
    title <- sprintf("Para Politikası Kurulu %d. Toplantı Kararı", meeting_no)
    # Base statement
    if (lastDecision == "increase") {
      statement <- sprintf(
        "Komite, enflasyonun %.1f%% (hedef %s%%, sapma %+ .1f puan) olması nedeniyle politika faizini %s puan artırarak %0.2f%%'ye yükseltti.",
        infl, pi_star, infl_dev, lastRateChange, infl + lastRateChange / 100
      )
    } else if (lastDecision == "decrease") {
      statement <- sprintf(
        "Komite, işsizliğin %.1f%% (hedef %s%%, sapma %+ .1f puan) nedeniyle politika faizini %s puan indirerek %0.2f%%'ye düşürdü.",
        unemp, u_star, unemp_dev, lastRateChange, infl - lastRateChange / 100
      )
    } else {
      statement <- sprintf(
        "Komite, mevcut verileri (%0.1f%% enflasyon, %0.1f%% işsizlik) değerlendirdi ve orta vadede istikrar için politika faizini sabit tutmaya karar verdi.",
        infl, unemp
      )
    }
    # Add shock context
    if (has_shock && !is.null(shock)) {
      statement <- paste(
        statement,
        sprintf("Ayrıca, '%s' adlı ekonomik şok (etki: %s) yakından izleniyor: %s.",
                shock$headline_tr %||% shock$headline,
                shock$severity,
                shock$description_tr %||% shock$description),
        sep = " "
      )
    }
    # Closing
    statement <- paste(
      statement,
      "Komite, fiyat istikrarı ve tam istihdam dengesini sağlamak için kararlı adımlarını sürdürecektir."
    )
  } else {
    meeting_no <- month + 1
    title <- sprintf("Monetary Policy Committee Meeting %d Decision", meeting_no)
    if (lastDecision == "increase") {
      statement <- sprintf(
        "The Committee raised the policy rate by %s basis points to %.2f%% in response to inflation at %.1f%% (target %s%%, deviation %+ .1f pp).",
        lastRateChange, infl + lastRateChange / 100, infl, pi_star, infl_dev
      )
    } else if (lastDecision == "decrease") {
      statement <- sprintf(
        "The Committee cut the policy rate by %s basis points to %.2f%% to support employment, as unemployment stands at %.1f%% (target %s%%, deviation %+ .1f pp).",
        lastRateChange, infl - lastRateChange / 100, unemp, u_star, unemp_dev
      )
    } else {
      statement <- sprintf(
        "Based on current data (inflation %.1f%%, unemployment %.1f%%), the Committee decided to hold the policy rate steady to maintain medium-term price stability and support growth.",
        infl, unemp
      )
    }
    if (has_shock && !is.null(shock)) {
      statement <- paste(
        statement,
        sprintf("Additionally, the '%s' shock (severity: %s) is being closely monitored: %s.",
                shock$headline,
                shock$severity,
                shock$description)
      )
    }
    statement <- paste(
      statement,
      "We remain committed to achieving our dual mandate of price stability and maximum sustainable employment."
    )
  }
  
  list(title = title, statement = statement)
}
