# R/shocks.R
# Module: Shock generation and management for the Central Bank simulation

#' Start a new economic shock
#'
#' @param shock_list List of shock definitions (each a list with keys: key, headline, description, type, severity, inflEffect, unempEffect, gdpEffect)
#' @param diff_mult   Numeric multiplier for shock severity (based on difficulty)
#' @return A list representing the active shock, with fields:
#'   key, headline, description, type, severity, inflEffect, unempEffect, gdpEffect,
#'   duration, mag
#' @examples
#' shock <- econ_start_new_shock(shockList, 1.2)
#' str(shock)
econ_start_new_shock <- function(shock_list, diff_mult = 1) {
  # sample one definition
  def <- sample(shock_list, 1)[[1]]
  
  # scale effects by difficulty multiplier
  infl_eff  <- def$inflEffect  * diff_mult
  unemp_eff <- def$unempEffect * diff_mult
  gdp_eff   <- def$gdpEffect   * diff_mult
  
  # assign a random duration (2–4 periods) and a starting magnitude
  duration  <- sample(2:4, 1)
  mag       <- runif(1, 0.8, 1.2)
  
  list(
    key          = def$key,
    headline      = def$headline,
    headline_tr   = def$headline_tr,
    description   = def$description,
    description_tr= def$description_tr,
    type          = def$type,
    severity      = def$severity,
    inflEffect    = def$inflEffect  * diff_mult,
    unempEffect   = def$unempEffect * diff_mult,
    gdpEffect     = def$gdpEffect   * diff_mult,
    duration      = duration,
    mag           = mag
  )
}

#' Advance or fade an existing shock
#'
#' @param shock      A shock object returned by econ_start_new_shock or previous update
#' @param decay_rate Numeric factor (0&lt;decay_rate&lt;1) to shrink magnitude each period
#' @return A list with updated duration, mag, and effects
#' @examples
#' updated <- econ_update_shock(my_shock, 0.85)
econ_update_shock <- function(shock, decay_rate = 0.85) {
  shock$duration <- shock$duration - 1
  shock$mag      <- shock$mag * decay_rate
  shock
}
