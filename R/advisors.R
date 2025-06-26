# R/advisors.R
# Module: Generate educational, realistic monetary policy advisor recommendations

#' Generate monetary policy advisor options
#'
#' This function produces three distinct policy recommendations (Balancer, Dove, Hawk)
#' tailored to current economic indicators, credibility, and shocks. Each advisor:
#'  - References an internal policy framework and macro context
#'  - Adjusts for shock type & severity with scenario-specific language
#'  - Embodies a persona with historical analogies and data-driven arguments
#'  - May provoke dismissal decisions if their advice is repeatedly ignored
#'
#' @param infl          Current inflation rate (%)
#' @param unemp         Current unemployment rate (%)
#' @param cred          Policy credibility score (0-100)
#' @param current_rate  Current policy rate (%)
#' @param language      'tr' or 'en'
#' @param shock         Optional list describing current shock (type, severity, headline, inflEffect)
#' @param month         Current month index (0-based)
#' @param lastApplied   Named list of last months each advisor was heeded (opt1, opt2, opt3)
#' @return A list of three advisor options, each a list(name, rate, rationale, press, notice)
econ_generate_advisor_options <- function(infl, unemp, cred,
                                          current_rate, language,
                                          shock = NULL, month = 0,
                                          lastApplied = list(opt1=NA,opt2=NA,opt3=NA),
                                          pi_star = 5, u_star = 8, r_star = 3) {
  
  ## Taylor damping (already added earlier)
  taylor_mult <- if (infl < pi_star || unemp > 15) 0.5 else 1
  
  infl_gap   <- infl - pi_star
  unemp_gap  <- u_star - unemp
  output_gap <- 0.5 * unemp_gap
  
  base_rate  <- r_star + infl +
    0.6 * taylor_mult * infl_gap +
    0.4 * taylor_mult * output_gap
  
  opt_rate   <- round(base_rate / 0.25) * 0.25
  
  
  # --- Shock context ---
  shock_active <- !is.null(shock)
  shock_sev    <- if (shock_active) shock$severity else NA_character_
  shock_note   <- ""
  if (shock_active) {
    sev_label <- switch(shock_sev,
                        high   = if(language=='tr') '🔴 Yüksek şiddet'   else '🔴 High-severity',
                        medium = if(language=='tr') '🟠 Orta şiddet'     else '🟠 Medium-severity',
                        low    = if(language=='tr') '🟢 Düşük şiddet'    else '🟢 Low-severity',
                        ''
    )
    shock_note <- if(language=='tr') {
      sprintf("%s şoku: %s", sev_label, shock$headline_tr %||% shock$headline)
    } else {
      sprintf("%s shock: %s", sev_label, shock$headline)
    }
  }
  
  # --- Persona step sizes (bps) ---
  dove_step <- if (unemp > 12) 75 else if (shock_active && shock$inflEffect < 0) 50 else 40
  hawk_step <- if (infl > 10) 75 else if (shock_active && shock$inflEffect > 0) 60 else 50
  
  # --- Compute advisor rates ---
  bal_rate <- max(0, opt_rate)                   # ⬅️ clamp
  dov_rate <- max(0, round((opt_rate - dove_step/100) / 0.25) * 0.25)
  haw_rate <- max(0, round((opt_rate + hawk_step/100) / 0.25) * 0.25)
  
  # --- Localization helper ---
  T_ <- function(tr, en) if (language=='tr') tr else en
  
  # --- Advisor personas (all 'Dr.') ---
  name_bal <- T_('Dr. Selim Kaya', 'Dr. Selim Kaya')
  name_dov <- T_('Dr. Aylin Demir', 'Dr. Aylin Demir')
  name_haw <- T_('Dr. Murat Gür', 'Dr. Murat Gur')
  
  # --- Detailed rationales ---
  bal_rat <- T_(
    sprintf("İç modelimiz %.2f%% optimal; enflasyon %.1f%%, işsizlik %.1f%%. %s", 
            base_rate, infl, unemp, shock_note),
    sprintf("Our framework calls for %.2f%%; inflation at %.1f%%, unemployment at %.1f%%. %s", 
            base_rate, infl, unemp, shock_note)
  )
  bal_rat <- paste(
    bal_rat,
    T_(
      "Bu strateji geçmiş örneklerden (Volcker, 1990’lar Japonya) ve veri analizinden güç alır.",
      "This strategy draws on historical cases (Volcker, 1990s Japan) and data analytics."
    )
  )
  
  dov_rat <- T_(
    sprintf("İşsizlik %0.1f%%; hane halkı riski var. %d bp indirim öneriyorum: %0.2f%%. %s", 
            unemp, dove_step, dov_rate, shock_note),
    sprintf("Unemployment at %0.1f%% creating household strains. %d bps cut → %0.2f%%. %s", 
            unemp, dove_step, dov_rate, shock_note)
  )
  dov_rat <- paste(
    dov_rat,
    T_(
      "2016 Brezilya örneğinde olduğu gibi, istihdamı desteklemek uzun vadeli büyümeyi güçlendirir.",
      "As in Brazil 2016, supporting employment strengthens long-term growth."
    )
  )
  
  haw_rat <- T_(
    sprintf("Enflasyon %0.1f%%; fiyat baskıları artıyor. %d bp artır: %0.2f%%. %s", 
            infl, hawk_step, haw_rate, shock_note),
    sprintf("Inflation at %0.1f%%; rising pressures call for action. %d bps hike → %0.2f%%. %s", 
            infl, hawk_step, haw_rate, shock_note)
  )
  haw_rat <- paste(
    haw_rat,
    T_(
      "1980’lerde Volcker yaklaşımı beklentileri sabitledi.",
      "Volcker-era tactics anchored expectations."
    )
  )
  
  # --- Press summaries ---
  bal_press <- T_(
    sprintf("Ay %d: faiz %.2f%% sabit—denge korunuyor.", month+1, bal_rate),
    sprintf("Month %d: rate held at %.2f%%—balance maintained.", month+1, bal_rate)
  )
  dov_press <- T_(
    sprintf("Ay %d: faiz %.2f%%'e indirildi—istihdam önceliğinde.", month+1, dov_rate),
    sprintf("Month %d: cut to %.2f%%—prioritizing jobs.", month+1, dov_rate)
  )
  haw_press <- T_(
    sprintf("Ay %d: faiz %.2f%%'e yükseltildi—enflasyona net tepki.", month+1, haw_rate),
    sprintf("Month %d: raised to %0.2f%%—firm response to inflation.", month+1, haw_rate)
  )
  
  # --- Compute dismissal notice per advisor with emotional tone after 5 turns ---
  compute_notice <- function(slot) {
    last <- lastApplied[[slot]]
    # if never heeded and month >= 5, or heeded more than 5 turns ago
    if ((is.na(last) && month >= 5) || (!is.na(last) && (month - last) >= 5)) {
      switch(slot,
             opt1 = T_(
               'Üzgünüm, stratejilerim sürekli gözardı edilirse istifa etmeyi düşünebilirim.',
               'I’m sincerely disappointed; if my strategy continues to be overlooked, I may consider resigning.'
             ),
             opt2 = T_(
               'Önerilerim dinlenmediği için üzerimde baskı artıyor. Görevi bırakabilirim.',
               'I might step down; the pressure mounts when my recommendation isn’t heeded.'
             ),
             opt3 = T_(
               'Lütfen artık beni dikkate alın, yoksa görevden affımı istemek zorunda kalacağım.',
               'Please take my advice seriously; otherwise, I’ll have to request relief from duty.'
             ),
             NULL
      )
    } else {
      NULL
    }
  }
  
  bal_notice <- compute_notice('opt1')
  dov_notice <- compute_notice('opt2')
  haw_notice <- compute_notice('opt3')
  
  # --- Return advisor list ---
  list(
    list(name = name_bal, rate = bal_rate, rationale = bal_rat, press = bal_press, notice = bal_notice),
    list(name = name_dov, rate = dov_rate, rationale = dov_rat, press = dov_press, notice = dov_notice),
    list(name = name_haw, rate = haw_rate, rationale = haw_rat, press = haw_press, notice = haw_notice)
  )
}
