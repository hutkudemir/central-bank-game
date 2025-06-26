library(shiny)
library(DT)
library(plotly)
library(rintrojs)
library(shinyjs)
library(bsicons)
library(dplyr)


# Source helpers
source("R/utils_economic_model.R", local = TRUE)
source("R/translations.R", local = TRUE)
source("R/shocks.R", local = TRUE)
source("R/press.R", local = TRUE)
source("R/advisors.R", local = TRUE)



`%||%` <- function(x, y) if (!is.null(x) && nzchar(x)) x else y


server <- function(input, output, session) {
  
  # ── AUDIO OBSERVERS ────────────────────────────────────────────────
  # 1) Mute / un-mute background music when the toggle button is clicked
  observeEvent(input$toggleMusic, {
    session$sendCustomMessage("toggle-music", list())   # handled in custom.js
  })
  
  # 2) A helper we’ll call whenever a *new* shock is generated
  playShock <- function() {
    session$sendCustomMessage("play-shock", list())     # handled in custom.js
  }
  # ------------------------------------------------------------------
  stopShock  <- function() session$sendCustomMessage("stop-shock",  list())
  
  observeEvent(input$region, {
    cfg <- region_defaults[[input$region]]
    
    rv$pi_star <- cfg$pi_star
    rv$u_star  <- cfg$u_star
    rv$r_star  <- cfg$r_star
    
    # reset starting month only if at month 0
    if (rv$month == 0) {
      rv$infl[1]     <- cfg$init_pi
      rv$unemp[1]    <- cfg$init_u
      rv$interest[1] <- cfg$init_r
      updateNumericInput(session, "interest", value = cfg$init_r)
    }
  })
  
  

  # ---- Language Reactivity ----
  observe({
    if (rv$language != input$language) {
      rv$language <- input$language
      rv$press_questions <- list()
      removeModal()
    }
  })
  
  # ---- Difficulty Reactivity ----
  observeEvent(input$difficulty, {
    rv$difficulty <- input$difficulty
  })
  
  
  
  # ---- Keep advisor buttons in the current language -----------------
  observe({
    new_lbl <- translations[[rv$language]]$apply_policy  # "Uygula" or "Apply"
    updateActionButton(session, "apply_opt1", label = new_lbl)
    updateActionButton(session, "apply_opt2", label = new_lbl)
    updateActionButton(session, "apply_opt3", label = new_lbl)
  })
  
  # ---- UI Label Outputs (loop) ----
  label_keys <- c(
    "game_title", "difficulty_label", "help_btn", "interest_control", "increase_btn", "decrease_btn", "apply_decision",
    "econ_news", "inflation", "unemployment", "econ_health", "credibility", "game_progress", "shock_history", "press_conference", "press_info", "time_series", "phillips_curve", "policy_analysis", "advisor_recommendations", "policy_options",
    "press_suggestion", "apply_policy", "press_statement", "game_stats", "download_results", "reset_game", "region_label"
  )
  for (key in label_keys) {
    local({
      k <- key
      output[[k]] <- renderText({ translations[[rv$language]][[k]] })
    })
  }
  output$language_label <- renderText({ translations[[rv$language]]$language_label })
  output$difficulty_label <- renderText({ translations[[rv$language]]$difficulty_label })
  output$start_press_lbl <- renderText({
    translations[[rv$language]]$start_press
  })
  
  # ---- Difficulty Choices ----
  observe({
    updateSelectInput(session, "difficulty",
                      choices = if (rv$language == "tr") c("Kolay" = "easy", "Orta" = "medium", "Zor" = "hard")
                      else c("Easy" = "easy", "Medium" = "medium", "Hard" = "hard"),
                      selected = rv$difficulty
    )
  })
  
  # ---- Interest Buttons ----
  observeEvent(input$increaseBtn, {
    updateNumericInput(session, "interest", value = min(input$interest + 0.25, 50))
  })
  observeEvent(input$decreaseBtn, {
    updateNumericInput(session, "interest", value = max(input$interest - 0.25, 0))
  })
  
  # ---- Reset Game ----
  observeEvent(input$resetGame, {
    stopShock()
    shinyjs::enable("goBtn")
    rv$shockKeysUsed <- character(0)
    rv$month <- 0
    rv$infl <- c(15.0, numeric(36))
    rv$unemp <- c(9.5, numeric(36))
    rv$interest <- c(12.0, numeric(36))
    rv$gdp_growth <- c(2.5, numeric(36))
    rv$shock <- NULL
    rv$actionNews <- translations[[rv$language]]$action_maintain
    rv$shockNews <- translations[[rv$language]]$shock_no_event
    rv$pressStatement <- translations[[rv$language]]$press_maintain
    rv$pressTitle <- ""
    rv$result <- ""
    rv$shockCount <- 0
    rv$nextShockMonth <- sample(3:6, 1)
    rv$shockHistory <- data.frame(Ay = integer(), Olay = character(), Etki = character(), stringsAsFactors = FALSE)
    rv$lastDecision <- "maintain"
    rv$lastRateChange <- 0
    rv$policy_buffer <- list(rate = numeric(0), effect = numeric(0))
    rv$credibility <- 100
    updateNumericInput(session, "interest", value = 12.0)
  })
  
  # ---- HELP BUTTON / Tutorial ----
  observeEvent(input$helpBtn, {
    steps <- data.frame(
      element = c(".control-panel", "#shockHeadline", "#timeSeriesPlot", "#phillipsCurve"),
      intro = c(
        translations[[rv$language]]$tutorial_control,
        translations[[rv$language]]$tutorial_shock,
        translations[[rv$language]]$tutorial_time_series,
        translations[[rv$language]]$tutorial_phillips
      )
    )
    introjs(session, options = list(steps = steps))
  })
  
  
  # ---------- helper: dismiss an advisor slot -----------------------------
  fire_advisor <- function(slot) {
    rv$fired[slot]       <- TRUE
    rv$credibility       <- max(30, rv$credibility - 10)   # credibility hit
    rv$empty_since[slot] <- rv$month                       # remember when fired
    
    ## NEW – headline for the news panel
    rv$shockNews <- if (rv$language == "tr") "Basın: Danışman kovuldu!"
    else "Media: Advisor dismissed!"
    rv$fired_headline_turn <- rv$month  
    
    idx <- switch(slot, opt1 = 1, opt2 = 2, opt3 = 3)
    rv$advisor_options[[idx]] <- list(
      name      = if (rv$language == "tr") "—Boş Koltuk—" else "—Vacant—",
      rate      = rv$interest[rv$month + 1],              # neutral
      rationale = if (rv$language == "tr") "Bu danışman görevden alındı."
      else "This advisor was dismissed.",
      press     = ""
    )
  }
  
  
  # ---- Update Advisor Options Whenever State Changes ----
  # ---------- update advisors each month (plus respawn logic) -------------
  observe({
    req(rv$month >= 0)
    
    # 1. fresh advice for all three bias types
    new_opts <- econ_generate_advisor_options(
      infl = rv$infl[rv$month+1],
      unemp = rv$unemp[rv$month+1],
      cred  = rv$credibility,
      current_rate = rv$interest[rv$month+1],
      language     = rv$language,
      shock        = rv$shock,
      month        = rv$month,
      lastApplied  = rv$lastApplied
    )
    
    
    # 2. loop over the three slots to handle vacancies / respawn
    for (slot in c("opt1", "opt2", "opt3")) {
      
      idx <- switch(slot, opt1 = 1, opt2 = 2, opt3 = 3)
      
      if (rv$fired[slot]) {                      # seat is currently vacant
        # if four turns have elapsed, hire a replacement
        if (!is.na(rv$empty_since[slot]) &&
            (rv$month - rv$empty_since[slot]) >= 4) {
          
          recruit <- new_opts[[idx]]             # keep same bias type
          recruit$name <- if (rv$language=="tr")
            paste("Yeni", recruit$name)
          else
            paste("New",  recruit$name)
          # weaken quality: add random ±1 pp
          recruit$rate <- round(recruit$rate + runif(1, -1, 1), 2)
          
          new_opts[[idx]]       <- recruit
          rv$fired[slot]        <- FALSE
          rv$empty_since[slot]  <- NA
        } else {
          # still vacant → keep the placeholder already stored in rv$advisor_options
          new_opts[[idx]] <- rv$advisor_options[[idx]]
        }
      }
    }
    
    # 3. write back to reactive values
    rv$advisor_options <- new_opts
  })
  
  
  # ---- Main Economic Step (Advances simulation each turn) ----
  observeEvent(input$goBtn, {
    if (rv$month >= rv$max_months) {
      showNotification("Game complete!", type = "message")
      return()
    }
    curr <- rv$month
    currIndex <- curr + 1
    nextIndex <- curr + 2
    
    # 1. Chosen rate
    i_chosen <- max(0, input$interest)
    pi_t <- rv$infl[currIndex]
    u_t  <- rv$unemp[currIndex]
    r_star <- rv$r_star
    pi_star <- rv$pi_star
    u_star <- rv$u_star
    
    # 2. Decision info
    rate_change <- round(abs(i_chosen - rv$interest[currIndex]), 2)
    if (i_chosen > rv$interest[currIndex]) {
      rv$lastDecision <- "increase"
    } else if (i_chosen < rv$interest[currIndex]) {
      rv$lastDecision <- "decrease"
    } else {
      rv$lastDecision <- "maintain"
    }
    rv$lastRateChange <- rate_change
    
    # 3. Calculate optimal policy rate
    ## ----------  NEW DAMPING SWITCH  ----------
    # If the economy is already in deflation (π < 0) or deep slump (u > 15),
    # cut the Taylor weights in half to avoid runaway feedback.
    
    output_gap <- (u_star - u_t) * 0.5
    taylor_mult <- if (pi_t < 0 || u_t > 15) 0.5 else 1
    i_opt <- r_star + pi_t + 0.6 * taylor_mult * (pi_t - pi_star) + 0.4 * taylor_mult * output_gap
    diff <- i_chosen - i_opt
    
    # Credibility effect
    if (rv$month > 0) {
      if (abs(i_chosen - rv$interest[currIndex]) > 2) {
        rv$credibility <- max(30, rv$credibility - 15)
      } else if (rv$lastDecision == "maintain") {
        rv$credibility <- min(100, rv$credibility + 5)
      }
    }
    
    # --- ECON DYNAMICS (simplified, can replace with your own model) ---
    # Policy lag buffer
    buffer_index <- length(rv$policy_buffer$rate) + 1
    rv$policy_buffer$rate[buffer_index] <- diff
    rv$policy_buffer$effect[buffer_index] <- 2
    delayed_effect <- 0
    if (length(rv$policy_buffer$effect) > 0) {
      rv$policy_buffer$effect <- rv$policy_buffer$effect - 1
      due_indices <- which(rv$policy_buffer$effect == 0)
      if (length(due_indices) > 0) {
        delayed_effect <- sum(rv$policy_buffer$rate[due_indices])
        rv$policy_buffer$rate <- rv$policy_buffer$rate[-due_indices]
        rv$policy_buffer$effect <- rv$policy_buffer$effect[-due_indices]
      }
    }
    total_effect <- diff + delayed_effect
    
    # decide how many shocks this game may have
    shockCap <- switch(rv$difficulty,
                       "hard"   = 6,   # up to 6 shocks on Hard
                       "medium" = 4,   # 4 on Medium
                       "easy"   = 3)   # 3 on Easy
    
    
    # --- SHOCKS (simplified example, use your shock system here) ---
    if (curr >= rv$nextShockMonth && rv$shockCount < shockCap && is.null(rv$shock)) {
      # Choose a new shock from your list of scenarios
      # ---------- choose a shock that has not been used yet ----------------
      diff_mult <- ifelse(rv$difficulty == "hard", 1.5, 1)
      
      unused <- shockList[ !sapply(shockList, `[[`, "key") %in% rv$shockKeysUsed ]
      if (length(unused) == 0) unused <- shockList          # all used: allow repeats
      
      rv$shock <- econ_start_new_shock(unused, diff_mult)
      playShock()
      rv$shockKeysUsed <- c(rv$shockKeysUsed, rv$shock$key) # mark it as used
      
      if (rv$language == "tr") {
        rv$shockNews <- rv$shock$headline_tr  %||% rv$shock$headline
        rv$shockDesc <- rv$shock$description_tr %||% rv$shock$description
      } else {
        rv$shockNews <- rv$shock$headline
        rv$shockDesc <- rv$shock$description
      }
      rv$shockType <- rv$shock$type          # NEW: Track type (e.g. demand, supply)
      # ---------- choose micro-headline ----------------------------------
      shock_type <- rv$shock$type
      sev        <- rv$shock$severity
      cand <- subset(headline_db, type == shock_type & severity == sev)
      if (nrow(cand) == 0) cand <- headline_db    # fallback if none match
      row  <- cand[sample.int(nrow(cand), 1), ]
      rv$microHeadline <- if (rv$language == "tr") row$tr else row$en
      rv$headlineTurn  <- rv$month
      rv$shockCount <- rv$shockCount + 1
      rv$nextShockMonth <- curr + if (rv$difficulty == "hard") sample(3:6, 1)
      else                       sample(6:10, 1)
      new_shock <- data.frame(
        Ay = as.integer(curr + 1),
        Olay = as.character(rv$shock$key),
        Etki = as.character(rv$shock$severity),
        stringsAsFactors = FALSE
      )
      
      # Reorder to match the original structure
      new_shock <- new_shock[, c("Ay", "Olay", "Etki")]

      # Make sure both dataframes have the same structure
      if (nrow(rv$shockHistory) == 0) {
        rv$shockHistory <- new_shock
      } else {
        rv$shockHistory <- dplyr::bind_rows(rv$shockHistory, new_shock)
      }
      
    } else if (!is.null(rv$shock)) {
      rv$shock <- econ_update_shock(rv$shock, decay_rate = 0.85)
      if (rv$shock$duration <= 0 || rv$shock$mag < 0.1) {
        rv$shockNews <- "Previous shock faded."
        rv$shockDesc <- ""
        rv$shockType <- ""
        rv$shock <- NULL
        stopShock()
      } else {
        if (rv$language == "tr") {
          rv$shockNews <- rv$shock$headline_tr  %||% rv$shock$headline
          rv$shockDesc <- rv$shock$description_tr %||% rv$shock$description
        } else {
          rv$shockNews <- rv$shock$headline
          rv$shockDesc <- rv$shock$description
        }
        rv$shockType <- rv$shock$type
      }
    } else {
      rv$shockNews <- "No shock."
      rv$shockDesc <- ""
      rv$shockType <- ""
    }
    
    # --- Apply Economic Effects ---
    shock_inf   <- if (!is.null(rv$shock)) rv$shock$inflEffect * rv$shock$mag else 0
    shock_unemp <- if (!is.null(rv$shock)) rv$shock$unempEffect * rv$shock$mag else 0
    shock_gdp   <- if (!is.null(rv$shock)) rv$shock$gdpEffect * rv$shock$mag else 0
    cred_mult <- 0.5 + (rv$credibility / 200)
    k_inf  <- 0.30 * (1 + 0.07 * max(0, pi_t - 10)) * cred_mult
    k_unemp <- 0.25 * (1 - 0.04 * min(8, u_t)) * cred_mult
    k_gdp <- 0.30
    
    eps_inf   <- rnorm(1, mean = 0, sd = 0.1)
    eps_unemp <- rnorm(1, mean = 0, sd = 0.08)
    eps_gdp   <- rnorm(1, mean = 0, sd = 0.15)

    
    rv$interest[nextIndex] <- i_chosen
    # new: only 95 % of last month “decays”, 5 % pulled to target
    rv$infl[nextIndex] <- 0.95 * pi_t + 0.05 * pi_star +
      (-k_inf * total_effect) + shock_inf + eps_inf
    phillips_effect <- -0.3 * (rv$infl[nextIndex] - pi_t)
    rv$unemp[nextIndex] <- 0.80 * u_t + 0.20 * u_star + (k_unemp * total_effect) + shock_unemp + phillips_effect + eps_unemp
    rv$gdp_growth[nextIndex] <- 0.7 * rv$gdp_growth[currIndex] + 0.3 * 3.0 + (-k_gdp * total_effect) + shock_gdp + eps_gdp
    
    rv$infl[nextIndex] <- max(-2, min(40, rv$infl[nextIndex]))
    rv$unemp[nextIndex] <- max(3, min(25, rv$unemp[nextIndex]))
    rv$gdp_growth[nextIndex] <- max(-10, min(15, rv$gdp_growth[nextIndex]))
    
    rv$month <- curr + 1
    
    # ---------- end-of-game verdict -------------------------------------
    if (rv$month >= rv$max_months) {
      
      # simple score: average absolute deviation from targets
      infl_dev <- mean(abs(rv$infl[2:(rv$month+1)] - 5.0))
      unemp_dev <- mean(abs(rv$unemp[2:(rv$month+1)] - 8.0))
      score    <- infl_dev*1.5 + unemp_dev
      ## set stricter threshold for higher difficulty
      threshold <- switch(rv$difficulty,
                          hard   = 1.5,
                          medium = 2.0,
                          easy   = 2.5
                          )
      if (score <= threshold && rv$credibility >= 60) {
        rv$result <- translations[[rv$language]]$verdict_hired
        } else {
          rv$result <- translations[[rv$language]]$verdict_fired
          }
      
      showModal(modalDialog(
        title = "Final verdict",
        rv$result,
        easyClose = TRUE, footer = NULL
      ))
      
      # (optional) disable main action button
      shinyjs::disable("goBtn")
    }
    
    
    if (rv$month - rv$headlineTurn >= 2) {
      rv$microHeadline <- ""
    }
    
    # ---------- clear 'advisor dismissed' headline after 2 turns ------------
    if (!is.na(rv$fired_headline_turn) &&
        (rv$month - rv$fired_headline_turn) >= 2) {
      
      rv$shockNews <- translations[[rv$language]]$shock_no_event
      rv$fired_headline_turn <- NA
    }
    
    # --- Update press statement using function
    press <- econ_generate_press(
      lastDecision   = rv$lastDecision,
      lastRateChange = rv$lastRateChange,
      infl           = rv$infl[rv$month + 1],
      unemp          = rv$unemp[rv$month + 1],
      has_shock      = !is.null(rv$shock),
      month          = rv$month,
      language       = rv$language
    )
    rv$pressTitle     <- press$title
    rv$pressStatement <- press$statement
  })
  
  # --- ADVISOR OUTPUTS (Tab) ---
  output$opt1_name     <- renderText({ rv$advisor_options[[1]]$name })
  output$opt1_rate     <- renderText({ sprintf(translations[[rv$language]]$recommended_rate, sprintf("%.2f", rv$advisor_options[[1]]$rate)) })
  output$opt_rationale1 <- renderUI({ HTML(rv$advisor_options[[1]]$rationale) })
  output$opt_press1    <- renderUI({ HTML(rv$advisor_options[[1]]$press) })
  output$opt2_name     <- renderText({ rv$advisor_options[[2]]$name })
  output$opt2_rate     <- renderText({ sprintf(translations[[rv$language]]$recommended_rate, sprintf("%.2f", rv$advisor_options[[2]]$rate)) })
  output$opt_rationale2 <- renderUI({ HTML(rv$advisor_options[[2]]$rationale) })
  output$opt_press2    <- renderUI({ HTML(rv$advisor_options[[2]]$press) })
  output$opt3_name     <- renderText({ rv$advisor_options[[3]]$name })
  output$opt3_rate     <- renderText({ sprintf(translations[[rv$language]]$recommended_rate, sprintf("%.2f", rv$advisor_options[[3]]$rate)) })
  output$opt_rationale3 <- renderUI({ HTML(rv$advisor_options[[3]]$rationale) })
  output$opt_press3    <- renderUI({ HTML(rv$advisor_options[[3]]$press) })
  
  # --- Advisor Dismissal Notices ---
  output$opt1_notice <- renderUI({
    req(rv$advisor_options[[1]]$notice)
    span(rv$advisor_options[[1]]$notice, style = "font-style: italic;")
  })
  output$opt2_notice <- renderUI({
    req(rv$advisor_options[[2]]$notice)
    span(rv$advisor_options[[2]]$notice, style = "font-style: italic;")
  })
  output$opt3_notice <- renderUI({
    req(rv$advisor_options[[3]]$notice)
    span(rv$advisor_options[[3]]$notice, style = "font-style: italic;")
  })
  
  
  
  # Helper to snap any rate to your 0.25-step grid  ---------------------------
  nearest_quarter <- function(x) round(x / 0.25) * 0.25
  
  # --- Advisor 1 -------------------------------------------------------------
  observeEvent(input$apply_opt1, {
    sel_rate <- nearest_quarter(rv$advisor_options[[1]]$rate)          # NEW
    updateNumericInput(session, "interest", value = sel_rate)          # CHANGED
    rv$pressStatement <- rv$advisor_options[[1]]$press
    rv$pressTitle     <- translations[[rv$language]]$advisor_opt1_press_title
    rv$lastApplied$opt1 <- rv$month
  })
  
  # --- Advisor 2 -------------------------------------------------------------
  observeEvent(input$apply_opt2, {
    sel_rate <- nearest_quarter(rv$advisor_options[[2]]$rate)          # NEW
    updateNumericInput(session, "interest", value = sel_rate)          # CHANGED
    rv$pressStatement <- rv$advisor_options[[2]]$press
    rv$pressTitle     <- translations[[rv$language]]$advisor_opt2_press_title
    rv$lastApplied$opt2 <- rv$month
  })
  
  # --- Advisor 3 -------------------------------------------------------------
  observeEvent(input$apply_opt3, {
    sel_rate <- nearest_quarter(rv$advisor_options[[3]]$rate)          # NEW
    updateNumericInput(session, "interest", value = sel_rate)          # CHANGED
    rv$pressStatement <- rv$advisor_options[[3]]$press
    rv$pressTitle     <- translations[[rv$language]]$advisor_opt3_press_title
    rv$lastApplied$opt3 <- rv$month
  })
  
  # ---------- FIRE ADVISOR 1 ----------------------------------------------
  observeEvent(input$fire_opt1, {
    showModal(modalDialog(
      title = if (rv$language == "tr") "Danışmanı kov?" else "Dismiss advisor?",
      paste(rv$advisor_options[[1]]$name),
      footer = tagList(
        modalButton(if (rv$language=="tr") "İptal" else "Cancel"),
        actionButton("confirm_fire1",
                     if (rv$language=="tr") "Evet, kov" else "Yes, dismiss",
                     class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_fire1, {
    removeModal()
    fire_advisor("opt1")
  })
  
  # ---------- FIRE ADVISOR 2 ----------------------------------------------
  observeEvent(input$fire_opt2, {
    showModal(modalDialog(
      title = if (rv$language == "tr") "Danışmanı kov?" else "Dismiss advisor?",
      paste(rv$advisor_options[[2]]$name),
      footer = tagList(
        modalButton(if (rv$language=="tr") "İptal" else "Cancel"),
        actionButton("confirm_fire2",
                     if (rv$language=="tr") "Evet, kov" else "Yes, dismiss",
                     class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_fire2, {
    removeModal()
    fire_advisor("opt2")
  })
  
  # ---------- FIRE ADVISOR 3 ----------------------------------------------
  observeEvent(input$fire_opt3, {
    showModal(modalDialog(
      title = if (rv$language == "tr") "Danışmanı kov?" else "Dismiss advisor?",
      paste(rv$advisor_options[[3]]$name),
      footer = tagList(
        modalButton(if (rv$language=="tr") "İptal" else "Cancel"),
        actionButton("confirm_fire3",
                     if (rv$language=="tr") "Evet, kov" else "Yes, dismiss",
                     class = "btn-danger")
      )
    ))
  })
  observeEvent(input$confirm_fire3, {
    removeModal()
    fire_advisor("opt3")
  })
  
  
  # --- ECON INDICATOR OUTPUTS (RIGHT PANEL) ---
  output$currentInflation <- renderText({ paste0("%", sprintf("%.1f", rv$infl[rv$month + 1])) })
  output$currentUnemployment <- renderText({ paste0("%", sprintf("%.1f", rv$unemp[rv$month + 1])) })
  output$inflationDeviation <- renderText({
    sprintf(translations[[rv$language]]$target_deviation, sprintf("%+.1f%%", rv$infl[rv$month + 1] - 5.0))
  })
  output$unemploymentDeviation <- renderText({
    sprintf(translations[[rv$language]]$target_deviation, sprintf("%+.1f%%", rv$unemp[rv$month + 1] - 8.0))
  })
  output$economicHealth <- renderText({
    infl_score <- pmax(0, 100 - abs(rv$infl[rv$month + 1] - 5.0) * 10)
    unemp_score <- pmax(0, 100 - abs(rv$unemp[rv$month + 1] - 8.0) * 12)
    health_score <- pmax(0, (infl_score * 0.6 + unemp_score * 0.4))
    paste0(sprintf("%.0f", health_score), "/100")
  })
  output$healthDescription <- renderText({
    infl_score <- pmax(0, 100 - abs(rv$infl[rv$month + 1] - 5.0) * 10)
    unemp_score <- pmax(0, 100 - abs(rv$unemp[rv$month + 1] - 8.0) * 12)
    health_score <- pmax(0, (infl_score * 0.6 + unemp_score * 0.4))
    health_desc <- list(
      tr = c(
        health_score >= 90 ~ "Mükemmel",
        health_score >= 75 ~ "İyi",
        health_score >= 60 ~ "Orta",
        health_score >= 40 ~ "Riskli",
        TRUE ~ "Kritik"
      ),
      en = c(
        health_score >= 90 ~ "Excellent",
        health_score >= 75 ~ "Good",
        health_score >= 60 ~ "Moderate",
        health_score >= 40 ~ "Risky",
        TRUE ~ "Critical"
      )
    )
    health_desc[[rv$language]][which.min(abs(as.numeric(names(health_desc[[rv$language]])) - health_score))]
  })
  output$credibilityScore <- renderText({ paste0(sprintf("%.0f", rv$credibility), "/100") })
  output$monthDisplay <- renderText({ sprintf(translations[[rv$language]]$month_display, rv$month, rv$max_months) })
  output$performanceScore <- renderText({
    if (rv$month > 0) {
      infl_dev <- mean(abs(rv$infl[2:(rv$month + 1)] - 5.0))
      unemp_dev <- mean(abs(rv$unemp[2:(rv$month + 1)] - 8.0))
      score <- pmax(0, 100 - ((infl_dev * 1.5 + unemp_dev) * 8))
      sprintf(translations[[rv$language]]$performance_score, sprintf("%.0f", score))
    } else {
      sprintf(translations[[rv$language]]$performance_score, "--")
    }
  })
  output$gameProgress <- renderUI({
    progress_val <- round((rv$month / rv$max_months) * 100)
    div(
      div(style = paste0("width: ", progress_val, "%; background: linear-gradient(90deg, #0d6efd, #4facfe); height: 20px; border-radius: 10px; transition: width 0.3s ease;"),
          class = "progress-bar"),
      style = "width: 100%; background-color: #e9ecef; border-radius: 10px; margin: 10px 0;"
    )
  })
  output$shockHeadline <- renderText({ rv$shockNews })
  output$actionHeadline <- renderText({ rv$actionNews })
  output$microHeadline <- renderText({ rv$microHeadline })
  output$pressTitle <- renderText({ rv$pressTitle })
  output$pressStatement <- renderText({ rv$pressStatement })
  output$decisionsCount <- renderText({ as.character(rv$month) })
  output$avgDeviation <- renderText({
    if (rv$month > 0) {
      infl_dev <- mean(abs(rv$infl[2:(rv$month + 1)] - 5.0))
      unemp_dev <- mean(abs(rv$unemp[2:(rv$month + 1)] - 8.0))
      sprintf("%.1f", (infl_dev + unemp_dev) / 2)
    } else {
      "0.0"
    }
  })
  output$gameResult <- renderText({ rv$result })
  
  output$shockDescription <- renderText({ rv$shockDesc })
  output$shockType <- renderText({
    if (!is.null(rv$shockType) && nzchar(rv$shockType)) {
      paste("Shock type:", tools::toTitleCase(rv$shockType), "shock")
    } else { "" }
  })
  
  
  # --- TIME SERIES PLOT ---
  output$timeSeriesPlot <- renderPlotly({
    req(rv$month >= 0)
    months <- 0:rv$month
    df <- data.frame(
      Month = months,
      Inflation = rv$infl[1:(rv$month + 1)],
      Unemployment = rv$unemp[1:(rv$month + 1)],
      Policy_Rate = rv$interest[1:(rv$month + 1)],
      GDP_Growth = rv$gdp_growth[1:(rv$month + 1)]
    )
    colnames(df) <- c(
      translations[[rv$language]]$month,
      translations[[rv$language]]$inflation,
      translations[[rv$language]]$unemployment,
      translations[[rv$language]]$policy_rate,
      translations[[rv$language]]$gdp_growth
    )
    color_map <- setNames(
      c("#dc3545", "#0d6efd", "#198754", "#ffc107"),
      c(translations[[rv$language]]$inflation,
        translations[[rv$language]]$unemployment,
        translations[[rv$language]]$policy_rate,
        translations[[rv$language]]$gdp_growth)
    )
    p <- ggplot(df, aes(x = .data[[colnames(df)[1]]])) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 4.0, ymax = 6.0, alpha = 0.1, fill = "#dc3545") +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 7.0, ymax = 9.0, alpha = 0.1, fill = "#0d6efd") +
      geom_line(aes(y = .data[[colnames(df)[2]]], color = colnames(df)[2]), size = 1.2) +
      geom_line(aes(y = .data[[colnames(df)[3]]], color = colnames(df)[3]), size = 1.2) +
      geom_line(aes(y = .data[[colnames(df)[4]]], color = colnames(df)[4]), size = 1.2) +
      geom_line(aes(y = .data[[colnames(df)[5]]], color = colnames(df)[5]), size = 1.0, alpha = 0.8) +
      geom_hline(yintercept = 5.0, color = "#dc3545", linetype = "dashed", alpha = 0.6) +
      geom_hline(yintercept = 8.0, color = "#0d6efd", linetype = "dashed", alpha = 0.6) +
      scale_color_manual(name = translations[[rv$language]]$indicators, values = color_map) +
      labs(x = translations[[rv$language]]$month, y = translations[[rv$language]]$value, 
           title = translations[[rv$language]]$econ_indicators) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    ggplotly(p, tooltip = "y") %>% 
      layout(hovermode = "x unified",
             colorway = cb_palette[1:4])
  })
  
  # --- PHILLIPS CURVE ---
  output$phillipsCurve <- renderPlotly({
    req(rv$month > 0)
    months <- 1:rv$month
    data_plot <- data.frame(
      Inflation = rv$infl[months + 1],
      Unemployment = rv$unemp[months + 1],
      Month = months
    )
    colnames(data_plot) <- c(translations[[rv$language]]$inflation,
                             translations[[rv$language]]$unemployment,
                             translations[[rv$language]]$month)
    p <- ggplot(data_plot, aes(x = .data[[translations[[rv$language]]$unemployment]], 
                               y = .data[[translations[[rv$language]]$inflation]], 
                               color = .data[[translations[[rv$language]]$month]])) +
      geom_point(size = 3, alpha = 0.8) +
      geom_path(alpha = 0.6) +
      geom_hline(yintercept = 5.0, color = "#dc3545", linetype = "dashed", alpha = 0.7) +
      geom_vline(xintercept = 8.0, color = "#0d6efd", linetype = "dashed", alpha = 0.7) +
      scale_color_gradient(low = "#4facfe", high = "#f093fb") +
      labs(title = sprintf("%s - %s vs %s", translations[[rv$language]]$phillips_curve, 
                           translations[[rv$language]]$inflation, translations[[rv$language]]$unemployment),
           x = sprintf("%s (%%)", translations[[rv$language]]$unemployment), 
           y = sprintf("%s (%%)", translations[[rv$language]]$inflation)) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # --- POLICY ANALYSIS ---
  output$policyAnalysis <- renderPlotly({
    req(rv$month > 0)
    months <- 1:rv$month
    data_plot <- data.frame(
      Month = months,
      Applied_Rate = rv$interest[months + 1],
      Inflation = rv$infl[months + 1],
      Optimal_Rate = 3.0 + rv$infl[months + 1] + 0.6 * (rv$infl[months + 1] - 5.0) + 0.4 * (8.0 - rv$unemp[months + 1])
    )
    colnames(data_plot) <- c(translations[[rv$language]]$month,
                             translations[[rv$language]]$applied_rate,
                             translations[[rv$language]]$inflation,
                             translations[[rv$language]]$optimal_rate)
    color_map <- setNames(
      c("#198754", "#dc3545", "#28a745"),
      c(translations[[rv$language]]$applied_rate,
        translations[[rv$language]]$inflation,
        translations[[rv$language]]$optimal_rate)
    )
    p <- ggplot(data_plot, aes(x = .data[[colnames(data_plot)[1]]])) +
      geom_line(aes(y = .data[[colnames(data_plot)[2]]], color = colnames(data_plot)[2]), size = 1.2) +
      geom_line(aes(y = .data[[colnames(data_plot)[4]]], color = colnames(data_plot)[4]), size = 1.2, linetype = "dashed") +
      geom_line(aes(y = .data[[colnames(data_plot)[3]]], color = colnames(data_plot)[3]), size = 1.2, alpha = 0.7) +
      scale_color_manual(name = translations[[rv$language]]$indicator, values = color_map) +
      labs(title = sprintf("%s - %s vs %s", translations[[rv$language]]$policy_analysis,
                           translations[[rv$language]]$interest_rates, translations[[rv$language]]$inflation),
           x = translations[[rv$language]]$months, y = translations[[rv$language]]$rate) +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "transparent"),
            panel.background = element_rect(fill = "transparent"),
            text = element_text(color = "white"),
            legend.text = element_text(color = "white"),
            axis.text = element_text(color = "white"),
            plot.title = element_text(hjust = 0.5, face = "bold"),
            legend.position = "bottom")
    ggplotly(p, tooltip = c("x", "y", "colour")) %>% layout(hovermode = "x unified")
  })
  
  # --- SHOCK HISTORY TABLE ---
  output$shockHistory <- renderDataTable({
    shock_history_df <- rv$shockHistory
    if (nrow(shock_history_df) == 0) {
      # Show a blank row or a message instead of table if no shocks yet
      shock_history_df <- data.frame(
        Ay = integer(),
        Olay = character(),
        Etki = character(),
        stringsAsFactors = FALSE
      )
    } else {
      # Only do translation if the column exists and has data
      shock_history_df$Olay <- sapply(shock_history_df$Olay, function(key) {
        val <- translations[[rv$language]][[key]]
        if (is.null(val)) key else val
      })
      shock_history_df$Etki <- sapply(shock_history_df$Etki, function(sev_code) {
        translations[[rv$language]][[paste0("shock_severity_", sev_code)]]
      })
    }
    colnames(shock_history_df) <- c(translations[[rv$language]]$month,
                                    translations[[rv$language]]$event,
                                    translations[[rv$language]]$impact)
    DT::datatable(shock_history_df,
                  options = list(pageLength = 5, searching = FALSE, info = FALSE)) %>%
      DT::formatStyle(translations[[rv$language]]$impact,
                      backgroundColor = DT::styleEqual(
                        c(translations[[rv$language]]$shock_severity_high,
                          translations[[rv$language]]$shock_severity_medium,
                          translations[[rv$language]]$shock_severity_low),
                        c("#f8d7da", "#fff3cd", "#d4edda")
                      ))
  })
  
  
  # --- DOWNLOAD GAME DATA (CSV) ---
  output$downloadData <- downloadHandler(
    filename = function() { sprintf(translations[[rv$language]]$download_filename, Sys.Date()) },
    content = function(file) {
      req(rv$month > 0)
      months <- 1:rv$month
      data_export <- data.frame(
        Month = months,
        Inflation = rv$infl[months + 1],
        Unemployment = rv$unemp[months + 1],
        Policy_Rate = rv$interest[months + 1],
        GDP_Growth = rv$gdp_growth[months + 1]
      )
      colnames(data_export) <- c(translations[[rv$language]]$month,
                                 translations[[rv$language]]$inflation,
                                 translations[[rv$language]]$unemployment,
                                 translations[[rv$language]]$policy_rate,
                                 translations[[rv$language]]$gdp_growth)
      write.csv(data_export, file, row.names = FALSE)
    }
  )
  
  # Press Conference Modal (Q&A session)
  observeEvent(input$start_press, {
    req(rv$month > 0)
    # Generate a list of press questions tailored to current state
    rv$press_questions <- generate_press_questions(
      infl = rv$infl[rv$month + 1],
      unemp = rv$unemp[rv$month + 1],
      cred = rv$credibility,
      last_decision = rv$lastDecision,
      shock = rv$shock,
      language = rv$language
    )
    rv$current_question_index <- 1
    
    # Show the press conference modal dialog
    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$span(translations[[rv$language]]$press_conference_title, style = "font-size: 20px;"),
        actionButton("end_press_conference", translations[[rv$language]]$end_press, class = "btn-danger btn-sm")
      ),
      fluidRow(
        column(width = 6,
               card(
                 card_header(translations[[rv$language]]$press_qa, style = "font-size: 16px;"),
                 card_body(
                   uiOutput("current_question"),
                   textAreaInput("press_response", label = NULL, 
                                 placeholder = translations[[rv$language]]$response_placeholder,
                                 rows = 4, width = "100%"),
                   actionButton("submit_response", translations[[rv$language]]$submit_response, class = "btn-primary btn-sm")
                 )
               ),
               uiOutput("response_feedback")
        ),
        column(width = 6,
               card(
                 card_header(translations[[rv$language]]$prep_notes, style = "font-size: 16px;"),
                 card_body(
                   textAreaInput("prep_notes", label = NULL, 
                                 placeholder = translations[[rv$language]]$notes_placeholder,
                                 rows = 6, width = "100%"),
                   h5(translations[[rv$language]]$eval_criteria, style = "margin-top: 15px; font-size: 14px;"),
                   tags$ul(
                     tags$li(translations[[rv$language]]$clarity),
                     tags$li(translations[[rv$language]]$consistency),
                     tags$li(translations[[rv$language]]$confidence),
                     tags$li(translations[[rv$language]]$data_usage)
                   )
                 )
               )
        )
      ),
      footer = NULL,
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # Display the current press question in the modal
  output$current_question <- renderUI({
    req(length(rv$press_questions) > 0)
    question <- rv$press_questions[[rv$current_question_index]]
    tags$div(
      p(strong(sprintf(translations[[rv$language]]$reporter, question$reporter))),
      p(em(sprintf(translations[[rv$language]]$media, question$media))),
      p(question$text, style = "font-size: 16px; margin-top: 10px;")
    )
  })
  
  # Handle submitted press conference response
  observeEvent(input$submit_response, {
    req(input$press_response)
    # Evaluate the player's response to the current question
    evaluation <- evaluate_response(
      response = input$press_response,
      question = rv$press_questions[[rv$current_question_index]]$text,
      last_decision = rv$lastDecision,
      infl = rv$infl[rv$month + 1],
      unemp = rv$unemp[rv$month + 1],
      language = rv$language
    )
    # Update credibility based on response score (scaled to max ±10 points)
    cred_change <- round(evaluation$score / 100 * 10)
    rv$credibility <- min(100, max(30, rv$credibility + cred_change))
    # Show feedback for this response
    output$response_feedback <- renderUI({
      card(
        card_header(translations[[rv$language]]$feedback_title, style = "font-size: 16px;"),
        card_body(
          p(sprintf(translations[[rv$language]]$score, evaluation$score)),
          p(sprintf(translations[[rv$language]]$cred_change, if (cred_change >= 0) "+" else "", cred_change)),
          h5(translations[[rv$language]]$feedback_improve, style = "margin-top: 15px;"),
          tags$ul(lapply(evaluation$feedback, tags$li))
        )
      )
    })
    # Advance to the next question or end the press conference if done
    if (rv$current_question_index < length(rv$press_questions)) {
      rv$current_question_index <- rv$current_question_index + 1
      updateTextAreaInput(session, "press_response", value = "")
    } else {
      showNotification(translations[[rv$language]]$press_complete, type = "message")
      removeModal()
    }
  })
  
  # End press conference early
  observeEvent(input$end_press_conference, {
    showNotification(translations[[rv$language]]$press_complete, type = "message")
    removeModal()
  })
}
