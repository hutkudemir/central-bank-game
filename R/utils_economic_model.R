# --- project_root/R/utils_economic_model.R ---
# Helper functions for economic simulation logic

# Ensure translations are loaded
source("R/translations.R")

# Start a new random economic shock from the predefined shock list
start_new_shock <- function(shock_list, diff_mult) {
  shock_def <- sample(shock_list, 1)[[1]]
  list(
    key = shock_def$key,
    headline = translations[[rv$language]][[shock_def$key]],
    inflEffect = shock_def$inflEffect * diff_mult,
    unempEffect = shock_def$unempEffect * diff_mult,
    gdpEffect = shock_def$gdpEffect * diff_mult,
    severity = shock_def$severity,
    duration = sample(2:4, 1),
    mag = runif(1, 0.8, 1.2)
  )
}

# Generate an official press statement (title and body) after each policy decision
generate_press_statement <- function(decision, rate_change, infl, unemp, shock_active, month, language) {
  # Title example: "Monetary Policy Committee Meeting 5 Decision"
  title <- if (language == "tr") {
    sprintf("Para Politikası Kurulu %d. Toplantı Kararı", month + 1)
  } else {
    sprintf("Monetary Policy Committee Meeting %d Decision", month + 1)
  }
  # Base statement depending on decision type
  if (decision == "maintain") {
    statement <- translations[[language]]$press_maintain
  } else if (decision == "increase") {
    statement <- sprintf(translations[[language]]$press_increase, rate_change)
  } else {  # decrease
    statement <- sprintf(translations[[language]]$press_decrease, rate_change)
  }
  # Additional context if needed
  if (infl > 10) {
    # High inflation context
    statement <- paste(statement, sprintf(translations[[language]]$context_high_infl, infl))
  } else if (unemp > 10) {
    # High unemployment context
    statement <- paste(statement, sprintf(translations[[language]]$context_high_unemp, unemp))
  }
  if (shock_active) {
    # Mention active shocks
    statement <- paste(statement, translations[[language]]$context_shock)
  }
  return(list(title = title, statement = statement))
}

# Generate a list of press conference questions based on the current economic situation
generate_press_questions <- function(infl, unemp, cred, last_decision, shock, language) {
  reporters <- list(
    tr = c("Ayşe Yılmaz", "Mehmet Demir", "Fatma Kaya", "Ahmet Şahin", "Zeynep Çelik"),
    en = c("Alice Smith", "Bob Johnson", "Clara Brown", "David Wilson", "Emma Taylor")
  )[[language]]
  media_outlets <- list(
    tr = c("Hürriyet", "CNNTürk", "BloombergHT", "Sabah", "Ekonomist"),
    en = c("Reuters", "Bloomberg", "Financial Times", "The Economist", "CNBC")
  )[[language]]
  
  questions <- list()
  # Question 1: Inflation justification
  questions[[1]] <- list(
    text = sprintf(translations[[language]]$question_infl, infl, translations[[language]][[paste0("decision_", last_decision)]]),
    reporter = sample(reporters, 1),
    media = sample(media_outlets, 1)
  )
  # Question 2: Currency fluctuations
  questions[[2]] <- list(
    text = translations[[language]]$question_currency,
    reporter = sample(reporters, 1),
    media = sample(media_outlets, 1)
  )
  # Question 3: Unemployment or future roadmap
  if (unemp > 10) {
    questions[[3]] <- list(
      text = sprintf(translations[[language]]$question_unemp, unemp),
      reporter = sample(reporters, 1),
      media = sample(media_outlets, 1)
    )
  } else {
    questions[[3]] <- list(
      text = translations[[language]]$question_roadmap,
      reporter = sample(reporters, 1),
      media = sample(media_outlets, 1)
    )
  }
  # Question 4: Credibility or shock response
  if (cred < 50) {
    # Low credibility scenario
    low_cred_qs <- c(translations[[language]]$question_cred_low1,
                     translations[[language]]$question_cred_low2)
    questions[[4]] <- list(
      text = sample(low_cred_qs, 1),
      reporter = sample(reporters, 1),
      media = sample(media_outlets, 1)
    )
  } else if (!is.null(shock)) {
    # Ongoing shock scenario
    questions[[4]] <- list(
      text = sprintf(translations[[language]]$question_shock, gsub("[🛢️📉💰💱🏭🌾🏠📱🚢⚡]", "", shock$headline)),
      reporter = sample(reporters, 1),
      media = sample(media_outlets, 1)
    )
  } else {
    # General credibility question if no shock and credibility is okay
    questions[[4]] <- list(
      text = sprintf(translations[[language]]$question_cred, cred),
      reporter = sample(reporters, 1),
      media = sample(media_outlets, 1)
    )
  }
  return(questions)
}

# Evaluate the player's press conference response and provide a score and feedback
evaluate_response <- function(response, question, last_decision, infl, unemp, language) {
  score <- 0
  feedback <- list()
  
  # Clarity: length of response
  word_count <- lengths(strsplit(response, "\\s+"))
  if (word_count >= 30) {
    score <- score + 25
  } else if (word_count >= 15) {
    score <- score + 15
    feedback <- c(feedback, translations[[language]]$feedback_clarity)
  } else {
    feedback <- c(feedback, translations[[language]]$feedback_clarity_short)
  }
  
  # Consistency: does the response address the question's main topic?
  keywords <- list(
    tr = c("enflasyon", "işsizlik", "faiz", "ekonomi", "politika", "döviz", "güven"),
    en = c("inflation", "unemployment", "rate", "economy", "policy", "currency", "confidence")
  )[[language]]
  question_lower <- tolower(question)
  response_lower <- tolower(response)
  if (grepl("inflation", question_lower) || grepl("enflasyon", question_lower)) {
    if (grepl("inflation", response_lower) || grepl("enflasyon", response_lower)) {
      score <- score + 25
    } else {
      feedback <- c(feedback, if (language == "tr") {
        "Soru enflasyon hakkındaydı, ancak yanıtınızda enflasyondan bahsetmediniz."
      } else {
        "The question was about inflation, but you did not mention inflation in your answer."
      })
    }
  } else if (grepl("unemployment", question_lower) || grepl("işsizlik", question_lower)) {
    if (grepl("unemployment", response_lower) || grepl("işsizlik", response_lower)) {
      score <- score + 25
    } else {
      feedback <- c(feedback, if (language == "tr") {
        "Soru işsizlik ile ilgiliydi, ancak cevabınızda işsizlikten bahsetmediniz."
      } else {
        "The question was about unemployment, but you did not address unemployment in your answer."
      })
    }
  } else {
    if (any(sapply(keywords, function(kw) grepl(kw, response_lower, ignore.case = TRUE)))) {
      score <- score + 25
    } else {
      feedback <- c(feedback, translations[[language]]$feedback_consistency)
    }
  }
  
  # Confidence: assertive tone or emphasis
  if (grepl("!", response) || grepl("\\b(kararlı|kesin|commit|firm)\\b", response, ignore.case = TRUE)) {
    score <- score + 25
  } else {
    feedback <- c(feedback, translations[[language]]$feedback_confidence)
  }
  
  # Data usage: presence of numbers or factual figures
  if (grepl("\\d", response)) {
    score <- score + 25
  } else {
    feedback <- c(feedback, translations[[language]]$feedback_data)
  }
  
  # If perfect score, replace feedback with a special praise
  if (score == 100) {
    feedback <- c(translations[[language]]$feedback_perfect)
  }
  return(list(score = score, feedback = feedback))
}
