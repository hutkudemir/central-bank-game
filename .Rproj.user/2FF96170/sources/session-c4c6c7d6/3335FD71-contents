# --- project_root/ui.R ---
library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(rintrojs)
library(plotly)

# Optional: Google fonts. Comment out if font_google gives warnings.
custom_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary   = cb_palette[1],   # replaces your “primary” blue
  secondary = cb_palette[2],   # replaces your “secondary” red
  success   = cb_palette[3],   # replaces your “success” green
  warning   = cb_palette[4],   # replaces your “warning” yellow
  info      = cb_palette[5],   # add an “info” channel if needed
  # base_font = font_google("Montserrat"),
  # code_font = font_google("Fira Mono")
  )

fluidPage(
  theme = custom_theme,
  useShinyjs(),
  introjsUI(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$style(HTML("
      .compact-panel { margin: 14px 0 8px 0; padding: 12px 16px; background: #222b38cc; border-radius: 14px; }
      .advisor-container { display: flex; flex-wrap: wrap; gap: 16px; margin-top: 14px; }
      .advisor-card { flex: 1 1 250px; min-width: 220px; background: #23272b; border-radius: 18px; box-shadow: 0 1px 6px #0003; margin-bottom: 8px; }
      .advisor-card h5 { font-weight: 600; font-size: 17px; }
      .press-statement { margin: 18px 0 8px 0; }
      .progress-bar { background: linear-gradient(90deg, #0d6efd, #4facfe); height: 20px; border-radius: 10px; }
      .reset-btn, .help-btn { margin-left: 5px; }
    ")),
    tags$audio(
      id      = "bgMusic",
      src     = "ambient2.mp3",
      type    = "audio/mp3",
      loop    = NA,
      autoplay = NA,        
      preload = "auto",
      style   = "display:none;"
      ),
    tags$audio(
      id       = "shockAlarm",
      src      = "shock_alarm.mp3",
      type     = "audio/mp3",
      loop     = NA,          # keep looping while a shock is active
      muted    = NA,          # starts muted to satisfy browsers
      preload  = "auto",      # so it is ready to play instantly
      style    = "display:none;"
    ),
    tags$script(src = "custom.js")
  ),
  
  titlePanel(
    div(style = "display: flex; align-items: center; gap: 16px;",
        icon("chart-bar", class = NULL, lib = "font-awesome"),
        span(textOutput("game_title"), style = "font-weight: 700; font-size: 1.7em; letter-spacing: 1px;")
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      card(
        class = "control-panel",
        card_header(
          div(
            style = "display: flex; align-items: flex-end; gap: 16px;",
            div(
              style = "display: flex; flex-direction: column;",
              strong(textOutput("language_label"), style = "font-size: 13px;"),
              selectInput("language", label = NULL, choices = c("Türkçe" = "tr", "English" = "en"), width = "90px", selected = "tr")
            ),
            div(
              style = "display: flex; flex-direction: column;",
              strong(textOutput("difficulty_label"), style = "font-size: 13px;"),
              selectInput("difficulty", label = NULL,
                          choices = c("Easy"="easy", "Medium"="medium", "Hard"="hard"),
                          selected = "easy",
                          width = "90px")
            ),
            div(
              style = "margin-bottom: 3px;",
              actionButton("helpBtn", label = NULL, icon = icon("question-circle"), class = "help-btn btn-outline-info btn-sm")
            ),
            # ---------- NEW music toggle button ----------
            actionButton("toggleMusic", "🔇 Mute Music",
                         class = "btn-outline-secondary btn-sm")
          )
        ),
        card_body(
          div(style = "font-size: 14px; margin: 6px 0 2px 0;",
              span(textOutput("econ_news")),
              br(),
              strong(textOutput("shockHeadline"),
                     style = "color:#ffc107; font-size:15px;"),
              p(textOutput("shockDescription"),
                style = "color:#ffc107; font-size:13px; margin:0 0 2px 0;"),
              
              ## === NEW line (grey micro-headline) ===============================
              p(textOutput("microHeadline"),
                style = "color:#adb5bd; font-size:13px; margin:0 0 4px 0;"),
              ## ==================================================================
              
              em(textOutput("shockType"),
                 style = "color:#ffc107; font-size:12px; margin:0 0 4px 0;"),
              br(),
              strong(textOutput("actionHeadline"),
                     style = "color:#4facfe; font-size:15px;"),
              br()
          ),
          div(style = "font-size: 13px; margin: 8px 0;",
              fluidRow(
                column(7, strong(textOutput("inflation")), textOutput("currentInflation")),
                column(5, strong(textOutput("unemployment")), textOutput("currentUnemployment"))
              ),
              fluidRow(
                column(6, textOutput("inflationDeviation")),
                column(6, textOutput("unemploymentDeviation"))
              ),
              fluidRow(
                column(6, strong(textOutput("econ_health")), textOutput("economicHealth"), textOutput("healthDescription")),
                column(6, strong(textOutput("credibility")), textOutput("credibilityScore"))
              )
          ),
          div(style = "margin: 6px 0;",
              strong(textOutput("interest_control")),
              numericInput("interest", label = NULL, value = 12.0, min = 0, max = 50, step = 0.25, width = "60%"),
              actionButton("decreaseBtn", label = textOutput("decrease_btn"), icon = icon("chevron-down"), class = "btn-outline-danger btn-sm"),
              actionButton("increaseBtn", label = textOutput("increase_btn"), icon = icon("chevron-up"), class = "btn-outline-success btn-sm"),
              actionButton("goBtn", label = textOutput("apply_decision"), class = "btn-primary btn-sm", style = "margin-left: 7px;")
          ),
          div(style = "margin-top: 8px; font-size: 13px;",
              strong(textOutput("policy_rate_lbl")),   # the static label
              textOutput("policy_rate_val"),           # the changing number
              br(),
              strong(textOutput("inflationTarget")),
              br(),
              strong(textOutput("unemploymentTarget"))
          ),
          hr(),
          strong(textOutput("game_progress")),
          uiOutput("gameProgress"),
          br(),
          strong(textOutput("shock_history")),
          dataTableOutput("shockHistory")
        )
      )
    ),
    
    column(
      width = 8,
      tabsetPanel(
        id = "chartTabs",
        tabPanel(textOutput("time_series"), plotlyOutput("timeSeriesPlot")),
        tabPanel(textOutput("phillips_curve"), plotlyOutput("phillipsCurve")),
        tabPanel(textOutput("policy_analysis"), plotlyOutput("policyAnalysis")),
        tabPanel(textOutput("advisor_recommendations"),
                 div(class = "advisor-container",
                     # --- Advisor 1 ---
                     div(class = "advisor-card p-3",
                         h5(textOutput("opt1_name")),
                         div(textOutput("opt1_rate"), style = "font-size: 14px; margin-bottom: 4px;"),
                         htmlOutput("opt_rationale1", style = "font-size: 13px; margin-bottom: 4px;"),
                         htmlOutput("opt_press1",     style = "font-size: 12px; color:#8ec4ff; margin-bottom: 6px;"),
                         htmlOutput("opt1_notice",    style = "font-size: 12px; color:#dc3545; margin-bottom: 6px;"),
                         
                         # single row with both buttons
                         div(style = "display:flex; gap:6px; margin-top:7px;",
                             actionButton("apply_opt1", label = "",       class = "btn-outline-primary btn-xs"),
                             actionButton("fire_opt1",  label = icon("trash"), class = "btn-outline-danger btn-xs")
                         )
                     ),
                     # --- Advisor 2 ---
                     div(class = "advisor-card p-3",
                         h5(textOutput("opt2_name")),
                         div(textOutput("opt2_rate"), style = "font-size: 14px; margin-bottom: 4px;"),
                         htmlOutput("opt_rationale2", style = "font-size: 13px; margin-bottom: 4px;"),
                         htmlOutput("opt_press2",     style = "font-size: 12px; color:#8ec4ff; margin-bottom: 6px;"),
                         htmlOutput("opt2_notice",    style = "font-size: 12px; color:#dc3545; margin-bottom: 6px;"),
                         div(style = "display:flex; gap:6px; margin-top:7px;",
                             actionButton("apply_opt2", label = "",       class = "btn-outline-primary btn-xs"),
                             actionButton("fire_opt2",  label = icon("trash"), class = "btn-outline-danger btn-xs")
                         )
                     ),
                     # --- Advisor 3 ---
                     div(class = "advisor-card p-3",
                         h5(textOutput("opt3_name")),
                         div(textOutput("opt3_rate"), style = "font-size: 14px; margin-bottom: 4px;"),
                         htmlOutput("opt_rationale3", style = "font-size: 13px; margin-bottom: 4px;"),
                         htmlOutput("opt_press3",     style = "font-size: 12px; color:#8ec4ff; margin-bottom: 6px;"),
                         htmlOutput("opt3_notice",    style = "font-size: 12px; color:#dc3545; margin-bottom: 6px;"),
                         div(style = "display:flex; gap:6px; margin-top:7px;",
                             actionButton("apply_opt3", label = "",       class = "btn-outline-primary btn-xs"),
                             actionButton("fire_opt3",  label = icon("trash"), class = "btn-outline-danger btn-xs")
                         )
                     )
                 )
        )
      ), # <-- CLOSE tabsetPanel
      
      # --- Press Statement Panel (ALWAYS VISIBLE BELOW TABS) ---
      div(class = "press-statement compact-panel",
          h5(textOutput("press_statement"),
             style = "margin: 0 0 8px 0; font-weight: bold; font-size: 14px;"),
          div(style = "font-size: 13px;",
              strong(textOutput("pressTitle")),
              p(textOutput("pressStatement"),
                style = "margin: 8px 0; line-height: 1.3; font-size: 13px;")
          )
      ),
      
      # --- PRESS CONFERENCE BUTTON ---
      div(
        style = "margin-bottom: 14px;",
        actionButton(
          "start_press",
          label = textOutput("start_press_lbl"),
          icon = icon("microphone"),
          class = "btn-outline-info btn-sm"
        )
      ),
      
      # --- Game Statistics and Controls ---
      fluidRow(
        column(width = 6,
               card(
                 card_header(textOutput("game_stats")),
                 card_body(
                   fluidRow(
                     column(width = 6,
                            p(textOutput("decisions_count_label"), style = "margin: 0; font-size: 13px;"),
                            h4(textOutput("decisionsCount"), style = "color: #0d6efd; margin: 5px 0;")
                     ),
                     column(width = 6,
                            p(textOutput("avg_deviation_label"), style = "margin: 0; font-size: 13px;"),
                            h4(textOutput("avgDeviation"), style = "color: #dc3545; margin: 5px 0;")
                     )
                   )
                 )
               )
        ),
        column(width = 6,
               div(style = "text-align: center; margin-top: 10px;",
                   downloadButton("downloadData", textOutput("download_results"),
                                  class = "btn-outline-primary btn-sm"),
                   br(),
                   actionButton("resetGame", textOutput("reset_game"),
                                class = "btn-outline-warning btn-sm reset-btn",
                                style = "margin-top: 5px;"),
                   br(),
                   h5(textOutput("gameResult"),
                      style = "margin-top: 10px; font-weight:bold; color:#f8f9fa;")
               )   
        )
      )
    )
  )
)
