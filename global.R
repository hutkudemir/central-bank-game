# --- project_root/global.R ---

# Load libraries (if needed in global scope)
library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(bslib)
library(bsicons)
library(DT)
library(rintrojs)
library(jsonlite)
library(RColorBrewer)

# Define a 5-color color-blind-friendly palette
cb_palette <- brewer.pal(5, "Set2")


# How many decision months in the game
TOTAL_PERIODS <- 36

# ── Region-specific macro targets ----------------------------------------------
region_defaults <- list(
  "Türkiye" = list(
    pi_star  = 5,   u_star  = 8,   r_star  = 3,
    init_pi  = 12,  init_u  = 8,   init_r  = 12
  ),
  "United States" = list(
    pi_star  = 2,   u_star  = 4,   r_star  = 0.5,
    init_pi  = 3,   init_u  = 4.2, init_r  = 4.5
  ),
  "Euro Area" = list(
    pi_star  = 2,   u_star  = 6.5, r_star  = 0,
    init_pi  = 2.5, init_u  = 6.7, init_r  = 2.5
  )
)


# Initialize reactive values for game state
rv <- reactiveValues(
  # Game settings and state
  language = "tr",                # default language (Turkish)
  difficulty = "easy",    # default difficulty scaling
  max_months = TOTAL_PERIODS,                # total number of decision months in the game
  month = 0,                      # current month (starts at 0 before any decision)
  
  pi_star = 5.0,
  u_star  = 8.0,
  r_star  = 3.0,
  
  # Economic indicators (initial values plus placeholders for future months)
  infl       = c(12.0, numeric(TOTAL_PERIODS)),
  unemp      = c(8.0,  numeric(TOTAL_PERIODS)),
  interest   = c(12.0, numeric(TOTAL_PERIODS)),
  gdp_growth = c(2.5,  numeric(TOTAL_PERIODS)),
  
  # Shock and news state
  shock        = NULL,    # current shock event (if any, as a list)
  shockNews    = "",      # headline for current shock or "no event"
  actionNews   = "",      # headline for policy stance feedback (dovish/hawkish etc.)
  shockCount       = 0,
  nextShockMonth   = sample(3:6, 1),
  shockHistory = data.frame(Ay = integer(), Olay = character(), Etki = character(), 
                            stringsAsFactors = FALSE),  # empty shock log
  
  # Internal tracking 
  shockKeysUsed      = character(0),
  microHeadline      = "",
  headlineTurn       = -10,
  fired_headline_turn= NA,
  fired             = c(opt1=FALSE,opt2=FALSE,opt3=FALSE),
  empty_since       = c(opt1=NA,opt2=NA,opt3=NA),
  shockDesc         = "",
  shockType         = "",
  
  # Policy decision memory
  lastDecision   = "maintain",  # "increase", "decrease", or "maintain"
  lastRateChange = 0,           # size of last rate change
  
  # Policy lag & credibility
  policy_buffer = list(rate = numeric(0), effect = numeric(0)), 
  credibility = 100,            # starts at max credibility (100)
  
  # Press & advisor
  pressStatement = "",   # stores the latest press statement text
  pressTitle     = "",   # stores the latest press statement title
  result         = "",   # end-of-game result text
  advisor_options        = list(),
  press_questions = list(),     # list of generated press conference questions
  current_question_index = 1,    # index of current press question being answered
  # remember when each advisor was last applied
  lastApplied = list(opt1 = NA_integer_, opt2 = NA_integer_, opt3 = NA_integer_)
)

headline_db <- fromJSON("www/headlines.json", simplifyDataFrame = TRUE)

shockList <- list(
  
  list(
    key = "shock_energy",
    headline     = "Energy Crisis!",
    headline_tr  = "Enerji Krizi!",
    description  = "Oil prices surge after geopolitical tensions.",
    description_tr = "Jeopolitik gerilimler petrol fiyatını sıçrattı.",
    type = "supply", severity = "high",
    inflEffect = 0.4, unempEffect = 0.2, gdpEffect = -0.3
  ),
  
  list(
    key = "shock_consumer",
    headline     = "Consumer Confidence Falls!",
    headline_tr  = "Tüketici Güveni Düştü!",
    description  = "Unexpected drop in household sentiment cuts demand.",
    description_tr = "Hane halkı güvenindeki beklenmedik düşüş talebi kesti.",
    type = "demand", severity = "medium",
    inflEffect = -0.2, unempEffect = 0.3, gdpEffect = -0.4
  ),
  
  list(
    key = "shock_finance",
    headline     = "Fiscal Stimulus!",
    headline_tr  = "Mali Teşvik!",
    description  = "Government ramps up spending; economy gets a boost.",
    description_tr = "Hükümet harcamaları artırdı; ekonomiye destek geldi.",
    type = "demand", severity = "medium",
    inflEffect = 0.3, unempEffect = -0.3, gdpEffect = 0.5
  ),
  
  list(
    key = "shock_currency",
    headline     = "Currency Volatility!",
    headline_tr  = "Döviz Oynaklığı!",
    description  = "Sharp swings in the exchange rate unsettle markets.",
    description_tr = "Kurda keskin dalgalanmalar piyasayı tedirgin etti.",
    type = "currency", severity = "high",
    inflEffect = 0.5, unempEffect = 0.1, gdpEffect = -0.5
  ),
  
  list(
    key = "shock_industry",
    headline     = "Industrial Boom!",
    headline_tr  = "Sanayi Patlaması!",
    description  = "Manufacturing sector expands faster than expected.",
    description_tr = "İmalat sektörü beklenenden hızlı büyüdü.",
    type = "positive", severity = "medium",
    inflEffect = -0.3, unempEffect = -0.4, gdpEffect = 0.6
  ),
  
  list(
    key = "shock_agriculture",
    headline     = "Agricultural Crisis!",
    headline_tr  = "Tarım Krizi!",
    description  = "Drought sends food prices higher.",
    description_tr = "Kuraklık gıda fiyatlarını yükseltti.",
    type = "supply", severity = "high",
    inflEffect = 0.4, unempEffect = 0.2, gdpEffect = -0.3
  ),
  
  list(
    key = "shock_construction",
    headline     = "Construction Boom!",
    headline_tr  = "İnşaat Patlaması!",
    description  = "Real-estate frenzy fuels jobs and prices.",
    description_tr = "Gayrimenkul çılgınlığı istihdamı ve fiyatları artırıyor.",
    type = "demand", severity = "medium",
    inflEffect = 0.2, unempEffect = -0.5, gdpEffect = 0.4
  ),
  
  list(
    key = "shock_tech",
    headline     = "Tech Investment Surge!",
    headline_tr  = "Teknoloji Yatırımı Patladı!",
    description  = "Digital transformation gains momentum.",
    description_tr = "Dijital dönüşüm ivme kazandı.",
    type = "positive", severity = "medium",
    inflEffect = -0.2, unempEffect = -0.2, gdpEffect = 0.5
  ),
  
  list(
    key = "shock_logistics",
    headline     = "Logistics Disruptions!",
    headline_tr  = "Lojistik Aksaklıkları!",
    description  = "Global supply chain delays raise costs.",
    description_tr = "Küresel tedarik zinciri gecikmeleri maliyetleri artırdı.",
    type = "supply", severity = "medium",
    inflEffect = 0.3, unempEffect = 0.2, gdpEffect = -0.4
  ),
  
  list(
    key = "shock_energy_eff",
    headline     = "Energy Efficiency Gains!",
    headline_tr  = "Enerji Verimliliği Artışı!",
    description  = "Renewables cut production costs.",
    description_tr = "Yenilenebilir enerji üretim maliyetlerini düşürüyor.",
    type = "positive", severity = "low",
    inflEffect = -0.3, unempEffect = -0.1, gdpEffect = 0.3
  )
)


