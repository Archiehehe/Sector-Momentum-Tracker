library(shiny)
library(tidyquant)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reactable)
library(bslib)
library(TTR)
library(plotly)
library(quantmod) 
library(scales)
library(shinycssloaders)

# --- 1. DATA CONFIGURATION ---

company_map <- c(
  "NVDA" = "NVIDIA", "AAPL" = "Apple", "MSFT" = "Microsoft", "AMZN" = "Amazon", "GOOGL" = "Alphabet A",
  "META" = "Meta", "TSLA" = "Tesla", "BRK-B" = "Berkshire", "LLY" = "Eli Lilly", "AVGO" = "Broadcom",
  "ASML" = "ASML (NL)", "SAP" = "SAP SE (DE)", "KYCOY" = "Keyence (JP)", "SSNLF" = "Samsung (KR)",
  "JPM" = "JPMorgan", "XOM" = "Exxon", "UNH" = "UnitedHealth", "V" = "Visa", "PG" = "P&G",
  "MA" = "Mastercard", "COST" = "Costco", "JNJ" = "J&J", "HD" = "Home Depot", "MRK" = "Merck",
  "ABBV" = "AbbVie", "CVX" = "Chevron", "WMT" = "Walmart", "KO" = "Coca-Cola", "PEP" = "PepsiCo",
  "BAC" = "Bank of America", "WFC" = "Wells Fargo", "GS" = "Goldman", "MS" = "Morgan Stanley", "C" = "Citi",
  "BLK" = "BlackRock", "CRM" = "Salesforce", "ADBE" = "Adobe", "ORCL" = "Oracle", "AMD" = "AMD",
  "NFLX" = "Netflix", "DIS" = "Disney", "NKE" = "Nike", "TGT" = "Target", "LOW" = "Lowe's",
  "BA" = "Boeing", "CAT" = "Caterpillar", "GE" = "GE Aero", "HON" = "Honeywell", "UNP" = "Union Pacific",
  "NEE" = "NextEra", "SO" = "Southern Co", "DUK" = "Duke Energy", "PLD" = "Prologis", "AMT" = "Amer Tower",
  "TSM" = "Taiwan Semi", "TXN" = "Texas Inst", "QCOM" = "Qualcomm", "AMAT" = "Applied Mat", "LRCX" = "Lam Research",
  "MU" = "Micron", "ADI" = "Analog Dev", "PANW" = "Palo Alto", "CRWD" = "CrowdStrike", "FTNT" = "Fortinet",
  "PLTR" = "Palantir", "NOW" = "ServiceNow", "INTU" = "Intuit", "SNOW" = "Snowflake", "SQ" = "Block",
  "PYPL" = "PayPal", "COIN" = "Coinbase", "HOOD" = "Robinhood", "ISRG" = "Intuitive Surg", "PATH" = "UiPath",
  "DELL" = "Dell", "HPE" = "HP Ent", "NTAP" = "NetApp", "STX" = "Seagate", "WDC" = "Western Dig",
  "KLAC" = "KLA Corp", "SNPS" = "Synopsys", "CDNS" = "Cadence", "MCHP" = "Microchip", "ON" = "ON Semi",
  "ANET" = "Arista Net", "APH" = "Amphenol", "IBM" = "IBM", "CSCO" = "Cisco", "INTC" = "Intel",
  "SMCI" = "Super Micro",
  # Additional mappings
  "7203.T" = "Toyota", "6758.T" = "Sony", "7267.T" = "Honda", "8306.T" = "Mitsubishi UFJ",
  "8411.T" = "Mizuho", "8316.T" = "Sumitomo Mitsui", "9984.T" = "SoftBank", "6861.T" = "Keyence",
  "6502.T" = "Toshiba", "4063.T" = "Shin-Etsu", "2802.T" = "Ajinomoto", "4502.T" = "Takeda",
  "6954.T" = "Fanuc", "6594.T" = "Nidec", "4568.T" = "Daiichi Sankyo", "4503.T" = "Astellas",
  "NESN.SW" = "Nestle", "NOVN.SW" = "Novartis", "ROG.SW" = "Roche", "UBSG.SW" = "UBS",
  "ZURN.SW" = "Zurich Ins", "ABBN.SW" = "ABB", "CFR.SW" = "Richemont", "HOLN.SW" = "Holcim",
  "SIKA.SW" = "Sika", "GEBN.SW" = "Geberit", "SLHN.SW" = "Swiss Life", "LONN.SW" = "Lonza",
  "005930.KS" = "Samsung Elec", "000660.KS" = "SK Hynix", "035420.KS" = "Naver",
  "005380.KS" = "Hyundai Motor", "051910.KS" = "LG Chem", "030200.KS" = "KT&G",
  "035720.KS" = "Kakao", "207940.KS" = "Samsung Bio", "006400.KS" = "Samsung SDI",
  "SAP.DE" = "SAP", "SIE.DE" = "Siemens", "DTE.DE" = "Deutsche Telekom", "ALV.DE" = "Allianz",
  "VOW3.DE" = "Volkswagen", "MBG.DE" = "Mercedes-Benz", "BMW.DE" = "BMW", "BAS.DE" = "BASF",
  "MC.PA" = "LVMH", "OR.PA" = "L'Oreal", "SAN.PA" = "Sanofi", "TTE.PA" = "TotalEnergies",
  "AIR.PA" = "Airbus", "BN.PA" = "Danone", "AI.PA" = "Air Liquide"
)

get_name <- function(ticker) {
  ifelse(ticker %in% names(company_map), company_map[ticker], ticker)
}

get_currency_symbol <- function(ticker) {
  if (grepl("\\.T$", ticker)) return("¥")
  if (grepl("\\.KS$", ticker)) return("₩")
  if (grepl("\\.DE$", ticker)) return("€")
  if (grepl("\\.PA$", ticker)) return("€")
  if (grepl("\\.SW$", ticker)) return("CHF ")
  if (grepl("\\.L$", ticker)) return("£")
  if (grepl("\\.HK$", ticker)) return("HK$")
  return("$")
}

gics_list <- list(
  "XLK" = c("MSFT", "AAPL", "NVDA", "AVGO", "ORCL", "CRM", "AMD", "ADBE", "QCOM", "CSCO", "TXN", "INTU", "IBM", "AMAT", "NOW", "MU", "LRCX", "ADI", "PANW", "SNPS", "KLAC", "CDNS", "APH", "ANET", "MSI", "IT", "HPE", "FTNT", "TEL", "GLW", "HPQ", "STM", "KEYS", "ZBRA", "TRMB", "STX", "WDC", "NTAP", "PSTG", "FFIV", "JNPR", "GEN", "QRVO", "SWKS", "TER"),
  "XLC" = c("GOOGL", "GOOG", "META", "NFLX", "DIS", "CMCSA", "TMUS", "VZ", "T", "CHTR", "EA", "WBD", "TTWO", "OMC", "IPG", "LYV", "SIRI", "FOXA", "PARA", "MTCH", "ZG", "DISH", "ROKU", "PINS", "SNAP", "DASH", "SPOT", "BIDU", "NTES", "TME", "IQ", "BILI"),
  "XLY" = c("AMZN", "TSLA", "HD", "MCD", "NKE", "LOW", "SBUX", "BKNG", "TJX", "TGT", "LULU", "MAR", "F", "GM", "CMG", "YUM", "HLT", "RCL", "DHI", "LEN", "ORLY", "AZO", "TSCO", "ROST", "EXPE", "ULTA", "DRI", "BBY", "GRMN", "CCL", "HAS", "MGM", "WYNN", "PHM", "KMX", "LKQ", "POOL", "DKNG", "ETSY", "EBAY"),
  "XLF" = c("BRK-B", "JPM", "V", "MA", "BAC", "WFC", "MS", "GS", "SPGI", "AXP", "BLK", "C", "CB", "MMC", "PGR", "SCHW", "CME", "ICE", "AON", "USB", "TRV", "PNC", "AIG", "COF", "MET", "BK", "TFC", "HIG", "AJG", "DFS", "FITB", "RF", "HBAN", "KEY", "CFG", "SYF", "NTRS", "MTB", "STT", "RJF", "KKR", "BX", "APO"),
  "XLV" = c("LLY", "UNH", "JNJ", "MRK", "ABBV", "TMO", "ABT", "AMGN", "ISRG", "PFE", "DHR", "SYK", "ELV", "VRTX", "REGN", "BMY", "GILD", "CVS", "CI", "BSX", "MDT", "ZTS", "HCA", "BDX", "MCK", "EMR", "EW", "IQV", "A", "RMD", "DXCM", "MTD", "WST", "CNC", "HUM", "LH", "ALGN", "HOLX", "BAX", "STE", "COO", "WAT"),
  "XLE" = c("XOM", "CVX", "COP", "SLB", "EOG", "MPC", "PSX", "VLO", "OXY", "HES", "WMB", "KMI", "BKR", "HAL", "DVN", "TPL", "FANG", "CTRA", "MRO", "APA", "TRGP", "OKE", "EQT", "OVV", "PXD", "CHK", "MTDR", "CHX", "MUR", "CIVI", "SWN", "AR", "RRC", "VNOM", "CNX", "PDCE", "MGY"),
  "XLI" = c("CAT", "GE", "UNP", "HON", "UPS", "RTX", "BA", "LMT", "DE", "ADP", "ETN", "WM", "NOC", "GD", "ITW", "CSX", "NSC", "FDX", "EMR", "PH", "TT", "CARR", "PCAR", "GWW", "PAYX", "CTAS", "TDG", "VRSK", "FAST", "OTIS", "AME", "ROK", "EFX", "URI", "RSG", "ODFL", "CMI", "PWR", "DAL", "UAL", "LUV"),
  "XLU" = c("NEE", "SO", "DUK", "SRE", "AEP", "D", "PEG", "XEL", "ED", "PCG", "WEC", "ES", "EIX", "DTE", "ETR", "FE", "PPL", "AEE", "CMS", "CNP", "ATO", "LNT", "EVRG", "NI", "PNW", "NRG", "AES", "AWK", "CEG", "VST", "AGR", "IDA"),
  "XLRE" = c("PLD", "AMT", "EQIX", "CCI", "PSA", "O", "SPG", "DLR", "VICI", "WELL", "CSGP", "AVB", "EQR", "CBRE", "EXR", "INVH", "WY", "ARE", "IRM", "MAA", "VTR", "ESS", "CPT", "UDR", "HST", "KIM", "REG", "SBAC", "BXP", "FRT")
)

gics_map <- c(
  "XLK" = "Technology", "XLC" = "Communication", "XLY" = "Discretionary",
  "XLF" = "Financials", "XLV" = "Healthcare", "XLE" = "Energy",
  "XLI" = "Industrials", "XLU" = "Utilities", "XLRE" = "Real Estate",
  "SPY" = "S&P 500"
)

theme_list <- list(
  "SMH (Semis)" = c("NVDA", "TSM", "AVGO", "AMD", "TXN", "QCOM", "AMAT", "LRCX", "MU", "ADI", "KLAC", "INTC", "MRVL", "SNPS", "CDNS", "NXPI", "MCHP", "ON", "STM", "MPWR", "TER", "ENTG", "SWKS", "QRVO", "WOLF", "RMBS", "LSCC", "SLAB", "COHR", "DIOD", "ACLS", "MKSI", "ONTO", "FORM", "CAMT", "NVMI"),
  "IHI (Med Devices)" = c("ABT", "MDT", "SYK", "BSX", "EW", "ISRG", "BDX", "ZBH", "ALGN", "BAX", "RMD", "COO", "HOLX", "STE", "TFX", "PODD", "GMED", "PEN", "ATRC", "SWAV", "LANT", "NVCR", "GKOS", "NUVA", "MASI", "QDEL", "ICUI", "NEOG", "AXNX", "OMCL", "ITGR", "SILK", "ANGO", "CRY", "AORT"),
  "IGV (Software)" = c("CRM", "ADBE", "ORCL", "NOW", "INTU", "PLTR", "SNPS", "CDNS", "WDAY", "MSFT", "SAP", "PANW", "ROP", "TEAM", "ADSK", "DDOG", "ANSS", "MDB", "HUBS", "DT", "ZM", "NET", "ZS", "OKTA", "SSNC", "TYL", "PTC", "CRWD", "BSY", "PCTY", "GEN", "DOCU", "NICE", "MANH", "CFLT", "OTEX", "BL", "APP", "PATH", "GTLB"),
  "XTH (Hardware)" = c("AAPL", "CSCO", "DELL", "HPE", "NTAP", "STX", "WDC", "ANET", "HPQ", "PSTG", "MSI", "GLW", "JNPR", "FFIV", "ZBRA", "TRMB", "CIEN", "LITE", "VIAV", "COMM", "EXTR", "SATS", "VSAT", "SMCI"),
  "CIBR (Cyber)" = c("PANW", "CRWD", "FTNT", "ZS", "OKTA", "GEN", "CHKP", "NET", "TENB", "VRNS", "RPD", "QLYS", "SENT", "S", "AKAM", "CACI", "SAIC", "LDOS", "BAH", "TEL", "FDS", "VRSN", "MIME", "RDWR", "ATEN"),
  "KRE (Reg Banks)" = c("KEY", "RF", "CFG", "FITB", "HBAN", "ZION", "WAL", "MTB", "SIVB", "CMA", "EWBC", "FRC", "FHN", "PNFP", "SYN", "BOKF", "WBS", "CFR", "UMBF", "ASB", "CBSH", "FNB", "UBSI", "ONB", "BOH", "SFNC", "PB", "HWC"),
  "FINX (FinTech)" = c("FI", "FIS", "SQ", "COIN", "HOOD", "AFRM", "PYPL", "V", "MA", "GPN", "TOST", "BILL", "PATH", "INTU", "SSNC", "FICO", "JKHY", "WEX", "FLEET", "FOUR", "FLT", "MQ", "UPST", "LC", "SOFI", "NCNO", "ALKT", "FLY", "RPAY", "ENV"),
  "CARZ (Auto)" = c("TSLA", "TM", "F", "GM", "HMC", "RACE", "STLA", "LI", "XPEV", "NIO", "LCID", "RIVN", "HYMTF", "MBGYY", "VWAGY", "BMWYY", "NSANY", "MZDAY", "SUBHu"),
  "ITA (Defense)" = c("GE", "RTX", "LMT", "BA", "GD", "NOC", "TDG", "HWM", "LHX", "TXT", "HII", "LDOS", "AXON", "CACI", "SAIC", "HEI", "BWXT", "CW", "HXL", "KTOS", "AVAV", "SPR", "MTSI", "AJRD", "RKLB", "PSN"),
  "PAVE (Infra)" = c("ETN", "CAT", "DE", "URI", "VMC", "MLM", "PWR", "EMR", "TT", "CARR", "J", "ACM", "FLR", "GVA", "STRL", "PRIM", "MTZ", "DY", "FIX", "EME", "AGX", "NCS", "GFF", "AQUA"),
  "ICLN (Clean)" = c("FSLR", "ENPH", "SEDG", "PLUG", "RUN", "NEE", "BE", "FCEL", "BLDP", "SHLS", "ARRY", "NXT", "SPWR", "CSIQ", "JKS", "DQ", "NOVA", "AMRC", "HASI", "ORA", "CWEN", "NEP", "AGR", "AY", "TPIC"),
  "URNM (Uranium)" = c("CCJ", "UEC", "NXE", "UUUU", "LEU", "DNN", "URA", "URNM", "SRUUF", "PDN", "BOE", "EU", "URG", "LAM", "DYL", "BMN", "LOT", "PEN", "AGE")
)
theme_choices <- names(theme_list)

country_list <- list(
  "SPY (USA S&P 500)" = c("MSFT", "AAPL", "NVDA", "AMZN", "META", "GOOGL", "BRK-B", "LLY", "AVGO", "JPM", "TSLA", "XOM", "UNH", "V", "PG", "COST", "MA", "JNJ", "HD", "MRK"),
  "EWC (Canada)" = c("RY", "TD", "SHOP", "ENB", "CNQ", "CP", "BMO", "TRI", "BNS", "TRP", "SU", "MFC", "BCE", "CM"),
  "EWZ (Brazil)" = c("VALE", "PBR", "ITUB", "ABEV", "BBD", "NU", "BSBR", "ERJ", "SUZ", "GGB"),
  "EWG (Germany)" = c("SAP.DE", "SIE.DE", "DTE.DE", "ALV.DE", "VOW3.DE", "MBG.DE", "BMW.DE", "BAS.DE", "SAP", "SIEGY", "DTEGY", "ALIZY", "VWAGY", "MBGYY", "BMWYY", "BASFY"),
  "EWQ (France)" = c("MC.PA", "OR.PA", "SAN.PA", "TTE.PA", "AIR.PA", "BN.PA", "AI.PA", "LVMUY", "TTE", "SNY", "OR", "SBGSY", "AXAHY", "BNPQY"),
  "EWL (Switzerland)" = c("NESN.SW", "NOVN.SW", "ROG.SW", "UBSG.SW", "ZURN.SW", "ABBN.SW", "CFR.SW", "HOLN.SW", "SIKA.SW", "GEBN.SW", "SLHN.SW", "LONN.SW", "NESN", "ROG", "NOVN", "UBS", "ZURN", "ABBN", "CFR", "HOLN", "LOGI", "SIKA"),
  "EWJ (Japan)" = c("7203.T", "6758.T", "6861.T", "7267.T", "8306.T", "4502.T", "8411.T", "9984.T", "8316.T", "6954.T", "6594.T", "4568.T", "4503.T", "TM", "SONY", "HMC", "MUFG", "TAK", "SMFG", "MFG", "IX", "NMR", "KYO", "CAJ", "NJ", "FANUY"),
  "FXI (China)" = c("BABA", "JD", "PDD", "BIDU", "TCEHY", "NIO", "LI", "XPEV", "TCOM", "NTES", "ZTO", "YUMC", "BEKE", "HTHT"),
  "EPI (India)" = c("INFY", "HDB", "IBN", "WIT", "MMYT", "AZRE", "RDY", "TATAMOTORS"),
  "EWY (S. Korea)" = c("005930.KS", "000660.KS", "035420.KS", "005380.KS", "051910.KS", "030200.KS", "035720.KS", "207940.KS", "006400.KS", "SSNLF", "PKX", "KB", "SHG", "LPL", "KT", "KEP", "SKM")
)

country_choices <- list(
  "Americas" = c("SPY (USA S&P 500)", "EWC (Canada)", "EWZ (Brazil)"),
  "Europe" = c("EWG (Germany)", "EWQ (France)", "EWL (Switzerland)"),
  "Asia / Pacific" = c("EWJ (Japan)", "FXI (China)", "EPI (India)", "EWY (S. Korea)")
)

# --- 2. UI ---
ui <- page_sidebar(
  header = tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png")
  ),
  title = "Sector Momentum Tracker",
  theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#2c3e50"),
  fillable = FALSE, 
  
  sidebar = sidebar(
    title = "Controls",
    dateRangeInput("dates", "Chart Range:", start = Sys.Date() - 365, end = Sys.Date()),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Macro (GICS)' || input.main_tabs == 'Deep Dive (GICS)'",
      checkboxGroupInput("gics_visible", "Select Sectors:",
                         choices = setNames(names(gics_map)[1:9], gics_map[1:9]),
                         selected = c("XLK", "XLF", "XLE", "XLV")),
      selectInput("gics_active", "Deep Dive Sector:", 
                  choices = setNames(names(gics_map)[1:9], gics_map[1:9]), selected = "XLK")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Macro (Themes)' || input.main_tabs == 'Deep Dive (Themes)'",
      selectInput("theme_visible", "Compare Themes:",
                  choices = theme_choices,
                  selected = c("SMH (Semis)", "IHI (Med Devices)", "KRE (Reg Banks)"),
                  multiple = TRUE),
      selectInput("theme_active", "Deep Dive Theme:", 
                  choices = theme_choices, selected = "SMH (Semis)")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Macro (Countries)' || input.main_tabs == 'Deep Dive (Countries)'",
      selectInput("country_visible", "Compare Regions:",
                  choices = country_choices,
                  selected = c("SPY (USA S&P 500)", "EWJ (Japan)", "FXI (China)", "EPI (India)"),
                  multiple = TRUE),
      selectInput("country_active", "Deep Dive Region:", 
                  choices = names(country_list), selected = "SPY (USA S&P 500)")
    ),
    
    hr(),
    actionButton("refresh", "Refresh Data", class = "btn-primary w-100"),
    div(style="margin-top: 15px;", downloadButton("downloadData", "Export (CSV)", class = "btn-sm w-100"))
  ),
  
  navset_tab(
    id = "main_tabs",
    nav_panel("Macro (GICS)", card(plotlyOutput("plotGICS", height = "500px")), card(reactableOutput("tableGICS"))),
    nav_panel("Deep Dive (GICS)", card(uiOutput("boxesGICS") %>% withSpinner(color="#2c3e50")), reactableOutput("stockTableGICS") %>% withSpinner(color="#2c3e50")),
    nav_panel("Macro (Themes)", card(plotlyOutput("plotThemes", height = "500px")), card(reactableOutput("tableThemes"))),
    nav_panel("Deep Dive (Themes)", card(uiOutput("boxesThemes") %>% withSpinner(color="#2c3e50")), reactableOutput("stockTableThemes") %>% withSpinner(color="#2c3e50")),
    nav_panel("Macro (Countries)", card(plotlyOutput("plotCountries", height = "500px")), card(reactableOutput("tableCountries"))),
    nav_panel("Deep Dive (Countries)", card(uiOutput("boxesCountries") %>% withSpinner(color="#2c3e50")), reactableOutput("stockTableCountries") %>% withSpinner(color="#2c3e50"))
  )
)

# --- 3. SERVER ---

server <- function(input, output, session) {
  
  # --- HELPER 1: Safe Data Fetching ---
  safe_get_data <- function(tickers, from_date) {
    tryCatch({
      df <- tq_get(tickers, from = from_date)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df
    }, error = function(e) { NULL })
  }

  # --- HELPER 2: Robust Market Cap Cleaning ---
  clean_mcap <- function(x) {
    if (is.na(x) || x == "N/A" || x == "") return(0)
    if (is.numeric(x)) return(x)
    x <- gsub("[$,]", "", x)
    last_char <- toupper(substr(x, nchar(x), nchar(x)))
    num_part <- as.numeric(gsub("[^0-9\\.]", "", x))
    if (is.na(num_part)) return(0)
    if (last_char == "T") { return(num_part * 1e12) } 
    else if (last_char == "B") { return(num_part * 1e9) } 
    else if (last_char == "M") { return(num_part * 1e6) } 
    else { return(num_part) }
  }

  # --- MAIN DATA FETCHING FUNCTION ---
  fetch_stock_data <- function(tickers) {
    start_date <- Sys.Date() - 400 
    
    # 1. Fetch Price History
    df_raw <- safe_get_data(tickers, start_date)
    if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
    
    # 2. Fetch Market Cap (Snapshot)
    mcap_df <- tryCatch({
      q <- getQuote(tickers, what = yahooQF("Market Capitalization"))
      q$symbol <- rownames(q) 
      q
    }, error = function(e) {
      data.frame(symbol = tickers, "Market Capitalization" = 0)
    })
    
    colnames(mcap_df)[which(names(mcap_df) == "Market Capitalization")] <- "mcap_raw_string"
    
    # 3. Calculate Metrics
    metrics <- df_raw %>%
      group_by(symbol) %>%
      filter(!is.na(adjusted)) %>%
      arrange(date) %>%
      summarise(
        Price = last(adjusted),
        `1M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-30)]) - 1, error=function(e) NA),
        `3M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-90)]) - 1, error=function(e) NA),
        `6M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-180)]) - 1, error=function(e) NA),
        `1Y` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-365)]) - 1, error=function(e) NA),
        `Vol` = tryCatch(sd(diff(log(adjusted)), na.rm = TRUE) * sqrt(252), error=function(e) NA),
        `RSI` = tryCatch(last(RSI(adjusted, n=14)), error=function(e) NA),
        .groups = "drop"
      )
    
    # 4. Merge Metrics with Mcap
    final_df <- metrics %>%
      left_join(mcap_df, by = "symbol") %>%
      rowwise() %>%                              # Enter row-by-row mode
      mutate(
        Mcap_Clean = clean_mcap(mcap_raw_string),
        Name = get_name(symbol),
        Cur = get_currency_symbol(symbol)
      ) %>%
      ungroup() %>%                              # <--- FIXED: Exit row-by-row mode
      select(Name, symbol, Mcap_Clean, Price, Cur, `1M`, `3M`, `6M`, `1Y`, Vol, RSI) %>%
      arrange(desc(Mcap_Clean))
    
    return(final_df)
  }

  # --- RENDER TABLE ---
  render_stock_table <- function(df) {
    req(df)
    reactable(
      df, columns = list(
        Mcap_Clean = colDef(name = "Mcap", cell = function(v, index) {
          if (is.na(v) || v == 0) return("-")
          val <- v
          suffix <- ""
          if (v >= 1e12) { val <- v/1e12; suffix <- "T" }
          else if (v >= 1e9) { val <- v/1e9; suffix <- "B" }
          else if (v >= 1e6) { val <- v/1e6; suffix <- "M" }
          cur <- df$Cur[index]
          paste0(cur, round(val, 1), suffix)
        }),
        symbol = colDef(name = "Ticker", style = list(fontWeight = "bold")),
        Price = colDef(cell = function(value, index) {
          paste0(df$Cur[index], format(round(value, 2), nsmall=2, big.mark=","))
        }),
        Cur = colDef(show = FALSE),
        `1M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(is.na(v)) "black" else if(v>0) "#2ecc71" else "#e74c3c", fontWeight = "bold")),
        `3M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(is.na(v)) "black" else if(v>0) "#2ecc71" else "#e74c3c")),
        `6M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(is.na(v)) "black" else if(v>0) "#2ecc71" else "#e74c3c")),
        `1Y` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(is.na(v)) "black" else if(v>0) "#2ecc71" else "#e74c3c")),
        Vol = colDef(format = colFormat(percent = TRUE, digits = 1)),
        RSI = colDef(format = colFormat(digits = 0))
      ), 
      striped = TRUE, compact = TRUE, defaultPageSize = 15,
      theme = reactableTheme(headerStyle = list(background = "#f8f9fa"))
    )
  }
  
  # --- VALUE BOXES (Green/Red) ---
  make_value_boxes <- function(df) {
    req(df); if (nrow(df) == 0) return(NULL)
    
    df_clean <- df %>% filter(!is.na(`1M`))
    if(nrow(df_clean) == 0) return(NULL)
    
    # We use head(1) just to be extra safe
    winner <- df_clean %>% arrange(desc(`1M`)) %>% head(1)
    laggard <- df_clean %>% arrange(`1M`) %>% head(1)
    avg_ret <- mean(df_clean$`1M`, na.rm=TRUE)
    
    layout_columns(
      value_box(title = "Top Performer (1M)", 
                value = paste0(winner$Name, ": ", percent(winner$`1M`, 0.1)), 
                theme = "success"),
      value_box(title = "Lagging (1M)", 
                value = paste0(laggard$Name, ": ", percent(laggard$`1M`, 0.1)), 
                theme = "danger"),
      value_box(title = "Sector Average (1M)", 
                value = percent(avg_ret, 0.1), 
                theme = "primary")
    )
  }
  
  # --- PLOT FUNCTION ---
  create_plot <- function(data) {
    p <- ggplot(data, aes(x=date, y=cum_ret, color=label, group=label,
                          text = paste0("<b>", label, "</b>\nReturn: ", percent(cum_ret, 0.1), "\nDate: ", date))) + 
      geom_line(linewidth = 1) + 
      scale_y_continuous(labels=percent) + 
      labs(y = "Return", x = "") + 
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  }

  # --- REACTIVES: MACRO ---
  data_gics_macro <- eventReactive(input$refresh, {
    safe_get_data(c(names(gics_map)[1:9], "SPY"), input$dates[1]) %>%
      group_by(symbol) %>% mutate(cum_ret = (adjusted / first(adjusted)) - 1, label = gics_map[symbol])
  }, ignoreNULL = FALSE)
  
  output$plotGICS <- renderPlotly({ req(data_gics_macro()); create_plot(data_gics_macro() %>% filter(symbol %in% c(input$gics_visible, "SPY"))) })
  output$tableGICS <- renderReactable({
    req(data_gics_macro())
    r <- data_gics_macro() %>% group_by(label) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })

  data_theme_macro <- eventReactive(input$refresh, {
    tks <- unique(c(sub("\\s.*", "", input$theme_visible), "SPY"))
    safe_get_data(tks, input$dates[1]) %>% group_by(symbol) %>% mutate(cum_ret = (adjusted/first(adjusted))-1, label=symbol)
  }, ignoreNULL = FALSE)
  
  output$plotThemes <- renderPlotly({ req(data_theme_macro()); create_plot(data_theme_macro()) })
  output$tableThemes <- renderReactable({
    req(data_theme_macro())
    r <- data_theme_macro() %>% group_by(symbol) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })

  data_country_macro <- eventReactive(input$refresh, {
    tks <- unique(c(sub("\\s.*", "", input$country_visible), "SPY"))
    safe_get_data(tks, input$dates[1]) %>% group_by(symbol) %>% mutate(cum_ret = (adjusted/first(adjusted))-1, label=symbol)
  }, ignoreNULL = FALSE)
  
  output$plotCountries <- renderPlotly({ req(data_country_macro()); create_plot(data_country_macro()) })
  output$tableCountries <- renderReactable({
    req(data_country_macro())
    r <- data_country_macro() %>% group_by(symbol) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })

  # --- REACTIVES: DEEP DIVE ---
  data_gics_deep <- reactive({ fetch_stock_data(gics_list[[input$gics_active]]) })
  output$boxesGICS <- renderUI({ make_value_boxes(data_gics_deep()) })
  output$stockTableGICS <- renderReactable({ render_stock_table(data_gics_deep()) })
  
  data_theme_deep <- reactive({ fetch_stock_data(theme_list[[input$theme_active]]) })
  output$boxesThemes <- renderUI({ make_value_boxes(data_theme_deep()) })
  output$stockTableThemes <- renderReactable({ render_stock_table(data_theme_deep()) })
  
  data_country_deep <- reactive({ fetch_stock_data(country_list[[input$country_active]]) })
  output$boxesCountries <- renderUI({ make_value_boxes(data_country_deep()) })
  output$stockTableCountries <- renderReactable({ render_stock_table(data_country_deep()) })
  
  # --- DOWNLOAD ---
  output$downloadData <- downloadHandler(
    filename = function() { paste("market-data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(data_country_deep(), file) }
  )
}

shinyApp(ui, server)
