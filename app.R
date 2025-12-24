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

# --- 1. DATA CONFIGURATION ---

# A. NAME MAPPING
company_map <- c(
  "NVDA" = "NVIDIA Corp", "AAPL" = "Apple Inc", "MSFT" = "Microsoft", "AMZN" = "Amazon", "GOOGL" = "Alphabet A",
  "META" = "Meta Platforms", "TSLA" = "Tesla Inc", "BRK-B" = "Berkshire Hath", "LLY" = "Eli Lilly", "AVGO" = "Broadcom",
  "JPM" = "JPMorgan Chase", "XOM" = "Exxon Mobil", "UNH" = "UnitedHealth", "V" = "Visa Inc", "PG" = "Procter & Gamble",
  "MA" = "Mastercard", "COST" = "Costco Wholesale", "JNJ" = "Johnson & Johnson", "HD" = "Home Depot", "MRK" = "Merck & Co",
  "ABBV" = "AbbVie Inc", "CVX" = "Chevron Corp", "WMT" = "Walmart Inc", "KO" = "Coca-Cola", "PEP" = "PepsiCo",
  "BAC" = "Bank of America", "WFC" = "Wells Fargo", "GS" = "Goldman Sachs", "MS" = "Morgan Stanley", "C" = "Citigroup",
  "BLK" = "BlackRock", "CRM" = "Salesforce", "ADBE" = "Adobe Inc", "ORCL" = "Oracle Corp", "AMD" = "Adv Micro Dev",
  "NFLX" = "Netflix Inc", "DIS" = "Walt Disney", "NKE" = "Nike Inc", "TGT" = "Target Corp", "LOW" = "Lowe's Cos",
  "BA" = "Boeing Co", "CAT" = "Caterpillar", "GE" = "GE Aerospace", "HON" = "Honeywell", "UNP" = "Union Pacific",
  "NEE" = "NextEra Energy", "SO" = "Southern Co", "DUK" = "Duke Energy", "PLD" = "Prologis Inc", "AMT" = "American Tower",
  
  # Tech & Semi
  "TSM" = "Taiwan Semi", "TXN" = "Texas Inst", "QCOM" = "Qualcomm", "AMAT" = "Applied Mat", "LRCX" = "Lam Research",
  "MU" = "Micron Tech", "ADI" = "Analog Devices", "PANW" = "Palo Alto Net", "CRWD" = "CrowdStrike", "FTNT" = "Fortinet",
  "PLTR" = "Palantir Tech", "NOW" = "ServiceNow", "INTU" = "Intuit Inc", "SNOW" = "Snowflake", "SQ" = "Block Inc",
  "PYPL" = "PayPal Holdings", "COIN" = "Coinbase Global", "HOOD" = "Robinhood", "ISRG" = "Intuitive Surg", "PATH" = "UiPath Inc",
  "DELL" = "Dell Tech", "HPE" = "Hewlett Packard", "NTAP" = "NetApp Inc", "STX" = "Seagate Tech",
  
  # Healthcare
  "ABT" = "Abbott Labs", "SYK" = "Stryker Corp", "BSX" = "Boston Sci", "MDT" = "Medtronic", "EW" = "Edwards Life",
  "ELV" = "Elevance Health", "CVS" = "CVS Health", "CI" = "Cigna Group", "HUM" = "Humana Inc",
  "TMO" = "Thermo Fisher", "DHR" = "Danaher Corp", "ILMN" = "Illumina Inc", "A" = "Agilent Tech", "WAT" = "Waters Corp",
  
  # Financials
  "PGR" = "Progressive", "CB" = "Chubb Ltd", "ALL" = "Allstate Corp", "TRV" = "Travelers Cos", "AIG" = "AIG",
  "BK" = "BNY Mellon", "STT" = "State Street", "TROW" = "T. Rowe Price",
  
  # Consumer & Leisure
  "BKNG" = "Booking Hldgs", "MCD" = "McDonald's", "SBUX" = "Starbucks", "MAR" = "Marriott Intl", "HLT" = "Hilton Worldwide",
  "DRI" = "Darden Rest", "RCL" = "Royal Caribbean", "CCL" = "Carnival Corp",
  "TM" = "Toyota Motor", "HMC" = "Honda Motor", "RACE" = "Ferrari NV", "F" = "Ford Motor", "GM" = "General Motors",
  
  # Energy & Ind
  "COP" = "ConocoPhillips", "EOG" = "EOG Resources", "DVN" = "Devon Energy", "OXY" = "Occidental", "HES" = "Hess Corp",
  "SLB" = "Schlumberger", "HAL" = "Halliburton", "BKR" = "Baker Hughes", 
  
  # Housing & Infra
  "DHI" = "D.R. Horton", "LEN" = "Lennar Corp", "ETN" = "Eaton Corp", "URI" = "United Rentals", "CCJ" = "Cameco Corp",
  "FSLR" = "First Solar", "ENPH" = "Enphase Energy", "NEM" = "Newmont Corp", "FCX" = "Freeport-McMoRan", "X" = "US Steel",
  "RTX" = "RTX Corp", "LMT" = "Lockheed Martin", "GD" = "General Dynamics", "VRTX" = "Vertex Pharm", "REGN" = "Regeneron",
  "MRNA" = "Moderna Inc", "INTC" = "Intel Corp", "IBM" = "IBM Corp", "CSCO" = "Cisco Systems", "SPY" = "S&P 500 ETF"
)

# Helper to get name
get_name <- function(ticker) {
  ifelse(ticker %in% names(company_map), company_map[ticker], ticker)
}

# C. GICS SECTOR LIST
gics_list <- list(
  "XLK" = c("NVDA", "AAPL", "MSFT", "AVGO", "ADBE", "CRM", "AMD", "ORCL", "QCOM", "TXN", "CSCO", "IBM", "INTC"),
  "XLC" = c("GOOGL", "META", "NFLX", "DIS", "TMUS", "CMCSA", "VZ", "T", "CHTR", "EA", "TTWO", "WBD", "FOXA"),
  "XLY" = c("AMZN", "TSLA", "HD", "MCD", "NKE", "SBUX", "LOW", "BKNG", "TJX", "TGT", "F", "GM"),
  "XLF" = c("JPM", "BAC", "WFC", "GS", "MS", "C", "BLK", "AXP", "SPGI", "V", "MA", "SCHW"),
  "XLV" = c("LLY", "UNH", "JNJ", "ABBV", "MRK", "TMO", "ABT", "PFE", "AMGN", "DHR", "ISRG"),
  "XLE" = c("XOM", "CVX", "COP", "EOG", "SLB", "MPC", "PSX", "VLO", "OXY", "HES", "KMI"),
  "XLI" = c("GE", "CAT", "UNP", "HON", "RTX", "UPS", "BA", "DE", "LMT", "ADP", "ETN"),
  "XLU" = c("NEE", "SO", "DUK", "SRE", "AEP", "D"),
  "XLRE" = c("PLD", "AMT", "EQIX", "CCI", "PSA", "O")
)

gics_map <- c(
  "XLK" = "Technology", "XLC" = "Communication", "XLY" = "Discretionary",
  "XLF" = "Financials", "XLV" = "Healthcare", "XLE" = "Energy",
  "XLI" = "Industrials", "XLU" = "Utilities", "XLRE" = "Real Estate",
  "SPY" = "S&P 500"
)

# D. THEMATIC LIST
theme_list <- list(
  "IHI (Med Devices)" = c("ABT", "SYK", "ISRG", "BSX", "MDT", "EW", "TMO"),
  "IHE (Pharma)" = c("LLY", "JNJ", "MRK", "ABBV", "PFE", "AMGN", "BMY"),
  "IHF (Providers)" = c("UNH", "ELV", "CVS", "CI", "HUM", "CNC"),
  "IHRT (Tools)" = c("TMO", "DHR", "A", "ILMN", "WAT", "RVTY"),
  "XBI (Biotech)" = c("VRTX", "REGN", "MRNA", "ALNY", "BIIB", "NBIX"),
  "IGV (Software)" = c("CRM", "ADBE", "ORCL", "NOW", "INTU", "PLTR", "SNPS", "CDNS", "WDAY"),
  "SMH (Semis)" = c("NVDA", "TSM", "AVGO", "AMD", "TXN", "QCOM", "AMAT", "LRCX", "MU"),
  "XTH (Hardware)" = c("AAPL", "CSCO", "DELL", "HPE", "NTAP", "STX", "WDC"),
  "CIBR (Cyber)" = c("PANW", "CRWD", "FTNT", "ZS", "OKTA", "GEN", "CHKP", "NET"),
  "KRE (Reg Banks)" = c("KEY", "RF", "CFG", "FITB", "HBAN", "ZION", "WAL"),
  "KCE (Cap Mkts)" = c("MS", "GS", "BLK", "SCHW", "BK", "STT", "TROW"),
  "KIE (Insurance)" = c("PGR", "CB", "ALL", "TRV", "AIG", "HIG", "MET"),
  "FINX (FinTech)" = c("FI", "FIS", "SQ", "COIN", "HOOD", "AFRM", "PYPL"),
  "XRT (Retail)" = c("AMZN", "COST", "TGT", "LOW", "TJX", "ROST", "ULTA", "BBY"),
  "PEJ (Leisure)" = c("BKNG", "MCD", "SBUX", "MAR", "HLT", "DRI", "RCL", "CCL"),
  "CARZ (Auto)" = c("TSLA", "TM", "F", "GM", "HMC", "RACE", "STLA"),
  "XHB (Homes)" = c("DHI", "LEN", "PHM", "TOL", "NVR", "KBH", "MAS"),
  "ITA (Defense)" = c("GE", "RTX", "LMT", "BA", "GD", "NOC", "TDG", "HWM"),
  "IYT (Transport)" = c("UNP", "UPS", "FDX", "CSX", "NSC", "DAL", "UAL", "ODFL"),
  "PAVE (Infra)" = c("ETN", "CAT", "DE", "URI", "VMC", "MLM", "PWR", "EMR"),
  "XOP (Exploration)" = c("COP", "EOG", "DVN", "OXY", "HES", "MRO", "FANG"),
  "XES (Services)" = c("SLB", "HAL", "BKR", "NOV", "CHX"),
  "ICLN (Clean)" = c("FSLR", "ENPH", "SEDG", "PLUG", "RUN", "NEE", "BE"),
  "URNM (Uranium)" = c("CCJ", "UEC", "NXE", "UUUU", "LEU", "DNN")
)

theme_choices <- list(
  "Health Care Themes" = c("IHI (Med Devices)", "IHE (Pharma)", "IHF (Providers)", "IHRT (Tools)", "XBI (Biotech)"),
  "Tech Themes" = c("SMH (Semis)", "IGV (Software)", "XTH (Hardware)", "CIBR (Cyber)"),
  "Financial Themes" = c("KRE (Reg Banks)", "KCE (Cap Mkts)", "KIE (Insurance)", "FINX (FinTech)"),
  "Consumer Themes" = c("XRT (Retail)", "PEJ (Leisure)", "CARZ (Auto)", "XHB (Homes)"),
  "Industrial Themes" = c("ITA (Defense)", "IYT (Transport)", "PAVE (Infra)"),
  "Energy Themes" = c("XOP (Exploration)", "XES (Services)", "ICLN (Clean)", "URNM (Uranium)")
)

# --- 2. UI ---
ui <- page_sidebar(
  # --- HEAD SECTION: ICONS & SOCIAL TAGS ---
  header = tags$head(
    # Browser Tab Icon (Pointing to your PNG)
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    
    # Social Media Preview Tags
    tags$meta(property = "og:title", content = "Sector Momentum Tracker"),
    tags$meta(property = "og:description", content = "Real-time GICS & Thematic market analysis."),
    tags$meta(property = "og:image", content = "logo.png"),
    tags$meta(property = "og:image:width", content = "512"),
    tags$meta(property = "og:image:height", content = "512")
  ),
  
  title = "Sector Momentum Tracker",
  theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#2c3e50"),
  fillable = FALSE, 
  
  sidebar = sidebar(
    title = "Controls",
    dateRangeInput("dates", "Chart Range:", start = Sys.Date() - 365, end = Sys.Date()),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Macro (GICS)' || input.main_tabs == 'Deep Dive (GICS)'",
      h5("GICS Controls"),
      checkboxGroupInput("gics_visible", "Select Sectors:",
                         choices = setNames(names(gics_map)[1:9], gics_map[1:9]),
                         selected = c("XLK", "XLF", "XLE", "XLV")),
      selectInput("gics_active", "Deep Dive Sector:", 
                  choices = setNames(names(gics_map)[1:9], gics_map[1:9]), selected = "XLK")
    ),
    
    conditionalPanel(
      condition = "input.main_tabs == 'Macro (Themes)' || input.main_tabs == 'Deep Dive (Themes)'",
      h5("Theme Controls"),
      selectInput("theme_visible", "Compare Themes:",
                  choices = theme_choices,
                  selected = c("SMH (Semis)", "IHI (Med Devices)", "KRE (Reg Banks)"),
                  multiple = TRUE),
      selectInput("theme_active", "Deep Dive Theme:", 
                  choices = theme_choices, selected = "SMH (Semis)")
    ),
    
    hr(),
    actionButton("refresh", "Refresh Data", class = "btn-primary w-100"),
    div(style="margin-top: 15px;", downloadButton("downloadData", "Export CSV", class = "btn-sm w-100"))
  ),
  
  # --- 4 MAIN TABS ---
  navset_tab(
    id = "main_tabs",
    
    # TAB 1: GICS MACRO
    nav_panel("Macro (GICS)",
              card(
                min_height = "500px",
                card_header("Standard Sector Momentum (GICS)"),
                plotlyOutput("plotGICS", height = "500px") 
              ),
              card(card_header("Sector Rankings"), reactableOutput("tableGICS"))
    ),
    
    # TAB 2: GICS DEEP DIVE
    nav_panel("Deep Dive (GICS)",
              card(
                min_height = "800px",
                card_header(textOutput("headerGICS")),
                uiOutput("boxesGICS"),
                reactableOutput("stockTableGICS")
              )
    ),
    
    # TAB 3: THEME MACRO
    nav_panel("Macro (Themes)",
              card(
                min_height = "500px",
                card_header("Thematic Engine Momentum"),
                plotlyOutput("plotThemes", height = "500px")
              ),
              card(card_header("Theme Rankings"), reactableOutput("tableThemes"))
    ),
    
    # TAB 4: THEME DEEP DIVE
    nav_panel("Deep Dive (Themes)",
              card(
                min_height = "800px",
                card_header(textOutput("headerThemes")),
                uiOutput("boxesThemes"),
                reactableOutput("stockTableThemes") 
              )
    )
  )
)

# --- GLOBAL HELPERS ---

safe_get_data <- function(tickers, from_date) {
  tryCatch({
    df <- tq_get(tickers, from = from_date)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df
  }, error = function(e) {
    NULL
  })
}

fetch_stock_data <- function(tickers) {
  start_date <- Sys.Date() - 400
  df_raw <- safe_get_data(tickers, start_date)
  if (is.null(df_raw)) return(NULL)
  
  # FETCH REAL MCAP DYNAMICALLY
  mcap_info <- tryCatch({
    quotes <- getQuote(tickers, what = yahooQF("Market Capitalization"))
    data.frame(symbol = rownames(quotes), Mcap_Raw = quotes$`Market Capitalization`)
  }, error = function(e) {
    data.frame(symbol = tickers, Mcap_Raw = 0)
  })
  
  date_1m <- Sys.Date() - 30
  date_3m <- Sys.Date() - 90
  date_6m <- Sys.Date() - 180
  date_1y <- Sys.Date() - 365
  
  metrics <- df_raw %>%
    group_by(symbol) %>%
    filter(!is.na(adjusted)) %>%
    summarise(
      Price = last(adjusted),
      `1M` = tryCatch(last(adjusted) / last(adjusted[date <= date_1m]) - 1, error=function(e) NA),
      `3M` = tryCatch(last(adjusted) / last(adjusted[date <= date_3m]) - 1, error=function(e) NA),
      `6M` = tryCatch(last(adjusted) / last(adjusted[date <= date_6m]) - 1, error=function(e) NA),
      `1Y` = tryCatch(last(adjusted) / last(adjusted[date <= date_1y]) - 1, error=function(e) NA),
      `Vol` = tryCatch(sd(diff(log(adjusted)), na.rm = TRUE) * sqrt(252), error=function(e) NA),
      `RSI` = tryCatch(last(RSI(adjusted, n=14)), error=function(e) NA)
    ) %>%
    na.omit()
  
  # Join with Mcap info
  if(!is.null(mcap_info)) {
    metrics <- left_join(metrics, mcap_info, by = "symbol") %>%
      mutate(Mcap_B = Mcap_Raw / 1e9) # Convert to Billions
  } else {
    metrics$Mcap_B <- 0
  }
  
  metrics %>%
    mutate(Mcap_B = ifelse(is.na(Mcap_B), 0, Mcap_B), Name = get_name(symbol)) %>%
    arrange(desc(Mcap_B)) %>%
    select(Name, symbol, Mcap_B, Price, `1M`, `3M`, `6M`, `1Y`, `Vol`, `RSI`)
}

make_value_boxes <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(h4("No data available"))
  
  winner <- df %>% arrange(desc(`1M`)) %>% slice(1)
  laggard <- df %>% arrange(`1M`) %>% slice(1)
  avg_perf <- mean(df$`1M`, na.rm = TRUE)
  
  layout_columns(
    value_box(
      title = "Top Performer (1M)",
      value = paste0(winner$symbol, ": ", scales::percent(winner$`1M`, accuracy = 0.1)),
      showcase = icon("arrow-up"),
      theme = "success"
    ),
    value_box(
      title = "Lagging (1M)",
      value = paste0(laggard$symbol, ": ", scales::percent(laggard$`1M`, accuracy = 0.1)),
      showcase = icon("arrow-down"),
      theme = "danger"
    ),
    value_box(
      title = "Sector Average (1M)",
      value = scales::percent(avg_perf, accuracy = 0.1),
      showcase = icon("chart-pie"),
      theme = "primary"
    )
  )
}

render_stock_table <- function(df) {
  if (is.null(df)) {
    return(reactable(data.frame(Message = "No data available")))
  }
  
  color_fmt <- function(val) { if (is.na(val)) "black" else if (val > 0) "#008000" else "#e00000" }
  mcap_fmt <- function(val) { if (val >= 1000) paste0("$", round(val / 1000, 1), "T") else paste0("$", round(val, 0), "B") }
  
  reactable(
    df, pagination = TRUE, defaultPageSize = 25,
    columns = list(
      Name = colDef(minWidth = 140, style = list(fontWeight = "600")),
      symbol = colDef(name = "Ticker", width = 80, style = list(fontWeight = "bold")),
      Mcap_B = colDef(name = "Mcap", width = 85, cell = function(v) mcap_fmt(v)),
      Price = colDef(name = "Price", width = 85, format = colFormat(currency = "USD")),
      `1M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = color_fmt(v), fontWeight = "bold")),
      `3M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = color_fmt(v), fontWeight = "bold")),
      `6M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = color_fmt(v), fontWeight = "bold")),
      `1Y` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = color_fmt(v), fontWeight = "bold")),
      `Vol` = colDef(name = "Vol", width = 80, format = colFormat(percent = TRUE, digits = 1),
                     style = function(value) {
                       normalized <- min(max((value - 0.15) / (0.7 - 0.15), 0), 1)
                       color <- if (value > 0.45) "#ffcccc" else "#ccffcc"
                       width <- paste0(normalized * 100, "%")
                       list(backgroundImage = paste0("linear-gradient(90deg, ", color, " ", width, ", transparent 0%)"),
                            backgroundSize = "100% 80%", backgroundRepeat = "no-repeat", backgroundPosition = "left center")
                     }),
      `RSI` = colDef(format = colFormat(digits = 0), width = 60, style = function(v) {
        if(is.na(v)) return(NULL)
        list(color = if(v > 70) "red" else if(v < 30) "green" else "black", fontWeight = "bold")
      })
    ),
    striped = TRUE, highlight = TRUE, bordered = TRUE
  )
}

# --- 3. SERVER ---
server <- function(input, output, session) {
  
  # --- MACRO LOGIC ---
  data_gics_macro <- eventReactive(input$refresh, {
    req(input$dates)
    df <- safe_get_data(c(names(gics_map)[1:9], "SPY"), input$dates[1])
    if(is.null(df)) return(NULL)
    
    df %>%
      group_by(symbol) %>%
      filter(!is.na(adjusted)) %>%
      mutate(cum_ret = (adjusted / first(adjusted)) - 1, 
             label = gics_map[symbol],
             style_group = ifelse(symbol == "SPY", "SPY", "Sector"))
  }, ignoreNULL = TRUE)
  
  output$plotGICS <- renderPlotly({
    df <- data_gics_macro()
    req(df)
    visible <- c(input$gics_visible, "SPY")
    plot_df <- df %>% filter(symbol %in% visible)
    
    p <- ggplot(plot_df, aes(x = date, y = cum_ret, 
                             color = label, group = label,
                             linewidth = style_group, linetype = style_group,
                             text = paste0("Date: ", date, "\nSector: ", label, "\nReturn: ", scales::percent(cum_ret, accuracy = 0.1)))) +
      geom_line() +
      scale_linewidth_manual(values = c("SPY" = 2.5, "Sector" = 1.5), guide = "none") +
      scale_linetype_manual(values = c("SPY" = "dashed", "Sector" = "solid"), guide = "none") +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "Return", x = NULL, color = "Sector") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3, font = list(size = 14)))
  })
  
  output$tableGICS <- renderReactable({
    df <- data_gics_macro()
    req(df)
    r <- df %>% group_by(label) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, striped = TRUE, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })
  
  # --- DEEP DIVE LOGIC ---
  output$headerGICS <- renderText({ paste("Deep Dive: ", gics_map[input$gics_active]) })
  
  data_gics_deep <- reactive({
    req(input$gics_active)
    fetch_stock_data(gics_list[[input$gics_active]])
  })
  
  output$boxesGICS <- renderUI({
    df <- data_gics_deep()
    req(df)
    make_value_boxes(df)
  })
  
  output$stockTableGICS <- renderReactable({
    df <- data_gics_deep()
    req(df)
    render_stock_table(df)
  })
  
  # --- THEME LOGIC ---
  data_theme_macro <- eventReactive(input$refresh, {
    req(input$dates)
    selected_long <- c(input$theme_visible, "SPY")
    clean_tickers <- sub("\\s.*", "", selected_long)
    
    df <- safe_get_data(clean_tickers, input$dates[1])
    if(is.null(df)) return(NULL)
    
    df %>%
      group_by(symbol) %>%
      filter(!is.na(adjusted)) %>%
      mutate(cum_ret = (adjusted / first(adjusted)) - 1,
             style_group = ifelse(symbol == "SPY", "SPY", "Theme")) 
  }, ignoreNULL = TRUE)
  
  output$plotThemes <- renderPlotly({
    df <- data_theme_macro()
    req(df)
    
    p <- ggplot(df, aes(x = date, y = cum_ret, 
                        color = symbol, group = symbol,
                        linewidth = style_group, linetype = style_group,
                        text = paste0("Date: ", date, "\nTheme: ", symbol, "\nReturn: ", scales::percent(cum_ret, accuracy = 0.1)))) +
      geom_line() +
      scale_linewidth_manual(values = c("SPY" = 2.5, "Theme" = 1.5), guide = "none") +
      scale_linetype_manual(values = c("SPY" = "dashed", "Theme" = "solid"), guide = "none") +
      scale_y_continuous(labels = scales::percent) +
      labs(y = "Return", x = NULL, color = "Theme") +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = "text") %>% 
      layout(legend = list(orientation = "h", x = 0.1, y = -0.3, font = list(size = 14)))
  })
  
  output$tableThemes <- renderReactable({
    df <- data_theme_macro()
    req(df)
    r <- df %>% group_by(symbol) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, striped = TRUE, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })
  
  output$headerThemes <- renderText({ paste("Deep Dive: ", input$theme_active) })
  
  data_theme_deep <- reactive({
    req(input$theme_active)
    fetch_stock_data(theme_list[[input$theme_active]])
  })
  
  output$boxesThemes <- renderUI({
    df <- data_theme_deep()
    req(df)
    make_value_boxes(df)
  })
  
  output$stockTableThemes <- renderReactable({
    df <- data_theme_deep()
    req(df)
    render_stock_table(df)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { "sector_data.csv" },
    content = function(file) { write.csv(data.frame(Note="Export feature active"), file) } 
  )
}

shinyApp(ui, server)