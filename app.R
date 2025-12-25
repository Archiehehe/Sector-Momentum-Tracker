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
  "SMCI" = "Super Micro"
)

get_name <- function(ticker) {
  ifelse(ticker %in% names(company_map), company_map[ticker], ticker)
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
  "IHI (Med Devices)" = c("ABT", "MDT", "SYK", "BSX", "EW", "ISRG", "BDX", "ZBH", "ALGN", "BAX", "RMD", "COO", "HOLX", "STE", "TFX", "PODD", "GMED", "PEN", "ATRC", "SWAV", "LANT", "NVCR", "GKOS", "NUVA", "MASI", "QDEL", "ICUI", "NEOG", "AXNX", "OMCL", "ITGR", "SILK", "ANGO", "CRY", "AORT"),
  "IHE (Pharma)" = c("LLY", "JNJ", "MRK", "ABBV", "PFE", "AMGN", "BMY", "ZTS", "VRTX", "GILD", "REGN", "BIIB", "SGEN", "ALNY", "INCY", "UTHR", "TECH", "NBIX", "ROY", "PRGO", "JAZZ", "PCRX", "PLPH", "AMPH", "SUPN", "ASRT", "COLL", "HRMY", "ANIP"),
  "IHF (Providers)" = c("UNH", "ELV", "CVS", "CI", "HUM", "CNC", "HCA", "MCK", "COR", "MOH", "UHS", "THC", "ACHC", "AMED", "CHNG", "AGL", "ENSG", "EHC", "SEM", "ADUS", "NHC", "PNTG", "BKD", "BHG"),
  "IHRT (Tools)" = c("TMO", "DHR", "A", "ILMN", "WAT", "RVTY", "MTD", "BIO", "WST", "TECH", "BRKR", "CRL", "RGEN", "IQV", "ICLR", "MEDP", "SYNH", "QTRX", "TXG", "NSTG", "PACB", "SOTK", "AVID", "CTKB"),
  "XBI (Biotech)" = c("VRTX", "REGN", "MRNA", "ALNY", "BIIB", "NBIX", "SGEN", "BNTX", "BGNE", "INCY", "UTHR", "CRSP", "NTLA", "BEAM", "FATE", "EDIT", "IONS", "SRPT", "EXAS", "PCVX", "ROIV", "ARGX", "GMAB", "HZNP", "LEGN", "KRTX", "ITCI", "CYTK", "RVMD", "MDGL", "IMGN", "RXDX", "MRTX", "APLS", "VKTX"),
  "IGV (Software)" = c("CRM", "ADBE", "ORCL", "NOW", "INTU", "PLTR", "SNPS", "CDNS", "WDAY", "MSFT", "SAP", "PANW", "ROP", "TEAM", "ADSK", "DDOG", "ANSS", "MDB", "HUBS", "DT", "ZM", "NET", "ZS", "OKTA", "SSNC", "TYL", "PTC", "CRWD", "BSY", "PCTY", "GEN", "DOCU", "NICE", "MANH", "CFLT", "OTEX", "BL", "APP", "PATH", "GTLB"),
  "SMH (Semis)" = c("NVDA", "TSM", "AVGO", "AMD", "TXN", "QCOM", "AMAT", "LRCX", "MU", "ADI", "KLAC", "INTC", "MRVL", "SNPS", "CDNS", "NXPI", "MCHP", "ON", "STM", "MPWR", "TER", "ENTG", "SWKS", "QRVO", "WOLF", "RMBS", "LSCC", "SLAB", "COHR", "DIOD", "ACLS", "MKSI", "ONTO", "FORM", "CAMT", "NVMI"),
  "XTH (Hardware)" = c("AAPL", "CSCO", "DELL", "HPE", "NTAP", "STX", "WDC", "ANET", "HPQ", "PSTG", "MSI", "GLW", "JNPR", "FFIV", "ZBRA", "TRMB", "CIEN", "LITE", "VIAV", "COMM", "EXTR", "SATS", "VSAT", "SMCI"),
  "CIBR (Cyber)" = c("PANW", "CRWD", "FTNT", "ZS", "OKTA", "GEN", "CHKP", "NET", "TENB", "VRNS", "RPD", "QLYS", "SENT", "S", "AKAM", "CACI", "SAIC", "LDOS", "BAH", "TEL", "FDS", "VRSN", "MIME", "RDWR", "ATEN"),
  "KRE (Reg Banks)" = c("KEY", "RF", "CFG", "FITB", "HBAN", "ZION", "WAL", "MTB", "SIVB", "CMA", "EWBC", "FRC", "FHN", "PNFP", "SYN", "BOKF", "WBS", "CFR", "UMBF", "ASB", "CBSH", "FNB", "UBSI", "ONB", "BOH", "SFNC", "PB", "HWC"),
  "KCE (Cap Mkts)" = c("MS", "GS", "BLK", "SCHW", "BK", "STT", "TROW", "SPGI", "MCO", "CME", "ICE", "NDAQ", "CBOE", "MKTX", "IBKR", "LPLA", "RJF", "SEIC", "AMG", "HLI", "EVR", "LAZ", "COWN", "PIPR", "SF"),
  "KIE (Insurance)" = c("PGR", "CB", "ALL", "TRV", "AIG", "HIG", "MET", "PFG", "PRU", "L", "CINF", "WRB", "ACGL", "RE", "EG", "AFG", "ERIE", "THG", "SIGI", "MCY", "KNSL", "RLI", "ORI", "AXS", "SAFT"),
  "FINX (FinTech)" = c("FI", "FIS", "SQ", "COIN", "HOOD", "AFRM", "PYPL", "V", "MA", "GPN", "TOST", "BILL", "PATH", "INTU", "SSNC", "FICO", "JKHY", "WEX", "FLEET", "FOUR", "FLT", "MQ", "UPST", "LC", "SOFI", "NCNO", "ALKT", "FLY", "RPAY", "ENV"),
  "XRT (Retail)" = c("AMZN", "COST", "TGT", "LOW", "TJX", "ROST", "ULTA", "BBY", "WMT", "HD", "DG", "DLTR", "KR", "BJ", "M", "KSS", "JWN", "GPS", "ANF", "URBN", "AEO", "BOOT", "DKS", "HIBB", "WSM", "RH", "BBWI", "VSCO", "FIVE"),
  "PEJ (Leisure)" = c("BKNG", "MCD", "SBUX", "MAR", "HLT", "DRI", "RCL", "CCL", "YUM", "CMG", "LVS", "MGM", "WYNN", "EXPE", "NCLH", "CZR", "PENN", "DKNG", "PLYA", "VAC", "TNL", "BLMN", "TXRH", "WING", "DPZ", "PZZA", "CBRL"),
  "CARZ (Auto)" = c("TSLA", "TM", "F", "GM", "HMC", "RACE", "STLA", "LI", "XPEV", "NIO", "LCID", "RIVN", "HYMTF", "MBGYY", "VWAGY", "BMWYY", "NSANY", "MZDAY", "SUBHu"),
  "XHB (Homes)" = c("DHI", "LEN", "PHM", "TOL", "NVR", "KBH", "MAS", "MDC", "TMHC", "MHO", "GRBK", "LGIH", "CCS", "IBP", "BLD", "FBHS", "AWI", "OC", "BECN", "GMS", "SITE", "TREX", "WMS"),
  "ITA (Defense)" = c("GE", "RTX", "LMT", "BA", "GD", "NOC", "TDG", "HWM", "LHX", "TXT", "HII", "LDOS", "AXON", "CACI", "SAIC", "HEI", "BWXT", "CW", "HXL", "KTOS", "AVAV", "SPR", "MTSI", "AJRD", "RKLB", "PSN"),
  "IYT (Transport)" = c("UNP", "UPS", "FDX", "CSX", "NSC", "DAL", "UAL", "ODFL", "JBHT", "LUV", "AAL", "ALK", "JBLU", "SAVE", "CHRW", "EXPD", "KNX", "SNDR", "WERN", "ARCB", "SAIA", "MATX", "KEX"),
  "PAVE (Infra)" = c("ETN", "CAT", "DE", "URI", "VMC", "MLM", "PWR", "EMR", "TT", "CARR", "J", "ACM", "FLR", "GVA", "STRL", "PRIM", "MTZ", "DY", "FIX", "EME", "AGX", "NCS", "GFF", "AQUA"),
  "XOP (Exploration)" = c("COP", "EOG", "DVN", "OXY", "HES", "MRO", "FANG", "CTRA", "APA", "PXD", "EQT", "MTDR", "PDCE", "RRC", "AR", "SWN", "SM", "CRK", "CIVI", "CHK", "MGY", "PR", "CNX", "VNOM"),
  "XES (Services)" = c("SLB", "HAL", "BKR", "NOV", "CHX", "FTI", "OII", "NINE", "RES", "PTEN", "HP", "LBRT", "PUMP", "NBR", "VAL", "RIG", "NE", "DO", "BORR", "HLX", "DRQ"),
  "ICLN (Clean)" = c("FSLR", "ENPH", "SEDG", "PLUG", "RUN", "NEE", "BE", "FCEL", "BLDP", "SHLS", "ARRY", "NXT", "SPWR", "CSIQ", "JKS", "DQ", "NOVA", "AMRC", "HASI", "ORA", "CWEN", "NEP", "AGR", "AY", "TPIC"),
  "URNM (Uranium)" = c("CCJ", "UEC", "NXE", "UUUU", "LEU", "DNN", "URA", "URNM", "SRUUF", "PDN", "BOE", "EU", "URG", "LAM", "DYL", "BMN", "LOT", "PEN", "AGE")
)
theme_choices <- names(theme_list)

country_list <- list(
  "SPY (USA S&P 500)" = c("MSFT", "AAPL", "NVDA", "AMZN", "META", "GOOGL", "BRK-B", "LLY", "AVGO", "JPM", "TSLA", "XOM", "UNH", "V", "PG", "COST", "MA", "JNJ", "HD", "MRK"),
  "VTI (USA Total Mkt)" = c("MSFT", "AAPL", "NVDA", "AMZN", "META", "GOOGL", "BRK-B", "LLY", "AVGO", "JPM", "TSLA", "XOM", "UNH", "V", "PG", "COST", "MA", "JNJ", "HD", "MRK"),
  "EWC (Canada)" = c("RY", "TD", "SHOP", "ENB", "CNQ", "CP", "BMO", "TRI", "BNS", "TRP", "SU", "MFC", "BCE", "CM"),
  "EWZ (Brazil)" = c("VALE", "PBR", "ITUB", "ABEV", "BBD", "NU", "BSBR", "ERJ", "SUZ", "GGB"),
  "FBZ (Brazil Alpha)" = c("VALE", "PBR", "ITUB", "ABEV", "BBD", "NU", "BSBR"), 
  "EWW (Mexico)" = c("AMX", "KOF", "BSMX", "CX", "FMX", "VLRS", "ASR", "PAC", "OMAB"),
  "ARGT (Argentina)" = c("MELI", "GGAL", "YPF", "BMA", "PAM", "TGS", "LOMA", "CEPU"),
  "GXG (Colombia)" = c("EC", "CIB", "AVHOQ"),
  "ECH (Chile)" = c("SQM", "BCH", "CCU", "ENIC"),
  "EPU (Peru)" = c("BVN", "CPAC", "SCCO"),
  "ILF (LatAm)" = c("VALE", "PBR", "ITUB", "AMX", "BSMX", "SQM", "MELI"),
  "EWU (UK)" = c("SHEL", "AZN", "HSBC", "UL", "BP", "BTI", "GSK", "DEO", "RIO", "RELX", "NGG", "VOD", "NWG", "LYG"),
  "EWG (Germany)" = c("SAP", "SIEGY", "DTEGY", "ALIZY", "VWAGY", "MBGYY", "BMWYY", "BASFY", "ADDYY", "BAYRY", "DHLGY"),
  "EWQ (France)" = c("LVMUY", "TTE", "SNY", "OR", "SBGSY", "AXAHY", "BNPQY", "AIR.PA", "CIEIY"),
  "EWI (Italy)" = c("E", "STLA", "RACE", "ISP", "UCG", "CNHI", "PRYMY"),
  "EWP (Spain)" = c("SAN", "BBVA", "TEF", "IBE", "ITX", "REP", "AENA"),
  "EWL (Switzerland)" = c("NESN", "ROG", "NOVN", "UBS", "ZURN", "ABBN", "CFR", "HOLN", "LOGI", "SIKA"),
  "EWN (Netherlands)" = c("ASML", "ING", "PHIA", "AKZA", "HEIA", "PROX"),
  "EWD (Sweden)" = c("ERIC", "VLVLY", "ATCO-A", "EQT", "SEB-A"),
  "EWK (Belgium)" = c("ABI", "KBC", "SOLB"),
  "EWO (Austria)" = c("EBS", "OMV", "VOE"),
  "EFA (Dev Mkts)" = c("ASML", "MC", "NVO", "NESN", "ROG", "SHEL", "AZN", "SAP", "SIEGY"),
  "EWJ (Japan)" = c("TM", "SONY", "KYCOY", "HMC", "MUFG", "TAK", "SMFG", "MFG", "IX", "NMR", "KYO", "CAJ", "NJ", "FANUY"),
  "DXJ (Japan Hedged)" = c("TM", "SONY", "HMC", "MUFG", "TAK", "SMFG", "MFG", "IX"), 
  "FXI (China)" = c("BABA", "JD", "PDD", "BIDU", "TCEHY", "NIO", "LI", "XPEV", "TCOM", "NTES", "ZTO", "YUMC", "BEKE", "HTHT"),
  "EPI (India)" = c("INFY", "HDB", "IBN", "WIT", "MMYT", "AZRE", "RDY", "TATAMOTORS"),
  "EWT (Taiwan)" = c("TSM", "UMC", "ASX", "CHT"),
  "EWY (S. Korea)" = c("SSNLF", "PKX", "KB", "SHG", "LPL", "KT", "KEP", "SKM"),
  "EWA (Australia)" = c("BHP", "WBK", "RIO", "CSL", "ANZ", "NAB", "WES", "WOW"),
  "EWH (Hong Kong)" = c("AIA", "HKEX", "CLP", "TR", "HLPP"),
  "EWS (Singapore)" = c("DBS", "UOB", "OCBC", "SE"),
  "EWM (Malaysia)" = c("MAYBANK", "CIMB", "PCHEM"),
  "EPHE (Philippines)" = c("ALI", "SM", "BDO"),
  "VWO (Emerging)" = c("TSM", "TCEHY", "BABA", "SSNLF", "INFY", "VALE", "HDB", "PDD"),
  "SCHE (Emerging)" = c("TSM", "TCEHY", "BABA", "SSNLF", "INFY"),
  "SPEM (Emerging)" = c("TSM", "TCEHY", "BABA", "SSNLF", "INFY"),
  "EZA (S. Africa)" = c("GFI", "AU", "SSL", "NPSNY", "SBK", "HAR"),
  "FRN (Frontier)" = c("VN", "KAZ", "MPO"),
  "ACWX (Global exUS)" = c("TSM", "NVO", "ASML", "TCEHY", "NESN", "ROG", "SHEL", "AZN", "SAP"),
  "FM (Frontier)" = c("VN", "KAZ", "MPO")
)

country_choices <- list(
  "Americas" = c("VTI (USA Total Mkt)", "SPY (USA S&P 500)", "EWC (Canada)", "EWZ (Brazil)", "FBZ (Brazil Alpha)", "EWW (Mexico)", "ARGT (Argentina)", "GXG (Colombia)", "ECH (Chile)", "EPU (Peru)", "ILF (LatAm)"),
  "Europe" = c("EWU (UK)", "EWG (Germany)", "EWQ (France)", "EWI (Italy)", "EWP (Spain)", "EWL (Switzerland)", "EWN (Netherlands)", "EWD (Sweden)", "EWK (Belgium)", "EWO (Austria)"),
  "Asia / Pacific" = c("EWJ (Japan)", "DXJ (Japan Hedged)", "FXI (China)", "EPI (India)", "EWT (Taiwan)", "EWY (S. Korea)", "EWA (Australia)", "EWH (Hong Kong)", "EWS (Singapore)", "EWM (Malaysia)", "EPHE (Philippines)"),
  "Global & Regions" = c("VWO (Emerging)", "SCHE (Emerging)", "SPEM (Emerging)", "EFA (Dev Mkts)", "ACWX (Global exUS)", "EZA (S. Africa)", "FM (Frontier)")
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
    nav_panel("Deep Dive (GICS)", card(uiOutput("boxesGICS"), reactableOutput("stockTableGICS"))),
    nav_panel("Macro (Themes)", card(plotlyOutput("plotThemes", height = "500px")), card(reactableOutput("tableThemes"))),
    nav_panel("Deep Dive (Themes)", card(uiOutput("boxesThemes"), reactableOutput("stockTableThemes"))),
    nav_panel("Macro (Countries)", card(plotlyOutput("plotCountries", height = "500px")), card(reactableOutput("tableCountries"))),
    nav_panel("Deep Dive (Countries)", card(uiOutput("boxesCountries"), reactableOutput("stockTableCountries")))
  )
)

# --- 3. SERVER ---

server <- function(input, output, session) {
  
  # OTC Logic: Ensures foreign symbols fetch data via getQuote
  safe_get_data <- function(tickers, from_date) {
    tryCatch({
      df <- tq_get(tickers, from = from_date)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df
    }, error = function(e) { NULL })
  }
  
  fetch_stock_data <- function(tickers) {
    start_date <- Sys.Date() - 450 
    df_raw <- safe_get_data(tickers, start_date)
    if (is.null(df_raw)) return(NULL)
    
    # OTC FIX: getQuote handles T/B/M raw data and prices for foreign stocks
    mcap_info <- tryCatch({
      quotes <- getQuote(tickers, what = yahooQF(c("Market Capitalization", "Last Trade (Price Only)")))
      data.frame(
        symbol = rownames(quotes), 
        Mcap_Raw = quotes$`Market Capitalization`,
        Price_Latest = quotes$`Last Trade (Price Only)`
      )
    }, error = function(e) { data.frame(symbol = tickers, Mcap_Raw = 0, Price_Latest = NA) })
    
    metrics <- df_raw %>%
      group_by(symbol) %>%
      filter(!is.na(adjusted)) %>%
      mutate(rsi_raw = RSI(adjusted, n=14)) %>% 
      summarise(
        Price_Hist = last(adjusted),
        `1M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-30)]) - 1, error=function(e) NA),
        `3M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-90)]) - 1, error=function(e) NA),
        `6M` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-180)]) - 1, error=function(e) NA),
        `1Y` = tryCatch(last(adjusted) / last(adjusted[date <= (Sys.Date()-365)]) - 1, error=function(e) NA),
        `Vol` = tryCatch(sd(diff(log(adjusted)), na.rm = TRUE) * sqrt(252), error=function(e) NA),
        `RSI` = last(rsi_raw)
      ) %>%
      left_join(mcap_info, by = "symbol") %>%
      # OTC FIX: Fallback to Historical Price if getQuote is missing
      mutate(
        Price = ifelse(is.na(Price_Latest), Price_Hist, Price_Latest),
        Name = get_name(symbol)
      ) %>%
      # TABLE FIX: Rearrange Order (Name, Ticker, Mcap...)
      select(Name, symbol, Mcap_Raw, Price, `1M`, `3M`, `6M`, `1Y`, Vol, RSI) %>%
      arrange(desc(Mcap_Raw))
    
    return(metrics)
  }
  
  make_value_boxes <- function(df) {
    req(df); if (nrow(df) == 0) return(NULL)
    winner <- df %>% arrange(desc(`1M`)) %>% slice(1)
    laggard <- df %>% arrange(`1M`) %>% slice(1)
    layout_columns(
      value_box(title = "Top Performer (1M)", value = paste0(winner$symbol, ": ", percent(winner$`1M`, 0.1)), theme = "success"),
      value_box(title = "Lagging (1M)", value = paste0(laggard$symbol, ": ", percent(laggard$`1M`, 0.1)), theme = "danger"),
      value_box(title = "Sector Average (1M)", value = percent(mean(df$`1M`, na.rm=T), 0.1), theme = "primary")
    )
  }
  
  # --- TABLE UI FIX: TRUNCATION & CLEANING LOGIC ---
  render_stock_table <- function(df) {
    req(df)
    reactable(
      df, columns = list(
        # Mcap Truncation Logic ($T, $B, $M)
        Mcap_Raw = colDef(name = "Mcap", cell = function(v) {
          if(is.na(v) || v == 0) return("")
          if(v >= 1e12) paste0("$", round(v/1e12, 1), "T")
          else if(v >= 1e9) paste0("$", round(v/1e9, 0), "B")
          else paste0("$", round(v/1e6, 0), "M")
        }),
        symbol = colDef(name = "Ticker", style = list(fontWeight = "bold")),
        Price = colDef(format = colFormat(currency = "USD", digits = 2)),
        # Rounded Returns to 1-Decimal and Color Logic
        `1M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(v>0) "#2ecc71" else "#e74c3c", fontWeight = "bold")),
        `3M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(v>0) "#2ecc71" else "#e74c3c")),
        `6M` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(v>0) "#2ecc71" else "#e74c3c")),
        `1Y` = colDef(format = colFormat(percent = TRUE, digits = 1), style = function(v) list(color = if(v>0) "#2ecc71" else "#e74c3c")),
        Vol = colDef(format = colFormat(percent = TRUE, digits = 1)),
        RSI = colDef(format = colFormat(digits = 0), style = function(v) {
          color <- if(!is.na(v) && v>70) "#e74c3c" else if(!is.na(v) && v<30) "#2ecc71" else "black"
          list(color = color, fontWeight = "bold")
        })
      ), 
      striped = TRUE, 
      compact = TRUE,
      defaultPageSize = 15,
      theme = reactableTheme(headerStyle = list(background = "#f8f9fa"))
    )
  }
  
  # --- LOGIC: GICS ---
  data_gics_macro <- eventReactive(input$refresh, {
    safe_get_data(c(names(gics_map)[1:9], "SPY"), input$dates[1]) %>%
      group_by(symbol) %>% mutate(cum_ret = (adjusted / first(adjusted)) - 1, label = gics_map[symbol])
  }, ignoreNULL = FALSE)
  
  output$plotGICS <- renderPlotly({
    req(data_gics_macro())
    p <- ggplot(data_gics_macro() %>% filter(symbol %in% c(input$gics_visible, "SPY")), aes(x=date, y=cum_ret, color=label)) + geom_line() + scale_y_continuous(labels=percent) + theme_minimal()
    ggplotly(p)
  })
  
  output$tableGICS <- renderReactable({
    req(data_gics_macro())
    r <- data_gics_macro() %>% group_by(label) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })
  
  data_gics_deep <- reactive({ fetch_stock_data(gics_list[[input$gics_active]]) })
  output$boxesGICS <- renderUI({ make_value_boxes(data_gics_deep()) })
  output$stockTableGICS <- renderReactable({ render_stock_table(data_gics_deep()) })
  
  # --- LOGIC: THEMES ---
  data_theme_macro <- eventReactive(input$refresh, {
    tks <- unique(c(sub("\\s.*", "", input$theme_visible), "SPY"))
    safe_get_data(tks, input$dates[1]) %>% group_by(symbol) %>% mutate(cum_ret = (adjusted/first(adjusted))-1)
  }, ignoreNULL = FALSE)
  
  output$plotThemes <- renderPlotly({
    req(data_theme_macro())
    p <- ggplot(data_theme_macro(), aes(x=date, y=cum_ret, color=symbol)) + geom_line() + scale_y_continuous(labels=percent) + theme_minimal()
    ggplotly(p)
  })
  
  output$tableThemes <- renderReactable({
    req(data_theme_macro())
    r <- data_theme_macro() %>% group_by(symbol) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })
  
  data_theme_deep <- reactive({ fetch_stock_data(theme_list[[input$theme_active]]) })
  output$boxesThemes <- renderUI({ make_value_boxes(data_theme_deep()) })
  output$stockTableThemes <- renderReactable({ render_stock_table(data_theme_deep()) })
  
  # --- LOGIC: COUNTRIES ---
  data_country_macro <- eventReactive(input$refresh, {
    tks <- unique(c(sub("\\s.*", "", input$country_visible), "VTI"))
    safe_get_data(tks, input$dates[1]) %>% group_by(symbol) %>% mutate(cum_ret = (adjusted/first(adjusted))-1)
  }, ignoreNULL = FALSE)
  
  output$plotCountries <- renderPlotly({
    req(data_country_macro())
    p <- ggplot(data_country_macro(), aes(x=date, y=cum_ret, color=symbol)) + geom_line() + scale_y_continuous(labels=percent) + theme_minimal()
    ggplotly(p)
  })
  
  output$tableCountries <- renderReactable({
    req(data_country_macro())
    r <- data_country_macro() %>% group_by(symbol) %>% summarise(Return = last(cum_ret)) %>% arrange(desc(Return))
    reactable(r, columns = list(Return = colDef(format = colFormat(percent = TRUE, digits = 1))))
  })
  
  data_country_deep <- reactive({ fetch_stock_data(country_list[[input$country_active]]) })
  output$boxesCountries <- renderUI({ make_value_boxes(data_country_deep()) })
  output$stockTableCountries <- renderReactable({ render_stock_table(data_country_deep()) })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("market-data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { write.csv(data_country_deep(), file) }
  )
}

shinyApp(ui, server)
