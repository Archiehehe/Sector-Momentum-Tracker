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
library(zoo)

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

get_currency_symbol <- function(ticker) {
  if (grepl("\\.T$", ticker)) return("¥") 
  if (grepl("\\.HK$", ticker)) return("HK$")
  if (grepl("\\.DE$|\\.PA$|\\.MC$|\\.AS$|\\.LS$|\\.BR$|\\.MI$|\\.HE$|\\.VI$|\\.AT$", ticker)) return("€")
  if (grepl("\\.L$", ticker)) return("£")
  if (grepl("\\.KS$", ticker)) return("₩")
  if (grepl("\\.TW$", ticker)) return("NT$")
  if (grepl("\\.NS$|\\.BO$", ticker)) return("₹")
  if (grepl("\\.SA$", ticker)) return("R$") # Brazil
  if (grepl("\\.JO$|\\.ZA$", ticker)) return("R")  # South Africa
  if (grepl("\\.SR$", ticker)) return("﷼")  # Saudi
  if (grepl("\\.TO$", ticker)) return("C$")
  if (grepl("\\.MX$", ticker)) return("Mex$")
  return("$")
}

clean_mcap <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || x == "N/A" || x == "") return(0)
  if (is.numeric(x)) return(x)
  num_part <- as.numeric(gsub("[^0-9\\.]", "", x))
  if (is.na(num_part)) return(0)
  last_char <- toupper(substr(x, nchar(x), nchar(x)))
  if (last_char == "T") return(num_part * 1e12)
  if (last_char == "B") return(num_part * 1e9)
  if (last_char == "M") return(num_part * 1e6)
  return(num_part)
}

# ==============================================================================
# 2. MASTER DATA: THEMES (EXHAUSTIVE & CATEGORIZED)
# ==============================================================================

THEME_DATABASE <- list(
  # --- 1. COMMODITIES & HARD ASSETS ---
  list(group="Commodities", label="Copper Miners (COPX)", ticker="COPX", holdings=c("FCX", "SCCO", "TECK", "HBM", "ERO", "BHP", "RIO", "IVN.TO", "TRQ", "LUN.TO", "FM.TO", "CMMC.TO", "ALS.TO", "CS.TO")),
  list(group="Commodities", label="Gold Miners (GDX)", ticker="GDX", holdings=c("NEM", "GOLD", "AEM", "WPM", "KGC", "AU", "GFI", "RGLD", "IAG", "NG", "BTG", "EQX", "PAAS", "SSRM", "AG", "CDE", "HL", "EXK")),
  list(group="Commodities", label="Lithium & Battery (LIT)", ticker="LIT", holdings=c("ALB", "SQM", "TSLA", "BYDDF", "LAC", "PILBF", "LTHM", "GANX", "SGML", "PLL", "MIN.AX", "AKE.AX", "PLS.AX", "LTR.AX")),
  list(group="Commodities", label="Agribusiness (MOO)", ticker="MOO", holdings=c("DE", "CTVA", "NTR", "ADM", "TSN", "BG", "MOS", "FMC", "AGCO", "CNHI", "ANDE", "CALM", "CF", "ICL", "YARA.OL")),
  list(group="Commodities", label="Timber & Forestry (WOOD)", ticker="WOOD", holdings=c("WY", "RFP", "CTT", "PCH", "RYN", "LPX", "WFG", "IP", "WRK", "PKG", "SON", "SEE", "SCA-B.ST", "HOLM-B.ST")),
  list(group="Commodities", label="Rare Earths (REMX)", ticker="REMX", holdings=c("MP", "LYSDY", "ILUKA.AX", "LAC", "PLL", "TROX", "ARE", "Lynas", "ASM.AX")),
  list(group="Commodities", label="Steel (SLX)", ticker="SLX", holdings=c("NUE", "STLD", "X", "CLF", "MT", "TX", "RS", "CMC", "GGB", "SID", "PKX", "TATASTEEL.NS", "JSWSTEEL.NS")),
  list(group="Commodities", label="Water (PHO)", ticker="PHO", holdings=c("XYL", "AWK", "WTS", "DHR", "ECL", "PNR", "AWR", "CWT", "SBS", "YORW")),
  
  # --- 2. DEEP TECH & FUTURE ---
  list(group="Future Tech", label="Semiconductors (SMH)", ticker="SMH", holdings=c("NVDA", "TSM", "AVGO", "AMD", "INTC", "QCOM", "TXN", "MU", "AMAT", "LRCX", "ADI", "NXPI", "MCHP", "ON", "STM", "SWKS", "QRVO", "KLAC", "TER", "ENTG", "AMKR", "SNPS", "CDNS", "ARM", "MRVL", "WOLF", "LSCC", "MPWR", "RMBS", "DIOD", "SLAB")),
  list(group="Future Tech", label="AI & Robotics (BOTZ)", ticker="BOTZ", holdings=c("ISRG", "NVDA", "ABB", "6861.T", "6954.T", "PATH", "UPST", "AI", "TER", "IRBT", "KEY", "HUBS", "CRWD", "SNOW", "DDOG", "ZS", "NET", "PLTR", "SYM", "ROK", "CGNX", "PRO", "WK", "NVTS")),
  list(group="Future Tech", label="Cloud Computing (SKYY)", ticker="SKYY", holdings=c("ORCL", "MSFT", "GOOGL", "AMZN", "CRM", "ADBE", "INTU", "NOW", "SNOW", "PLTR", "DDOG", "NET", "MDB", "ZS", "CRWD", "PANW", "FTNT", "OKTA", "ZM", "DOCU", "TWLO", "BOX", "DBX", "SHOP", "SQ", "VEEV", "WDAY", "TEAM", "ADSK")),
  list(group="Future Tech", label="Cybersecurity (CIBR)", ticker="CIBR", holdings=c("PANW", "CRWD", "FTNT", "OKTA", "ZS", "NET", "CHKP", "CYBR", "TENB", "RPD", "QLYS", "VRNS", "SENT", "GEN", "RDWR", "S", "HACK", "FEYE")),
  list(group="Future Tech", label="Fintech (FINX)", ticker="FINX", holdings=c("SQ", "PYPL", "COIN", "AFRM", "SOFI", "HOOD", "FIS", "FISV", "GPN", "MA", "V", "AXP", "DFS", "NU", "STNE", "PAGS", "MELI", "TOST", "BILL", "MQ", "UPST", "LC", "JKHY")),
  list(group="Future Tech", label="Blockchain (BLOK)", ticker="BLOK", holdings=c("MSTR", "COIN", "MARA", "RIOT", "CLSK", "SQ", "PYPL", "NVDA", "AMD", "TSM", "INTC", "CME", "ICE", "OSTK", "HUT", "BITF", "CORZ")),
  list(group="Future Tech", label="Social Media (SOCL)", ticker="SOCL", holdings=c("META", "GOOGL", "SNAP", "PINS", "TWTR", "MTCH", "BMBL", "SPOT", "ROKU", "YELP", "WB", "BILI", "IQ", "YY", "TCEHY", "NAVER.KS", "KAKAO.KS")),
  list(group="Future Tech", label="Metaverse (METV)", ticker="METV", holdings=c("RBLX", "U", "META", "NVDA", "QCOM", "SONY", "MTTR", "MSFT", "ATVI", "EA", "TTWO", "ADBE", "SNAP")),
  list(group="Future Tech", label="Quantum Computing (QTUM)", ticker="QTUM", holdings=c("IONQ", "RGTI", "QUBT", "HON", "IBM", "GOOGL", "MSFT", "INTC", "NVDA", "AMD")),
  list(group="Future Tech", label="Space (ROKT)", ticker="ROKT", holdings=c("RKLB", "SPCE", "LMT", "NOC", "BA", "IRDM", "ASTS", "PL", "VSAT", "GSAT", "SID", "MAXR")),
  list(group="Future Tech", label="3D Printing (PRNT)", ticker="PRNT", holdings=c("SSYS", "DDD", "XONE", "PRLB", "MTLS", "DM", "VJET", "NNDM")),
  
  # --- 3. ENERGY & GREEN REVOLUTION ---
  list(group="Energy", label="Oil & Gas E&P (XOP)", ticker="XOP", holdings=c("XOM", "CVX", "COP", "EOG", "OXY", "HES", "DVN", "PXD", "MRO", "FANG", "CTRA", "APA", "EQT", "VLO", "MPC", "PSX", "HAL", "SLB", "BKR", "WMB", "KMI", "OKE", "TRGP", "LNG", "CHK", "AR", "RRC")),
  list(group="Energy", label="Clean Energy (ICLN)", ticker="ICLN", holdings=c("PLUG", "ENPH", "FSLR", "ED", "CIG", "ORSTED.CO", "SEDG", "RUN", "NOVA", "BE", "FCEL", "SPWR", "CSIQ", "ARRY", "NXT", "SHLS", "TPIC", "NEP", "CWEN", "HASI")),
  list(group="Energy", label="Oil Services (OIH)", ticker="OIH", holdings=c("SLB", "HAL", "BKR", "NOV", "FTI", "CHX", "LB", "NEX", "OII", "PTEN", "RES", "WHD", "NBR", "HP")),
  list(group="Energy", label="Solar (TAN)", ticker="TAN", holdings=c("FSLR", "ENPH", "SEDG", "RUN", "SPWR", "NOVA", "SHLS", "JKS", "DQ", "CSIQ", "ARRY", "MAXN")),
  list(group="Energy", label="Wind (FAN)", ticker="FAN", holdings=c("NEP", "GE", "TPIC", "CWEN", "VWDRY", "VWS.CO", "ORSTED.CO", "GAMESA.MC", "NPI.TO")),
  list(group="Energy", label="Hydrogen (HJEN)", ticker="HJEN", holdings=c("PLUG", "BE", "FCEL", "BLDP", "LIN", "APD", "HYGS", "NKLA", "CMI")),
  list(group="Energy", label="Uranium (URA)", ticker="URA", holdings=c("CCJ", "UUUU", "NXE", "UEC", "DNN", "URA", "KAP.IL", "PDN.AX", "DYL.AX", "BOE.AX", "YCA.L")),
  list(group="Energy", label="EVs (DRIV)", ticker="DRIV", holdings=c("TSLA", "RIVN", "LCID", "NIO", "XPEV", "LI", "F", "GM", "BYDDF", "TM", "HMC", "STLA", "RACE")),
  list(group="Energy", label="Smart Grid (GRID)", ticker="GRID", holdings=c("ETN", "ITRI", "PWR", "HUBB", "EME", "ROK", "Schneider", "ABB", "SIE.DE", "HON", "GE")),
  
  # --- 4. INDUSTRIALS ---
  list(group="Industrials", label="Infrastructure (PAVE)", ticker="PAVE", holdings=c("ETN", "DE", "CAT", "URI", "VMC", "PWR", "VRSK", "WAB", "EFX", "CSX", "NSC", "UNP", "WM", "RSG", "MMM")),
  list(group="Industrials", label="Aerospace & Def (ITA)", ticker="ITA", holdings=c("RTX", "LMT", "BA", "NOC", "GD", "LHX", "TDG", "AXON", "HII", "TXT", "KTOS", "AVAV", "LDOS", "CACI")),
  list(group="Industrials", label="Transport (IYT)", ticker="IYT", holdings=c("UNP", "UPS", "CSX", "NSC", "FDX", "ODFL", "JBHT", "LSTR", "KNX", "XPO", "GXO", "SAIA")),
  list(group="Industrials", label="Airlines (JETS)", ticker="JETS", holdings=c("DAL", "UAL", "LUV", "AAL", "ALK", "JBLU", "SAVE", "HA", "SKYW", "RYAAY", "WIZZ.L", "EZJ.L")),
  list(group="Industrials", label="Shipping (BOAT)", ticker="BOAT", holdings=c("ZIM", "MATX", "GNK", "EGLE", "SBLK", "DAC", "GSL", "CMRE", "NMM", "FRO", "EURN")),
  
  # --- 5. CONSUMER ---
  list(group="Consumer", label="Online Retail (IBUY)", ticker="IBUY", holdings=c("AMZN", "MELI", "EBAY", "ETSY", "CPNG", "CHWY", "W", "RVLV", "CVNA", "OSTK", "PTON", "FVRR", "UPWK", "POSH", "REAL")),
  list(group="Consumer", label="Sports Betting (BETZ)", ticker="BETZ", holdings=c("DKNG", "FLUT", "MGM", "LVS", "PENN", "CZR", "WYNN", "RUSH", "GENI", "EVO.SS", "ENT.L")),
  list(group="Consumer", label="Video Games (HERO)", ticker="HERO", holdings=c("EA", "TTWO", "RBLX", "SONY", "NTDOY", "UBSFY", "NCBDY", "SE", "TCEHY", "NTES", "BILI", "ZNGA")),
  list(group="Consumer", label="Luxury (LUXE)", ticker="LUXE", holdings=c("LVMUY", "HESAY", "RACE", "EL", "NKE", "TPR", "CPRI", "KER.PA", "RMS.PA", "CFR.SW", "MONC.MI", "SFER.MI", "BRBY.L")),
  list(group="Consumer", label="Cannabis (MSOS)", ticker="MSOS", holdings=c("GTBIF", "CURLF", "TCNNF", "TLRY", "CGC", "CRON", "ACB", "SNDL", "MAPS", "GRWG", "IIPR")),
  list(group="Consumer", label="Pet Care (PAWZ)", ticker="PAWZ", holdings=c("CHWY", "ZTS", "IDXX", "PETS", "ELAN", "FRPT", "CVET", "CL", "NESTLE")),
  list(group="Consumer", label="Millennial Spending (MILN)", ticker="MILN", holdings=c("UBER", "ABNB", "LULU", "CMG", "SBUX", "COST", "TGT", "NKE", "DIS", "NFLX", "GOOGL", "META")),
  list(group="Consumer", label="Homebuilders (XHB)", ticker="XHB", holdings=c("DHI", "LEN", "PHM", "TOL", "NVR", "KBH", "TMHC", "MDC", "MTH", "TPH", "LGIH", "GRBK")),
  list(group="Consumer", label="Travel (PEJ)", ticker="PEJ", holdings=c("BKNG", "EXPE", "MAR", "HLT", "ABNB", "CCL", "RCL", "NCLH", "TRIP", "TCOM", "DESP", "MMYT")),
  
  # --- 6. HEALTHCARE ---
  list(group="Healthcare", label="Biotech (XBI)", ticker="XBI", holdings=c("VRTX", "AMGN", "REGN", "MRNA", "BNTX", "SGEN", "ALNY", "BMRN", "INCY", "UTHR", "NBIX", "TECH", "SRPT", "EXAS", "NTRA", "CRSP", "EDIT", "NTLA", "BEAM")),
  list(group="Healthcare", label="Genomics (ARKG)", ticker="ARKG", holdings=c("CRSP", "PACB", "TWST", "EXAS", "TDOC", "NTLA", "BEAM", "NVTA", "RPTX", "SDGR", "TXG", "MASS")),
  list(group="Healthcare", label="Medical Devices (IHI)", ticker="IHI", holdings=c("TMO", "ABT", "MDT", "DHR", "SYK", "BSX", "EW", "ISRG", "BDX", "ZBH", "COO", "ALGN", "RMD", "BAX", "HCA", "STE")),
  list(group="Healthcare", label="Telemedicine (EDOC)", ticker="EDOC", holdings=c("TDOC", "DOCS", "AMWL", "HIMS", "DGLY", "LFST", "ONEM")),
  
  # --- 7. FINANCIALS ---
  list(group="Financials", label="Regional Banks (KRE)", ticker="KRE", holdings=c("DPST", "NYCB", "WAL", "KEY", "RF", "HBAN", "ZION", "CMA", "FITB", "MTB", "CFG", "SBNY")),
  list(group="Financials", label="Private Equity (PSP)", ticker="PSP", holdings=c("BLK", "KKR", "BX", "APO", "CG", "ARES", "OWL", "TROW", "BEN", "IVZ")),
  list(group="Financials", label="REITs (IYR)", ticker="IYR", holdings=c("PLD", "AMT", "EQIX", "CCI", "PSA", "O", "SPG", "VICI", "WELL", "DLR", "AVB", "EQR", "CBRE", "CSGP")),
  list(group="Financials", label="Data Center REITs (SRVR)", ticker="SRVR", holdings=c("EQIX", "DLR", "AMT", "CCI", "SBAC", "IRM", "UNIT", "COR")),
  
  # --- 8. FACTORS & MACRO ---
  list(group="Factors", label="Momentum (MTUM)", ticker="MTUM", holdings=c("NVDA", "LLY", "META", "AVGO", "AMZN", "MSFT", "GOOGL", "TSLA")),
  list(group="Factors", label="Value (VLUE)", ticker="VLUE", holdings=c("INTC", "T", "GM", "F", "C", "VZ", "WBA", "KHC", "DOW")),
  list(group="Factors", label="High Dividend (DIV)", ticker="DIV", holdings=c("MO", "T", "VZ", "KMI", "EPD", "MMP", "O", "MAIN", "ARCC")),
  list(group="Macro", label="US Dollar (UUP)", ticker="UUP", holdings=c("UUP", "USDU")),
  list(group="Macro", label="Euro (FXE)", ticker="FXE", holdings=c("FXE", "EURUSD=X")),
  list(group="Macro", label="Japanese Yen (FXY)", ticker="FXY", holdings=c("FXY", "JPY=X")),
  list(group="Macro", label="Long Bonds (TLT)", ticker="TLT", holdings=c("TLT", "TMF", "IEF", "SHY")),
  list(group="Macro", label="High Yield Bonds (HYG)", ticker="HYG", holdings=c("HYG", "JNK")),
  list(group="Macro", label="Volatility (VXX)", ticker="VXX", holdings=c("VXX", "UVXY", "SVXY"))
)

# ==============================================================================
# 3. MASTER DATA: COUNTRIES (EXHAUSTIVE ROSTERS)
# ==============================================================================

country_data <- list(
  # --- AMERICAS ---
  "United States (SPY)" = list(etf="SPY", stocks=c("AAPL", "MSFT", "NVDA", "AMZN", "META", "GOOGL", "BRK-B", "LLY", "JPM", "TSLA", "V", "XOM", "UNH", "MA", "PG", "JNJ", "HD", "MRK", "COST", "ABBV", "CVX", "CRM", "BAC", "WMT", "AMD", "NFLX", "KO", "PEP", "TMO", "LIN", "ADBE", "DIS", "MCD", "CSCO", "ABT", "TMUS", "INTC", "CMCSA", "PFE", "VZ", "NKE", "WFC", "INTU", "QCOM", "TXN", "DHR", "PM", "CAT", "IBM", "AMGN", "GE", "UNP")),
  "Canada (EWC)" = list(etf="EWC", stocks=c("RY", "TD", "SHOP", "ENB", "CNQ", "CP", "BMO", "TRI", "BNS", "TRP", "CNR", "ATD", "CSU", "POW", "FTS", "WCN", "AEM", "MFC", "IMO", "PPL", "BCE", "GIB-A", "MG", "TECK", "DOL", "SLF", "NA", "L", "WN", "EMA", "BAM", "BN", "TOU", "FM")),
  "Mexico (EWW)" = list(etf="EWW", stocks=c("AMX", "KOF", "CX", "FMX", "OMAB", "PAC", "ASR", "VLRS", "WALMEX.MX", "GMEXICOB.MX", "CEMEXCPO.MX", "GAPB.MX", "BIMBOA.MX", "GRUMAB.MX", "ALFAA.MX", "GFNORTEO.MX", "PE&OLES.MX", "KIMBERA.MX", "ORBIA.MX")),
  "Brazil (EWZ)" = list(etf="EWZ", stocks=c("PBR", "VALE", "ITUB", "BBD", "NU", "BSBR", "SUZ", "GGB", "ERJ", "SID", "ABEV", "JBSS3.SA", "WEGE3.SA", "RENT3.SA", "BBAS3.SA", "ITSA4.SA", "B3SA3.SA", "ELET3.SA", "LREN3.SA", "RAIL3.SA", "EQTL3.SA", "VIVT3.SA", "PRIO3.SA", "CSAN3.SA", "PETR3.SA", "PETR4.SA", "VALE3.SA", "ITUB4.SA", "BBDC4.SA")),
  "Argentina (ARGT)" = list(etf="ARGT", stocks=c("MELI", "YPF", "BMA", "PAM", "GGAL", "TEO", "CEPU", "VIST", "TGS", "CRESY", "EDN", "LOMA", "SUPV", "IRS")),
  "Chile (ECH)" = list(etf="ECH", stocks=c("SQM", "BCH", "CCU", "LTM", "ENIC", "BSAC")),
  "Colombia (GXG)" = list(etf="GXG", stocks=c("CIB", "EC", "PFBCOLOM.CN", "ECOPETROL.CN", "GEB.CN")), 
  "Peru (EPU)" = list(etf="EPU", stocks=c("SCCO", "BAP", "BVN", "CPAC", "IFS")),
  
  # --- EUROPE: MAJOR ---
  "United Kingdom (EWU)" = list(etf="EWU", stocks=c("SHEL", "AZN", "HSBC", "UL", "BP", "DEO", "BTI", "GSK", "RIO", "RELX", "NGG", "VOD", "LLOY", "BARC", "NWG", "RR.L", "AAL.L", "GLEN.L", "LSEG.L", "CPG.L", "BA.L", "TSCO.L", "PRU.L", "STAN.L", "EXPN.L", "CRDA.L", "DGE.L", "BATS.L", "BP.L", "SHEL.L", "HSBA.L", "AZN.L", "ULVR.L", "GSK.L", "RIO.L")),
  "Germany (EWG)" = list(etf="EWG", stocks=c("SAP", "SIEGY", "DTEGY", "ALIZY", "VWAGY", "MBGYY", "BMWYY", "BASFY", "BAYRY", "DHLGY", "MUV2.DE", "IFX.DE", "DBK.DE", "ADS.DE", "VOW3.DE", "BEI.DE", "EOAN.DE", "RWE.DE", "SY1.DE", "DB1.DE", "HEI.DE", "CON.DE", "FRE.DE", "SIE.DE", "ALV.DE", "DTE.DE", "BMW.DE", "BAS.DE", "BAYN.DE", "DHL.DE")),
  "France (EWQ)" = list(etf="EWQ", stocks=c("LVMUY", "TTE", "SNY", "OR", "SBGSY", "AXAHY", "BNPQY", "EADSY", "DANOY", "MC.PA", "OR.PA", "TTE.PA", "SAN.PA", "AIR.PA", "RMS.PA", "KER.PA", "EL.PA", "BNP.PA", "CS.PA", "DG.PA", "SGO.PA", "ORA.PA", "VIE.PA", "VIV.PA", "EN.PA", "CAP.PA", "LR.PA", "ACA.PA")),
  "Switzerland (EWL)" = list(etf="EWL", stocks=c("NSRGY", "RHHBY", "NVS", "UBS", "ABBNY", "CFRUY", "LZAGY", "SIKA.SW", "LONN.SW", "NOVN.SW", "ROG.SW", "ZURN.SW", "ALC.SW", "GIVN.SW")),
  "Netherlands (EWN)" = list(etf="EWN", stocks=c("ASML", "ING", "STLA", "PHG", "HEINY", "AKZAY", "RAND", "IMBBY", "ADYEN.AS", "DSM.AS", "UNA.AS", "SHELL.AS", "NN.AS", "WKL.AS", "ASM.AS")),
  "Italy (EWI)" = list(etf="EWI", stocks=c("E", "STLA", "RACE", "PPRUY", "IIJIY", "ISP.MI", "ENI.MI", "UCG.MI", "ENEL.MI", "FER.MI", "G.MI", "STMMI.MI", "CNHI.MI", "TRN.MI", "MONC.MI", "PRY.MI", "SRG.MI", "TIT.MI")),
  "Spain (EWP)" = list(etf="EWP", stocks=c("BBVA", "SAN", "TEF", "ITX.MC", "IBE.MC", "AENA.MC", "REP.MC", "CABK.MC", "ACS.MC", "FER.MC", "RED.MC", "NTGY.MC", "EDP.MC", "GRF.MC")),
  "Portugal (via EZU)" = list(etf="EZU", stocks=c("EDP.LS", "GALP.LS", "JMT.LS", "BCP.LS", "NOS.LS")),
  
  # --- EUROPE: NORDICS ---
  "Denmark (EDEN)" = list(etf="EDEN", stocks=c("NVO", "DNKEY", "VWS.CO", "MAERSK-B.CO", "ORSTED.CO", "DSV.CO", "GMAB.CO", "CARL-B.CO", "NZYM-B.CO", "DEMANT.CO")),
  "Sweden (EWD)" = list(etf="EWD", stocks=c("ERIC", "VLVLY", "ATCO-A.ST", "ELUX-B.ST", "SAND.ST", "EVO.ST", "SEB-A.ST", "NDA-SE.ST", "HM-B.ST", "INVE-B.ST", "TEL2-B.ST", "EPI-A.ST", "ALFA.ST", "BOL.ST", "SCA-B.ST")),
  "Norway (NORW)" = list(etf="NORW", stocks=c("EQNR", "NHYDY", "YAR.OL", "MOWI.OL", "DNB.OL", "TEL.OL", "AKERBP.OL", "ORK.OL", "SUBC.OL", "SALM.OL", "GJF.OL", "TOM.OL")),
  "Finland (EFNL)" = list(etf="EFNL", stocks=c("NOK", "KNEBV.HE", "SAMPO.HE", "NDA-FI.HE", "UPM.HE", "WRT1V.HE", "FORTUM.HE", "NESTE.HE", "KCR.HE", "ELISA.HE", "METSO.HE", "VALMT.HE")),
  
  # --- EUROPE: NICHE & FRONTIER ---
  "Ireland (EIRL)" = list(etf="EIRL", stocks=c("RYAAY", "CRH", "FLUT", "AIB", "BIRG.IR", "KRX.IR", "DCC.L", "KC.IR")),
  "Belgium (EWK)" = list(etf="EWK", stocks=c("BUD", "KBC.BR", "UCB.BR", "ABI.BR", "SOLB.BR", "ARGX.BR", "ACKB.BR", "GBLB.BR", "UMI.BR")),
  "Austria (EWO)" = list(etf="EWO", stocks=c("EBS.VI", "OMV.VI", "VER.VI", "VOE.VI", "RBI.VI", "ANDR.VI", "WIE.VI")),
  "Poland (EPOL)" = list(etf="EPOL", stocks=c("PKO.WA", "PKN.WA", "PZU.WA", "DNP.WA", "KGH.WA", "PEO.WA", "ALE.WA", "LPP.WA", "CDR.WA", "SPL.WA")),
  "Greece (GREK)" = list(etf="GREK", stocks=c("EEE", "OTE", "OPAP.AT", "ALPHA.AT", "ETE.AT", "EUROB.AT", "TPEIR.AT", "PPC.AT", "MOH.AT", "MYTIL.AT", "JUMBO.AT")),
  "Turkey (TUR)" = list(etf="TUR", stocks=c("THYAO.IS", "KCHOL.IS", "BIMAS.IS", "AKBNK.IS", "GARAN.IS", "TUPRS.IS", "SISE.IS", "EREGL.IS", "FROTO.IS", "YKBNK.IS", "ASELS.IS", "TCELL.IS", "PGSUS.IS", "SAHOL.IS", "ISCTR.IS")),
  "Baltics Proxy (via CEE)" = list(etf="CEE", stocks=c("PKO", "OTP", "CEZ")),
  
  # --- ASIA GIANTS ---
  "India (EPI)" = list(etf="EPI", stocks=c("RELIANCE.NS", "TCS.NS", "HDFCBANK.NS", "ICICIBANK.NS", "INFY.NS", "HINDUNILVR.NS", "ITC.NS", "SBIN.NS", "BHARTIARTL.NS", "KOTAKBANK.NS", "LT.NS", "AXISBANK.NS", "BAJFINANCE.NS", "HCLTECH.NS", "ASIANPAINT.NS", "TITAN.NS", "MARUTI.NS", "ULTRACEMCO.NS", "SUNPHARMA.NS", "TATAMOTORS.NS", "WIPRO.NS", "ADANIENT.NS", "ONGC.NS", "NTPC.NS", "POWERGRID.NS", "JSWSTEEL.NS", "TATASTEEL.NS", "M&M.NS", "LTIM.NS", "ADANIPORTS.NS", "COALINDIA.NS", "SIEMENS.NS", "SBILIFE.NS", "BAJAJFINSV.NS", "GRASIM.NS", "TECHM.NS", "PIDILITIND.NS", "HINDALCO.NS", "INDUSINDBK.NS", "NESTLEIND.NS")),
  "China (FXI)" = list(etf="FXI", stocks=c("0700.HK", "9988.HK", "3690.HK", "0939.HK", "1299.HK", "0941.HK", "1398.HK", "0005.HK", "0883.HK", "9999.HK", "2318.HK", "0388.HK", "2015.HK", "1211.HK", "9618.HK", "1810.HK", "1024.HK", "9888.HK", "0992.HK", "1928.HK", "2269.HK", "2331.HK", "6690.HK", "1109.HK", "1177.HK", "2319.HK", "0669.HK", "1929.HK", "9866.HK", "9868.HK", "0762.HK")),
  "Japan (EWJ)" = list(etf="EWJ", stocks=c("7203.T", "6758.T", "8035.T", "6861.T", "9984.T", "9432.T", "6954.T", "8306.T", "6902.T", "7974.T", "8058.T", "8031.T", "8001.T", "4063.T", "4502.T", "7741.T", "9983.T", "6501.T", "6702.T", "6367.T", "4519.T", "4568.T", "6981.T", "3382.T", "8316.T", "6594.T", "6273.T", "9022.T", "9020.T", "7267.T", "8766.T", "8802.T", "4452.T", "2914.T", "5108.T")),
  "South Korea (EWY)" = list(etf="EWY", stocks=c("005930.KS", "000660.KS", "373220.KS", "207940.KS", "005380.KS", "000270.KS", "005490.KS", "035420.KS", "068270.KS", "051910.KS", "006400.KS", "035720.KS", "011200.KS", "032830.KS", "015760.KS", "034020.KS", "010130.KS", "096770.KS", "017670.KS", "018260.KS", "009150.KS", "028260.KS", "329180.KS", "033780.KS", "003550.KS")),
  "Taiwan (EWT)" = list(etf="EWT", stocks=c("2330.TW", "2454.TW", "2317.TW", "2308.TW", "2303.TW", "2881.TW", "2882.TW", "1301.TW", "1303.TW", "2002.TW", "1216.TW", "2886.TW", "3711.TW", "2382.TW", "2395.TW", "2891.TW", "2884.TW", "5880.TW", "2892.TW", "3008.TW", "3045.TW", "4904.TW", "1101.TW", "2912.TW")),
  
  # --- ASIA: DEVELOPED & ASEAN ---
  "Australia (EWA)" = list(etf="EWA", stocks=c("BHP.AX", "CBA.AX", "CSL.AX", "NAB.AX", "WBC.AX", "ANZ.AX", "FMG.AX", "WES.AX", "TLS.AX", "WOW.AX", "STO.AX", "QBE.AX", "SUN.AX", "ORG.AX", "AMC.AX", "GMG.AX", "S32.AX", "NST.AX", "MIN.AX", "PLS.AX")),
  "New Zealand (ENZL)" = list(etf="ENZL", stocks=c("XRO.AX", "FPH.NZ", "AIA.NZ", "SPK.NZ", "MEL.NZ", "IFT.NZ", "EBO.NZ", "MFT.NZ", "ATM.NZ")),
  "Hong Kong (EWH)" = list(etf="EWH", stocks=c("AIA", "HKEX", "0016.HK", "0003.HK", "0002.HK", "0011.HK", "0066.HK", "0823.HK", "0006.HK", "1997.HK", "0017.HK")),
  "Singapore (EWS)" = list(etf="EWS", stocks=c("D05.SI", "O39.SI", "Z74.SI", "C52.SI", "U11.SI", "S68.SI", "C61U.SI", "A17U.SI", "C38U.SI")),
  "Indonesia (EIDO)" = list(etf="EIDO", stocks=c("BBCA.JK", "BBRI.JK", "BMRI.JK", "BBNI.JK", "ASII.JK", "TLKM.JK", "UNVR.JK", "ICBP.JK", "GOTO.JK", "ADRO.JK", "KLBF.JK", "INDF.JK", "CPIN.JK")),
  "Thailand (THD)" = list(etf="THD", stocks=c("PTT.BK", "AOT.BK", "CPALL.BK", "SCC.BK", "ADVANC.BK", "BDMS.BK", "SCB.BK", "KBANK.BK", "PTTEP.BK", "GULF.BK", "CPN.BK", "INTUCH.BK")),
  "Malaysia (EWM)" = list(etf="EWM", stocks=c("1155.KL", "1295.KL", "5347.KL", "1023.KL", "5183.KL", "6033.KL", "1066.KL", "5819.KL", "3816.KL", "5681.KL", "4065.KL")),
  "Philippines (EPHE)" = list(etf="EPHE", stocks=c("SM", "SMPH", "ALI", "AC", "JFC", "ICT", "BPI", "BDO", "MER", "TEL", "GLO", "URC")),
  "Vietnam (VNM)" = list(etf="VNM", stocks=c("VNM", "VCB.VN", "VIC.VN", "VHM.VN", "VRE.VN", "VNM.VN", "GAS.VN", "MSN.VN", "HPG.VN", "BID.VN", "CTG.VN", "VPB.VN", "TCB.VN", "MBB.VN", "SAB.VN", "NVL.VN", "GVR.VN")),
  
  # --- MIDDLE EAST & AFRICA ---
  "Saudi Arabia (KSA)" = list(etf="KSA", stocks=c("2222.SR", "1120.SR", "1180.SR", "2010.SR", "7010.SR", "4030.SR", "2020.SR", "1010.SR", "1150.SR", "5110.SR", "1211.SR", "2280.SR", "2310.SR", "2350.SR", "1111.SR", "2330.SR", "4190.SR")),
  "United Arab Emirates (UAE)" = list(etf="UAE", stocks=c("EMAAR.AE", "FAB.AE", "ETISALAT.AE", "ENBD.DU", "ADCB.AD", "ALDAR.AD", "DIB.DU", "EAND.AD", "ADIB.AD")),
  "Qatar (QAT)" = list(etf="QAT", stocks=c("QNBK.QA", "IQCD.QA", "ORDS.QA", "QIBK.QA", "CBQK.QA", "QGTS.QA", "MARK.QA", "QFLS.QA", "BRES.QA")),
  "Israel (EIS)" = list(etf="EIS", stocks=c("NICE", "TEVA", "CHKP", "CYBR", "MBLY", "TSEM", "WIX", "ICL", "ESLT", "FVRR", "INMD", "PERI", "SEDG", "AUDC", "CAMT")),
  "South Africa (EZA)" = list(etf="EZA", stocks=c("GFI", "AU", "HMY", "SBSW", "NPSNY", "SSL", "SHP.JO", "NED.JO", "FSR.JO", "SBK.JO", "SOL.JO", "MTN.JO", "VOD.JO", "IMP.JO", "AMS.JO", "BTI.JO", "CFR.JO", "NPN.JO", "CPI.JO", "DSY.JO")),
  "Frontier Proxy (FM)" = list(etf="FM", stocks=c("KSP.IL", "Banca Transilvania", "Vinhomes", "KKB.KZ")),
  "Africa Proxy (via AFK)" = list(etf="AFK", stocks=c("MTN", "Safaricom", "Attijariwafa"))
)

# --- D. GICS (STANDARD) ---
gics_list <- list(
  "XLK" = c("MSFT", "AAPL", "NVDA", "AVGO", "ORCL", "CRM", "AMD", "ADBE", "QCOM", "CSCO", "INTU", "IBM", "AMAT", "NOW", "TXN", "LRCX", "MU", "ADI", "PANW", "SNPS", "KLAC", "CDNS", "ROP", "NXPI", "FTNT", "APH", "TEL", "TDY", "ANSS", "KEYS"),
  "XLC" = c("GOOGL", "META", "NFLX", "DIS", "CMCSA", "TMUS", "VZ", "T", "EA", "WBD", "CHTR", "TTWO", "OMC", "IPG", "LYV", "SIRI", "FOXA", "PARA"),
  "XLY" = c("AMZN", "TSLA", "HD", "MCD", "NKE", "LOW", "SBUX", "BKNG", "TJX", "TGT", "LULU", "MAR", "HLT", "CMG", "ROST", "F", "GM", "YUM", "RCL", "CCL"),
  "XLF" = c("BRK-B", "JPM", "V", "MA", "BAC", "WFC", "MS", "GS", "SPGI", "AXP", "BLK", "C", "CB", "PGR", "MMC", "SCHW", "AON", "USB", "TFC"),
  "XLV" = c("LLY", "UNH", "JNJ", "MRK", "ABBV", "TMO", "ABT", "AMGN", "ISRG", "PFE", "DHR", "VRTX", "BMY", "SYK", "ELEV", "REGN", "ZTS", "BSX"),
  "XLE" = c("XOM", "CVX", "COP", "SLB", "EOG", "MPC", "PSX", "VLO", "OXY", "HES", "DVN", "HAL", "BKR", "KMI", "WMB", "OKE", "FANG", "CTRA", "MRO"),
  "XLI" = c("CAT", "GE", "UNP", "HON", "UPS", "RTX", "BA", "LMT", "DE", "ADP", "ETN", "WM", "ITW", "GD", "FDX", "CSX", "NSC", "EMR"),
  "XLU" = c("NEE", "SO", "DUK", "SRE", "AEP", "D", "PEG", "XEL", "ED", "PCG", "EIX", "WEC", "ES", "DTE", "ETR", "FE", "PPL", "AEE"),
  "XLRE" = c("PLD", "AMT", "EQIX", "CCI", "PSA", "O", "SPG", "DLR", "VICI", "WELL", "CBRE", "AVB", "EQR", "CSGP", "EXR", "INVH", "ARE")
)

gics_map <- c("XLK"="Technology", "XLC"="Communication", "XLY"="Discretionary", 
              "XLF"="Financials", "XLV"="Healthcare", "XLE"="Energy", 
              "XLI"="Industrials", "XLU"="Utilities", "XLRE"="Real Estate", "SPY"="S&P 500")

# --- UI CHOICES ---
create_ui_choices <- function(db) {
  groups <- unique(sapply(db, function(x) x$group))
  choices <- list()
  for(g in groups) {
    items <- Filter(function(x) x$group == g, db)
    choices[[g]] <- sapply(items, function(x) x$label)
  }
  return(choices)
}
theme_choices_grouped <- create_ui_choices(THEME_DATABASE)

country_choices_grouped <- list(
  "Americas" = c("United States (SPY)", "Canada (EWC)", "Mexico (EWW)", "Brazil (EWZ)", "Argentina (ARGT)", "Chile (ECH)", "Colombia (GXG)", "Peru (EPU)"),
  "Europe (Major)" = c("United Kingdom (EWU)", "Germany (EWG)", "France (EWQ)", "Switzerland (EWL)", "Netherlands (EWN)", "Italy (EWI)", "Spain (EWP)", "Portugal (via EZU)"),
  "Europe (Nordics)" = c("Denmark (EDEN)", "Sweden (EWD)", "Norway (NORW)", "Finland (EFNL)"),
  "Europe (Niche & Frontier)" = c("Ireland (EIRL)", "Belgium (EWK)", "Austria (EWO)", "Poland (EPOL)", "Greece (GREK)", "Turkey (TUR)", "Baltics Proxy (via CEE)"),
  "Asia (Giants)" = c("China (FXI)", "India (EPI)", "Japan (EWJ)", "South Korea (EWY)", "Taiwan (EWT)"),
  "Asia (Developed & ASEAN)" = c("Australia (EWA)", "Hong Kong (EWH)", "Singapore (EWS)", "New Zealand (ENZL)", "Indonesia (EIDO)", "Thailand (THD)", "Malaysia (EWM)", "Philippines (EPHE)", "Vietnam (VNM)"),
  "Middle East & Africa" = c("Saudi Arabia (KSA)", "United Arab Emirates (UAE)", "Qatar (QAT)", "Israel (EIS)", "South Africa (EZA)", "Frontier Proxy (FM)", "Africa Proxy (via AFK)")
)

get_ticker_from_label <- function(lbl) {
  entry <- Filter(function(x) x$label == lbl, THEME_DATABASE)
  if(length(entry) > 0) return(entry[[1]]$ticker)
  return(NULL)
}

get_holdings_from_label <- function(lbl) {
  entry <- Filter(function(x) x$label == lbl, THEME_DATABASE)
  if(length(entry) > 0) return(entry[[1]]$holdings)
  return(NULL)
}

# ==============================================================================
# 4. UI
# ==============================================================================

ui <- page_sidebar(
  title = "Sector Momentum Tracker",
  theme = bs_theme(version = 5, bootswatch = "zephyr", primary = "#2c3e50"),
  fillable = FALSE, 
  
  sidebar = sidebar(
    title = "Market Controls",
    dateRangeInput("dates", "Chart Range:", start = Sys.Date() - 365, end = Sys.Date()),
    
    # --- SECTOR CONTROLS ---
    conditionalPanel(
      condition = "input.main_tabs.includes('GICS')",
      checkboxGroupInput("gics_visible", "Select Sectors:", 
                         choices = setNames(names(gics_map)[1:9], gics_map[1:9]),
                         selected = c("XLK", "XLF", "XLE")),
      hr(),
      selectInput("gics_active", "Deep Dive Sector:", 
                  choices = setNames(names(gics_map)[1:9], gics_map[1:9]),
                  selected = "XLK")
    ),
    
    # --- THEME CONTROLS ---
    conditionalPanel(
      condition = "input.main_tabs.includes('Themes')",
      selectInput("theme_visible", "Compare Themes:",
                  choices = theme_choices_grouped, 
                  selected = c("Semiconductors (SMH)", "AI & Robotics (BOTZ)", "Copper Miners (COPX)"),
                  multiple = TRUE),
      selectInput("theme_active", "Deep Dive Theme:", 
                  choices = theme_choices_grouped, selected = "Semiconductors (SMH)")
    ),
    
    # --- COUNTRY CONTROLS ---
    conditionalPanel(
      condition = "input.main_tabs.includes('Countries')",
      selectInput("country_visible", "Compare Regions:",
                  choices = country_choices_grouped,
                  selected = c("United States (SPY)", "China (FXI)", "India (EPI)"),
                  multiple = TRUE),
      selectInput("country_active", "Deep Dive Region:", 
                  choices = country_choices_grouped, selected = "India (EPI)")
    ),
    
    hr(),
    actionButton("refresh", "Refresh Data", class = "btn-primary w-100"),
    div(style="margin-top: 15px;", downloadButton("downloadData", "Export CSV", class = "btn-sm w-100"))
  ),
  
  navset_tab(
    id = "main_tabs",
    nav_panel("Macro (GICS)", card(plotlyOutput("plotGICS", height = "500px") %>% withSpinner()), card(reactableOutput("tableGICS"))),
    nav_panel("Deep Dive (GICS)", card(uiOutput("boxesGICS")), reactableOutput("stockTableGICS") %>% withSpinner()),
    
    nav_panel("Macro (Themes)", card(plotlyOutput("plotThemes", height = "500px") %>% withSpinner()), card(reactableOutput("tableThemes"))),
    nav_panel("Deep Dive (Themes)", card(uiOutput("boxesThemes")), reactableOutput("stockTableThemes") %>% withSpinner()),
    
    nav_panel("Macro (Countries)", card(plotlyOutput("plotCountries", height = "500px") %>% withSpinner()), card(reactableOutput("tableCountries"))),
    nav_panel("Deep Dive (Countries)", card(uiOutput("boxesCountries")), reactableOutput("stockTableCountries") %>% withSpinner())
  )
)

# ==============================================================================
# 5. SERVER
# ==============================================================================

server <- function(input, output, session) {
  
  # --- SAFE DATA FETCHING ---
  safe_get_data <- function(tickers, from_date) {
    if(is.null(tickers) || length(tickers) == 0) return(NULL)
    tryCatch({
      # get="stock.prices" gets OHLCV data directly
      df <- tq_get(tickers, from = from_date, get = "stock.prices")
      if (is.null(df) || nrow(df) == 0) return(NULL)
      
      # Fill Missing Dates (LOCF) for International Stocks
      # This fixes the "NA returns" issue for holidays
      df <- df %>%
        group_by(symbol) %>%
        complete(date = seq.Date(min(date), max(date), by="day")) %>%
        fill(c(open, high, low, close, volume, adjusted), .direction = "down") %>%
        na.omit() %>%
        ungroup()
      
      return(df)
    }, error = function(e) { return(NULL) })
  }
  
  # --- STOCK METRICS ENGINE (FIXED) ---
  fetch_stock_data <- function(tickers) {
    req(tickers)
    # Fetch 400 days to ensure enough history for RSI and 1Y return
    df_raw <- safe_get_data(tickers, Sys.Date() - 400) 
    if (is.null(df_raw)) return(NULL)
    
    # Helper to get return from X days ago (closest available date)
    calc_ret <- function(curr, vec_price, vec_date, days_ago) {
      target_date <- Sys.Date() - days_ago
      # Find index of date closest to target but <= target
      idx <- which(vec_date <= target_date)
      if(length(idx) == 0) return(NA)
      old_price <- vec_price[max(idx)]
      if(is.na(old_price) || old_price == 0) return(NA)
      return((curr / old_price) - 1)
    }
    
    # 1. Calculate Metrics entirely from historical data (bypasses getQuote issues for Intl stocks)
    metrics <- df_raw %>%
      group_by(symbol) %>%
      arrange(date) %>%
      summarise(
        Price = last(adjusted),
        # Pass vector columns to helper
        `1M` = calc_ret(last(adjusted), adjusted, date, 30),
        `3M` = calc_ret(last(adjusted), adjusted, date, 90),
        `6M` = calc_ret(last(adjusted), adjusted, date, 180),
        `1Y` = calc_ret(last(adjusted), adjusted, date, 365),
        Vol = tryCatch(sd(diff(log(adjusted)), na.rm=TRUE)*sqrt(252), error=function(e) NA),
        RSI = tryCatch(last(RSI(adjusted, n=14)), error=function(e) NA),
        .groups = "drop"
      )
    
    # 2. Try to get Names/MarketCap separately, but don't fail if it crashes
    info_df <- tryCatch({
      # Attempt to get quotes for names only
      q <- getQuote(tickers, what = yahooQF(c("Market Capitalization", "Name")))
      data.frame(symbol = rownames(q), mcap_raw = q$`Market Capitalization`, name = q$Name)
    }, error = function(e) {
      # Fallback if getQuote fails entirely
      data.frame(symbol = tickers, mcap_raw = 0, name = tickers)
    })
    
    # Merge and Clean
    final <- metrics %>%
      left_join(info_df, by="symbol") %>%
      rowwise() %>%
      mutate(Mcap = clean_mcap(mcap_raw), Cur = get_currency_symbol(symbol)) %>%
      ungroup() %>%
      arrange(desc(Mcap)) %>%
      select(name, symbol, Mcap, Price, `1M`, `3M`, `6M`, `1Y`, Vol, RSI, Cur)
    
    # Handle NA names (fill with ticker if missing)
    final$name <- ifelse(is.na(final$name) | final$name == "", final$symbol, final$name)
    
    colnames(final)[1] <- "Name"
    colnames(final)[2] <- "Ticker"
    
    return(final)
  }
  
  # --- PLOT HELPERS ---
  create_plot <- function(data) {
    validate(need(!is.null(data) && nrow(data) > 0, "No data available."))
    p <- ggplot(data, aes(x=date, y=cum_ret, color=label, group=label,
                          text = paste0("<b>", label, "</b>\nReturn: ", percent(cum_ret, 0.1)))) + 
      geom_line(linewidth = 1) + 
      scale_y_continuous(labels=percent) + 
      labs(x = "", y = "Return") + theme_minimal()
    ggplotly(p, tooltip = "text")
  }
  
  # --- TABLE RENDERERS ---
  render_macro_table <- function(df, group_name) {
    req(df)
    summary_df <- df %>% 
      group_by(Name = label) %>% 
      summarise(Return = last(cum_ret)) %>% 
      arrange(desc(Return))
    
    colnames(summary_df)[1] <- group_name
    
    reactable(summary_df, columns = list(
      Return = colDef(format = colFormat(percent = TRUE, digits = 1), 
                      style = function(value) {
                        color <- if (value > 0) "#008000" else "#e00000"
                        list(color = color, fontWeight = "bold")
                      })
    ), striped = TRUE, compact = TRUE)
  }
  
  render_stock_table <- function(df) {
    req(df)
    display_df <- df %>% select(-Cur)
    
    reactable(display_df, columns = list(
      Name = colDef(minWidth = 140, style = list(fontWeight = "bold")),
      Ticker = colDef(maxWidth = 90),
      Mcap = colDef(cell = function(v) {
        if(is.na(v)) return("-")
        if(v >= 1e12) paste0(round(v/1e12,1), "T") else if(v >= 1e9) paste0(round(v/1e9,1), "B") else "-"
      }),
      `1M` = colDef(format=colFormat(percent=TRUE, digits=1), style=function(v) list(color=if(is.na(v)) "black" else if(v>0)"green" else "red")),
      `3M` = colDef(format=colFormat(percent=TRUE, digits=1), style=function(v) list(color=if(is.na(v)) "black" else if(v>0)"green" else "red")),
      `6M` = colDef(format=colFormat(percent=TRUE, digits=1), style=function(v) list(color=if(is.na(v)) "black" else if(v>0)"green" else "red")),
      `1Y` = colDef(format=colFormat(percent=TRUE, digits=1), style=function(v) list(color=if(is.na(v)) "black" else if(v>0)"green" else "red")),
      Price = colDef(cell=function(v,i) paste0(df$Cur[i], round(v,2))),
      Vol = colDef(format = colFormat(percent = TRUE, digits = 1)),
      RSI = colDef(format = colFormat(digits = 2))
    ), compact=TRUE, striped=TRUE, pagination = TRUE, defaultPageSize = 25)
  }
  
  make_value_boxes <- function(df) {
    req(df); if (nrow(df) == 0) return(NULL)
    df_clean <- df %>% filter(!is.na(`1M`))
    if(nrow(df_clean) == 0) return(NULL)
    
    winner <- df_clean %>% arrange(desc(`1M`)) %>% head(1)
    laggard <- df_clean %>% arrange(`1M`) %>% head(1)
    avg_ret <- mean(df_clean$`1M`, na.rm=TRUE)
    
    layout_columns(
      value_box(title = "Top Performer (1M)", value = paste0(winner$Name, ": ", percent(winner$`1M`, 0.1)), theme = "success"),
      value_box(title = "Lagging (1M)", value = paste0(laggard$Name, ": ", percent(laggard$`1M`, 0.1)), theme = "danger"),
      value_box(title = "Average (1M)", value = percent(avg_ret, 0.1), theme = "primary")
    )
  }
  
  # --- SERVER LOGIC ---
  
  # 1. THEMES
  data_theme_macro <- eventReactive(input$refresh, {
    labels <- c(input$theme_visible, "S&P 500 (SPY)")
    ticker_map <- setNames(sapply(labels, function(x) {
      if(x == "S&P 500 (SPY)") return("SPY")
      get_ticker_from_label(x)
    }), labels)
    ticker_map <- ticker_map[!sapply(ticker_map, is.null)]
    valid_tickers <- unname(ticker_map)
    
    df <- safe_get_data(valid_tickers, input$dates[1])
    if(is.null(df)) return(NULL)
    
    df %>% 
      group_by(symbol) %>% 
      mutate(cum_ret = (adjusted/first(adjusted))-1, 
             label = names(ticker_map)[match(symbol, ticker_map)])
  }, ignoreNULL=FALSE)
  
  output$plotThemes <- renderPlotly({ req(data_theme_macro()); create_plot(data_theme_macro()) })
  output$tableThemes <- renderReactable({ render_macro_table(data_theme_macro(), "Theme") })
  
  data_theme_deep <- reactive({ 
    req(input$theme_active)
    holdings <- get_holdings_from_label(input$theme_active)
    fetch_stock_data(holdings) 
  })
  output$boxesThemes <- renderUI({ make_value_boxes(data_theme_deep()) })
  output$stockTableThemes <- renderReactable({ render_stock_table(data_theme_deep()) })
  
  # 2. GICS
  data_gics <- eventReactive(input$refresh, {
    req(input$gics_visible)
    selected_sectors <- input$gics_visible
    # Ensure tickers exist
    df <- safe_get_data(c(selected_sectors, "SPY"), input$dates[1])
    if(is.null(df)) return(NULL)
    
    df %>%
      group_by(symbol) %>% 
      mutate(cum_ret=(adjusted/first(adjusted))-1, label=gics_map[symbol])
  }, ignoreNULL=FALSE)
  
  output$plotGICS <- renderPlotly({ create_plot(data_gics()) })
  output$tableGICS <- renderReactable({ render_macro_table(data_gics(), "Sector") })
  
  data_gics_deep <- reactive({ 
    req(input$gics_active) 
    fetch_stock_data(gics_list[[input$gics_active]]) 
  })
  output$boxesGICS <- renderUI({ make_value_boxes(data_gics_deep()) })
  output$stockTableGICS <- renderReactable({ render_stock_table(data_gics_deep()) })
  
  # 3. COUNTRIES
  data_country <- eventReactive(input$refresh, {
    req(input$country_visible)
    
    # Map selection name to ticker safely
    tix <- sapply(input$country_visible, function(x) {
      if(is.null(country_data[[x]])) return(NA)
      country_data[[x]]$etf
    })
    tix <- tix[!is.na(tix)]
    tix <- c(tix, "USA (S&P 500)"="SPY")
    
    df <- safe_get_data(unname(tix), input$dates[1])
    if(is.null(df)) return(NULL)
    
    df %>% 
      group_by(symbol) %>% 
      mutate(cum_ret=(adjusted/first(adjusted))-1, label=names(tix)[match(symbol, tix)])
  }, ignoreNULL=FALSE)
  
  output$plotCountries <- renderPlotly({ create_plot(data_country()) })
  output$tableCountries <- renderReactable({ render_macro_table(data_country(), "Region") })
  
  data_country_deep <- reactive({ 
    req(input$country_active)
    req(country_data[[input$country_active]])
    fetch_stock_data(country_data[[input$country_active]]$stocks)
  })
  output$boxesCountries <- renderUI({ make_value_boxes(data_country_deep()) })
  output$stockTableCountries <- renderReactable({ render_stock_table(data_country_deep()) })
  
  # --- DOWNLOAD ---
  output$downloadData <- downloadHandler(
    filename = function() { paste("market-data-", Sys.Date(), ".csv", sep="") },
    content = function(file) { 
      # Default to saving whatever deep dive is active
      write.csv(data_country_deep(), file) 
    }
  )
}

shinyApp(ui, server)
