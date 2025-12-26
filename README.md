# Sector Momentum Tracker

Launch Live Dashboard: https://archiehehe.shinyapps.io/SectorMomentumTracker/ 

Overview

The Sector Momentum Tracker is a financial intelligence dashboard designed to visualize Sector Rotation in real-time.

Moving beyond standard GICS classifications, this application leverages a custom Thematic Engine and various countries to track unique, high-beta market baskets—such as Uranium, Cybersecurity, and Regional Banks—that traditional screeners often miss. It automates the ingestion of financial data to compute technical metrics and visualize capital flow through interactive charts.


# Sector Momentum Tracker

Sector Momentum Tracker is a production-grade R Shiny application designed to analyze and visualize equity market momentum across GICS sectors, investment themes, and global regions. The application combines multi-horizon return analysis, technical indicators, and market-cap-aware ranking to provide an interactive tool for both macro-level analysis and detailed stock-level exploration.

**Live Application:** https://archiehehe.shinyapps.io/SectorMomentumTracker/

## Motivation

Momentum is one of the most robust and widely studied factors in asset pricing and portfolio management. Understanding how momentum evolves across sectors, themes, and regions is critical for investors, researchers, and analysts.

This tool aims to bridge academic momentum concepts with a practical, interactive analytics system that enables users to:

* Identify leading and lagging market segments
* Explore cross-sectional and relative performance
* Perform structured deep dives into individual securities


## Key Features

### 1. Macro-Level Momentum Analysis

* Cumulative return comparison across:

  * GICS sectors
  * Investment themes
  * Global equity regions
* Interactive time-series visualization with benchmark comparison (S&P 500)

### 2. Deep-Dive Stock Analysis

* Stock-level breakdown within selected sectors, themes, or regions
* Automatic ranking by market capitalization
* Summary indicators highlighting top performers, laggards, and group averages

### 3. Momentum & Risk Metrics

For each stock, the following metrics are computed:

* 1-month, 3-month, 6-month, and 1-year returns
* Annualized volatility
* Relative Strength Index (RSI)
* Latest adjusted price with currency normalization

### 4. Interactive Data Tables

* Sortable and filterable tables using `reactable`
* Conditional formatting for performance metrics
* Export functionality for further offline analysis

### 5. Robust Data Handling

* Safe and fault-tolerant data retrieval from Yahoo Finance
* Graceful handling of missing data and API failures
* Market capitalization normalization across different formats and currencies

## Methodology

### Momentum Definition

Momentum is defined as cumulative returns over multiple lookback horizons (1M, 3M, 6M, 1Y), calculated using adjusted closing prices. These horizons capture both short-term and medium-term market trends.

### Key Indicators

* **RSI** is used to identify overbought and oversold conditions.
* **Volatility** is calculated as the annualized standard deviation of daily log returns.

### Data Sources

* Historical price data is retrieved via Yahoo Finance using `tidyquant` and `quantmod`.
* Market capitalization data is fetched as a snapshot and normalized to numeric values.

## Application Architecture

The application is built using a modular Shiny design:

* **UI Layer:** User controls, navigation, and interactive layouts using `bslib`
* **Server Layer:** Reactive data pipelines, metric computation, and rendering logic
* **Helper Functions:** Data cleaning, error handling, and metric calculation utilities

Reactive expressions are used to optimize performance and prevent unnecessary recomputation.

## Technology Stack

* R
* Shiny / bslib
* tidyquant
* quantmod
* dplyr / tidyr
* ggplot2 / plotly
* reactable

How to Run Locally

1. Clone the repository:
git clone [https://github.com/archiehehe/Sector-Momentum-Tracker.git]
3. Install dependencies:
install.packages(c("shiny", "tidyquant", "dplyr", "ggplot2", "tidyr", "reactable", "bslib", "plotly", "quantmod"))
4. Run the App:
Click the Run App button in the top right of the RStudio script editor.

## Deployment

The application is deployed on `shinyapps.io` - https://archiehehe.shinyapps.io/SectorMomentumTracker/

To run locally:

```r
install.packages(c(
  "shiny", "tidyquant", "quantmod", "dplyr", "tidyr",
  "ggplot2", "plotly", "reactable", "bslib", "TTR", "scales"
))

shiny::runApp()
```

## Use Cases
* Exploratory market analysis
* Portfolio monitoring and sector rotation screening across sectors, themes, and countries

## Author
**Archie**

Not for commercial use. Read the license for further details.
.
