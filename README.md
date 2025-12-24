# Sector-Momentum-Tracker
Real-time dashboard tracking Sector Rotation across GICS &amp; unique Thematic baskets (Uranium, Semis, Space etc). Uses interactive Plotly charts to visualize trends vs SPY. Automates live ingestion to compute metrics: Relative Strength, Volatility, RSI &amp; Market Cap weighting. A full-stack R Shiny tool for identifying hidden capital flow.


Sector Momentum Tracker

Launch Live Dashboard: https://archiehehe.shinyapps.io/SectorMomentumTracker/ 

Overview
The Sector Momentum Tracker is a financial intelligence dashboard designed to visualize Sector Rotation in real-time.

Moving beyond standard GICS classifications, this application leverages a custom Thematic Engine to track unique, high-beta market baskets—such as Uranium, Cybersecurity, and Regional Banks—that traditional screeners often miss. It automates the ingestion of financial data to compute technical metrics and visualize capital flow through interactive charts.

Key Features

1. Sector Rotation & Macro Analysis

* GICS vs. Benchmark: Instantly compare standard sectors (Technology XLK, Financials XLF, etc.) against the S&P 500 (SPY) to identify leaders and laggards.
* Time-Series Visualization: Interactive line charts allow users to analyze performance over dynamic timeframes (1M, 3M, 6M, 1Y).

2. Unique Thematic Engine

* Beyond the Basics: Tracks non-standard industry groups that don't fit into neat GICS boxes.
* Custom Baskets: Monitor specific trends including Semiconductors (SMH), Clean Energy (ICLN), Homebuilders (XHB), and Defense (ITA).

3. Automated Metrics & Analytics

* Real-Time Calculations: The app computes relevant metrics on the fly using live data:
-- Relative Strength (RS): 1, 3, and 6-month performance rankings.
-- Volatility: Annualized standard deviation to measure risk.
-- RSI (14-Day): Relative Strength Index to spot overbought/oversold conditions.
-- Market Cap Weighting: Dynamically weights importance based on live market capitalization (Billions/Trillions).

4. Deep Dive Capabilities

* Drill down from a broad sector view into individual constituent stocks.
* Sortable data tables with heat-mapped performance columns.

Tech Stack

* Core Framework: R, Shiny
* Data Ingestion: tidyquant, quantmod (Yahoo Finance API)
* Visualization: ggplot2, plotly (Interactive web-based charts)
* UI/UX: bslib (Modern Bootstrap 5 theme), reactable (Interactive data tables)

How to Run Locally

1. Clone the repository:
git clone [https://github.com/archiehehe/Sector-Momentum-Tracker.git]
3. Install dependencies:
install.packages(c("shiny", "tidyquant", "dplyr", "ggplot2", "tidyr", "reactable", "bslib", "plotly", "quantmod"))
4. Run the App:
Click the Run App button in the top right of the RStudio script editor.

License
MIT
