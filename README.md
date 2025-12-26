# Sector Momentum Tracker

**Live Dashboard:** https://archiehehe.shinyapps.io/SectorMomentumTracker/  
**Repository:** https://github.com/Archiehehe/Sector-Momentum-Tracker

---

## Overview

The **Sector Momentum Tracker** is a production-grade R Shiny application designed to analyze and visualize **equity market momentum across global regions, GICS sectors, and investment themes**.

The application integrates:
- **48 country and regional equity ETFs**
- **GICS sector classifications**
- **59 investment themes**
- Stock-level constituent analysis

It provides a structured, top-down framework for understanding **global capital rotation**, relative strength, and market regimes. The app is designed as a **research and decision-support tool**, not a signal-generating or trading system.

---

## Motivation

Momentum is one of the most robust and persistent effects documented in asset pricing and portfolio management. However, most tools analyze momentum in isolation—by country, sector, or theme.

This project integrates all three dimensions into a **single analytical framework**, allowing users to answer questions such as:

- Which **countries or regions** are leading or lagging globally?
- Are sector trends **global or region-specific**?
- Which **themes** are gaining momentum within strong sectors?
- Which **stocks** are driving performance within a theme or region?

---

## Key Features

### 1. Macro-Level Momentum Analysis

Interactive cumulative return comparisons across:
- **48 global country and regional ETFs**
- **GICS sectors**
- **59 investment themes**
- Benchmark comparison against the S&P 500

Users can dynamically select assets and time ranges to evaluate relative performance.

---

### 2. Deep-Dive Stock Analysis

Stock-level breakdowns within selected:
- Countries / regions
- Sectors
- Investment themes

Includes:
- Automatic ranking by market capitalization
- Identification of top performers and laggards
- Group-level average performance indicators

---

### 3. Momentum & Risk Metrics

For each security, the app computes:

- 1-month, 3-month, 6-month, and 1-year returns
- Annualized volatility
- Relative Strength Index (RSI, 14-period)
- Latest adjusted price with currency-aware formatting

Metrics are **descriptive and comparative**, intended to support analysis rather than prediction.

---

### 4. Interactive Tables & Visualizations

- Interactive time-series charts using `ggplot2` and `plotly`
- Sortable and filterable tables via `reactable`
- Conditional formatting for returns and momentum indicators
- CSV export functionality for offline analysis

---

### 5. Robust Data Handling

- Market data sourced via Yahoo Finance using `tidyquant` and `quantmod`
- Defensive data retrieval with graceful handling of missing or failed API responses
- Normalized market capitalization data for consistent ranking
- Reactive pipelines designed to minimize unnecessary recomputation

---

## Methodology

### Momentum Definition

Momentum is defined using cumulative returns over multiple lookback horizons (1M, 3M, 6M, 1Y), calculated from adjusted closing prices. These horizons capture short- and medium-term trends and enable cross-asset comparability.

### Risk Indicators

- **Volatility** is calculated as the annualized standard deviation of daily log returns.
- **RSI** is used to highlight potential overbought or oversold conditions.

---

## Application Structure

The application follows a modular Shiny architecture:

- **UI Layer:** Navigation, layout, and controls built with `bslib`
- **Server Layer:** Reactive data pipelines, metric computation, and rendering logic
- **Helper Functions:** Data cleaning, metric calculation, and error handling

A consistent **Macro → Deep Dive** pattern is used across countries, sectors, and themes.

---

## Technology Stack

- R
- Shiny / bslib
- tidyquant
- quantmod
- dplyr / tidyr
- ggplot2 / plotly
- reactable
- TTR
- scales



---
Use Cases

Global macro and asset allocation research

Sector and thematic rotation analysis

Cross-country relative strength comparison

Portfolio context and regime assessment

Educational exploration of market structure and capital flows

Author - Archie (archie.nadkarni@gmail.com) 
Not for Commercial Use. Read the License for details.

## How to Run Locally

1. Clone the repository:
```bash
git clone https://github.com/Archiehehe/Sector-Momentum-Tracker.git

