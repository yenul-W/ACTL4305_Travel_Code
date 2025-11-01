---
editor_options: 
  markdown: 
    wrap: 72
---

# README

This repository contains a reproducible R Markdown workflow for
cleaning, enriching, analysing and modeling Freely quote data. It
integrates external datasets (e.g., Google Trends, ABS, forex rates),
prepares a modeling-ready dataset and builds several GLM and XGBoost
models.

# Overview

The project is designed to: - Clean and normalize Freely quote data,
including robust date parsing and destination standardisation - Merge
external behavioral, economic, and geographic datasets - Engineer
features for modeling conversion propensity - Exploratory data analysis
(EDA) - Modeling - Sensitivity analysis

# Features

-   Robust date parsing and correction (Excel serials, ambiguous
    formats, dd/mm swaps)
-   Destination standardization and regional tagging using countrycode
    and city lookup
-   Integration of external data: Google Trends, ABS age shares, forex
    rates, peace index, PPP
-   Feature engineering: demographics, time-of-day, trip length, price
    per traveller
-   Boost window clamping and audit diagnostics
-   Modular structure for reproducibility and extensibility

# File Structure

-   UG20_reproducible_code.Rmd\
-   Freely_quote_data.xlsx\
-   README.md

# Installation

-   Open UG20_reproducible_code.Rmd in RStudio.
-   Required R packages will be auto-installed via the load() function.
    Alternatively, install manually: install.packages(c("readxl",
    "dplyr", "tidyr", "lubridate", "ggplot2", "xgboost", "caret",
    "countrycode", "janitor", "stringr", "sf", "maps", "plotly",
    "MLmetrics", "pROC", "fastDummies", "statmod", "glmnet",
    "doParallel", "PRROC", "tidytext", "pdp"))

## Usage

-   Place Freely_quote_data.xlsx in your working directory.
-   Knit the R Markdown file to HTML:
    rmarkdown::render("UG20_reproducible_code.Rmd")
-   Output includes:
-   Cleaned and enriched dataset
-   Diagnostic summaries (date violations, boost window clamping)
-   Feature-engineered variables for modeling
-   Sensitivity analysis
-   There is a complementary
    [dashboard](https://ug20.shinyapps.io/Travel_Insurance_Conversions_Dashboard/)
    to this project

## External Data Sources

-   [Google Trends: Travel
    Insurance](https://docs.google.com/spreadsheets/d/e/2PACX-1vTKgVCtjuU4V2zkrfiLxOcRnQEHDyn7Fe8RO64_oaGlRXAMf1-1GEsbHP2XcjyDBEgzqYkuBRncBXYP/pub?gid=777635616&single=true&output=csv)
-   [ABS Age
    Shares](https://docs.google.com/spreadsheets/d/e/2PACX-1vREVnhczQGCB-IeKIHyBqQQsS6lmIDfc2_FHuC0PCV1DEfbmWdYt4wUaRv6apVV0Wfr8bXv_qHu66F5/pub?gid=114299855&single=true&output=csv)
-   [Global Peace
    Index](https://docs.google.com/spreadsheets/d/e/2PACX-1vSCjXipNqILguZALp5R0b7aWYmcAGAuLbxlDCApSZdjYDxw1bPWEv357ipk2RL8sg/pub?gid=1375262227&single=true&output=csv)
-   [Purchasing Power
    Index](https://docs.google.com/spreadsheets/d/e/2PACX-1vTS75iif3KmnxQ78EkfdtqfmktInOEBQx2-eawDDPdzFhCoxXJWHOCfwh-42LzbcA/pub?gid=374751837&single=true&output=csv)

Note: XGBoost models involve hyperparameter tuning. To avoid tuning, use
the pre-selected parameters or alternatively load the model locally from
your working directory.
