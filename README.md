# South African Economic & Inflation Analysis System

A reproducible R project that analyses South African inflation, GDP growth, unemployment and the SARB repo rate for 2000-2024. It covers descriptive statistics, correlation, hypothesis tests, regression with assumption checks, time-series analysis and inflation forecasting. Results are presented in a Shiny dashboard and an R Markdown report.

## Quick start

```r
# 1. Restore packages (first time only)
renv::restore()

# 2. Run the whole pipeline (collection -> cleaning -> analysis -> evaluation)
source("run_analysis.R")          # or, in a terminal: Rscript run_analysis.R

# 3. Render the report
source("render_report.R")         # or open reports/economic_analysis.Rmd and click Knit

# 4. Launch the dashboard
shiny::runApp("dashboard")

# Optional: run the tests
testthat::test_dir("tests/testthat")
```

`Rscript run_analysis.R --refresh` re-copies the World Bank files and downloads the latest SARB repo rate.

## Data

| Variable | Source | Code | Frequency | Units |
|---|---|---|---|---|
| Inflation | World Bank WDI | FP.CPI.TOTL.ZG | Annual | % change in CPI |
| GDP_Growth | World Bank WDI | NY.GDP.MKTP.KD.ZG | Annual | % (constant prices) |
| Unemployment | World Bank WDI (ILO modelled estimate) | SL.UEM.TOTL.ZS | Annual | % of labour force |
| Population_Growth | World Bank WDI | SP.POP.GROW | Annual | % |
| GDP_Per_Capita | World Bank WDI | NY.GDP.PCAP.CD | Annual | Current US$ |
| Interest_Rate | SARB Web API, SARB Policy (repo) Rate | MMRD002A | Daily, averaged to annual | % p.a. |

* The full source register, with definitions, original sources and update dates, is written to `data/raw/data_sources.csv`.
* Raw files are stored unchanged in `data/raw/`. The World Bank bulk CSVs are copied from the folder above the project.
* The annual repo rate is a **time-weighted calendar-year average** of the daily rate. The year-end rate is also kept.
* **Known gap:** the SARB API series starts on 31 Dec 2001, so the repo rate for 2000-2001 is `NA` and analyses that use it cover 2002-2024 (n = 23). Missing values are never replaced with zero.

## Project structure

```text
sa-economic-analysis/
├── data/
│   ├── raw/                  # unchanged source files + data_sources.csv
│   └── processed/            # sa_economic_annual.csv, data_quality_report.csv, monthly repo rate
├── R/
│   ├── utils.R               # paths, settings (CONFIG), labels, helpers
│   ├── data_collection.R     # Phase 2: collect raw data, write source register
│   ├── data_cleaning.R       # Phase 3: import, quality checks, integrate
│   ├── exploratory_analysis.R
│   ├── descriptive_statistics.R
│   ├── correlation_analysis.R
│   ├── hypothesis_testing.R
│   ├── regression_analysis.R
│   ├── time_series_analysis.R
│   └── model_evaluation.R    # hold-out regression scoring, ARIMA vs baselines, final forecast
├── dashboard/app.R           # Shiny dashboard (5 sections)
├── reports/economic_analysis.Rmd
├── outputs/                  # result tables (CSV), analysis_results.rds, session_info.txt
├── visualisations/           # all figures (PNG)
├── tests/testthat/           # data and helper tests
├── run_analysis.R            # runs the full pipeline
├── render_report.R
└── renv.lock
```

## Methods summary

* **Hypothesis tests** (α = 0.05): Welch t-test with a Wilcoxon check and Hedges' g. The tests compare inflation in high vs low unemployment years, GDP growth and inflation before vs after the 2009 GFC, the inflation-repo correlation, and mean inflation vs the 4.5% target midpoint.
* **Regression:** inflation on the repo rate (simple), plus GDP growth and unemployment (multiple), and the same model in first differences. Checks cover Shapiro-Wilk, Breusch-Pagan, Durbin-Watson, Breusch-Godfrey, RESET and VIF, with Newey-West standard errors reported.
* **Time series:** moving averages, linear trends, LOESS trend/remainder decomposition (annual data has no seasonality), ACF/PACF, and ADF + KPSS tests in levels and differences.
* **Evaluation:** chronological split, training 2000-2018 and test 2019-2024. ARIMA is compared with naive, mean and SES forecasts using MAE, RMSE and MAPE, for both multi-step and rolling one-step forecasts.

## Main findings

1. Inflation is stationary around about 5% with no trend. It was within the 3-6% target in 15 of 25 years and became much less volatile after 2009.
2. Average real GDP growth fell from 4.2% (2000-2008) to 1.1% (2009-2024), p < 0.001, and unemployment rose by about 10 pp.
3. Inflation and the repo rate are positively associated, most strongly in year-on-year changes (r ≈ 0.65). This is consistent with the SARB reacting to inflation and is **not** evidence that rates cause inflation.
4. There is no reliable link between inflation and unemployment or growth in these annual data.
5. The levels regressions have autocorrelated residuals, and none of the regressions generalised well to 2019-2024.
6. ARIMA beats naive and mean forecasts only for one-step-ahead forecasts. Multi-year forecasts are indicative only.

## Limitations

Small annual sample; the repo-rate gap for 2000-2001; ILO-modelled unemployment differs from Stats SA's official QLFS rate; structural breaks (GFC, COVID-19); autocorrelation; reverse causality between inflation and policy rates; omitted variables (exchange rate, oil and food prices, electricity supply). Results describe association, not causation.

## Future work

Quarterly or monthly data, lagged monetary-policy models, VAR/VECM and cointegration analysis, CPI category analysis, exchange-rate analysis, international comparisons and automated data updates.
