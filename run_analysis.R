# =============================================================================
# run_analysis.R
# Runs the full pipeline in order. From the project root:
#   Rscript run_analysis.R            # use existing raw files
#   Rscript run_analysis.R --refresh  # re-copy / re-download raw data
#
# Outputs
#   data/raw/            raw files and data_sources.csv (source register)
#   data/processed/      cleaned annual dataset, monthly repo rate, quality report
#   outputs/             result tables (CSV) and analysis_results.rds
#   visualisations/      all figures (PNG)
# =============================================================================

source("R/utils.R")
for (f in c("data_collection", "data_cleaning", "exploratory_analysis",
            "descriptive_statistics", "correlation_analysis", "hypothesis_testing",
            "regression_analysis", "time_series_analysis", "model_evaluation")) {
  source(file.path("R", paste0(f, ".R")))
}

refresh <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

message("1/9 Data collection");      register <- run_data_collection(refresh = refresh)
message("2/9 Data cleaning");        cleaned  <- run_data_cleaning()
df <- load_economic_data()
message("3/9 Exploratory analysis"); eda      <- run_exploratory_analysis(df)
message("4/9 Descriptive stats");    desc     <- run_descriptive_statistics(df)
message("5/9 Correlation");          corr     <- run_correlation_analysis(df)
message("6/9 Hypothesis tests");     hyp      <- run_hypothesis_tests(df)
message("7/9 Regression");           reg      <- run_regression_analysis(df)
message("8/9 Time series");          tsa      <- run_time_series_analysis(df)
message("9/9 Model evaluation");     evals    <- run_model_evaluation(df)

saveRDS(
  list(
    created = Sys.time(), register = register, quality = cleaned$quality,
    descriptive = desc[c("overall", "by_period", "target")],
    correlation = corr, hypothesis = hyp[c("results", "confounding", "unemployment_median")],
    regression = reg[c("models", "coefficients", "diagnostics", "vif")],
    time_series = tsa[c("trends", "stationarity", "ljung_box", "decomposition")],
    evaluation = list(regression = evals$regression,
                      accuracy = evals$forecasting$accuracy,
                      test_predictions = evals$forecasting$test_predictions,
                      arima_label = evals$forecasting$arima_label,
                      residual_tests = evals$forecasting$residual_ljung_box,
                      final_forecast = evals$final$table,
                      final_model = evals$final$model)
  ),
  file.path(PATHS$outputs, "analysis_results.rds")
)

writeLines(capture.output(sessionInfo()), file.path(PATHS$outputs, "session_info.txt"))
message("Done. Results in outputs/ and visualisations/.")
