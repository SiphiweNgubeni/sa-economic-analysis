# =============================================================================
# model_evaluation.R  (Phases 9 and 10, forecasting and out-of-sample checks)
#
# Chronological split (never random):
#   training: CONFIG$start_year to CONFIG$test_start_year - 1  (2000-2018)
#   test:     CONFIG$test_start_year to CONFIG$end_year         (2019-2024)
#
# 1. Regression models are refitted on the training years and scored on the
#    test years. They use same-year regressor values, so this measures how well
#    the fitted relationship holds out of sample. It is not a true forecast.
# 2. Inflation forecasts: ARIMA (auto.arima) vs naive (random walk), historical
#    mean and simple exponential smoothing, scored on
#    (a) a 6-year multi-step test window, and
#    (b) rolling one-step-ahead forecasts (expanding window).
# 3. A final ARIMA is refitted on all years to forecast the next 3 years.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}
if (!exists("fit_models")) source(proj_path("R", "regression_analysis.R"))
if (!exists("to_ts")) source(proj_path("R", "time_series_analysis.R"))

metrics <- function(actual, predicted) {
  e <- actual - predicted
  tibble(
    n    = sum(!is.na(e)),
    MAE  = mean(abs(e), na.rm = TRUE),
    RMSE = sqrt(mean(e^2, na.rm = TRUE)),
    # MAPE is only meaningful when actual values are well away from zero
    MAPE = if (all(abs(actual) > 1, na.rm = TRUE)) mean(abs(e / actual), na.rm = TRUE) * 100 else NA_real_
  )
}

# ---- 1. Regression hold-out evaluation --------------------------------------
evaluate_regressions <- function(df) {
  d <- prepare_regression_data(df)
  train <- d |> filter(Year < CONFIG$test_start_year)
  test  <- d |> filter(Year >= CONFIG$test_start_year)

  purrr::imap_dfr(MODEL_FORMULAS, function(f, nm) {
    m <- lm(f, data = train)
    y <- all.vars(f)[1]
    pred <- predict(m, newdata = test)
    baseline <- mean(train[[y]], na.rm = TRUE)
    s <- summary(m)
    bind_rows(
      metrics(test[[y]], pred) |> mutate(model = nm, method = MODEL_DESCRIPTIONS[[nm]]),
      metrics(test[[y]], rep(baseline, nrow(test))) |>
        mutate(model = nm, method = "Baseline: training-period mean")
    ) |>
      mutate(train_r_squared = s$r.squared, train_adj_r_squared = s$adj.r.squared,
             train_n = nobs(m), .before = n)
  }) |>
    relocate(model, method)
}

# ---- 2. Inflation forecasting -----------------------------------------------
forecast_methods <- function(train_ts, h, arima_order = NULL) {
  arima_fit <- if (is.null(arima_order)) {
    forecast::auto.arima(train_ts, stepwise = FALSE, approximation = FALSE)
  } else {
    forecast::Arima(train_ts, order = arima_order[1:3], include.mean = arima_order[4] == 1,
                    include.drift = arima_order[5] == 1)
  }
  list(
    ARIMA = forecast::forecast(arima_fit, h = h),
    Naive = forecast::naive(train_ts, h = h),
    Mean  = forecast::meanf(train_ts, h = h),
    SES   = forecast::ses(train_ts, h = h)
  )
}

# Encode an ARIMA specification as (p, d, q, include_mean, include_drift)
arima_spec <- function(fit) {
  o <- forecast::arimaorder(fit)
  cf <- names(coef(fit))
  c(o[1:3], "intercept" %in% cf || "mean" %in% cf, "drift" %in% cf)
}

evaluate_forecasts <- function(df) {
  infl <- to_ts(df, "Inflation")
  train <- window(infl, end = CONFIG$test_start_year - 1)
  test  <- window(infl, start = CONFIG$test_start_year)
  h <- length(test)

  fc <- forecast_methods(train, h)
  arima_train_fit <- fc$ARIMA$model
  spec <- arima_spec(arima_train_fit)

  multi_step <- purrr::imap_dfr(fc, ~ metrics(as.numeric(test), as.numeric(.x$mean)) |>
                                  mutate(method = .y, evaluation = "Multi-step (2019-2024)", .before = 1))

  test_predictions <- purrr::imap_dfr(fc, ~ tibble(Year = as.integer(time(test)), method = .y,
                                                   actual = as.numeric(test),
                                                   forecast = as.numeric(.x$mean)))

  # Rolling one-step-ahead: expanding window, ARIMA order fixed from training fit
  origins <- seq(CONFIG$test_start_year - 1, CONFIG$end_year - 1)
  one_step <- purrr::map_dfr(origins, function(o) {
    tr <- window(infl, end = o)
    f <- forecast_methods(tr, 1, arima_order = spec)
    tibble(Year = o + 1, actual = as.numeric(window(infl, start = o + 1, end = o + 1)),
           method = names(f), forecast = purrr::map_dbl(f, ~ as.numeric(.x$mean[1])))
  })
  one_step_metrics <- one_step |>
    group_by(method) |>
    group_modify(~ metrics(.x$actual, .x$forecast)) |>
    ungroup() |>
    mutate(evaluation = "Rolling one-step (2019-2024)", .before = 1)

  # Residual checks for the ARIMA fitted to the training data
  lb <- Box.test(residuals(arima_train_fit), lag = 5, type = "Ljung-Box",
                 fitdf = sum(forecast::arimaorder(arima_train_fit)[c(1, 3)]))

  list(
    train = train, test = test, forecasts = fc,
    arima_train = arima_train_fit,
    arima_label = as.character(arima_train_fit),
    accuracy = bind_rows(multi_step, one_step_metrics),
    test_predictions = test_predictions,
    one_step_predictions = one_step,
    residual_ljung_box = tibble(model = as.character(arima_train_fit),
                                statistic = unname(lb$statistic), df = unname(lb$parameter),
                                p_value = lb$p.value)
  )
}

final_forecast <- function(df, h = 3) {
  infl <- to_ts(df, "Inflation")
  fit <- forecast::auto.arima(infl, stepwise = FALSE, approximation = FALSE)
  fc <- forecast::forecast(fit, h = h, level = c(80, 95))
  lb <- Box.test(residuals(fit), lag = 5, type = "Ljung-Box",
                 fitdf = sum(forecast::arimaorder(fit)[c(1, 3)]))
  list(
    model = fit, forecast = fc,
    table = tibble(
      Year = as.integer(time(fc$mean)), forecast = as.numeric(fc$mean),
      lo80 = as.numeric(fc$lower[, "80%"]), hi80 = as.numeric(fc$upper[, "80%"]),
      lo95 = as.numeric(fc$lower[, "95%"]), hi95 = as.numeric(fc$upper[, "95%"]),
      model = as.character(fit)
    ),
    ljung_box = tibble(model = as.character(fit), statistic = unname(lb$statistic),
                       df = unname(lb$parameter), p_value = lb$p.value)
  )
}

plot_forecast_evaluation <- function(ev, df) {
  hist <- df |> filter(!is.na(Inflation)) |> select(Year, Inflation)
  ggplot() +
    annotate("rect", xmin = CONFIG$test_start_year - 0.5, xmax = CONFIG$end_year + 0.5,
             ymin = -Inf, ymax = Inf, alpha = 0.08, fill = "grey20") +
    geom_line(data = hist, aes(Year, Inflation), colour = "black", linewidth = 0.9) +
    geom_point(data = hist, aes(Year, Inflation), colour = "black", size = 1.5) +
    geom_line(data = ev$test_predictions, aes(Year, forecast, colour = method), linewidth = 0.9) +
    labs(title = "Inflation forecasts on the 2019-2024 test period",
         subtitle = paste("Models fitted on 2000-2018 only. ARIMA selected:", ev$arima_label),
         x = NULL, y = "Inflation (%)", colour = "Method") +
    theme_sa()
}

plot_final_forecast <- function(ff, df) {
  hist <- df |> filter(!is.na(Inflation)) |> select(Year, Inflation)
  t <- ff$table
  ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = 6, alpha = 0.1, fill = "darkgreen") +
    geom_ribbon(data = t, aes(Year, ymin = lo95, ymax = hi95), fill = "#aed6f1", alpha = 0.6) +
    geom_ribbon(data = t, aes(Year, ymin = lo80, ymax = hi80), fill = "#5dade2", alpha = 0.6) +
    geom_line(data = hist, aes(Year, Inflation), linewidth = 0.9) +
    geom_point(data = hist, aes(Year, Inflation), size = 1.5) +
    geom_line(data = t, aes(Year, forecast), colour = "#1f4e79", linewidth = 1, linetype = "dashed") +
    geom_point(data = t, aes(Year, forecast), colour = "#1f4e79", size = 2) +
    labs(title = sprintf("Inflation forecast %d-%d: %s", min(t$Year), max(t$Year), t$model[1]),
         subtitle = "Shaded bands: 80% and 95% prediction intervals; green band: 3-6% target",
         x = NULL, y = "Inflation (%)") +
    theme_sa()
}

run_model_evaluation <- function(df = load_economic_data()) {
  set.seed(CONFIG$seed)
  reg_eval <- evaluate_regressions(df)
  fc_eval  <- evaluate_forecasts(df)
  ff       <- final_forecast(df)

  p_eval  <- plot_forecast_evaluation(fc_eval, df)
  p_final <- plot_final_forecast(ff, df)
  save_plot(p_eval, "forecast_test_period.png")
  save_plot(p_final, "forecast_inflation_final.png")

  save_table(reg_eval, "evaluation_regression_holdout.csv")
  save_table(fc_eval$accuracy, "evaluation_forecast_accuracy.csv")
  save_table(fc_eval$test_predictions, "evaluation_forecast_test_predictions.csv")
  save_table(fc_eval$one_step_predictions, "evaluation_forecast_one_step_predictions.csv")
  save_table(bind_rows(fc_eval$residual_ljung_box |> mutate(fit = "Training (2000-2018)"),
                       ff$ljung_box |> mutate(fit = "Full sample (2000-2024)")),
             "evaluation_arima_residual_tests.csv")
  save_table(ff$table, "forecast_inflation_final.csv")

  invisible(list(regression = reg_eval, forecasting = fc_eval, final = ff,
                 plots = list(evaluation = p_eval, final = p_final)))
}

if (sys.nframe() == 0) run_model_evaluation()
