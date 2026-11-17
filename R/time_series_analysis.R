# =============================================================================
# time_series_analysis.R  (Phase 9)
# Trend analysis, moving averages, ACF/PACF, stationarity tests and a
# trend/remainder decomposition.
#
# The data are annual, so there is no seasonal component to decompose. The
# decomposition therefore splits each series into a smooth trend (LOESS) and
# a remainder (cyclical/irregular part).
#
# Stationarity: the ADF test (H0: unit root) and the KPSS test
# (H0: stationary) are used together, because with about 25 annual
# observations each test has low power on its own.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

to_ts <- function(df, var) {
  d <- df |> filter(!is.na(.data[[var]])) |> arrange(Year)
  stopifnot(all(diff(d$Year) == 1))  # series must be gap-free
  ts(d[[var]], start = min(d$Year), frequency = 1)
}

add_moving_averages <- function(df, vars = CORE_INDICATORS, k = 3) {
  df |> arrange(Year) |>
    mutate(across(all_of(vars), ~ as.numeric(stats::filter(.x, rep(1 / k, k), sides = 2)),
                  .names = "{.col}_MA{k}"))
}

linear_trends <- function(df, vars = CORE_INDICATORS) {
  purrr::map_dfr(vars, function(v) {
    m <- lm(reformulate("Year", v), data = df)
    ct <- lmtest::coeftest(m, vcov. = sandwich::NeweyWest(m, prewhite = FALSE))
    tibble(variable = v, slope_per_year = coef(m)[["Year"]],
           hac_p_value = ct["Year", "Pr(>|t|)"], r_squared = summary(m)$r.squared)
  })
}

loess_decomposition <- function(df, vars = CORE_INDICATORS, span = 0.6) {
  purrr::map_dfr(vars, function(v) {
    d <- df |> filter(!is.na(.data[[v]]))
    fit <- loess(reformulate("Year", v), data = d, span = span)
    tibble(Year = d$Year, variable = v, observed = d[[v]],
           trend = fitted(fit), remainder = d[[v]] - fitted(fit))
  })
}

stationarity_tests <- function(df, vars = CORE_INDICATORS) {
  purrr::map_dfr(vars, function(v) {
    x <- to_ts(df, v)
    purrr::map_dfr(c("Level", "First difference"), function(form) {
      y <- if (form == "Level") x else diff(x)
      adf  <- suppressWarnings(tseries::adf.test(y))
      kpss <- suppressWarnings(tseries::kpss.test(y, null = "Level"))
      # urca ADF with drift, lag length chosen by AIC (max 2 lags for annual data)
      ur <- urca::ur.df(y, type = "drift", lags = 2, selectlags = "AIC")
      tau <- ur@teststat[1, "tau2"]; crit5 <- ur@cval["tau2", "5pct"]
      adf_stationary  <- adf$p.value < CONFIG$alpha
      kpss_stationary <- kpss$p.value >= CONFIG$alpha
      tibble(
        variable = v, form = form, n = length(y),
        adf_statistic = unname(adf$statistic), adf_p = adf$p.value,
        kpss_statistic = unname(kpss$statistic), kpss_p = kpss$p.value,
        urca_tau = tau, urca_crit_5pct = crit5,
        conclusion = case_when(
          adf_stationary & kpss_stationary   ~ "Stationary (both tests agree)",
          !adf_stationary & !kpss_stationary ~ "Non-stationary (both tests agree)",
          !adf_stationary & kpss_stationary  ~ "Inconclusive (ADF: unit root; KPSS: stationary)",
          TRUE                               ~ "Inconclusive (ADF: stationary; KPSS: non-stationary)"
        )
      )
    })
  })
}

acf_table <- function(df, var = "Inflation", lag_max = 8) {
  x <- to_ts(df, var)
  a <- acf(x, lag.max = lag_max, plot = FALSE)
  p <- pacf(x, lag.max = lag_max, plot = FALSE)
  lb <- Box.test(x, lag = 5, type = "Ljung-Box")
  list(
    table = tibble(lag = 1:lag_max, acf = a$acf[-1], pacf = p$acf[, 1, 1],
                   bound_95 = qnorm(0.975) / sqrt(length(x))),
    ljung_box = tibble(variable = var, lag = 5, statistic = unname(lb$statistic), p_value = lb$p.value)
  )
}

run_time_series_analysis <- function(df = load_economic_data()) {
  ma <- add_moving_averages(df)
  trends <- linear_trends(df)
  decomposition <- loess_decomposition(df)
  stationarity <- stationarity_tests(df)
  acfs <- purrr::map(CORE_INDICATORS, ~ acf_table(df, .x)) |> setNames(CORE_INDICATORS)
  ljung_box <- purrr::map_dfr(acfs, "ljung_box")

  # Plots ------------------------------------------------------------------
  p_ma <- ma |>
    select(Year, Inflation, Inflation_MA3) |>
    pivot_longer(-Year, names_to = "series", values_to = "value") |>
    mutate(series = recode(series, Inflation = "Annual inflation",
                           Inflation_MA3 = "3-year centred moving average")) |>
    filter(!is.na(value)) |>
    ggplot(aes(Year, value, colour = series, linetype = series)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = 6, alpha = 0.1, fill = "darkgreen") +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = c("#1f4e79", "#c0392b")) +
    labs(title = "Inflation and its 3-year moving average", x = NULL, y = "%",
         colour = NULL, linetype = NULL) +
    theme_sa()
  save_plot(p_ma, "ts_inflation_moving_average.png")

  p_dec <- decomposition |>
    pivot_longer(c(observed, trend, remainder), names_to = "component", values_to = "value") |>
    mutate(component = factor(component, levels = c("observed", "trend", "remainder")),
           variable = INDICATOR_LABELS[variable]) |>
    ggplot(aes(Year, value)) +
    geom_hline(data = \(d) filter(d, component == "remainder"), aes(yintercept = 0), colour = "grey60") +
    geom_line(colour = "#1f4e79") +
    facet_grid(component ~ variable, scales = "free_y") +
    labs(title = "Trend / remainder decomposition (LOESS, span = 0.6)", x = NULL, y = NULL) +
    theme_sa() + theme(strip.text.x = element_text(size = 8))
  save_plot(p_dec, "ts_decomposition.png", width = 13, height = 7)

  infl_ts <- to_ts(df, "Inflation")
  p_acf  <- forecast::ggAcf(infl_ts, lag.max = 8) + labs(title = "ACF: annual inflation") + theme_sa()
  p_pacf <- forecast::ggPacf(infl_ts, lag.max = 8) + labs(title = "PACF: annual inflation") + theme_sa()
  save_plot(p_acf, "ts_inflation_acf.png", width = 7, height = 4)
  save_plot(p_pacf, "ts_inflation_pacf.png", width = 7, height = 4)

  # Tables -----------------------------------------------------------------
  save_table(trends, "ts_linear_trends.csv")
  save_table(stationarity, "ts_stationarity_tests.csv")
  save_table(ljung_box, "ts_ljung_box.csv")
  save_table(acfs$Inflation$table, "ts_inflation_acf_pacf.csv")

  invisible(list(moving_averages = ma, trends = trends, decomposition = decomposition,
                 stationarity = stationarity, acf = acfs, ljung_box = ljung_box,
                 plots = list(ma = p_ma, decomposition = p_dec, acf = p_acf, pacf = p_pacf)))
}

if (sys.nframe() == 0) run_time_series_analysis()
