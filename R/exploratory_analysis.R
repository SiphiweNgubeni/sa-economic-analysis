# =============================================================================
# exploratory_analysis.R  (Phase 4)
# Trend and relationship plots for the core indicators.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

SOURCE_CAPTION <- "Sources: World Bank WDI; South African Reserve Bank (repo rate)."

# Line chart of one indicator with optional event markers and the 3-6% target band
plot_indicator_trend <- function(df, var, show_events = TRUE, show_target = (var == "Inflation")) {
  d <- df |> filter(!is.na(.data[[var]]))
  p <- ggplot(d, aes(x = Year, y = .data[[var]]))

  if (show_target) {
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf,
                      ymin = CONFIG$inflation_target_low, ymax = CONFIG$inflation_target_high,
                      alpha = 0.12, fill = "darkgreen") +
      annotate("text", x = min(d$Year) + 0.2, y = CONFIG$inflation_target_high + 0.4,
               label = "SARB 3-6% target range", hjust = 0, size = 3, colour = "darkgreen")
  }
  if (show_events) {
    ev <- ECONOMIC_EVENTS |> filter(between(Year, min(d$Year), max(d$Year)))
    p <- p + geom_vline(data = ev, aes(xintercept = Year), linetype = "dotted", colour = "grey55") +
      geom_text(data = ev, aes(x = Year, y = Inf, label = Event), angle = 90,
                vjust = -0.4, hjust = 1.05, size = 2.8, colour = "grey35", inherit.aes = FALSE)
  }

  p + geom_line(colour = "#1f4e79", linewidth = 0.9) +
    geom_point(colour = "#1f4e79", size = 1.8) +
    scale_x_continuous(breaks = seq(2000, 2030, 4)) +
    labs(title = paste("South Africa:", INDICATOR_LABELS[[var]]),
         x = "Year", y = INDICATOR_LABELS[[var]], caption = SOURCE_CAPTION) +
    theme_sa()
}

# Faceted overview of several indicators
plot_all_trends <- function(df, vars = CORE_INDICATORS) {
  df |>
    select(Year, all_of(vars)) |>
    pivot_longer(-Year, names_to = "Indicator", values_to = "Value") |>
    filter(!is.na(Value)) |>
    mutate(Indicator = factor(INDICATOR_LABELS[Indicator], levels = INDICATOR_LABELS[vars])) |>
    ggplot(aes(Year, Value)) +
    geom_line(colour = "#1f4e79", linewidth = 0.8) +
    geom_point(colour = "#1f4e79", size = 1.2) +
    facet_wrap(~Indicator, scales = "free_y", ncol = 2) +
    labs(title = "South African core economic indicators", x = NULL, y = NULL,
         caption = SOURCE_CAPTION) +
    theme_sa()
}

# Scatterplot with OLS line; points labelled by year so the time path is visible
plot_relationship <- function(df, x, y, label_years = TRUE) {
  d <- df |> filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  r <- cor(d[[x]], d[[y]])
  p <- ggplot(d, aes(.data[[x]], .data[[y]])) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#c0392b", fill = "#f5b7b1") +
    geom_point(aes(colour = Year), size = 2.5) +
    scale_colour_viridis_c(option = "D") +
    labs(title = paste(INDICATOR_LABELS[[y]], "vs", INDICATOR_LABELS[[x]]),
         subtitle = sprintf("Pearson r = %.2f, n = %d (association only, not causation)", r, nrow(d)),
         x = INDICATOR_LABELS[[x]], y = INDICATOR_LABELS[[y]], caption = SOURCE_CAPTION) +
    theme_sa()
  if (label_years) p <- p + geom_text(aes(label = Year), size = 2.6, vjust = -0.9, colour = "grey30")
  p
}

# Notable observations used in the written interpretation
find_extremes <- function(df, vars = CORE_INDICATORS) {
  purrr::map_dfr(vars, function(v) {
    d <- df |> filter(!is.na(.data[[v]]))
    tibble(
      variable = v,
      max_year = d$Year[which.max(d[[v]])], max_value = max(d[[v]]),
      min_year = d$Year[which.min(d[[v]])], min_value = min(d[[v]]),
      first_value = d[[v]][1], last_value = d[[v]][nrow(d)],
      change_first_to_last = d[[v]][nrow(d)] - d[[v]][1]
    )
  })
}

run_exploratory_analysis <- function(df = load_economic_data()) {
  plots <- list(
    inflation_trend       = plot_indicator_trend(df, "Inflation"),
    gdp_growth_trend      = plot_indicator_trend(df, "GDP_Growth"),
    unemployment_trend    = plot_indicator_trend(df, "Unemployment"),
    interest_rate_trend   = plot_indicator_trend(df, "Interest_Rate"),
    core_indicators       = plot_all_trends(df),
    inflation_vs_interest = plot_relationship(df, "Interest_Rate", "Inflation"),
    inflation_vs_unemployment = plot_relationship(df, "Unemployment", "Inflation"),
    gdp_vs_unemployment   = plot_relationship(df, "GDP_Growth", "Unemployment")
  )
  purrr::iwalk(plots, ~ save_plot(.x, paste0("eda_", .y, ".png"),
                                  height = if (.y == "core_indicators") 7 else 5.5))

  extremes <- find_extremes(df)
  save_table(extremes, "eda_extremes.csv")
  invisible(list(plots = plots, extremes = extremes))
}

if (sys.nframe() == 0) run_exploratory_analysis()
