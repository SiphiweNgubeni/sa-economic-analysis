# =============================================================================
# descriptive_statistics.R  (Phase 5)
# Mean, median, SD, variance, min/max, quartiles, skewness and kurtosis.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

describe_indicators <- function(df, vars = names(INDICATOR_LABELS)) {
  purrr::map_dfr(vars, function(v) {
    x <- df[[v]][!is.na(df[[v]])]
    q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
    tibble(
      variable  = v,
      label     = INDICATOR_LABELS[[v]],
      n         = length(x),
      n_missing = sum(is.na(df[[v]])),
      mean      = mean(x),
      median    = median(x),
      sd        = sd(x),
      variance  = var(x),
      min       = min(x),
      q1        = q[1],
      q3        = q[2],
      max       = max(x),
      iqr       = q[2] - q[1],
      cv        = sd(x) / abs(mean(x)),           # coefficient of variation
      skewness  = psych::skew(x),
      kurtosis  = psych::kurtosi(x)                # excess kurtosis
    )
  })
}

describe_by_period <- function(df, vars = CORE_INDICATORS) {
  df |>
    select(Period, all_of(vars)) |>
    pivot_longer(-Period, names_to = "variable", values_to = "value") |>
    filter(!is.na(value)) |>
    group_by(variable, Period) |>
    summarise(n = n(), mean = mean(value), median = median(value), sd = sd(value),
              min = min(value), max = max(value), .groups = "drop") |>
    arrange(variable, desc(Period))
}

# How often inflation was inside, above or below the SARB 3-6% target range
inflation_target_summary <- function(df) {
  df |>
    filter(!is.na(Inflation)) |>
    mutate(position = case_when(
      Inflation < CONFIG$inflation_target_low  ~ "Below 3%",
      Inflation > CONFIG$inflation_target_high ~ "Above 6%",
      TRUE ~ "Within 3-6%"
    )) |>
    count(position, name = "years") |>
    mutate(share = years / sum(years))
}

run_descriptive_statistics <- function(df = load_economic_data()) {
  overall <- describe_indicators(df)
  by_period <- describe_by_period(df)
  target <- inflation_target_summary(df)

  save_table(overall, "descriptive_statistics.csv")
  save_table(by_period, "descriptive_statistics_by_period.csv")
  save_table(target, "inflation_target_summary.csv")

  # Distribution plot (boxplot + points) of the core indicators
  p <- df |>
    select(all_of(CORE_INDICATORS)) |>
    pivot_longer(everything(), names_to = "variable", values_to = "value") |>
    filter(!is.na(value)) |>
    mutate(variable = INDICATOR_LABELS[variable]) |>
    ggplot(aes(x = "", y = value)) +
    geom_boxplot(fill = "#d6eaf8", outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.08, alpha = 0.7, colour = "#1f4e79") +
    facet_wrap(~variable, scales = "free_y", ncol = 4) +
    labs(title = "Distribution of annual observations", x = NULL, y = NULL) +
    theme_sa()
  save_plot(p, "descriptive_boxplots.png", width = 11, height = 4.5)

  invisible(list(overall = overall, by_period = by_period, target = target, boxplot = p))
}

if (sys.nframe() == 0) run_descriptive_statistics()
