# =============================================================================
# data_cleaning.R  (Phase 3)
# Imports the raw files, checks data quality and builds one tidy annual
# dataset: data/processed/sa_economic_annual.csv
#
# Rules followed
#   * Raw files are read only, never modified.
#   * Missing observations stay NA (never replaced with zero).
#   * The analysis window is CONFIG$start_year to CONFIG$end_year.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

# Plausibility bounds for South African annual data (used to flag, not delete)
PLAUSIBLE_RANGES <- tibble::tribble(
  ~variable,           ~min,  ~max,
  "Inflation",          -5,    30,
  "GDP_Growth",        -15,    15,
  "Unemployment",        0,    60,
  "Interest_Rate",       0,    25,
  "Population_Growth",  -2,     5,
  "GDP_Per_Capita",    100, 20000
)

# ---- World Bank ---------------------------------------------------------------
# WDI bulk CSVs have 4 metadata lines before the header and one column per year.
read_wb_indicator <- function(file, variable, country_code = "ZAF") {
  raw <- readr::read_csv(file, skip = 4, show_col_types = FALSE, name_repair = "unique_quiet",
                         col_types = readr::cols(.default = readr::col_character()))
  raw |>
    filter(`Country Code` == country_code) |>
    select(matches("^\\d{4}$")) |>
    pivot_longer(everything(), names_to = "Year", values_to = "value") |>
    mutate(
      Year  = as.integer(Year),
      value = readr::parse_double(na_if(value, ""))
    ) |>
    rename(!!variable := value)
}

import_world_bank <- function(register) {
  wb <- register |> filter(stringr::str_detect(source, "World Bank"))
  purrr::map2(proj_path(wb$raw_file), wb$variable, read_wb_indicator) |>
    purrr::reduce(full_join, by = "Year") |>
    arrange(Year)
}

# ---- SARB repo rate -------------------------------------------------------------
import_sarb_daily <- function(file = file.path(PATHS$raw, "sarb", "sarb_policy_rate_daily.json")) {
  jsonlite::fromJSON(file) |>
    as_tibble() |>
    transmute(
      Date  = as.Date(lubridate::ymd_hms(Period)),
      Value = as.numeric(Value)
    ) |>
    filter(!is.na(Date), !is.na(Value)) |>
    distinct(Date, .keep_all = TRUE) |>
    arrange(Date)
}

# The repo rate is a step function: each published rate applies until the next
# change. Observations are expanded to every calendar day (carrying the last
# value forward) so the annual mean is time-weighted.
sarb_to_calendar_days <- function(daily) {
  tibble(Date = seq(min(daily$Date), max(daily$Date), by = "day")) |>
    left_join(daily, by = "Date") |>
    tidyr::fill(Value, .direction = "down")
}

aggregate_repo_annual <- function(daily) {
  sarb_to_calendar_days(daily) |>
    mutate(Year = lubridate::year(Date)) |>
    group_by(Year) |>
    summarise(
      Interest_Rate          = mean(Value),
      Interest_Rate_YearEnd  = dplyr::last(Value),
      repo_days_covered      = n(),
      .groups = "drop"
    )
}

aggregate_repo_monthly <- function(daily) {
  sarb_to_calendar_days(daily) |>
    mutate(Month = lubridate::floor_date(Date, "month")) |>
    group_by(Month) |>
    summarise(Interest_Rate = mean(Value), .groups = "drop")
}

# ---- Data-quality checks -------------------------------------------------------
check_data_quality <- function(df, vars = names(INDICATOR_LABELS)) {
  long <- df |>
    select(Year, all_of(vars)) |>
    pivot_longer(-Year, names_to = "variable", values_to = "value")

  missing <- long |>
    group_by(variable) |>
    summarise(
      n_years       = n(),
      n_missing     = sum(is.na(value)),
      missing_years = paste(Year[is.na(value)], collapse = ", "),
      first_year    = suppressWarnings(min(Year[!is.na(value)])),
      last_year     = suppressWarnings(max(Year[!is.na(value)])),
      .groups = "drop"
    )

  out_of_range <- long |>
    inner_join(PLAUSIBLE_RANGES, by = "variable") |>
    filter(!is.na(value), value < min | value > max) |>
    count(variable, name = "n_out_of_range")

  types <- tibble(variable = vars,
                  type = purrr::map_chr(df[vars], ~ class(.x)[1]))

  missing |>
    left_join(out_of_range, by = "variable") |>
    left_join(types, by = "variable") |>
    mutate(
      n_out_of_range  = tidyr::replace_na(n_out_of_range, 0L),
      duplicate_years = sum(duplicated(df$Year))
    )
}

# ---- Main ---------------------------------------------------------------------
run_data_cleaning <- function() {
  register <- readr::read_csv(file.path(PATHS$raw, "data_sources.csv"), show_col_types = FALSE)

  wb_all     <- import_world_bank(register)
  repo_daily <- import_sarb_daily()
  repo_year  <- aggregate_repo_annual(repo_daily)

  # Coverage of every indicator before trimming to the analysis window
  coverage_full <- wb_all |>
    full_join(repo_year |> select(Year, Interest_Rate), by = "Year") |>
    check_data_quality() |>
    select(variable, first_year, last_year)

  economic <- wb_all |>
    left_join(repo_year, by = "Year") |>
    filter(between(Year, CONFIG$start_year, CONFIG$end_year)) |>
    # A year counts as having a repo observation only if the whole year is covered
    mutate(
      Interest_Rate = if_else(repo_days_covered >= 365, Interest_Rate, NA_real_),
      Period = if_else(Year < CONFIG$gfc_break_year, "Pre-GFC (2000-2008)", "Post-GFC (2009-2024)")
    ) |>
    select(Year, all_of(names(INDICATOR_LABELS)), Interest_Rate_YearEnd, Period) |>
    arrange(Year)

  quality <- check_data_quality(economic) |>
    left_join(coverage_full |> rename(source_first_year = first_year,
                                      source_last_year = last_year),
              by = "variable")

  readr::write_csv(economic, file.path(PATHS$processed, "sa_economic_annual.csv"))
  readr::write_csv(aggregate_repo_monthly(repo_daily),
                   file.path(PATHS$processed, "sarb_repo_rate_monthly.csv"))
  readr::write_csv(quality, file.path(PATHS$processed, "data_quality_report.csv"))

  message("Processed dataset: ", nrow(economic), " years x ", ncol(economic), " columns")
  invisible(list(data = economic, quality = quality, repo_daily = repo_daily))
}

if (sys.nframe() == 0) run_data_cleaning()
