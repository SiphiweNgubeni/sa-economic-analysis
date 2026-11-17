# =============================================================================
# data_collection.R  (Phase 2)
# Collects the raw datasets into data/raw/ and writes a source register.
#
# Sources
#   * World Bank World Development Indicators (bulk CSV downloads, stored in the
#     folder above this project): inflation, GDP growth, unemployment,
#     population growth, GDP per capita.
#   * South African Reserve Bank (SARB) Web API: daily SARB policy (repo) rate.
#
# Raw files are copied/saved unchanged. Existing raw files are never
# overwritten unless refresh = TRUE.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

WB_INDICATORS <- tibble::tribble(
  ~variable,           ~code,               ~folder,
  "Inflation",         "FP.CPI.TOTL.ZG",    "API_FP.CPI.TOTL.ZG_DS2_en_csv_v2_64135",
  "GDP_Growth",        "NY.GDP.MKTP.KD.ZG", "API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_60678",
  "Unemployment",      "SL.UEM.TOTL.ZS",    "API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_59463",
  "Population_Growth", "SP.POP.GROW",       "API_SP.POP.GROW_DS2_en_csv_v2_73084",
  "GDP_Per_Capita",    "NY.GDP.PCAP.CD",    "API_NY.GDP.PCAP.CD_DS2_en_csv_v2_62374"
)

SARB_POLICY_RATE_CODE <- "MMRD002A"
SARB_API_URL <- paste0(
  "https://custom.resbank.co.za/SarbWebApi/WebIndicators/Shared/",
  "GetTimeseriesObservations/%s/%s/%s"
)

# Copy the World Bank bulk-download folders into data/raw/world_bank/
collect_world_bank <- function(source_dir = dirname(PROJECT_ROOT), refresh = FALSE) {
  dest_root <- file.path(PATHS$raw, "world_bank")
  dir.create(dest_root, showWarnings = FALSE, recursive = TRUE)

  purrr::pmap_dfr(WB_INDICATORS, function(variable, code, folder) {
    src  <- file.path(source_dir, folder)
    dest <- file.path(dest_root, folder)
    data_file <- file.path(dest, paste0(folder, ".csv"))

    if (!file.exists(data_file) || refresh) {
      if (!dir.exists(src)) {
        stop("World Bank folder not found: ", src,
             "\nDownload it from https://data.worldbank.org/indicator/", code,
             "?locations=ZA (CSV) and place it there or in data/raw/world_bank/.")
      }
      dir.create(dest, showWarnings = FALSE)
      file.copy(list.files(src, full.names = TRUE), dest, overwrite = refresh)
    }

    # Read the "Last Updated Date" and the indicator metadata from the raw file
    header <- readLines(data_file, n = 3, warn = FALSE)
    last_updated <- stringr::str_match(header[3], '"Last Updated Date","([^"]+)"')[, 2]
    meta_file <- list.files(dest, pattern = "^Metadata_Indicator", full.names = TRUE)
    meta <- readr::read_csv(meta_file, show_col_types = FALSE, name_repair = "unique_quiet")

    tibble(
      variable        = variable,
      source          = "World Bank, World Development Indicators",
      indicator_code  = code,
      indicator_name  = meta$INDICATOR_NAME[1],
      definition      = meta$SOURCE_NOTE[1],
      original_source = meta$SOURCE_ORGANIZATION[1],
      frequency       = "Annual",
      units           = dplyr::case_when(
        variable == "GDP_Per_Capita" ~ "Current US$",
        TRUE ~ "Percent"
      ),
      source_last_updated = last_updated,
      raw_file        = file.path("data/raw/world_bank", folder, paste0(folder, ".csv")),
      url             = paste0("https://data.worldbank.org/indicator/", code, "?locations=ZA")
    )
  })
}

# Download the daily SARB policy (repo) rate from the SARB Web API
collect_sarb_repo_rate <- function(start = "1998-01-01", end = Sys.Date(), refresh = FALSE) {
  dest_dir <- file.path(PATHS$raw, "sarb")
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(dest_dir, "sarb_policy_rate_daily.json")

  if (!file.exists(dest) || refresh) {
    url <- sprintf(SARB_API_URL, SARB_POLICY_RATE_CODE, start, format(as.Date(end)))
    message("Downloading SARB policy rate: ", url)
    txt <- paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "")
    if (nchar(txt) < 10) stop("SARB API returned no observations.")
    writeLines(txt, dest, useBytes = TRUE)
    writeLines(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
               file.path(dest_dir, "download_timestamp.txt"))
  }

  stamp_file <- file.path(dest_dir, "download_timestamp.txt")
  tibble(
    variable        = "Interest_Rate",
    source          = "South African Reserve Bank (SARB) Web API",
    indicator_code  = SARB_POLICY_RATE_CODE,
    indicator_name  = "SARB Policy Rate (repo rate)",
    definition      = paste("The interest rate set by the SARB Monetary Policy Committee",
                            "to achieve the inflation target. Daily observations;",
                            "aggregated to a time-weighted calendar-year average."),
    original_source = "SARB Monetary Policy Committee",
    frequency       = "Daily (aggregated to annual)",
    units           = "Percent per annum",
    source_last_updated = if (file.exists(stamp_file)) readLines(stamp_file)[1] else NA_character_,
    raw_file        = "data/raw/sarb/sarb_policy_rate_daily.json",
    url             = "https://www.resbank.co.za/en/home/what-we-do/statistics/key-statistics/current-market-rates"
  )
}

run_data_collection <- function(refresh = FALSE) {
  register <- dplyr::bind_rows(
    collect_world_bank(refresh = refresh),
    collect_sarb_repo_rate(refresh = refresh)
  ) |>
    mutate(register_written = format(Sys.Date()))

  readr::write_csv(register, file.path(PATHS$raw, "data_sources.csv"))
  message("Source register written to data/raw/data_sources.csv")
  invisible(register)
}

if (sys.nframe() == 0) run_data_collection()
