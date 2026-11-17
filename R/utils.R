# =============================================================================
# utils.R
# Shared configuration, paths and helper functions used by every module.
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ---- Project root -----------------------------------------------------------
# Scripts can be run from the project root, from dashboard/ or from reports/.
find_project_root <- function(start = getwd()) {
  path <- normalizePath(start, winslash = "/", mustWork = FALSE)
  for (i in 1:5) {
    if (length(list.files(path, pattern = "\\.Rproj$")) > 0) return(path)
    parent <- dirname(path)
    if (identical(parent, path)) break
    path <- parent
  }
  stop("Could not find the project root (folder containing the .Rproj file).")
}

PROJECT_ROOT <- find_project_root()

proj_path <- function(...) file.path(PROJECT_ROOT, ...)

PATHS <- list(
  raw            = proj_path("data", "raw"),
  processed      = proj_path("data", "processed"),
  outputs        = proj_path("outputs"),
  visualisations = proj_path("visualisations")
)

for (p in PATHS) if (!dir.exists(p)) dir.create(p, recursive = TRUE)

# ---- Analysis settings ------------------------------------------------------
CONFIG <- list(
  start_year      = 2000,
  end_year        = 2024,  # last year with complete coverage of all indicators
  test_start_year = 2019,  # chronological hold-out: 2019-2024 (6 years)
  alpha           = 0.05,  # significance level for all hypothesis tests
  gfc_break_year  = 2009,  # first post-global-financial-crisis year
  inflation_target_low  = 3,
  inflation_target_high = 6,
  seed            = 2024
)

# Human-readable labels for the indicator columns
INDICATOR_LABELS <- c(
  Inflation         = "Inflation (CPI, annual %)",
  GDP_Growth        = "Real GDP growth (annual %)",
  Unemployment      = "Unemployment (% of labour force)",
  Interest_Rate     = "SARB repo rate (annual average, %)",
  Population_Growth = "Population growth (annual %)",
  GDP_Per_Capita    = "GDP per capita (current US$)"
)

CORE_INDICATORS <- c("Inflation", "GDP_Growth", "Unemployment", "Interest_Rate")

# ---- Helpers ----------------------------------------------------------------
save_plot <- function(plot, filename, width = 9, height = 5.5, dpi = 150) {
  ggsave(file.path(PATHS$visualisations, filename), plot,
         width = width, height = height, dpi = dpi, bg = "white")
  invisible(file.path(PATHS$visualisations, filename))
}

save_table <- function(df, filename) {
  readr::write_csv(df, file.path(PATHS$outputs, filename))
  invisible(file.path(PATHS$outputs, filename))
}

load_economic_data <- function() {
  f <- file.path(PATHS$processed, "sa_economic_annual.csv")
  if (!file.exists(f)) {
    stop("Processed dataset not found. Run R/data_cleaning.R (or run_analysis.R) first.")
  }
  readr::read_csv(f, show_col_types = FALSE)
}

# Annotated economic events used in plots and interpretation
ECONOMIC_EVENTS <- tibble::tribble(
  ~Year, ~Event,
  2003, "Inflation targeting bedded in",
  2008, "Food & fuel price shock",
  2009, "Global financial crisis recession",
  2016, "Drought / commodity slump",
  2020, "COVID-19 lockdown",
  2022, "Post-COVID global inflation"
)

theme_sa <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(colour = "grey40", size = 9),
      legend.position = "bottom"
    )
}

fmt_p <- function(p) ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
