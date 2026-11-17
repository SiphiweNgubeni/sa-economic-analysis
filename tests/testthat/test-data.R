# Data and helper checks. Run from the project root:
#   testthat::test_dir("tests/testthat")

.root <- getwd()
while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
source(file.path(.root, "R", "utils.R"))
source(file.path(.root, "R", "data_cleaning.R"))
source(file.path(.root, "R", "hypothesis_testing.R"))
source(file.path(.root, "R", "model_evaluation.R"))

df <- load_economic_data()

test_that("processed dataset covers the analysis window with one row per year", {
  expect_equal(df$Year, CONFIG$start_year:CONFIG$end_year)
  expect_false(any(duplicated(df$Year)))
  expect_true(all(names(INDICATOR_LABELS) %in% names(df)))
})

test_that("missing values are kept as NA, not replaced with zero", {
  expect_true(all(is.na(df$Interest_Rate[df$Year %in% 2000:2001])))
  expect_false(any(df$Interest_Rate == 0, na.rm = TRUE))
})

test_that("values fall inside plausible ranges", {
  for (i in seq_len(nrow(PLAUSIBLE_RANGES))) {
    x <- df[[PLAUSIBLE_RANGES$variable[i]]]
    expect_true(all(x >= PLAUSIBLE_RANGES$min[i] & x <= PLAUSIBLE_RANGES$max[i], na.rm = TRUE),
                info = PLAUSIBLE_RANGES$variable[i])
  }
})

test_that("repo-rate annual average is time-weighted across rate changes", {
  daily <- tibble::tibble(Date = as.Date(c("2001-12-31", "2002-07-02", "2002-12-31")),
                          Value = c(10, 12, 12))
  annual <- aggregate_repo_annual(daily)
  y2002 <- annual[annual$Year == 2002, ]
  # 182 days at 10% (1 Jan-1 Jul) and 183 days at 12% (2 Jul-31 Dec)
  expect_equal(y2002$Interest_Rate, (182 * 10 + 183 * 12) / 365)
  expect_equal(y2002$Interest_Rate_YearEnd, 12)
})

test_that("World Bank reader extracts South Africa and parses blanks as NA", {
  f <- tempfile(fileext = ".csv")
  writeLines(c('"Data Source","WDI",', "", '"Last Updated Date","2026-01-01",', "",
               '"Country Name","Country Code","Indicator Name","Indicator Code","2000","2001",',
               '"South Africa","ZAF","x","X","5.5","",',
               '"Zambia","ZMB","x","X","9","9",'), f)
  out <- read_wb_indicator(f, "Inflation")
  expect_equal(out$Year, c(2000L, 2001L))
  expect_equal(out$Inflation[1], 5.5)
  expect_true(is.na(out$Inflation[2]))
})

test_that("metrics and effect sizes are computed correctly", {
  m <- metrics(c(2, 4), c(1, 6))
  expect_equal(m$MAE, 1.5)
  expect_equal(m$RMSE, sqrt((1 + 4) / 2))
  d <- cohens_d(c(1, 2, 3), c(1, 2, 3))
  expect_equal(unname(d["d"]), 0)
})
