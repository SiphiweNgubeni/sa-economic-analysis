# =============================================================================
# correlation_analysis.R  (Phase 6)
# Pearson and Spearman correlations, significance tests, and a check on
# first-differenced data (trending series can show spurious correlation).
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

RESEARCH_PAIRS <- tibble::tribble(
  ~x,              ~y,             ~question,
  "Interest_Rate", "Inflation",    "Is inflation associated with interest rates?",
  "GDP_Growth",    "Unemployment", "Is unemployment associated with GDP growth?",
  "GDP_Growth",    "Inflation",    "Is inflation associated with GDP growth?",
  "Unemployment",  "Inflation",    "Is inflation associated with unemployment?"
)

correlation_matrix <- function(df, vars = names(INDICATOR_LABELS), method = "pearson") {
  cor(df[vars], use = "pairwise.complete.obs", method = method)
}

difference_data <- function(df, vars = names(INDICATOR_LABELS)) {
  df |> arrange(Year) |> mutate(across(all_of(vars), ~ .x - dplyr::lag(.x)))
}

normality_checks <- function(df, vars = names(INDICATOR_LABELS)) {
  purrr::map_dfr(vars, function(v) {
    x <- na.omit(df[[v]])
    sw <- shapiro.test(x)
    tibble(variable = v, n = length(x), shapiro_W = unname(sw$statistic),
           shapiro_p = sw$p.value, normal_at_5pct = sw$p.value >= CONFIG$alpha)
  })
}

correlation_tests <- function(df, pairs = RESEARCH_PAIRS, data_form = "Levels") {
  purrr::pmap_dfr(pairs, function(x, y, question) {
    d <- df |> filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    pe <- cor.test(d[[x]], d[[y]], method = "pearson")
    sp <- suppressWarnings(cor.test(d[[x]], d[[y]], method = "spearman", exact = FALSE))
    tibble(
      data_form = data_form, question = question, x = x, y = y, n = nrow(d),
      pearson_r = unname(pe$estimate), pearson_ci_low = pe$conf.int[1],
      pearson_ci_high = pe$conf.int[2], pearson_p = pe$p.value,
      spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
      significant_5pct = pe$p.value < CONFIG$alpha
    )
  })
}

save_corrplot <- function(mat, filename, title) {
  colnames(mat) <- rownames(mat) <- gsub("_", " ", colnames(mat))
  png(file.path(PATHS$visualisations, filename), width = 1100, height = 1000, res = 150)
  corrplot::corrplot(mat, method = "color", type = "upper", addCoef.col = "black",
                     number.cex = 0.75, tl.col = "black", tl.srt = 45, diag = FALSE,
                     title = title, mar = c(0, 0, 2, 0))
  dev.off()
}

run_correlation_analysis <- function(df = load_economic_data()) {
  pearson  <- correlation_matrix(df, method = "pearson")
  spearman <- correlation_matrix(df, method = "spearman")
  diffs    <- difference_data(df)
  pearson_diff <- correlation_matrix(diffs, method = "pearson")

  tests <- bind_rows(
    correlation_tests(df, data_form = "Levels"),
    correlation_tests(diffs, data_form = "First differences")
  )
  normality <- normality_checks(df)

  save_corrplot(pearson, "correlation_pearson.png", "Pearson correlation (levels)")
  save_corrplot(spearman, "correlation_spearman.png", "Spearman correlation (levels)")
  save_corrplot(pearson_diff, "correlation_pearson_differences.png",
                "Pearson correlation (first differences)")

  save_table(as_tibble(pearson, rownames = "variable"), "correlation_matrix_pearson.csv")
  save_table(as_tibble(spearman, rownames = "variable"), "correlation_matrix_spearman.csv")
  save_table(tests, "correlation_tests.csv")
  save_table(normality, "normality_checks.csv")

  invisible(list(pearson = pearson, spearman = spearman, pearson_diff = pearson_diff,
                 tests = tests, normality = normality))
}

if (sys.nframe() == 0) run_correlation_analysis()
