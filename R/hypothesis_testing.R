# =============================================================================
# hypothesis_testing.R  (Phase 7)
# Significance level: CONFIG$alpha (5%).
#
# Two-group comparisons use Welch's t-test (does not assume equal variances)
# and report the Wilcoxon rank-sum test as a distribution-free check, because
# the groups are small (about 9-16 years) and normality is uncertain.
# Effect size: Cohen's d (pooled SD) with Hedges' small-sample correction.
#
# Caveat for every test here: annual economic observations are autocorrelated,
# so the independence assumption is only approximately met. p-values should
# be read as indicative, not exact.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  sp <- sqrt(((nx - 1) * var(x) + (ny - 1) * var(y)) / (nx + ny - 2))
  d <- (mean(x) - mean(y)) / sp
  g <- d * (1 - 3 / (4 * (nx + ny) - 9))  # Hedges' g correction
  c(d = d, g = g)
}

effect_label <- function(d) {
  a <- abs(d)
  dplyr::case_when(a < 0.2 ~ "negligible", a < 0.5 ~ "small", a < 0.8 ~ "medium", TRUE ~ "large")
}

two_group_test <- function(df, value, group, name, h0, h1) {
  d <- df |> filter(!is.na(.data[[value]]), !is.na(.data[[group]]))
  lv <- levels(factor(d[[group]]))
  stopifnot(length(lv) == 2)
  x <- d[[value]][d[[group]] == lv[1]]
  y <- d[[value]][d[[group]] == lv[2]]

  welch  <- t.test(x, y, var.equal = FALSE)
  wilcox <- suppressWarnings(wilcox.test(x, y, exact = FALSE))
  levene <- car::leveneTest(d[[value]] ~ factor(d[[group]]))
  es <- cohens_d(x, y)

  tibble(
    test = name, H0 = h0, H1 = h1, outcome = value,
    group_1 = lv[1], group_2 = lv[2], n_1 = length(x), n_2 = length(y),
    mean_1 = mean(x), mean_2 = mean(y), difference = mean(x) - mean(y),
    ci_low = welch$conf.int[1], ci_high = welch$conf.int[2],
    statistic = unname(welch$statistic), df = unname(welch$parameter),
    p_value = welch$p.value, wilcoxon_p = wilcox$p.value,
    shapiro_p_1 = shapiro.test(x)$p.value, shapiro_p_2 = shapiro.test(y)$p.value,
    levene_p = levene$`Pr(>F)`[1],
    effect_size = unname(es["g"]), effect_size_type = "Hedges' g",
    effect_magnitude = effect_label(es["g"]),
    decision = if_else(welch$p.value < CONFIG$alpha, "Reject H0", "Fail to reject H0")
  )
}

run_hypothesis_tests <- function(df = load_economic_data()) {
  med_unemp <- median(df$Unemployment, na.rm = TRUE)
  df <- df |>
    mutate(
      Unemployment_Group = factor(if_else(Unemployment > med_unemp, "High unemployment", "Low unemployment"),
                                  levels = c("High unemployment", "Low unemployment")),
      GFC_Period = factor(Period, levels = c("Pre-GFC (2000-2008)", "Post-GFC (2009-2024)"))
    )

  # Test 1: inflation by unemployment group (median split)
  t1 <- two_group_test(
    df, "Inflation", "Unemployment_Group",
    name = "T1: Inflation, high vs low unemployment years",
    h0 = "Mean inflation is equal in high- and low-unemployment years",
    h1 = "Mean inflation differs between high- and low-unemployment years"
  )

  # Test 2: GDP growth before vs after the global financial crisis
  t2 <- two_group_test(
    df, "GDP_Growth", "GFC_Period",
    name = "T2: GDP growth, pre- vs post-GFC",
    h0 = "Mean GDP growth is equal before and after 2009",
    h1 = "Mean GDP growth differs before and after 2009"
  )

  # Test 3: inflation before vs after the global financial crisis
  t3 <- two_group_test(
    df, "Inflation", "GFC_Period",
    name = "T3: Inflation, pre- vs post-GFC",
    h0 = "Mean inflation is equal before and after 2009",
    h1 = "Mean inflation differs before and after 2009"
  )

  # Test 4: correlation between inflation and the repo rate differs from zero
  d4 <- df |> filter(!is.na(Inflation), !is.na(Interest_Rate))
  ct <- cor.test(d4$Inflation, d4$Interest_Rate)
  t4 <- tibble(
    test = "T4: Correlation of inflation and repo rate", outcome = "Inflation",
    H0 = "Population correlation = 0", H1 = "Population correlation != 0",
    n_1 = nrow(d4), difference = unname(ct$estimate),
    ci_low = ct$conf.int[1], ci_high = ct$conf.int[2],
    statistic = unname(ct$statistic), df = unname(ct$parameter), p_value = ct$p.value,
    effect_size = unname(ct$estimate), effect_size_type = "Pearson r",
    effect_magnitude = case_when(abs(ct$estimate) < 0.1 ~ "negligible", abs(ct$estimate) < 0.3 ~ "small",
                                 abs(ct$estimate) < 0.5 ~ "medium", TRUE ~ "large"),
    decision = if_else(ct$p.value < CONFIG$alpha, "Reject H0", "Fail to reject H0")
  )

  # Test 5: mean inflation vs the midpoint of the 3-6% target range (4.5%)
  midpoint <- mean(c(CONFIG$inflation_target_low, CONFIG$inflation_target_high))
  x5 <- na.omit(df$Inflation)
  ot <- t.test(x5, mu = midpoint)
  t5 <- tibble(
    test = "T5: Mean inflation vs 4.5% target midpoint", outcome = "Inflation",
    H0 = "Mean inflation = 4.5%", H1 = "Mean inflation != 4.5%",
    n_1 = length(x5), mean_1 = mean(x5), difference = mean(x5) - midpoint,
    ci_low = ot$conf.int[1] - midpoint, ci_high = ot$conf.int[2] - midpoint,
    statistic = unname(ot$statistic), df = unname(ot$parameter), p_value = ot$p.value,
    wilcoxon_p = suppressWarnings(wilcox.test(x5, mu = midpoint, exact = FALSE)$p.value),
    shapiro_p_1 = shapiro.test(x5)$p.value,
    effect_size = (mean(x5) - midpoint) / sd(x5), effect_size_type = "Cohen's d (one-sample)",
    effect_magnitude = effect_label((mean(x5) - midpoint) / sd(x5)),
    decision = if_else(ot$p.value < CONFIG$alpha, "Reject H0", "Fail to reject H0")
  )

  results <- bind_rows(t1, t2, t3, t4, t5)

  # The unemployment median split is mostly a split by time, because unemployment
  # rose steadily. Record how many high-unemployment years are post-GFC.
  confounding <- df |> count(Unemployment_Group, GFC_Period)

  save_table(results, "hypothesis_tests.csv")
  save_table(confounding, "hypothesis_unemployment_group_by_period.csv")

  p <- df |>
    select(Year, Inflation, GDP_Growth, Unemployment_Group, GFC_Period) |>
    pivot_longer(c(Unemployment_Group, GFC_Period), names_to = "grouping", values_to = "group") |>
    pivot_longer(c(Inflation, GDP_Growth), names_to = "outcome", values_to = "value") |>
    filter(!(grouping == "Unemployment_Group" & outcome == "GDP_Growth")) |>
    mutate(panel = paste(INDICATOR_LABELS[outcome], "by",
                         if_else(grouping == "GFC_Period", "period", "unemployment group"))) |>
    ggplot(aes(group, value)) +
    geom_boxplot(fill = "#d6eaf8", outlier.shape = NA, width = 0.5) +
    geom_jitter(width = 0.08, colour = "#1f4e79", alpha = 0.8) +
    facet_wrap(~panel, scales = "free", ncol = 3) +
    labs(title = "Group comparisons used in hypothesis tests", x = NULL, y = NULL) +
    theme_sa()
  save_plot(p, "hypothesis_group_comparisons.png", width = 12, height = 4.8)

  invisible(list(results = results, confounding = confounding, plot = p,
                 unemployment_median = med_unemp))
}

if (sys.nframe() == 0) run_hypothesis_tests()
