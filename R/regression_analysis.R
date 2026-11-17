# =============================================================================
# regression_analysis.R  (Phases 8 and 10, assumption checks)
#
# Model 1: Inflation ~ Interest_Rate                       (simple, levels)
# Model 2: Inflation ~ Interest_Rate + GDP_Growth + Unemployment  (multiple, levels)
# Model 3: same as Model 2 in first differences (year-on-year changes), which
#          reduces the risk of spurious regression between trending series.
#
# Diagnostics: residual normality (Shapiro-Wilk), heteroscedasticity
# (Breusch-Pagan), autocorrelation (Durbin-Watson, Breusch-Godfrey),
# functional form (Ramsey RESET), multicollinearity (VIF).
# Newey-West (HAC) standard errors are reported because autocorrelation is
# expected in annual macro data.
# =============================================================================

if (!exists("PROJECT_ROOT")) {
  .root <- getwd()
  while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
  source(file.path(.root, "R", "utils.R"))
}

MODEL_FORMULAS <- list(
  model1 = Inflation ~ Interest_Rate,
  model2 = Inflation ~ Interest_Rate + GDP_Growth + Unemployment,
  model3 = d_Inflation ~ d_Interest_Rate + d_GDP_Growth + d_Unemployment
)

MODEL_DESCRIPTIONS <- c(
  model1 = "Model 1: Simple regression (levels)",
  model2 = "Model 2: Multiple regression (levels)",
  model3 = "Model 3: Multiple regression (first differences)"
)

prepare_regression_data <- function(df) {
  df |>
    arrange(Year) |>
    mutate(across(all_of(CORE_INDICATORS), ~ .x - dplyr::lag(.x), .names = "d_{.col}"))
}

fit_models <- function(df) {
  d <- prepare_regression_data(df)
  purrr::map(MODEL_FORMULAS, ~ lm(.x, data = d))
}

model_diagnostics <- function(model) {
  k <- length(coef(model)) - 1
  res <- residuals(model)
  s <- summary(model)
  tibble(
    n             = nobs(model),
    r_squared     = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    sigma         = s$sigma,
    f_p_value     = pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE),
    aic           = AIC(model),
    in_sample_mae  = mean(abs(res)),
    in_sample_rmse = sqrt(mean(res^2)),
    shapiro_p     = shapiro.test(res)$p.value,
    breusch_pagan_p = lmtest::bptest(model)$p.value,
    durbin_watson = unname(lmtest::dwtest(model)$statistic),
    durbin_watson_p = lmtest::dwtest(model)$p.value,
    breusch_godfrey_p = lmtest::bgtest(model, order = 2)$p.value,
    reset_p       = lmtest::resettest(model, power = 2:3)$p.value,
    max_vif       = if (k > 1) max(car::vif(model)) else NA_real_
  )
}

coefficient_table <- function(model) {
  ols <- broom::tidy(model, conf.int = TRUE)
  # Newey-West HAC covariance; lag chosen automatically by sandwich
  hac <- lmtest::coeftest(model, vcov. = sandwich::NeweyWest(model, prewhite = FALSE))
  ols |>
    mutate(
      hac_std_error = hac[, "Std. Error"],
      hac_p_value   = hac[, "Pr(>|t|)"]
    )
}

save_residual_plots <- function(model, name) {
  png(file.path(PATHS$visualisations, paste0("regression_diagnostics_", name, ".png")),
      width = 1400, height = 1100, res = 150)
  op <- par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
  plot(model)
  mtext(MODEL_DESCRIPTIONS[[name]], outer = TRUE, font = 2)
  par(op)
  dev.off()
}

plot_residuals_over_time <- function(models, df) {
  d <- prepare_regression_data(df)
  purrr::imap_dfr(models, function(m, nm) {
    used <- as.integer(names(residuals(m)))
    tibble(Year = d$Year[used], residual = residuals(m), model = MODEL_DESCRIPTIONS[[nm]])
  }) |>
    ggplot(aes(Year, residual)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_line(colour = "#1f4e79") + geom_point(colour = "#1f4e79") +
    facet_wrap(~model, ncol = 1) +
    labs(title = "Regression residuals over time",
         subtitle = "Runs of same-signed residuals indicate autocorrelation",
         x = NULL, y = "Residual (percentage points)") +
    theme_sa()
}

run_regression_analysis <- function(df = load_economic_data()) {
  models <- fit_models(df)

  coefs <- purrr::imap_dfr(models, ~ coefficient_table(.x) |> mutate(model = .y, .before = 1))
  diagnostics <- purrr::imap_dfr(models, ~ model_diagnostics(.x) |>
                                   mutate(model = .y, description = MODEL_DESCRIPTIONS[[.y]], .before = 1))
  vif <- purrr::imap_dfr(models[c("model2", "model3")], ~ tibble(
    model = .y, term = names(car::vif(.x)), vif = unname(car::vif(.x))))

  purrr::iwalk(models, save_residual_plots)
  resid_plot <- plot_residuals_over_time(models, df)
  save_plot(resid_plot, "regression_residuals_over_time.png", height = 8)

  save_table(coefs, "regression_coefficients.csv")
  save_table(diagnostics, "regression_diagnostics.csv")
  save_table(vif, "regression_vif.csv")

  invisible(list(models = models, coefficients = coefs, diagnostics = diagnostics,
                 vif = vif, residual_plot = resid_plot))
}

if (sys.nframe() == 0) run_regression_analysis()
