# =============================================================================
# dashboard/app.R
# South African Economic & Inflation Analysis dashboard.
#
# Run from RStudio (open this file and click "Run App") or from the project root:
#   shiny::runApp("dashboard")
#
# Requires the processed data and results from run_analysis.R.
# =============================================================================

library(shiny)
library(bslib)
library(DT)
library(plotly)

.root <- getwd()
while (!length(list.files(.root, "\\.Rproj$")) && dirname(.root) != .root) .root <- dirname(.root)
source(file.path(.root, "R", "utils.R"))
for (f in c("exploratory_analysis", "descriptive_statistics", "correlation_analysis",
            "regression_analysis", "time_series_analysis", "model_evaluation")) {
  source(file.path(.root, "R", paste0(f, ".R")))
}

results_file <- file.path(PATHS$outputs, "analysis_results.rds")
if (!file.exists(results_file)) {
  stop("outputs/analysis_results.rds not found. Run `Rscript run_analysis.R` from the project root first.")
}
econ    <- load_economic_data()
results <- readRDS(results_file)
YEARS   <- range(econ$Year)

indicator_choices <- setNames(names(INDICATOR_LABELS), INDICATOR_LABELS)
core_choices      <- indicator_choices[indicator_choices %in% CORE_INDICATORS]

fmt_num <- function(x, d = 2) formatC(x, format = "f", digits = d, big.mark = ",")

dt_table <- function(df, digits = 3, ...) {
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  datatable(df, rownames = FALSE, options = list(dom = "tip", pageLength = 10, scrollX = TRUE), ...) |>
    formatRound(setdiff(num, c("Year", "n", "n_1", "n_2")), digits)
}

latest_box <- function(var, theme) {
  d <- econ |> filter(!is.na(.data[[var]]))
  last <- tail(d, 2)
  change <- diff(last[[var]])
  value_box(
    title = INDICATOR_LABELS[[var]],
    value = paste0(fmt_num(last[[var]][2]), "%"),
    p(sprintf("Latest observation: %d", last$Year[2])),
    p(sprintf("%s%.2f pp vs %d", if (change >= 0) "+" else "", change, last$Year[1])),
    theme = theme
  )
}

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "South African Economic & Inflation Analysis",
  theme = bs_theme(bootswatch = "flatly"),

  nav_panel(
    "Economic Overview",
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      latest_box("Inflation", "primary"),
      latest_box("GDP_Growth", "success"),
      latest_box("Unemployment", "danger"),
      latest_box("Interest_Rate", "warning")
    ),
    card(card_header("Core indicators, 2000-2024"),
         plotOutput("overview_plot", height = "480px")),
    card(card_header("Data sources"), DTOutput("sources_table"))
  ),

  nav_panel(
    "Economic Trends",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("trend_var", "Indicator", core_choices),
        sliderInput("trend_years", "Years", min = YEARS[1], max = YEARS[2],
                    value = YEARS, step = 1, sep = ""),
        checkboxInput("trend_events", "Show economic events", TRUE),
        checkboxInput("trend_ma", "Show 3-year moving average", FALSE)
      ),
      card(card_header("Trend"), plotlyOutput("trend_plot", height = "420px")),
      card(card_header("Summary statistics for the selected period"), DTOutput("trend_stats"))
    )
  ),

  nav_panel(
    "Economic Relationships",
    navset_card_tab(
      nav_panel(
        "Correlation matrix",
        layout_sidebar(
          sidebar = sidebar(
            radioButtons("cor_method", "Method", c(Pearson = "pearson", Spearman = "spearman")),
            radioButtons("cor_form", "Data", c("Levels", "First differences"))
          ),
          plotOutput("cor_plot", height = "520px"),
          p(class = "text-muted",
            "Correlations in levels can be inflated by shared trends. First differences ",
            "compare year-on-year changes instead. Correlation does not imply causation.")
        )
      ),
      nav_panel(
        "Scatterplot",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("sc_x", "X axis", indicator_choices, selected = "Interest_Rate"),
            selectInput("sc_y", "Y axis", indicator_choices, selected = "Inflation"),
            sliderInput("sc_years", "Years", min = YEARS[1], max = YEARS[2],
                        value = YEARS, step = 1, sep = "")
          ),
          plotOutput("scatter_plot", height = "480px")
        )
      ),
      nav_panel(
        "Regression results",
        layout_sidebar(
          sidebar = sidebar(
            selectInput("reg_model", "Model", setNames(names(MODEL_DESCRIPTIONS), MODEL_DESCRIPTIONS))
          ),
          h5("Coefficients (OLS and Newey-West HAC standard errors)"),
          DTOutput("reg_coef"),
          h5("Fit and assumption checks"),
          DTOutput("reg_diag"),
          p(class = "text-muted",
            "Durbin-Watson well below 2 and Breusch-Godfrey p < 0.05 indicate autocorrelated ",
            "residuals. The levels models (1 and 2) show this, so their ordinary standard errors ",
            "are unreliable. A positive repo-rate coefficient is consistent with the SARB raising ",
            "rates in response to inflation (reverse causality), not with higher rates causing inflation.")
        )
      ),
      nav_panel(
        "Hypothesis tests",
        DTOutput("hyp_table"),
        p(class = "text-muted",
          sprintf("Significance level: %.0f%%. Welch t-test with Wilcoxon rank-sum check; effect size Hedges' g. ",
                  CONFIG$alpha * 100),
          "Annual observations are autocorrelated, so p-values are indicative.")
      )
    )
  ),

  nav_panel(
    "Inflation Forecasting",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("fc_h", "Forecast horizon (years)", min = 1, max = 5, value = 3),
        checkboxGroupInput("fc_methods", "Test-period methods",
                           c("ARIMA", "Naive", "Mean", "SES"),
                           selected = c("ARIMA", "Naive", "Mean"))
      ),
      card(card_header(textOutput("fc_title")), plotOutput("fc_plot", height = "380px"),
           DTOutput("fc_table")),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Test-period forecasts (models fitted on 2000-2018)"),
             plotOutput("fc_eval_plot", height = "320px")),
        card(card_header("Forecast accuracy, 2019-2024"), DTOutput("fc_accuracy"),
             p(class = "text-muted",
               "Lower MAE/RMSE is better. With 6 test years, differences between methods are not ",
               "statistically meaningful; treat the forecast as indicative."))
      )
    )
  ),

  nav_panel(
    "Data Explorer",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput("dx_vars", "Indicators", indicator_choices, selected = CORE_INDICATORS),
        sliderInput("dx_years", "Years", min = YEARS[1], max = YEARS[2],
                    value = YEARS, step = 1, sep = ""),
        downloadButton("dx_download", "Download CSV")
      ),
      card(card_header("Selected data"), DTOutput("dx_table")),
      card(card_header("Selected indicators over time"), plotOutput("dx_plot", height = "420px"))
    )
  )
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  # Overview
  output$overview_plot <- renderPlot(plot_all_trends(econ))
  output$sources_table <- renderDT(
    results$register |>
      select(variable, source, indicator_code, indicator_name, frequency, units, source_last_updated, url) |>
      datatable(rownames = FALSE, options = list(dom = "t", scrollX = TRUE))
  )

  # Trends
  trend_data <- reactive(econ |> filter(between(Year, input$trend_years[1], input$trend_years[2])))
  output$trend_plot <- renderPlotly({
    d <- trend_data()
    p <- plot_indicator_trend(d, input$trend_var, show_events = input$trend_events)
    if (input$trend_ma) {
      ma <- add_moving_averages(d, vars = input$trend_var)
      p <- p + geom_line(data = ma, aes(y = .data[[paste0(input$trend_var, "_MA3")]]),
                         colour = "#c0392b", linetype = "dashed", na.rm = TRUE)
    }
    ggplotly(p + labs(caption = NULL))
  })
  output$trend_stats <- renderDT(
    describe_indicators(trend_data(), vars = input$trend_var) |> select(-label) |> dt_table()
  )

  # Correlation
  output$cor_plot <- renderPlot({
    d <- if (input$cor_form == "Levels") econ else difference_data(econ)
    m <- correlation_matrix(d, method = input$cor_method)
    colnames(m) <- rownames(m) <- gsub("_", " ", colnames(m))
    corrplot::corrplot(m, method = "color", type = "upper", addCoef.col = "black",
                       tl.col = "black", tl.srt = 45, diag = FALSE, number.cex = 0.9)
  })

  output$scatter_plot <- renderPlot({
    req(input$sc_x != input$sc_y)
    d <- econ |> filter(between(Year, input$sc_years[1], input$sc_years[2]))
    validate(need(sum(complete.cases(d[c(input$sc_x, input$sc_y)])) >= 4,
                  "Select a wider year range (at least 4 complete years needed)."))
    plot_relationship(d, input$sc_x, input$sc_y)
  })

  # Regression
  output$reg_coef <- renderDT(
    results$regression$coefficients |> filter(model == input$reg_model) |>
      select(term, estimate, std.error, p.value, conf.low, conf.high, hac_std_error, hac_p_value) |>
      dt_table()
  )
  output$reg_diag <- renderDT({
    results$regression$diagnostics |> filter(model == input$reg_model) |>
      select(-model, -description) |>
      pivot_longer(everything(), names_to = "statistic", values_to = "value") |>
      dt_table()
  })

  # Hypothesis tests
  output$hyp_table <- renderDT(
    results$hypothesis$results |>
      select(test, H0, n_1, n_2, mean_1, mean_2, difference, ci_low, ci_high,
             p_value, wilcoxon_p, effect_size, effect_size_type, effect_magnitude, decision) |>
      dt_table()
  )

  # Forecasting
  final_fc <- reactive(final_forecast(econ, h = input$fc_h))
  output$fc_title <- renderText(sprintf("Inflation forecast, %s fitted on %d-%d",
                                        as.character(final_fc()$model), YEARS[1], YEARS[2]))
  output$fc_plot  <- renderPlot(plot_final_forecast(final_fc(), econ))
  output$fc_table <- renderDT(final_fc()$table |> select(-model) |> dt_table())

  output$fc_eval_plot <- renderPlot({
    preds <- results$evaluation$test_predictions |> filter(method %in% input$fc_methods)
    hist <- econ |> select(Year, Inflation)
    ggplot() +
      geom_line(data = hist, aes(Year, Inflation), linewidth = 0.9) +
      geom_point(data = hist, aes(Year, Inflation)) +
      geom_line(data = preds, aes(Year, forecast, colour = method), linewidth = 1) +
      geom_vline(xintercept = CONFIG$test_start_year - 0.5, linetype = "dashed", colour = "grey50") +
      labs(x = NULL, y = "Inflation (%)", colour = NULL) + theme_sa()
  })
  output$fc_accuracy <- renderDT(
    results$evaluation$accuracy |> filter(method %in% input$fc_methods) |>
      select(evaluation, method, MAE, RMSE, MAPE) |> dt_table()
  )

  # Data explorer
  dx_data <- reactive({
    req(input$dx_vars)
    econ |> filter(between(Year, input$dx_years[1], input$dx_years[2])) |>
      select(Year, all_of(input$dx_vars))
  })
  output$dx_table <- renderDT(dt_table(dx_data()))
  output$dx_plot <- renderPlot({
    dx_data() |>
      pivot_longer(-Year, names_to = "Indicator", values_to = "Value") |>
      filter(!is.na(Value)) |>
      mutate(Indicator = INDICATOR_LABELS[Indicator]) |>
      ggplot(aes(Year, Value)) + geom_line(colour = "#1f4e79") + geom_point(colour = "#1f4e79") +
      facet_wrap(~Indicator, scales = "free_y") + labs(x = NULL, y = NULL) + theme_sa()
  })
  output$dx_download <- downloadHandler(
    filename = function() sprintf("sa_economic_%d_%d.csv", input$dx_years[1], input$dx_years[2]),
    content  = function(file) readr::write_csv(dx_data(), file)
  )
}

shinyApp(ui, server)
