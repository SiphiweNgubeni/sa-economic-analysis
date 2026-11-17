# Renders reports/economic_analysis.Rmd to reports/economic_analysis.html.
# Run from the project root after run_analysis.R:  Rscript render_report.R
# Inside RStudio you can also simply click "Knit".

if (!rmarkdown::pandoc_available()) {
  # Outside RStudio, pandoc is often not on the PATH; use RStudio's bundled copy.
  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/pandoc",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools"
  )
  found <- candidates[dir.exists(candidates)]
  if (length(found)) Sys.setenv(RSTUDIO_PANDOC = found[1])
}
stopifnot("Pandoc not found. Install it or render from RStudio." = rmarkdown::pandoc_available())

rmarkdown::render("reports/economic_analysis.Rmd", output_dir = "reports", quiet = TRUE)
message("Report written to reports/economic_analysis.html")
