##############################################################################
# SEEDCLIM CLIMATE PIPELINE — RUNNER
#
# Run this script from the project root to execute the full pipeline and
# produce an HTML report:
#
#   source("run_pipeline.R")          # from an R session
#   Rscript run_pipeline.R            # from the terminal
#
# The Quarto document pipeline_report.qmd does all the work:
#   - imports raw data
#   - cleans and deduplicates
#   - assigns soil/veg logger names
#   - detects outliers
#   - saves output CSVs
#   - renders diagnostic plots inline
#
# Output: pipeline_report.html  (self-contained, opens in browser)
#
# Requirements: quarto CLI must be installed (https://quarto.org)
#               R packages: data.table, lubridate, tidyverse, patchwork, knitr
##############################################################################

quarto::quarto_render("pipeline_report.qmd")
