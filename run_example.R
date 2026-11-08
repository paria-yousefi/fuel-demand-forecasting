# ============================================================
# run_example.R — run the forecasting pipeline on the synthetic sample data
# Run from the repository root:  Rscript run_example.R
# ============================================================

# Work from the folder that contains this script (so relative paths always resolve)
this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)
if (!is.null(this_dir)) setwd(this_dir)
if (!file.exists("R/forecast_models.R")) {
  stop("R/forecast_models.R not found. Extract the whole zip file to a folder first, ",
       "then run run_example.R from that folder.", call. = FALSE)
}

required <- c("tidyverse", "readr", "forecast", "openxlsx", "tseries",
              "zoo", "ARDL", "lmtest", "broom", "dynlm")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Please install the missing packages first:\n  install.packages(c(",
       paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
}

source("R/forecast_models.R")

result <- run_forecast(
  input_dir       = "data/sample",
  out_dir         = "output",
  run_linear      = TRUE,
  run_arimax      = TRUE,
  run_ardl        = TRUE,
  stability_check = TRUE    # reject explosive ARDL candidates during model selection
)

out_file <- if (is.list(result)) result$output_file else result
cat("\nResults written to:", out_file, "\n")
