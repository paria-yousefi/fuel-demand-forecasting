# ============================================================
# leap_bridge.R  —  two-way link between LEAP and the R forecasting models
# ------------------------------------------------------------
# Cycle for each scenario:
#   1) read future driver assumptions (Key\Historical\...) from LEAP
#   2) write them as *_forecast.csv files (all other inputs are copied unchanged)
#   3) run run_forecast() from forecast_models.R
#   4) write the new results back to LEAP, but only for regions whose LEAP
#      expression holds plain data. Regions with a live ARDL formula are left
#      alone, because LEAP recalculates them from the new assumptions itself.
#
# Requirements: Windows, LEAP installed and open, R package RDCOMClient.
# Years: input files use the Solar Hijri calendar; Gregorian = Solar Hijri + 621.
# Author: Paria Yousefi
# ============================================================

suppressPackageStartupMessages({
  library(RDCOMClient)
  library(readr)
  library(dplyr)
  library(openxlsx)
})

# ------------------------------------------------------------
# 0) Configuration — one cfg per demand category (Car, Taxi, ...)
#    Paths are examples; adapt them to your own folders.
# ------------------------------------------------------------
cfg_gasoline_car <- list(
  model_script  = "R/forecast_models.R",          # model code (functions only)
  input_dir     = "data/sample",                  # *_main, *_hist, *_forecast files
  main_file     = "Gasoline_main.csv",            # relative to input_dir; other *_main files are ignored
  work_dir      = "output/leap_runs/car",         # one sub-folder per scenario run

  leap_branch   = "Demand\\Gasoline\\Passenger\\Car\\Gasoline",
  leap_variable = "Total Energy",

  # R variable name (prefix of *_hist / *_forecast files)  =  LEAP branch
  key_vars = c(
    Car        = "Key\\Historical\\Car",
    income     = "Key\\Historical\\Income",
    Index      = "Key\\Historical\\Index",
    Population = "Key\\Historical\\Population",
    Price      = "Key\\Historical\\Price"
  ),

  # unit conversion: R value = LEAP Value() / scale  (find it with leap_check_units)
  key_scale    = c(Car = 1, income = 1, Index = 1, Population = 1, Price = 1),
  target_scale = 1000,          # e.g. LEAP reports litres, R data are in thousand litres

  scenarios      = c("Ref"),
  forecast_years = 1404:1420,   # Solar Hijri
  result_sheet   = "ARDL_Forecast",   # raw ARDL, i.e. what the live LEAP formulas compute

  # if a region is spelled differently in LEAP:  c("name in R" = "name in LEAP")
  region_map = c(),

  # regions that must receive R numbers even if they hold a formula in LEAP
  force_provinces = character(0)
)

# Further categories reuse the same assumptions with a different branch / target file
cfg_gasoline_taxi <- modifyList(cfg_gasoline_car, list(
  leap_branch  = "Demand\\Gasoline\\Passenger\\Taxi\\Gasoline",
  main_file    = "Gasoline_Taxi_main.csv",
  work_dir     = "output/leap_runs/taxi",
  result_sheet = "ARDL_Forecast_Final"   # post-checked forecasts
))

# One rail model split into passenger (40%) and freight (60%);
# regions without rail get zero instead of NA.
cfg_gasoil_prail <- modifyList(cfg_gasoline_car, list(
  leap_branch = "Demand\\GasOil\\Passenger\\Rail\\Diesel",
  main_file = "GASOIL_rail_main.csv", work_dir = "output/leap_runs/rail",
  result_sheet = "ARDL_Forecast_Final", share = 0.4, na_as_zero = TRUE))
cfg_gasoil_frail <- modifyList(cfg_gasoil_prail, list(
  leap_branch = "Demand\\GasOil\\Load\\Rail\\Diesel", share = 0.6))

# Categories updated together, e.g.:  for (cf in cfg_bridge) run_bridge(leap, cf, dry_run = FALSE)
cfg_bridge <- list(car = cfg_gasoline_car)

# ------------------------------------------------------------
# Basic helpers
# ------------------------------------------------------------
s2g <- function(s) s + 621   # Solar Hijri -> Gregorian year

source_model_functions <- function(path) {
  # load all model functions without triggering a direct model run
  exprs <- parse(path, encoding = "UTF-8")
  for (e in exprs) {
    first <- deparse(e)[1]
    if (grepl("run_forecast\\(\\)|is\\.list\\(result", first)) next
    eval(e, envir = globalenv())
  }
  cat("✓ Model functions loaded\n")
}

leap_connect <- function() {
  leap <- COMCreate("LEAP.LEAPApplication")
  cat("✓ Connected to LEAP:", leap[["ActiveArea"]][["Name"]], "\n")
  leap
}

com_names <- function(coll) {
  n <- as.integer(coll[["Count"]])
  if (is.na(n) || n < 1L) return(character(0))
  vapply(seq_len(n), function(i) {
    item <- RDCOMClient::.COM(coll, "Item", i)
    as.character(item[["Name"]])
  }, character(1))
}

to_region <- function(prov, cfg) {
  if (prov %in% names(cfg$region_map)) cfg$region_map[[prov]] else prov
}

read_main <- function(cfg) {
  f <- if (!is.null(cfg$main_file)) file.path(cfg$input_dir, cfg$main_file) else
         list.files(cfg$input_dir, pattern = "_main\\.csv$", full.names = TRUE)[1]
  if (is.na(f) || !file.exists(f)) stop("Main file not found: ", f)
  standardize_province_names(read_csv(f, show_col_types = FALSE))
}

get_provinces <- function(cfg) {
  p <- unique(read_main(cfg)$Province)
  p[!is.na(p) & nzchar(trimws(p)) & p != "Total"]   # drop note rows without a region name
}

leap_value <- function(leap, branch, variable, gyear) {
  # errors are deliberately not hidden, so the real COM failure is visible
  br <- RDCOMClient::.COM(leap, "Branch", branch)
  v  <- RDCOMClient::.COM(br, "Variable", variable)
  z  <- RDCOMClient::.COM(v, "Value", as.numeric(gyear))
  as.numeric(z)[1]
}

classify_expr <- function(e) {
  if (is.null(e) || !nzchar(e)) return("empty")        # inherits from the parent scenario
  if (grepl("PrevYearValue|Key\\\\", e)) return("formula")
  "data"
}

# ------------------------------------------------------------
# 1) Setup check: region names and branches
# ------------------------------------------------------------
leap_check_setup <- function(leap, cfg) {
  regions <- com_names(leap[["Regions"]])
  provs   <- get_provinces(cfg)
  miss    <- provs[!sapply(provs, to_region, cfg = cfg) %in% regions]
  if (length(miss)) {
    cat("❌ These regions were not found in LEAP (fill region_map):\n")
    print(miss)
  } else cat("✓ All", length(provs), "regions found in LEAP\n")

  for (b in c(cfg$leap_branch, cfg$key_vars)) {
    ok <- !inherits(tryCatch(RDCOMClient::.COM(leap, "Branch", b), error = function(e) e), "error")
    cat(if (ok) "✓" else "❌", b, "\n")
  }

  hist_vars <- sub("_hist\\.csv$", "", list.files(cfg$input_dir, pattern = "_hist\\.csv$"))
  unlinked  <- setdiff(hist_vars, names(cfg$key_vars))
  if (length(unlinked))
    cat("⚠️ These variables are not linked to LEAP; their forecasts come from the existing CSV:",
        paste(unlinked, collapse = ", "), "\n")
  invisible(miss)
}

# ------------------------------------------------------------
# 2) Unit check: compare the base year in LEAP with the R data
#    ratio ≈ 1 → scale = 1 ;  ratio ≈ 1000 → scale = 1000
# ------------------------------------------------------------
leap_check_units <- function(leap, cfg, check_year_s = 1403) {
  gy  <- s2g(check_year_s)
  col <- as.character(check_year_s)
  leap[["ActiveScenario"]] <- "Current Accounts"

  tabs <- list(TARGET = read_main(cfg))
  for (v in names(cfg$key_vars))
    tabs[[v]] <- standardize_province_names(
      read_csv(file.path(cfg$input_dir, paste0(v, "_hist.csv")), show_col_types = FALSE))

  out <- list()
  for (p in get_provinces(cfg)) {
    leap[["ActiveRegion"]] <- to_region(p, cfg)
    for (v in names(tabs)) {
      br  <- if (v == "TARGET") cfg$leap_branch else cfg$key_vars[[v]]
      var <- if (v == "TARGET") cfg$leap_variable else "Activity Level"
      r_val <- clean_numeric(tabs[[v]][tabs[[v]]$Province == p, col][[1]])[1]
      l_val <- leap_value(leap, br, var, gy)
      out[[length(out) + 1]] <- data.frame(Variable = v, Province = p,
                                           LEAP = l_val, R = r_val, Ratio = l_val / r_val)
    }
  }
  out <- bind_rows(out)
  cat("\n=== LEAP / R ratio in year", check_year_s, "===\n")
  print(out %>% group_by(Variable) %>%
          summarise(median = median(Ratio, na.rm = TRUE),
                    min = min(Ratio, na.rm = TRUE), max = max(Ratio, na.rm = TRUE),
                    n_NA = sum(is.na(Ratio))))
  invisible(out)
}

# ------------------------------------------------------------
# 3) Build R inputs from the LEAP assumptions of one scenario
# ------------------------------------------------------------
leap_export_forecasts <- function(leap, cfg, scenario) {
  run_dir <- file.path(cfg$work_dir, gsub("[^A-Za-z0-9_]", "_", scenario))
  in_dir  <- file.path(run_dir, "input")
  dir.create(in_dir, recursive = TRUE, showWarnings = FALSE)

  # copy all inputs except other categories' *_main files; only linked forecasts are overwritten
  unlink(list.files(in_dir, pattern = "_main\\.csv$", full.names = TRUE))
  src <- list.files(cfg$input_dir, pattern = "\\.csv$", full.names = TRUE)
  src <- src[!grepl("_main\\.csv$", basename(src))]
  mf  <- if (!is.null(cfg$main_file)) file.path(cfg$input_dir, cfg$main_file) else
           list.files(cfg$input_dir, pattern = "_main\\.csv$", full.names = TRUE)[1]
  file.copy(c(src, mf), in_dir, overwrite = TRUE)

  leap[["ActiveScenario"]] <- scenario
  provs <- get_provinces(cfg)
  yrs   <- cfg$forecast_years
  tabs  <- lapply(names(cfg$key_vars), function(v)
    matrix(NA_real_, length(provs), length(yrs), dimnames = list(provs, yrs)))
  names(tabs) <- names(cfg$key_vars)

  for (p in provs) {
    leap[["ActiveRegion"]] <- to_region(p, cfg)
    for (v in names(cfg$key_vars)) {
      tabs[[v]][p, ] <- sapply(yrs, function(y)
        leap_value(leap, cfg$key_vars[[v]], "Activity Level", s2g(y))) / cfg$key_scale[[v]]
    }
  }

  for (v in names(tabs)) {
    df <- data.frame(Province = provs, tabs[[v]], check.names = FALSE)
    if (anyNA(tabs[[v]])) cat("⚠️", v, ": some values could not be read from LEAP (NA)\n")
    write_csv(df, file.path(in_dir, paste0(v, "_forecast.csv")))
  }
  cat("✓ Assumptions of scenario", scenario, "read from LEAP →", in_dir, "\n")
  run_dir
}

# ------------------------------------------------------------
# 4) Write results back to LEAP (data-type regions only)
# ------------------------------------------------------------
leap_write_back <- function(leap, cfg, scenario, result_xlsx, dry_run = TRUE) {
  if (!dry_run) guard_protected(cfg$leap_branch, scenario, "leap_write_back / run_bridge")
  res <- read.xlsx(result_xlsx, sheet = cfg$result_sheet, check.names = FALSE)
  res <- res[res$Province != "Total", ]
  yrs <- cfg$forecast_years
  leap[["ActiveScenario"]] <- scenario

  log <- list()
  for (p in res$Province) {
    leap[["ActiveRegion"]] <- to_region(p, cfg)
    br_obj <- RDCOMClient::.COM(leap, "Branch", cfg$leap_branch)
    v_obj  <- RDCOMClient::.COM(br_obj, "Variable", cfg$leap_variable)
    old    <- v_obj[["Expression"]]; if (is.null(old)) old <- ""
    kind  <- classify_expr(old)

    vals <- as.numeric(unlist(res[res$Province == p, as.character(yrs)])) *
            (if (is.null(cfg$share)) 1 else cfg$share) *
            (if (is.null(cfg$value_scale)) 1 else cfg$value_scale)
    if (isTRUE(cfg$na_as_zero)) vals[!is.finite(vals)] <- 0
    new  <- paste0("Interp(", paste(s2g(yrs), sprintf("%.10g", vals),
                                    sep = ",", collapse = ","), ")")

    update <- (kind == "data" || p %in% cfg$force_provinces) && all(is.finite(vals))
    action <- if (!update) {
      if (kind == "formula") "skip: live formula"
      else if (kind == "empty") "skip: inherits (check!)"
      else "skip: invalid R values"
    } else if (dry_run) "WOULD UPDATE" else "UPDATED"

    if (update && !dry_run) v_obj[["Expression"]] <- new
    log[[p]] <- data.frame(Scenario = scenario, Province = p, Type = kind,
                           Action = action, Old = old, New = if (update) new else "")
  }
  log <- bind_rows(log)
  f <- file.path(dirname(dirname(result_xlsx)),
                 paste0("writeback_log_", gsub("[^A-Za-z0-9]+", "_", sub(".*Demand\\\\", "", cfg$leap_branch)),
                        "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
  write_csv(log, f)   # column Old = backup of the previous expressions
  cat("\n=== Scenario", scenario, "===\n")
  print(table(log$Action))
  cat("Log and backup:", f, "\n")
  invisible(log)
}

# ------------------------------------------------------------
# Write protection for branch/scenario pairs that must not be overwritten
# (e.g. calibrated categories). Set ALLOW_OVERWRITE_PROTECTED <- TRUE to override.
# ------------------------------------------------------------
PROTECTED_TARGETS <- data.frame(Branch = character(), Scenario = character(),
                                Label = character(), stringsAsFactors = FALSE)
if (!exists("ALLOW_OVERWRITE_PROTECTED")) ALLOW_OVERWRITE_PROTECTED <- FALSE

protected_label <- function(branch, scenario) {
  i <- which(tolower(PROTECTED_TARGETS$Branch) == tolower(branch) & PROTECTED_TARGETS$Scenario == scenario)
  if (length(i)) PROTECTED_TARGETS$Label[i[1]] else NA_character_
}
guard_protected <- function(branch, scenario, who = "") {
  nm <- protected_label(branch, scenario)
  if (!is.na(nm) && !isTRUE(ALLOW_OVERWRITE_PROTECTED))
    stop("🔒 ", branch, " [", scenario, "] is protected (", nm, "). ", who,
         " is not allowed to write here.", call. = FALSE)
  invisible(TRUE)
}

# ------------------------------------------------------------
# 5) Full cycle
# ------------------------------------------------------------
run_bridge <- function(leap, cfg, scenarios = cfg$scenarios, dry_run = TRUE) {
  for (sc in scenarios) {
    run_dir <- leap_export_forecasts(leap, cfg, sc)
    out <- run_forecast(input_dir = file.path(run_dir, "input"),
                        out_dir   = file.path(run_dir, "output"))
    xlsx <- if (is.list(out)) out$output_file else out
    leap_write_back(leap, cfg, sc, xlsx, dry_run = dry_run)
  }
  if (!dry_run) cat("\n✅ Done. Save the project in LEAP (File > Save).\n")
}

# ============================================================
# Usage (run line by line, with LEAP open)
# ============================================================
# cfg  <- cfg_gasoline_car
# source_model_functions(cfg$model_script)
# leap <- leap_connect()
# leap_check_setup(leap, cfg)            # names and branches
# u <- leap_check_units(leap, cfg)       # then set key_scale from the ratios
# run_bridge(leap, cfg, dry_run = TRUE)  # preview only, nothing is written
# run_bridge(leap, cfg, dry_run = FALSE) # write to LEAP
