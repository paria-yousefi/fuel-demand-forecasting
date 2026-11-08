# ============================================================
# forecast_models.R
# Province-level demand forecasting with Linear, ARIMAX and ARDL models
# ------------------------------------------------------------
# Input  : a folder of CSV files (see README)
#            <Target>_main.csv        historical target, one row per province
#            <Var>_hist.csv           historical values of each driver
#            <Var>_forecast.csv       scenario values of each driver
# Output : an Excel workbook with forecasts, coefficients and logs
#
# Main function: run_forecast(input_dir, out_dir, ...)
# Author: Paria Yousefi
# ============================================================
# ============================================================
# ===== 1. Packages =====
# ============================================================
suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
  library(forecast)
  library(openxlsx)
  library(tseries)
  library(zoo)
  library(ARDL)
  library(lmtest)
  library(broom)
  library(dynlm)
})
options(warn = -1)

# ============================================================
# ===== 2. Helper functions =====
# ============================================================

calculate_metrics <- function(actual, predicted) {
  actual <- as.numeric(actual)
  predicted <- as.numeric(predicted)
  
  valid_idx <- complete.cases(actual, predicted) & is.finite(actual) & is.finite(predicted)
  actual <- actual[valid_idx]
  predicted <- predicted[valid_idx]
  
  if (length(actual) < 2) {
    return(list(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA))
  }
  
  rmse <- sqrt(mean((actual - predicted)^2, na.rm = TRUE))
  mae <- mean(abs(actual - predicted), na.rm = TRUE)
  mape <- mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
  ss_res <- sum((actual - predicted)^2, na.rm = TRUE)
  ss_tot <- sum((actual - mean(actual, na.rm = TRUE))^2, na.rm = TRUE)
  r2 <- 1 - ss_res / ss_tot
  
  return(list(RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2))
}

# ---- Input/output folders are passed in by the caller ----


auto_detect_variables <- function(input_dir = "data/sample") {
  
  if(is.null(input_dir) || !dir.exists(input_dir)) {
    cat("⚠️ Input directory does not exist or is NULL:", input_dir, "\n")
    if(dir.exists("www")) {
      input_dir <- "www"
    } else {
      stop("No valid input directory found.")
    }
  }
  
  cat("📁 Looking for input files in:", input_dir, "\n")
  
  all_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)
  
  if(length(all_files) == 0) {
    cat("⚠️ No CSV files found in", input_dir, "\n")
    if(dir.exists("www")) {
      all_files <- list.files("www", pattern = "\\.csv$", full.names = TRUE)
    }
    if(length(all_files) == 0) {
      stop("No CSV files found in the given directories.")
    }
  }
  
  file_names <- basename(all_files)
  
  main_file <- all_files[grepl("_main\\.csv$", file_names)]
  if(length(main_file) == 0) {
    main_candidates <- all_files[grepl("main|Main|total|Total", file_names, ignore.case = TRUE)]
    if(length(main_candidates) > 0) {
      main_file <- main_candidates[1]
      cat("✅ Main file selected:", basename(main_file), "\n")
    } else {
      stop("Main file (*_main.csv) not found.")
    }
  } else {
    main_file <- main_file[1]
    cat("✅ Main file:", basename(main_file), "\n")
  }
  
  main_name <- gsub("_main\\.csv$", "", basename(main_file))
  main_data <- tryCatch({
    read_csv(main_file, show_col_types = FALSE)
  }, error = function(e) {
    cat("❌ Error reading main file:", e$message, "\n")
    stop("Error reading main file.")
  })
  main_data <- standardize_province_names(main_data)
  
  hist_files <- all_files[grepl("_hist\\.csv$", file_names)]
  forecast_files <- all_files[grepl("_forecast\\.csv$", file_names)]
  
  hist_names <- gsub("_hist\\.csv$", "", basename(hist_files))
  forecast_names <- gsub("_forecast\\.csv$", "", basename(forecast_files))
  common_names <- intersect(hist_names, forecast_names)
  
  variables <- list()
  for(var_name in common_names) {
    hist_file <- hist_files[grep(paste0(var_name, "_hist\\.csv$"), basename(hist_files))]
    forecast_file <- forecast_files[grep(paste0(var_name, "_forecast\\.csv$"), basename(forecast_files))]
    
    if(length(hist_file) > 0 && length(forecast_file) > 0) {
      tryCatch({
        variables[[var_name]] <- list(
          name = var_name,
          historical = standardize_province_names(read_csv(hist_file, show_col_types = FALSE)),
          forecast = standardize_province_names(read_csv(forecast_file, show_col_types = FALSE))
        )
        cat("  ✓ Detected variable:", var_name, "\n")
      }, error = function(e) {
        cat("  ❌ Error reading variable:", var_name, "-", e$message, "\n")
      })
    }
  }
  
  return(list(
    main_name = main_name,
    main_data = main_data,
    variables = variables
  ))
}

extract_years <- function(df) {
  year_cols <- names(df)[grepl("^[0-9]+$", names(df))]
  if (length(year_cols) == 0) {
    if ("Year" %in% names(df)) {
      year_cols <- as.character(df$Year)
    } else {
      stop("Cannot find year columns in data")
    }
  }
  return(sort(as.numeric(year_cols)))
}

# ============================================================
# ===== Harmonise province names across data sources =====
# ============================================================
standardize_province_names <- function(df) {
  name_map <- c(
    "East Azarbaijan" = "East Azerbaijan",
    "West Azarbaijan" = "West Azerbaijan",
    "Ardebil" = "Ardabil",
    "Esfahan" = "Isfahan",
    "Chahar Mahall and Bakhtiari" = "Chaharmahal and Bakhtiari",
    "Kordestan" = "Kurdistan",
    "Kohgiluyeh and Buyer Ahmad" = "Kohgiluyeh and Boyer-Ahmad",
    "Hamadan" = "Hamedan",
    "Iran" = "Total"
  )
  
  if ("Province" %in% names(df)) {
    df$Province <- ifelse(df$Province %in% names(name_map), 
                           name_map[df$Province], 
                           df$Province)
  }
  return(df)
}

# ============================================================
# ===== NOTE: DEBUG VERSION - prints diagnostics on mismatch =====
# ============================================================
build_province_data <- function(main_data, var_data_list, province_name, train_years, forecast_years) {
  
  clean_numeric <- function(x) {
    x <- as.character(x)
    # convert Persian / Arabic-Indic digits to ASCII before parsing
    x <- chartr("۰۱۲۳۴۵۶۷۸۹٠١٢٣٤٥٦٧٨٩", "01234567890123456789", x)
    x <- gsub("[^0-9eE\\-\\.\\+]", "", x)
    as.numeric(x)
  }
  
  train_df <- tibble(Year = train_years)
  
  main_vals <- main_data %>% 
    filter(Province == province_name) %>% 
    select(any_of(as.character(train_years))) %>% 
    unlist() %>% 
    clean_numeric()
  train_df[[var_data_list$main_name]] <- main_vals
  
  for (var_name in names(var_data_list$variables)) {
    var_info <- var_data_list$variables[[var_name]]
    hist_vals <- var_info$historical %>% 
      filter(Province == province_name) %>% 
      select(any_of(as.character(train_years))) %>% 
      unlist() %>% 
      clean_numeric()
    
    if (length(hist_vals) != length(train_years)) {
      cat("DEBUG PROBLEM - Variable:", var_name, "| Province:", province_name, "\n")
      cat("   Expected years:", length(train_years), "| Found:", length(hist_vals), "\n")
      cat("   Province exists in", var_name, "file?", province_name %in% var_info$historical$Province, "\n")
    }
    
    train_df[[var_name]] <- hist_vals
  }
  
  test_df <- tibble(Year = forecast_years)
  for (var_name in names(var_data_list$variables)) {
    var_info <- var_data_list$variables[[var_name]]
    forecast_vals <- var_info$forecast %>% 
      filter(Province == province_name) %>% 
      select(any_of(as.character(forecast_years))) %>% 
      unlist() %>% 
      clean_numeric()
    test_df[[var_name]] <- forecast_vals
  }
  
  return(list(train = train_df, test = test_df))
}

clean_numeric <- function(x) {
  x <- as.character(x)
  # convert Persian / Arabic-Indic digits to ASCII before parsing
  x <- chartr("۰۱۲۳۴۵۶۷۸۹٠١٢٣٤٥٦٧٨٩", "01234567890123456789", x)
  x <- gsub("[^0-9eE\\-\\.\\+]", "", x)
  as.numeric(x)
}

fix_length <- function(vals, years_target) {
  yrs <- as.character(years_target)
  vals <- suppressWarnings(as.numeric(vals))
  n <- length(yrs)
  
  if (is.null(vals) || length(vals) == 0) {
    return(rep(NA_real_, n))
  }
  
  if (length(vals) == n) return(vals)
  
  if (length(vals) < n) {
    return(c(vals, rep(NA_real_, n - length(vals))))
  }
  
  vals[seq_len(n)]
}

adf_auto <- function(x, max_lag = 3) {
  x <- as.numeric(x); x <- x[is.finite(x)]; x <- na.omit(x)
  if (length(x) < 10) return(list(pval=NA_real_, reject_p=NA, lag=NA))
  best_pval <- NA; best_lag <- NA
  for (k in 0:max_lag) {
    res <- tryCatch(tseries::adf.test(x, k=k), error=function(e) NULL)
    if (!is.null(res)) {
      pval <- as.numeric(res$p.value)
      if (is.na(best_pval) || pval < best_pval) { best_pval <- pval; best_lag <- k }
    }
  }
  list(pval=best_pval, reject_p=(best_pval<0.05), lag=best_lag)
}

check_stationarity_pval <- function(x, alpha=0.05, max_lag=3) {
  x <- as.numeric(x); x <- x[is.finite(x)]; x <- na.omit(x)
  if (length(x)<10) return(list(order="unknown",
                                level=list(pval=NA, reject_p=NA, lag=NA),
                                diff1=list(pval=NA, reject_p=NA, lag=NA),
                                diff2=list(pval=NA, reject_p=NA, lag=NA)))
  lvl <- adf_auto(x, max_lag)
  d1 <- adf_auto(diff(x), max_lag)
  d2 <- adf_auto(diff(diff(x)), max_lag)
  if (isTRUE(lvl$reject_p)) return(list(order="I(0)", level=lvl, diff1=d1, diff2=d2))
  if (isTRUE(d1$reject_p)) return(list(order="I(1)", level=lvl, diff1=d1, diff2=d2))
  if (isTRUE(d2$reject_p)) return(list(order="I(2)", level=lvl, diff1=d1, diff2=d2))
  list(order="indeterminate", level=lvl, diff1=d1, diff2=d2)
}

get_valid_vars <- function(train_df, all_vars) {
  var_orders <- list()
  var_pvals <- list()
  for (var in all_vars) {
    order_info <- check_stationarity_pval(train_df[[var]])
    var_orders[[var]] <- order_info$order
    var_pvals[[var]] <- order_info$level$pval
  }
  i2_vars <- names(var_orders)[var_orders == "I(2)"]
  valid_vars <- names(var_orders)[var_orders != "I(2)" & var_orders != "unknown"]
  valid_vars <- setdiff(valid_vars, i2_vars)
  return(list(
    valid_vars = valid_vars, 
    removed_vars = i2_vars,
    all_orders = var_orders,
    all_pvals = var_pvals
  ))
}

trend_based_forecast <- function(train_df, test_df, main_var) {
  
  cat("    🔧 Using trend-based forecast as fallback\n")
  
  y_hist <- train_df[[main_var]]
  y_hist <- y_hist[is.finite(y_hist) & y_hist > 0]
  
  if (length(y_hist) == 0) {
    cat("    ⚠️ No valid historical data, returning zeros\n")
    return(rep(0, nrow(test_df)))
  }
  
  if (length(y_hist) >= 3) {
    x <- 1:length(y_hist)
    lm_fit <- lm(y_hist ~ x)
    growth_rate <- coef(lm_fit)[2] / mean(y_hist, na.rm = TRUE)
    growth_rate <- max(min(growth_rate, 0.05), -0.03)
    cat("    📈 Historical growth rate:", round(growth_rate * 100, 2), "%\n")
  } else {
    growth_rate <- 0.02
    cat("    📈 Using default growth rate: 2%\n")
  }
  
  last_val <- tail(y_hist, 1)
  if (length(last_val) == 0 || is.na(last_val) || last_val <= 0) {
    cat("    ⚠️ Invalid last historical value, using mean\n")
    last_val <- mean(y_hist, na.rm = TRUE)
    if (is.na(last_val) || last_val <= 0) {
      return(rep(0, nrow(test_df)))
    }
  }
  
  n_forecast <- nrow(test_df)
  pred_vals <- numeric(n_forecast)
  
  if (n_forecast == 0) {
    return(numeric(0))
  }
  
  pred_vals[1] <- last_val * (1 + growth_rate)
  
  for (j in 2:n_forecast) {
    decay_factor <- 1 - (j-1) / n_forecast * 0.3
    current_growth <- growth_rate * decay_factor
    pred_vals[j] <- pred_vals[j-1] * (1 + current_growth)
  }
  
  pred_vals[pred_vals < 0] <- 0
  
  if (any(pred_vals > last_val * 2.5, na.rm = TRUE)) {
    cat("    ⚠️ Capping excessive growth\n")
    pred_vals[pred_vals > last_val * 2.5] <- last_val * 2.5
  }
  
  cat("    ✅ Trend-based forecast completed. Predictions:", length(pred_vals), "\n")
  return(pred_vals)
}

# ============================================================
# ===== 3. ARDL functions =====
# ============================================================

# Note: inside the ARDL functions the target variable is renamed to the internal
# placeholder "Diesel"; it stands for whatever *_main.csv contains.
# stability_check = TRUE rejects candidate models whose autoregressive terms sum
# to |sum| >= ar_limit, i.e. models that would explode in recursive forecasting.
select_best_ardl_model <- function(train_df, max_lags = 2, max_vars = NULL,
                                   stability_check = TRUE, ar_limit = 0.98) {
  
  var_names <- names(train_df)[!names(train_df) %in% c("Year", "Diesel")]
  
  if (is.null(max_vars)) {
    max_vars <- length(var_names)
  } else {
    max_vars <- min(max_vars, length(var_names))
  }
  
  n_obs <- nrow(train_df)
  max_possible <- floor(n_obs / 5)
  
  if (max_vars > max_possible) {
    cat("    ⚠️ Reducing max_vars from", max_vars, "to", max_possible, 
        "due to limited observations (", n_obs, ")\n")
    max_vars <- max_possible
  }
  
  best_model <- NULL
  best_aic <- Inf
  best_bic <- Inf
  best_lags <- NULL
  best_formula <- NULL
  best_vars <- NULL
  best_r2 <- -Inf
  best_adj_r2 <- -Inf
  
  train_df_num <- train_df %>% dplyr::mutate(across(-Year, ~ suppressWarnings(as.numeric(.))))
  z <- zoo::zoo(train_df_num %>% dplyr::select(-Year), order.by = train_df_num$Year)
  
  if(length(var_names) > 0) {
    for(k in 1:min(length(var_names), max_vars)) {
      combos <- combn(var_names, k, simplify = FALSE)
      
      for(combo_vars in combos) {
        for (lag_y in 1:max_lags) {
          formula_str <- paste0("Diesel ~ L(Diesel,", lag_y, ")")
          
          for (var in combo_vars) {
            formula_str <- paste0(formula_str, " + ", var)
            for (lag_var in 1:max_lags) {
              formula_str <- paste0(formula_str, " + L(", var, ",", lag_var, ")")
            }
          }
          
          f <- stats::as.formula(formula_str)
          fit <- tryCatch(dynlm::dynlm(f, data=z), error=function(e) NULL)
          
          if (!is.null(fit)) {
            if (stability_check) {
              cf <- stats::coef(fit)
              ar_terms <- cf[grepl("^L\\(Diesel,", gsub("\\s+", "", names(cf)))]
              if (length(ar_terms) == 0 || !is.finite(sum(ar_terms)) ||
                  abs(sum(ar_terms)) >= ar_limit) next
            }
            aic_val <- suppressWarnings(tryCatch(stats::AIC(fit), error=function(e) NA_real_))
            bic_val <- suppressWarnings(tryCatch(stats::BIC(fit), error=function(e) NA_real_))
            adj_r2 <- summary(fit)$adj.r.squared
            
            if (!is.na(aic_val) && !is.na(adj_r2)) {
              if (aic_val < best_aic || (abs(aic_val - best_aic) < 0.1 && adj_r2 > best_adj_r2)) {
                best_aic <- aic_val
                best_bic <- bic_val
                best_model <- fit
                best_formula <- formula_str
                best_vars <- combo_vars
                best_r2 <- summary(fit)$r.squared
                best_adj_r2 <- adj_r2
                best_lags <- list(Diesel = lag_y, AIC = aic_val, BIC = bic_val)
                for (var in combo_vars) {
                  best_lags[[var]] <- max_lags
                }
              }
            }
          }
        }
      }
    }
  }
  
  if(is.null(best_model)) {
    formula_str <- paste0("Diesel ~ L(Diesel, 1)")
    f <- stats::as.formula(formula_str)
    fit <- tryCatch(dynlm::dynlm(f, data=z), error=function(e) NULL)
    
    if(!is.null(fit)) {
      aic_val <- suppressWarnings(tryCatch(stats::AIC(fit), error=function(e) NA_real_))
      bic_val <- suppressWarnings(tryCatch(stats::BIC(fit), error=function(e) NA_real_))
      return(list(
        model = fit, 
        lags = list(Diesel = 1, AIC = aic_val, BIC = bic_val),
        formula = formula_str,
        vars = character(0),
        aic = aic_val,
        bic = bic_val,
        r2 = summary(fit)$r.squared,
        adj_r2 = summary(fit)$adj.r.squared
      ))
    }
  }
  
  return(list(
    model = best_model, 
    lags = best_lags,
    formula = best_formula,
    vars = best_vars,
    aic = best_aic,
    bic = best_bic,
    r2 = best_r2,
    adj_r2 = best_adj_r2
  ))
}

ardl_recursive_predict <- function(model, lags, train_df, test_df) {
  if (is.null(model)) return(rep(NA_real_, nrow(train_df) + nrow(test_df)))
  
  train_df <- train_df %>% dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  test_df <- test_df %>% dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  x_vars <- names(train_df)[!names(train_df) %in% c("Year", "Diesel")]
  
  all_x <- rbind(
    train_df %>% dplyr::select(all_of(x_vars)),
    test_df %>% dplyr::select(all_of(x_vars))
  )
  all_x <- as.data.frame(all_x)
  all_x[] <- lapply(all_x, function(col) suppressWarnings(as.numeric(col)))
  
  preds <- as.numeric(train_df$Diesel)
  
  n_hist <- nrow(train_df)
  n_total <- n_hist + nrow(test_df)
  
  coefs_vec <- stats::coef(model)
  coefs <- as.numeric(coefs_vec)
  names(coefs) <- names(coefs_vec)
  names(coefs) <- gsub(",\\s+", ",", names(coefs))
  
  lg <- list(Diesel = lags$Diesel)
  for (var in x_vars) {
    lg[[var]] <- ifelse(is.null(lags[[var]]), 0, lags[[var]])
  }
  
  for (t in (n_hist + 1):n_total) {
    lp <- 0.0
    if ("(Intercept)" %in% names(coefs)) {
      lp <- lp + as.numeric(coefs["(Intercept)"])
    }
    
    for (lag in 1:lg$Diesel) {
      idx <- t - lag
      nm <- paste0("L(Diesel,", lag, ")")
      if (nm %in% names(coefs) && idx >= 1 && idx <= length(preds)) {
        lp <- lp + as.numeric(coefs[nm]) * as.numeric(preds[idx])
      }
    }
    
    for (var in x_vars) {
      if (var %in% names(coefs)) {
        val <- as.numeric(all_x[t, var])
        if (!is.na(val) && is.finite(val)) {
          lp <- lp + as.numeric(coefs[var]) * val
        }
      }
      
      if (!is.null(lg[[var]]) && lg[[var]] > 0) {
        for (lag in 1:lg[[var]]) {
          nm <- paste0("L(", var, ",", lag, ")")
          if (nm %in% names(coefs)) {
            idx <- t - lag
            if (idx >= 1) {
              val <- as.numeric(all_x[idx, var])
              if (!is.na(val) && is.finite(val)) {
                lp <- lp + as.numeric(coefs[nm]) * val
              }
            }
          }
        }
      }
    }
    
    preds <- c(preds, lp)
  }
  
  return(as.numeric(preds))
}

run_ardl_optimized <- function(train_df, test_df, main_var, max_lags = 2, max_vars = NULL,
                               stability_check = TRUE) {
  
  x_vars <- names(train_df)[!names(train_df) %in% c("Year", main_var)]
  var_check <- get_valid_vars(train_df, x_vars)
  
  if (length(var_check$valid_vars) == 0) {
    return(list(
      success = FALSE,
      reason = "No valid variables",
      predictions = NULL,
      used_vars = character(0),
      removed_vars = var_check$removed_vars,
      model = NULL,
      coefficients = NULL,
      formula = NULL,
      full_formula = NULL,
      r2 = NA,
      adj_r2 = NA,
      aic = NA,
      bic = NA
    ))
  }
  
  train_df_ardl <- train_df %>% select(Year, all_of(main_var), all_of(var_check$valid_vars))
  test_df_ardl <- test_df %>% select(all_of(var_check$valid_vars))
  
  names(train_df_ardl)[names(train_df_ardl) == main_var] <- "Diesel"
  
  best_ardl <- select_best_ardl_model(train_df_ardl, max_lags = max_lags, max_vars = max_vars,
                                     stability_check = stability_check)
  
  if (is.null(best_ardl$model)) {
    return(list(
      success = FALSE,
      reason = "No valid ARDL model found",
      predictions = NULL,
      used_vars = var_check$valid_vars,
      removed_vars = var_check$removed_vars,
      model = NULL,
      coefficients = NULL,
      formula = NULL,
      full_formula = NULL,
      r2 = NA,
      adj_r2 = NA,
      aic = NA,
      bic = NA
    ))
  }
  
  model <- best_ardl$model
  coefs <- stats::coef(model)
  
  coef_df <- data.frame(
    Variable = names(coefs),
    Coefficient = as.numeric(coefs),
    stringsAsFactors = FALSE
  )
  
  formula_str <- best_ardl$formula
  
  if(!is.null(best_ardl$vars) && length(best_ardl$vars) > 0) {
    simple_formula <- paste(main_var, "~", paste(best_ardl$vars, collapse = " + "))
  } else {
    simple_formula <- formula_str
  }
  
  model_summary <- summary(model)
  r2 <- model_summary$r.squared
  adj_r2 <- model_summary$adj.r.squared
  aic <- AIC(model)
  bic <- BIC(model)
  
  pred_ardl_future <- ardl_recursive_predict(best_ardl$model, best_ardl$lags, train_df_ardl, test_df_ardl)
  
  n_hist <- nrow(train_df)
  future_predictions <- pred_ardl_future[(n_hist + 1):length(pred_ardl_future)]
  
  last_hist <- tail(train_df[[main_var]][train_df[[main_var]] > 0], 1)
  if (length(last_hist) > 0 && !is.na(last_hist) && last_hist > 0) {
    if (any(future_predictions < 0, na.rm = TRUE)) {
      future_predictions[future_predictions < 0] <- last_hist * 0.3
    }
  }
  
  cat("    ✅ ARDL successful\n")
  
  return(list(
    success = TRUE,
    reason = "ARDL successful",
    predictions = future_predictions,
    used_vars = best_ardl$vars,
    removed_vars = var_check$removed_vars,
    model = model,
    coefficients = coef_df,
    formula = simple_formula,
    full_formula = formula_str,
    r2 = r2,
    adj_r2 = adj_r2,
    aic = aic,
    bic = bic
  ))
}

extract_ardl_coefficients <- function(model, province) {
  if (is.null(model)) return(NULL)
  
  tryCatch({
    coefs <- stats::coef(model)
    if (length(coefs) == 0) return(NULL)
    
    coef_df <- data.frame(
      Province = province,
      Variable = names(coefs),
      Coefficient = as.numeric(coefs),
      stringsAsFactors = FALSE
    )
    
    model_summary <- summary(model)
    r2 <- model_summary$r.squared
    adj_r2 <- model_summary$adj.r.squared
    aic <- AIC(model)
    bic <- BIC(model)
    
    stats_rows <- data.frame(
      Province = province,
      Variable = c("R_Squared", "Adj_R_Squared", "AIC", "BIC"),
      Coefficient = c(r2, adj_r2, aic, bic),
      stringsAsFactors = FALSE
    )
    coef_df <- rbind(coef_df, stats_rows)
    
    return(coef_df)
  }, error = function(e) {
    cat("    Error extracting ARDL coefficients:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================
# ===== 4. ARIMAX functions =====
# ============================================================

extract_arimax_coefficients <- function(model, province) {
  if (is.null(model)) return(NULL)
  
  coefs <- stats::coef(model)
  if (length(coefs) == 0) return(NULL)
  
  coef_df <- data.frame(
    Province = province,
    Variable = names(coefs),
    Coefficient = as.numeric(coefs),
    stringsAsFactors = FALSE
  )
  
  tryCatch({
    aic <- AIC(model)
    bic <- BIC(model)
    
    stats_rows <- data.frame(
      Province = province,
      Variable = c("AIC", "BIC"),
      Coefficient = c(aic, bic),
      stringsAsFactors = FALSE
    )
    coef_df <- rbind(coef_df, stats_rows)
  }, error = function(e) {})
  
  return(coef_df)
}

run_arimax_with_model <- function(train_df, test_df, main_var, arimax_type = "simple") {
  
  if (arimax_type == "standard") {
    max_p <- 2
    max_q <- 2
    max_d <- 2
    cat("    Using STANDARD ARIMAX (max.p=2, max.q=2)\n")
  } else {
    max_p <- 1
    max_q <- 1
    max_d <- 1
    cat("    Using SIMPLE ARIMAX (max.p=1, max.q=1) - faster & more stable\n")
  }
  
  if (nrow(train_df) < 8) {
    cat("    ⚠️ Insufficient data for ARIMAX, using trend-based forecast\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  train_df <- train_df %>% 
    dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  test_df <- test_df %>% 
    dplyr::mutate(across(everything(), ~ suppressWarnings(as.numeric(.))))
  
  y_ts <- ts(train_df[[main_var]], start = min(train_df$Year, na.rm = TRUE), frequency = 1)
  x_vars <- names(train_df)[!names(train_df) %in% c("Year", main_var)]
  
  x_vars <- x_vars[sapply(train_df[, x_vars, drop = FALSE], function(col) sum(!is.na(col)) > 0)]
  
  if (length(x_vars) == 0) {
    cat("    ⚠️ No independent variables found, using trend-based forecast\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  var_check <- get_valid_vars(train_df, x_vars)
  valid_x_vars <- var_check$valid_vars
  
  if (length(valid_x_vars) == 0) {
    cat("    ⚠️ No valid independent variables, using trend-based forecast\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  x_train <- train_df %>% dplyr::select(all_of(valid_x_vars)) %>% as.matrix()
  
  valid_cols <- apply(x_train, 2, function(col) {
    var(col, na.rm = TRUE) > 1e-6 && sum(!is.na(col)) > 5
  })
  
  if (sum(valid_cols) == 0) {
    cat("    ⚠️ No valid columns after variance check\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  x_train <- x_train[, valid_cols, drop = FALSE]
  valid_x_vars <- valid_x_vars[valid_cols]
  
  complete_idx <- complete.cases(y_ts, x_train)
  
  if (sum(complete_idx) < 5) {
    cat("    ⚠️ Insufficient complete cases\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  y_clean <- y_ts[complete_idx]
  x_clean <- x_train[complete_idx, , drop = FALSE]
  
  model <- NULL
  
  tryCatch({
    cat("    Attempting ARIMAX with auto.arima...\n")
    model <- auto.arima(
      y_clean, 
      xreg = x_clean, 
      seasonal = FALSE,
      stepwise = TRUE, 
      approximation = TRUE,
      max.p = max_p, 
      max.q = max_q, 
      max.d = max_d,
      max.P = 0,
      max.Q = 0,
      max.D = 0,
      start.p = 0,
      start.q = 0,
      start.d = 0,
      ic = "aicc"
    )
    cat("    ✅ ARIMAX model built successfully\n")
  }, error = function(e) {
    cat("    ⚠️ ARIMAX with auto.arima failed:", e$message, "\n")
    model <<- NULL
  })
  
  if (is.null(model)) {
    cat("    Attempting simple ARIMA...\n")
    tryCatch({
      model <- auto.arima(
        y_clean, 
        seasonal = FALSE,
        stepwise = TRUE,
        approximation = TRUE,
        max.p = max_p, 
        max.q = max_q, 
        max.d = max_d,
        max.P = 0,
        max.Q = 0,
        max.D = 0,
        ic = "aicc"
      )
      cat("    ✅ Simple ARIMA built successfully\n")
    }, error = function(e) {
      cat("    ⚠️ Simple ARIMA failed:", e$message, "\n")
      model <<- NULL
    })
  }
  
  if (is.null(model)) {
    cat("    ⚠️ All ARIMA attempts failed. Using trend-based forecast.\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = NULL))
  }
  
  pred <- NULL
  
  if (!is.null(model$xreg)) {
    x_test <- tryCatch({
      test_df %>% dplyr::select(all_of(valid_x_vars)) %>% as.matrix()
    }, error = function(e) {
      cat("    ⚠️ Error creating x_test:", e$message, "\n")
      return(NULL)
    })
    
    if (!is.null(x_test) && ncol(x_test) == ncol(x_clean)) {
      tryCatch({
        pred <- forecast(model, xreg = x_test, h = nrow(test_df))$mean
        cat("    ✅ Forecast with xreg successful\n")
      }, error = function(e) {
        cat("    ⚠️ Forecast with xreg failed:", e$message, "\n")
        pred <- NULL
      })
    }
  }
  
  if (is.null(pred) || length(pred) == 0) {
    tryCatch({
      pred <- forecast(model, h = nrow(test_df))$mean
      cat("    ✅ Simple forecast successful\n")
    }, error = function(e) {
      cat("    ⚠️ Simple forecast failed:", e$message, "\n")
      pred <- NULL
    })
  }
  
  if (is.null(pred) || length(pred) == 0 || all(is.na(pred))) {
    cat("    ⚠️ All forecast attempts failed. Using trend-based forecast.\n")
    pred <- trend_based_forecast(train_df, test_df, main_var)
    return(list(predictions = pred, model = model))
  }
  
  pred <- as.numeric(pred)
  
  if (any(is.na(pred)) || any(!is.finite(pred))) {
    cat("    ⚠️ Some predictions are NA/Inf. Using trend-based for invalid values.\n")
    trend_pred <- trend_based_forecast(train_df, test_df, main_var)
    pred[is.na(pred) | !is.finite(pred)] <- trend_pred[is.na(pred) | !is.finite(pred)]
  }
  
  if (any(pred < 0, na.rm = TRUE)) {
    cat("    ⚠️ Negative values detected. Setting to 0.\n")
    pred[pred < 0] <- 0
  }
  
  last_hist <- tail(train_df[[main_var]][is.finite(train_df[[main_var]]) & train_df[[main_var]] > 0], 1)
  if (length(last_hist) > 0 && !is.na(last_hist) && last_hist > 0) {
    if (any(pred > last_hist * 3, na.rm = TRUE)) {
      cat("    ⚠️ Excessive growth detected. Capping at 3x historical.\n")
      pred[pred > last_hist * 3] <- last_hist * 3
    }
    if (any(pred < last_hist * 0.1, na.rm = TRUE)) {
      cat("    ⚠️ Excessive decline detected. Setting to 0.1x historical.\n")
      pred[pred < last_hist * 0.1] <- last_hist * 0.1
    }
  }
  
  cat("    ✅ ARIMAX completed. Predictions:", length(pred), "\n")
  return(list(predictions = pred, model = model))
}

# ============================================================
# ===== 5. Output table helpers =====
# ============================================================

make_variable_table <- function(var_name, var_info, provinces, train_years, forecast_years, is_main = FALSE) {
  
  clean_numeric <- function(x) {
    x <- as.character(x)
    # convert Persian / Arabic-Indic digits to ASCII before parsing
    x <- chartr("۰۱۲۳۴۵۶۷۸۹٠١٢٣٤٥٦٧٨٩", "01234567890123456789", x)
    x <- gsub("[^0-9eE\\-\\.\\+]", "", x)
    as.numeric(x)
  }
  
  all_years <- as.character(c(train_years, forecast_years))
  result <- tibble(Province = character())
  
  for (prov in provinces) {
    if (is_main) {
      hist_vals <- var_info %>% 
        filter(Province == prov) %>% 
        select(any_of(as.character(train_years))) %>% 
        unlist() %>% 
        clean_numeric()
      if (length(hist_vals) > 0) {
        last_val <- tail(hist_vals[is.finite(hist_vals)], 1)
        forecast_vals <- rep(last_val, length(forecast_years))
      } else {
        forecast_vals <- rep(NA_real_, length(forecast_years))
      }
    } else {
      hist_vals <- var_info$historical %>% 
        filter(Province == prov) %>% 
        select(any_of(as.character(train_years))) %>% 
        unlist() %>% 
        clean_numeric()
      
      forecast_vals <- var_info$forecast %>% 
        filter(Province == prov) %>% 
        select(any_of(as.character(forecast_years))) %>% 
        unlist() %>% 
        clean_numeric()
    }
    
    all_vals <- c(hist_vals, forecast_vals)
    all_vals_fixed <- fix_length(all_vals, all_years)
    
    prov_row <- tibble(Province = prov)
    for (i in seq_along(all_years)) {
      prov_row[[all_years[i]]] <- all_vals_fixed[i]
    }
    result <- bind_rows(result, prov_row)
  }
  
  totals <- result %>% 
    select(-Province) %>% 
    summarise(across(everything(), ~ sum(.x, na.rm = TRUE)))
  result <- bind_rows(result, tibble(Province = "Total") %>% bind_cols(totals))
  
  return(result)
}

fix_final_predictions <- function(final_df, arimax_df, train_years, forecast_years) {
  
  cat("\n========== FIXED: fix_final_predictions ==========\n")
  
  hist_years <- as.character(train_years)
  pred_years <- as.character(forecast_years)
  
  final_df <- as.data.frame(final_df)
  arimax_df <- as.data.frame(arimax_df)
  
  if(nrow(final_df) == 0 || nrow(arimax_df) == 0) {
    cat("  ⚠️ final_df or arimax_df is empty! Returning original data.\n")
    return(list(
      final_fixed = final_df, 
      fix_log = data.frame(Province = character(), Model = character(), Reason = character(), stringsAsFactors = FALSE)
    ))
  }
  
  all_year_cols <- names(final_df)[grepl("^[0-9]+$", names(final_df))]
  available_hist_years <- intersect(hist_years, all_year_cols)
  available_pred_years <- intersect(pred_years, all_year_cols)
  
  if (length(available_pred_years) == 0) {
    available_pred_years <- pred_years
    cat("  ⚠️ No pred years in final_df, using forecast_years\n")
  }
  
  cat("  available_hist_years:", length(available_hist_years), "years\n")
  cat("  available_pred_years:", length(available_pred_years), "years\n")
  
  for (year in c(available_hist_years, available_pred_years)) {
    if (!year %in% names(final_df)) {
      final_df[[year]] <- NA_real_
    }
    if (!year %in% names(arimax_df)) {
      arimax_df[[year]] <- NA_real_
    }
  }
  
  all_cols <- c("Province", available_hist_years, available_pred_years)
  final_df <- final_df[, intersect(all_cols, names(final_df)), drop = FALSE]
  arimax_df <- arimax_df[, intersect(all_cols, names(arimax_df)), drop = FALSE]
  
  fixed_final_df <- final_df
  fix_log <- data.frame(Province = character(), Model = character(), Reason = character(), stringsAsFactors = FALSE)
  
  n_rows <- nrow(final_df) - 1
  
  if(n_rows < 1) {
    cat("  ⚠️ No province rows found. Skipping fixes.\n")
    return(list(final_fixed = fixed_final_df, fix_log = fix_log))
  }
  
  for (i in 1:n_rows) {
    prov <- final_df$Province[i]
    
    arimax_row_idx <- which(arimax_df$Province == prov)
    if (length(arimax_row_idx) == 0) {
      cat("  ⚠️ Province", prov, "not found in ARIMAX, skipping\n")
      next
    }
    arimax_row <- arimax_row_idx[1]
    
    hist_vals <- as.numeric(unlist(final_df[i, available_hist_years]))
    final_pred_vals <- as.numeric(unlist(final_df[i, available_pred_years]))
    arimax_pred_vals <- as.numeric(unlist(arimax_df[arimax_row, available_pred_years]))
    
    needs_fix <- FALSE
    fix_reason <- ""
    
    final_valid <- sum(is.finite(final_pred_vals) & final_pred_vals > 0)
    arimax_valid <- sum(is.finite(arimax_pred_vals) & arimax_pred_vals > 0)
    
    if (final_valid < length(available_pred_years) * 0.3 && arimax_valid > 0) {
      needs_fix <- TRUE
      fix_reason <- "Insufficient valid predictions in final_df"
    }
    
    if (!needs_fix && length(final_pred_vals) >= 2) {
      for (j in 2:length(final_pred_vals)) {
        if (is.finite(final_pred_vals[j-1]) && is.finite(final_pred_vals[j]) && 
            final_pred_vals[j-1] > 0 && final_pred_vals[j] > 0) {
          drop <- (final_pred_vals[j-1] - final_pred_vals[j]) / final_pred_vals[j-1]
          if (drop > 0.4) {
            needs_fix <- TRUE
            fix_reason <- paste("Drop of", round(drop*100, 1), "% at year", available_pred_years[j])
            break
          }
        }
      }
    }
    
    if (!needs_fix) {
      last_hist <- tail(hist_vals[is.finite(hist_vals) & hist_vals > 0], 1)
      if (length(last_hist) > 0 && !is.na(last_hist) && last_hist > 0) {
        last_pred <- tail(final_pred_vals[is.finite(final_pred_vals) & final_pred_vals > 0], 1)
        if (length(last_pred) > 0 && !is.na(last_pred) && last_pred < last_hist * 0.5) {
          needs_fix <- TRUE
          fix_reason <- paste("Final < 50% of last historical")
        }
      }
    }
    
    if (needs_fix) {
      cat("  ✏️ Fixing", prov, "-", fix_reason, "\n")
      
      if (arimax_valid > length(available_pred_years) * 0.3) {
        cat("    ✅ Using ARIMAX values\n")
        for (j in seq_along(available_pred_years)) {
          new_val <- arimax_pred_vals[j]
          if (!is.finite(new_val) || new_val < 0) {
            last_hist <- tail(hist_vals[is.finite(hist_vals) & hist_vals > 0], 1)
            if (length(last_hist) > 0 && is.finite(last_hist) && last_hist > 0) {
              new_val <- last_hist * (1 + (j / length(available_pred_years)) * 0.02)
            } else {
              new_val <- 0
            }
          }
          fixed_final_df[i, available_pred_years[j]] <- new_val
        }
        fix_reason <- paste(fix_reason, "(replaced with ARIMAX)")
      } else {
        cat("    ⚠️ ARIMAX not valid, using trend-based\n")
        hist_clean <- hist_vals[is.finite(hist_vals) & hist_vals > 0]
        last_hist <- tail(hist_clean, 1)
        
        if (length(last_hist) > 0 && is.finite(last_hist) && last_hist > 0) {
          if (length(hist_clean) >= 3) {
            x <- 1:length(hist_clean)
            lm_fit <- lm(hist_clean ~ x)
            growth_rate <- coef(lm_fit)[2] / mean(hist_clean, na.rm = TRUE)
            growth_rate <- max(min(growth_rate, 0.05), -0.02)
          } else {
            growth_rate <- 0.02
          }
          
          pred_vals <- numeric(length(available_pred_years))
          pred_vals[1] <- last_hist * (1 + growth_rate)
          for (j in 2:length(available_pred_years)) {
            decay <- 1 - (j-1) / length(available_pred_years) * 0.3
            pred_vals[j] <- pred_vals[j-1] * (1 + growth_rate * decay)
          }
          
          for (j in seq_along(available_pred_years)) {
            fixed_final_df[i, available_pred_years[j]] <- pred_vals[j]
          }
        }
        fix_reason <- paste(fix_reason, "(replaced with trend-based)")
      }
      
      fix_log <- rbind(fix_log, data.frame(
        Province = prov, 
        Model = "Final", 
        Reason = fix_reason,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  cat("\n🔄 Recalculating Total rows...\n")
  total_row <- nrow(fixed_final_df)
  
  if(total_row > 1) {
    for (col in names(fixed_final_df)) {
      if (col != "Province" && grepl("^[0-9]+$", col)) {
        col_values <- as.numeric(unlist(fixed_final_df[1:(total_row-1), col]))
        fixed_final_df[total_row, col] <- sum(col_values[is.finite(col_values)], na.rm = TRUE)
      }
    }
  }
  
  cat("✅ fix_final_predictions completed\n")
  cat("  Fixed", nrow(fix_log), "provinces\n")
  
  return(list(
    final_fixed = fixed_final_df, 
    fix_log = fix_log
  ))
}

# ============================================================
# ===== 6. Main entry point =====
# ============================================================

# ---- Input/output folders are passed in by the caller ----

run_forecast <- function(input_dir = "data/sample", 
                         selected_vars = NULL, 
                         out_dir = "output",
                         run_linear = TRUE,
                         run_arimax = TRUE,
                         arimax_type = "simple",
                         run_ardl = TRUE,
                         stability_check = TRUE) {
  
  cat("\n========================================\n")
  cat("Starting forecast with parameters:\n")
  cat("  Input directory:", input_dir, "\n")
  cat("  Models selected:\n")
  cat("    Linear:", run_linear, "\n")
  cat("    ARIMAX:", run_arimax, "\n")
  cat("    ARDL:", run_ardl, "\n")
  cat("========================================\n\n")
  
  error_env <- new.env()
  error_env$error_log_df <- data.frame(Province = character(), Model = character(), Error = character(), stringsAsFactors = FALSE)
  
  if (is.null(out_dir)) {
    out_dir <- file.path(dirname(input_dir), "Forecasts_All_Models_Complete")
  }
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  cat("Auto-detecting variables from directory:\n")
  cat("  ", input_dir, "\n")
  
  var_data <- auto_detect_variables(input_dir)
  cat("\n✓ Main variable detected:", var_data$main_name, "\n")
  
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    var_data$variables <- var_data$variables[names(var_data$variables) %in% selected_vars]
    cat("✓ Filtered variables:", paste(names(var_data$variables), collapse=", "), "\n")
  } else {
    cat("✓ Independent variables detected:", paste(names(var_data$variables), collapse=", "), "\n")
  }
  
  main_data <- var_data$main_data
  years_all_main <- extract_years(main_data)
  
  if (length(var_data$variables) > 0) {
    first_var <- var_data$variables[[1]]
    hist_years_from_var <- extract_years(first_var$historical)
    forecast_years_from_var <- extract_years(first_var$forecast)
  } else {
    hist_years_from_var <- years_all_main
    forecast_years_from_var <- NULL
  }
  
  common_hist_years <- intersect(years_all_main, hist_years_from_var)
  
  if (length(common_hist_years) > 0) {
    train_years <- common_hist_years
    cat("✓ Using common historical years:", min(train_years), "-", max(train_years), "\n")
  } else {
    train_years <- years_all_main
    cat("⚠️ Using main file historical years only:", min(train_years), "-", max(train_years), "\n")
  }
  
  if (length(forecast_years_from_var) > 0) {
    forecast_years <- forecast_years_from_var
    cat("✓ Using forecast years from variables:", min(forecast_years), "-", max(forecast_years), "\n")
  } else {
    last_hist_year <- max(train_years)
    forecast_years <- (last_hist_year + 1):(last_hist_year + 17)
    cat("⚠️ No forecast years found. Generated:", min(forecast_years), "-", max(forecast_years), "\n")
  }
  
  if (any(forecast_years %in% train_years)) {
    forecast_years <- forecast_years[!forecast_years %in% train_years]
    cat("⚠️ Removed overlapping years from forecast\n")
  }
  
  cat("\n========================================\n")
  cat("📅 Final year ranges:\n")
  cat("  Training years (historical):", min(train_years), "-", max(train_years), "(", length(train_years), "years)\n")
  cat("  Forecast years:", min(forecast_years), "-", max(forecast_years), "(", length(forecast_years), "years)\n")
  cat("========================================\n\n")
  
  provinces <- setdiff(unique(main_data$Province), "Total")
  
  all_models_results <- list()
  
  ardl_removed_vars_df <- data.frame(Province = character(), Removed_Variables = character(), stringsAsFactors = FALSE)
  ardl_used_vars_df <- data.frame(Province = character(), Used_Variables = character(), stringsAsFactors = FALSE)
  
  ardl_replacement_log <- data.frame(Province = character(), Reason = character(), stringsAsFactors = FALSE)
  
  all_ardl_coefficients <- list()
  all_arimax_coefficients <- list()
  all_ardl_models <- list()
  all_arimax_models <- list()
  
  # ======================= Main loop over provinces =======================
  for (prov in provinces) {
    cat("\n========================================\n")
    cat("Processing province:", prov, "\n")
    cat("========================================\n")
    
    province_data <- build_province_data(
      main_data = var_data$main_data,
      var_data_list = var_data,
      province_name = prov,
      train_years = train_years,
      forecast_years = forecast_years
    )
    
    train_df <- province_data$train %>% na.omit()
    test_df <- province_data$test
    
    if (nrow(train_df) < 10) {
      cat("⚠️ Insufficient data for province:", prov, "\n")
      
      ardl_removed_vars_df <- rbind(ardl_removed_vars_df, 
                                    data.frame(Province = prov, 
                                               Removed_Variables = "Insufficient data", 
                                               stringsAsFactors = FALSE))
      ardl_used_vars_df <- rbind(ardl_used_vars_df, 
                                 data.frame(Province = prov, 
                                            Used_Variables = "Insufficient data", 
                                            stringsAsFactors = FALSE))
      next
    }
    
    # ===== Stationarity diagnostics =====
    cat("  📊 Calculating stationarity information for", prov, "...\n")
    
    var_orders <- list()
    var_pvals <- list()
    
    for (var_name in names(train_df)) {
      if (var_name == "Year") next
      
      x <- train_df[[var_name]]
      x <- x[is.finite(x)]
      
      if (length(x) >= 10) {
        order_info <- check_stationarity_pval(x)
        var_orders[[var_name]] <- order_info$order
        var_pvals[[var_name]] <- order_info$level$pval
      } else {
        var_orders[[var_name]] <- "unknown"
        var_pvals[[var_name]] <- NA
      }
    }
    
    cat("    Stationarity orders:\n")
    for (var_name in names(var_orders)) {
      cat("      ", var_name, ":", var_orders[[var_name]], 
          "(p-value:", round(var_pvals[[var_name]], 4), ")\n")
    }
    
    x_vars <- names(var_data$variables)
    x_vars <- x_vars[x_vars %in% names(test_df)]
    
    if (length(x_vars) == 0) {
      cat("⚠️ No matching variables for province:", prov, "\n")
      next
    }
    
    years_all <- as.character(c(train_years, forecast_years))
    
    prov_results <- list()
    prov_results$Province <- prov
    prov_results$train_actual <- train_df[[var_data$main_name]]
    
    # ===== 1. Linear model =====
    if(run_linear) {
      cat("  Running Linear model...\n")
      formula_str <- paste(var_data$main_name, "~", paste(x_vars, collapse = " + "))
      model_linear <- tryCatch(lm(as.formula(formula_str), data = train_df), error = function(e) NULL)
      
      pred_linear <- tryCatch({
        if (!is.null(model_linear)) {
          c(train_df[[var_data$main_name]], predict(model_linear, newdata = test_df))
        } else {
          rep(NA_real_, length(years_all))
        }
      }, error = function(e) rep(NA_real_, length(years_all)))
      
      prov_results$Linear <- fix_length(pred_linear, years_all)
      prov_results$Linear_metrics <- calculate_metrics(train_df[[var_data$main_name]], 
                                                       tail(prov_results$Linear, length(train_years)))
    } else {
      prov_results$Linear <- rep(NA_real_, length(years_all))
      prov_results$Linear_metrics <- list(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA)
    }
    
    # ===== 2. ARIMAX model =====
    arimax_model <- NULL
    if(run_arimax) {
      cat("  Running ARIMAX...\n")
      
      arimax_result <- tryCatch({
        run_arimax_with_model(train_df, test_df, var_data$main_name, arimax_type)
      }, error = function(e) {
        cat("    ARIMAX error:", e$message, "\n")
        list(predictions = rep(NA_real_, nrow(test_df)), model = NULL)
      })
      
      if(!is.null(arimax_result$predictions) && length(arimax_result$predictions) > 0) {
        pred_arimax_future <- arimax_result$predictions
        arimax_model <- arimax_result$model
      } else {
        pred_arimax_future <- rep(NA_real_, nrow(test_df))
        arimax_model <- NULL
      }
      
      pred_arimax <- c(train_df[[var_data$main_name]], pred_arimax_future)
      prov_results$ARIMAX <- fix_length(pred_arimax, years_all)
      prov_results$ARIMAX_metrics <- calculate_metrics(train_df[[var_data$main_name]], 
                                                       tail(prov_results$ARIMAX, length(train_years)))
      
      if(!is.null(arimax_model)) {
        arimax_coefs <- extract_arimax_coefficients(arimax_model, prov)
        if(!is.null(arimax_coefs)) {
          all_arimax_coefficients[[prov]] <- arimax_coefs
          all_arimax_models[[prov]] <- arimax_model
        }
      }
    } else {
      prov_results$ARIMAX <- rep(NA_real_, length(years_all))
      prov_results$ARIMAX_metrics <- list(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA)
    }
    
    # ===== 3. ARDL model =====
    if(run_ardl) {
      cat("  Running ARDL...\n")
      
      ardl_result <- tryCatch({
        run_ardl_optimized(train_df, test_df, var_data$main_name, 
                           max_lags = 2, 
                           max_vars = NULL,
                           stability_check = stability_check)
      }, error = function(e) {
        cat("    ARDL error:", e$message, "\n")
        list(success = FALSE, predictions = rep(NA_real_, nrow(test_df)), 
             used_vars = character(0), removed_vars = character(0),
             model = NULL, coefficients = NULL, formula = NULL,
             full_formula = NULL, r2 = NA, adj_r2 = NA, aic = NA, bic = NA)
      })
      
      if (length(ardl_result$removed_vars) > 0) {
        ardl_removed_vars_df <- rbind(ardl_removed_vars_df, 
                                      data.frame(Province = prov, 
                                                 Removed_Variables = paste(ardl_result$removed_vars, collapse = ", "), 
                                                 stringsAsFactors = FALSE))
      } else {
        ardl_removed_vars_df <- rbind(ardl_removed_vars_df, 
                                      data.frame(Province = prov, 
                                                 Removed_Variables = "No I(2) variables removed", 
                                                 stringsAsFactors = FALSE))
      }
      
      if (length(ardl_result$used_vars) > 0) {
        ardl_used_vars_df <- rbind(ardl_used_vars_df, 
                                   data.frame(Province = prov, 
                                              Used_Variables = paste(ardl_result$used_vars, collapse = ", "), 
                                              stringsAsFactors = FALSE))
      } else {
        ardl_used_vars_df <- rbind(ardl_used_vars_df, 
                                   data.frame(Province = prov, 
                                              Used_Variables = "No valid variables", 
                                              stringsAsFactors = FALSE))
      }
      
      if(ardl_result$success && !is.null(ardl_result$model)) {
        all_ardl_models[[prov]] <- ardl_result$model
        
        ardl_coefs <- extract_ardl_coefficients(ardl_result$model, prov)
        if(!is.null(ardl_coefs)) {
          all_ardl_coefficients[[prov]] <- ardl_coefs
        }
      }
      
      if(ardl_result$success && !is.null(ardl_result$predictions)) {
        pred_ardl <- c(train_df[[var_data$main_name]], ardl_result$predictions)
        prov_results$ARDL <- fix_length(pred_ardl, years_all)
        prov_results$ARDL_metrics <- calculate_metrics(train_df[[var_data$main_name]], 
                                                       tail(prov_results$ARDL, length(train_years)))
      } else {
        prov_results$ARDL <- rep(NA_real_, length(years_all))
        prov_results$ARDL_metrics <- list(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA)
      }
    } else {
      prov_results$ARDL <- rep(NA_real_, length(years_all))
      prov_results$ARDL_metrics <- list(RMSE = NA, MAE = NA, MAPE = NA, R2 = NA)
    }
    
    all_models_results[[prov]] <- prov_results
    cat("✅", prov, "completed\n")
  }
  
  # ======================= Build output tables =======================
  
  make_model_dataframe <- function(results_list, model_name, years_all) {
    df <- tibble(Province = character())
    
    for (prov in names(results_list)) {
      prov_result <- results_list[[prov]]
      if (!is.null(prov_result[[model_name]])) {
        prov_row <- tibble(Province = prov)
        pred_vals <- prov_result[[model_name]]
        for (i in seq_along(years_all)) {
          prov_row[[years_all[i]]] <- if(length(pred_vals) >= i) pred_vals[i] else NA
        }
        df <- bind_rows(df, prov_row)
      }
    }
    
    if (nrow(df) > 0) {
      numeric_cols <- names(df)[names(df) != "Province"]
      totals <- df %>% 
        select(-Province) %>% 
        summarise(across(all_of(numeric_cols), ~ sum(.x, na.rm = TRUE)))
      df <- bind_rows(df, tibble(Province = "Total") %>% bind_cols(totals))
    }
    
    return(df)
  }
  
  years_all <- as.character(c(train_years, forecast_years))
  
  linear_df <- make_model_dataframe(all_models_results, "Linear", years_all)
  arimax_df <- make_model_dataframe(all_models_results, "ARIMAX", years_all)
  ardl_df <- make_model_dataframe(all_models_results, "ARDL", years_all)
  
  # ============================================================
  # ===== Post-check of final forecasts (implausible ARDL paths -> ARIMAX / trend) =====
  # ============================================================
  
  cat("\n========================================\n")
  cat("🔧 Running fix_final_predictions...\n")
  cat("========================================\n")
  
  final_df <- ardl_df
  
  cat("  final_df has", nrow(final_df), "rows and", ncol(final_df), "columns\n")
  cat("  arimax_df has", nrow(arimax_df), "rows and", ncol(arimax_df), "columns\n")
  
  fix_results <- fix_final_predictions(
    final_df = final_df,
    arimax_df = arimax_df,
    train_years = train_years,
    forecast_years = forecast_years
  )
  
  final_fixed_df <- fix_results$final_fixed
  
  cat("✅ fix_final_predictions completed\n")
  cat("========================================\n\n")
  
  linear_df <- linear_df %>% select(Province, all_of(years_all))
  arimax_df <- arimax_df %>% select(Province, all_of(years_all))
  ardl_df <- ardl_df %>% select(Province, all_of(years_all))
  
  # ======================= Excel workbook =======================
  
  wb_final <- openxlsx::createWorkbook()
  
  # ===== Forecast sheets =====
  openxlsx::addWorksheet(wb_final, "Linear_Forecast")
  openxlsx::addWorksheet(wb_final, "ARIMAX_Forecast")
  openxlsx::addWorksheet(wb_final, "ARDL_Forecast")
  openxlsx::addWorksheet(wb_final, "ARDL_Forecast_Final")
  
  # ===== Coefficient sheets =====
  if(length(all_ardl_coefficients) > 0) {
    openxlsx::addWorksheet(wb_final, "ARDL_Coefficients")
  }
  if(length(all_arimax_coefficients) > 0) {
    openxlsx::addWorksheet(wb_final, "ARIMAX_Coefficients")
  }
  
  # ===== Summary sheets =====
  openxlsx::addWorksheet(wb_final, "ARDL_Coef_Summary")
  openxlsx::addWorksheet(wb_final, "ARIMAX_Coef_Summary")
  
  # ===== Log sheets =====
  openxlsx::addWorksheet(wb_final, "ARDL_Replacement_Log")
  openxlsx::addWorksheet(wb_final, "ARDL_Removed_Vars")
  openxlsx::addWorksheet(wb_final, "ARDL_Used_Vars")
  
  # ===== Variable sheets =====
  openxlsx::addWorksheet(wb_final, var_data$main_name)
  for (var_name in names(var_data$variables)) {
    openxlsx::addWorksheet(wb_final, var_name)
  }
  
  # ======================= Write data =======================
  
  openxlsx::writeData(wb_final, "Linear_Forecast", linear_df)
  openxlsx::writeData(wb_final, "ARIMAX_Forecast", arimax_df)
  openxlsx::writeData(wb_final, "ARDL_Forecast", ardl_df)
  
  hist_years <- as.character(train_years)
  pred_years <- as.character(forecast_years)
  
  ardl_final_corrected <- final_fixed_df
  for(col in hist_years) {
    if(col %in% names(ardl_final_corrected) && col %in% names(arimax_df)) {
      ardl_final_corrected[[col]] <- arimax_df[[col]]
    }
  }
  openxlsx::writeData(wb_final, "ARDL_Forecast_Final", ardl_final_corrected)
  
  # ===== Coefficient sheets =====
  if(length(all_ardl_coefficients) > 0) {
    ardl_coef_all <- bind_rows(all_ardl_coefficients)
    openxlsx::writeData(wb_final, "ARDL_Coefficients", ardl_coef_all)
    openxlsx::setColWidths(wb_final, "ARDL_Coefficients", cols = 1, widths = 25)
    openxlsx::setColWidths(wb_final, "ARDL_Coefficients", cols = 2, widths = 40)
    openxlsx::setColWidths(wb_final, "ARDL_Coefficients", cols = 3, widths = 20)
  }
  
  if(length(all_arimax_coefficients) > 0) {
    arimax_coef_all <- bind_rows(all_arimax_coefficients)
    openxlsx::writeData(wb_final, "ARIMAX_Coefficients", arimax_coef_all)
    openxlsx::setColWidths(wb_final, "ARIMAX_Coefficients", cols = 1, widths = 25)
    openxlsx::setColWidths(wb_final, "ARIMAX_Coefficients", cols = 2, widths = 40)
    openxlsx::setColWidths(wb_final, "ARIMAX_Coefficients", cols = 3, widths = 20)
  }
  
  # ===== Summary sheets =====
  if(length(all_ardl_coefficients) > 0) {
    ardl_coef_summary <- bind_rows(all_ardl_coefficients) %>%
      filter(!Variable %in% c("R_Squared", "Adj_R_Squared", "AIC", "BIC")) %>%
      pivot_wider(id_cols = Province, names_from = Variable, values_from = Coefficient)
    openxlsx::writeData(wb_final, "ARDL_Coef_Summary", ardl_coef_summary)
  }
  
  if(length(all_arimax_coefficients) > 0) {
    arimax_coef_summary <- bind_rows(all_arimax_coefficients) %>%
      filter(!Variable %in% c("AIC", "BIC")) %>%
      pivot_wider(id_cols = Province, names_from = Variable, values_from = Coefficient)
    openxlsx::writeData(wb_final, "ARIMAX_Coef_Summary", arimax_coef_summary)
  }
  
  # ===== Log sheets =====
  openxlsx::writeData(wb_final, "ARDL_Replacement_Log", ardl_replacement_log)
  openxlsx::writeData(wb_final, "ARDL_Removed_Vars", ardl_removed_vars_df)
  openxlsx::writeData(wb_final, "ARDL_Used_Vars", ardl_used_vars_df)
  
  # ===== Variable sheets =====
  main_variable_table <- make_variable_table(
    var_name = var_data$main_name,
    var_info = var_data$main_data,
    provinces = provinces,
    train_years = train_years,
    forecast_years = forecast_years,
    is_main = TRUE
  )
  openxlsx::writeData(wb_final, var_data$main_name, main_variable_table)
  
  for (var_name in names(var_data$variables)) {
    var_table <- make_variable_table(
      var_name = var_name,
      var_info = var_data$variables[[var_name]],
      provinces = provinces,
      train_years = train_years,
      forecast_years = forecast_years,
      is_main = FALSE
    )
    openxlsx::writeData(wb_final, var_name, var_table)
  }
  
  # ======================= Save file =======================
  
  output_file <- file.path(out_dir, "Forecasts_All_Models_Complete.xlsx")
  openxlsx::saveWorkbook(wb_final, output_file, overwrite = TRUE)
  
  cat("\n\n========================================\n")
  cat("✅ Processing finished successfully\n")
  cat("========================================\n")
  cat("📁 Output directory:", out_dir, "\n")
  cat("📄 Output file:", output_file, "\n")
  
  if(nrow(error_env$error_log_df) > 0) {
    cat("\n⚠️ Number of errors logged:", nrow(error_env$error_log_df), "\n")
    return(list(
      output_file = output_file,
      errors = error_env$error_log_df
    ))
  } else {
    return(output_file)
  }
}

# Example usage: see run_example.R
