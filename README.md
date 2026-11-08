# Provincial Fuel Demand Forecasting with ARDL, ARIMAX and LEAP

An R pipeline that forecasts transport fuel demand separately for each province, compares three model families (Linear, ARIMAX, ARDL), and links the results to the [LEAP](https://leap.sei.org/) energy planning model in both directions.

It was built for long-term demand projections of gasoline, diesel and CNG across the 31 provinces of Iran and several vehicle categories. **The data in this repository are synthetic**: six fictional regions with invented numbers that only mimic the structure of the real inputs, so the code can be run end to end.

## What the pipeline does

```
CSV inputs ──► clean & harmonise ──► stationarity tests ──► Linear / ARIMAX / ARDL per province
                                                                        │
     LEAP scenario assumptions ◄──── write back (data regions only) ◄── post-check of forecasts ──► Excel output
              │                                                                ▲
              └──────────── read drivers per scenario ──── rerun models ───────┘
```

1. **Input detection and cleaning** (`auto_detect_variables`, `build_province_data`). The target and its drivers are found from file names, province names are harmonised across sources, and Persian or Arabic-Indic digits are converted before parsing.
2. **Stationarity diagnostics.** Augmented Dickey–Fuller tests on levels, first and second differences. Drivers that are I(2) are dropped from the ARDL candidates.
3. **Three models per province.**
   - *Linear*: OLS on all drivers.
   - *ARIMAX*: `forecast::auto.arima` with drivers as external regressors, with fallbacks to a plain ARIMA or a damped trend.
   - *ARDL*: every combination of drivers (limited by sample size) and lags up to 2 is estimated with `dynlm`, and the specification with the lowest AIC is kept. With `stability_check = TRUE`, candidates whose autoregressive terms sum to 0.98 or more in absolute value are rejected, because they explode in recursive forecasting even when their in-sample fit is good.
4. **Recursive forecasting** of the ARDL model over the scenario years.
5. **Post-check** (`fix_final_predictions`). Province paths with too few valid values, a drop of more than 40% in one year, or a final value below half of the last observed value are replaced by the ARIMAX forecast, or by a damped trend if ARIMAX is also invalid. Every replacement is logged.
6. **Excel output** with forecasts from each model, coefficients, fit statistics and logs.

### LEAP bridge (`R/leap_bridge.R`)

LEAP holds the scenario assumptions (vehicle stock, income, prices, population) and uses the estimated ARDL coefficients as live formulas. The bridge connects R to LEAP through its COM interface:

- reads the driver assumptions of each scenario from LEAP and writes them as `*_forecast.csv`;
- reruns the models with those assumptions;
- writes results back only for regions whose LEAP expression holds plain data, and leaves regions with live formulas untouched;
- checks region names, branch paths and units (base-year ratio of LEAP to R) before any write;
- runs as a dry run by default, logs every change and keeps the previous expression as a backup;
- can protect selected branch/scenario pairs from being overwritten.

This removes manual re-entry when assumptions change: edit the scenario in LEAP, rerun the bridge, and all dependent results update.

## Repository structure

```
├── R/
│   ├── forecast_models.R    # data handling, tests, models, post-check, Excel output
│   └── leap_bridge.R        # two-way link with LEAP (Windows + RDCOMClient)
├── data/sample/             # synthetic example inputs
├── scripts/
│   └── make_synthetic_data.py
├── run_example.R            # runs the full pipeline on the sample data
└── output/                  # results are written here
```

## Input format

One CSV per series, one row per region, one column per year (Solar Hijri calendar; Gregorian = Solar Hijri + 621):

| File | Content |
| --- | --- |
| `<Target>_main.csv` | historical target, e.g. `Gasoline_main.csv` (an optional `Total` row is ignored) |
| `<Var>_hist.csv` | historical values of a driver, e.g. `Car_hist.csv` |
| `<Var>_forecast.csv` | scenario values of the same driver, e.g. `Car_forecast.csv` |

Any driver that has both a `_hist` and a `_forecast` file is picked up automatically.

## How to run

```r
# install once
install.packages(c("tidyverse", "forecast", "openxlsx", "tseries", "zoo",
                   "ARDL", "lmtest", "broom", "dynlm"))
```

From the repository root:

```bash
Rscript run_example.R
```

The workbook `output/Forecasts_All_Models_Complete.xlsx` contains:

| Sheet | Content |
| --- | --- |
| `Linear_Forecast`, `ARIMAX_Forecast`, `ARDL_Forecast` | history and forecast by province for each model |
| `ARDL_Forecast_Final` | ARDL forecasts after the post-check |
| `ARDL_Coefficients`, `ARIMAX_Coefficients`, `*_Coef_Summary` | estimated coefficients and fit statistics |
| `ARDL_Used_Vars`, `ARDL_Removed_Vars` | selected drivers and drivers dropped as I(2) |
| one sheet per variable | input data, history and scenario |

The LEAP bridge needs Windows, an open LEAP project and the `RDCOMClient` package; usage is described at the end of `R/leap_bridge.R`.

## Notes

- With roughly 19 annual observations per province, the models are deliberately small, and the stability check and post-check guard against implausible long-run paths.
- `scripts/make_synthetic_data.py` regenerates the sample data.

## Author

Paria Yousefi — Energy Systems Engineer (MSc, Sharif University of Technology)

## License

MIT
