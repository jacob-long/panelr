# Package index

## Regression models

- [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) : Panel
  regression models fit via multilevel modeling
- [`wbgee()`](https://panelr.jacob-long.com/reference/wbgee.md) : Panel
  regression models fit with GEE
- [`fdm()`](https://panelr.jacob-long.com/reference/fdm.md) : Estimate
  first differences models using GLS
- [`asym()`](https://panelr.jacob-long.com/reference/asym.md) : Estimate
  asymmetric effects models using first differences
- [`asym_gee()`](https://panelr.jacob-long.com/reference/asym_gee.md) :
  Asymmetric effects models fit with GEE
- [`wbm_stan()`](https://panelr.jacob-long.com/reference/wbm_stan.md) :
  Bayesian estimation of within-between models

## Panel data wrangling

- [`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  [`as_pdata.frame()`](https://panelr.jacob-long.com/reference/panel_data.md)
  [`as_panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  [`as_panel()`](https://panelr.jacob-long.com/reference/panel_data.md)
  : Create panel data frames
- [`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md)
  : Convert long panel data to wide format
- [`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
  : Convert wide panels to long format
- [`summary(`*`<panel_data>`*`)`](https://panelr.jacob-long.com/reference/summary.panel_data.md)
  : Summarize panel data frames
- [`complete_data()`](https://panelr.jacob-long.com/reference/complete_data.md)
  : Filter out entities with too few observations
- [`balance_panel()`](https://panelr.jacob-long.com/reference/balance_panel.md)
  : Balance panel data by filling gaps
- [`has_gaps()`](https://panelr.jacob-long.com/reference/has_gaps.md) :
  Check if panel data has gaps
- [`scan_gaps()`](https://panelr.jacob-long.com/reference/scan_gaps.md)
  : Scan for gaps in panel data
- [`model_frame()`](https://panelr.jacob-long.com/reference/model_frame.md)
  : Make model frames for panel_data objects
- [`unpanel()`](https://panelr.jacob-long.com/reference/unpanel.md) :
  Convert panel_data to regular data frame
- [`is_panel()`](https://panelr.jacob-long.com/reference/is_panel.md) :
  Check if object is panel_data

## Model utilities

- [`tidy(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/wbm_tidiers.md)
  [`glance(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/wbm_tidiers.md)
  [`glance(`*`<summ.wbm>`*`)`](https://panelr.jacob-long.com/reference/wbm_tidiers.md)
  [`tidy(`*`<summ.wbm>`*`)`](https://panelr.jacob-long.com/reference/wbm_tidiers.md)
  :

  Tidy methods for `wbm` models

- [`tidy(`*`<asym_gee>`*`)`](https://panelr.jacob-long.com/reference/wbgee_tidiers.md)
  [`tidy(`*`<wbgee>`*`)`](https://panelr.jacob-long.com/reference/wbgee_tidiers.md)
  [`glance(`*`<wbgee>`*`)`](https://panelr.jacob-long.com/reference/wbgee_tidiers.md)
  :

  Tidy methods for `wbgee` models

- [`tidy(`*`<asym>`*`)`](https://panelr.jacob-long.com/reference/fdm_tidiers.md)
  [`tidy(`*`<fdm>`*`)`](https://panelr.jacob-long.com/reference/fdm_tidiers.md)
  [`glance(`*`<fdm>`*`)`](https://panelr.jacob-long.com/reference/fdm_tidiers.md)
  :

  Tidy methods for `fdm` and `asym` models

- [`predict(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/predict.wbm.md)
  [`simulate(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/predict.wbm.md)
  : Predictions and simulations from within-between models

- [`predict(`*`<wbgee>`*`)`](https://panelr.jacob-long.com/reference/predict.wbgee.md)
  : Predictions and simulations from within-between GEE models

- [`formula(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/formula.wbm.md)
  :

  Retrieve model formulas from `wbm` objects

- [`nobs(`*`<wbm>`*`)`](https://panelr.jacob-long.com/reference/nobs.wbm.md)
  :

  Number of observations used in `wbm` models

- [`print(`*`<WBFormula>`*`)`](https://panelr.jacob-long.com/reference/print.WBFormula.md)
  : Print method for WBFormula

- [`wbm-class`](https://panelr.jacob-long.com/reference/wbm-class.md) :

  Within-Between Model (`wbm`) class

## Other utilities

- [`are_varying()`](https://panelr.jacob-long.com/reference/are_varying.md)
  : Check if variables are constant or variable over time.
- [`make_wb_data()`](https://panelr.jacob-long.com/reference/make_wb_data.md)
  : Prepare data for within-between modeling
- [`make_diff_data()`](https://panelr.jacob-long.com/reference/make_diff_data.md)
  : Generate differenced and asymmetric effects data
- [`get_wave()`](https://panelr.jacob-long.com/reference/get_wave.md)
  [`get_id()`](https://panelr.jacob-long.com/reference/get_wave.md)
  [`get_periods()`](https://panelr.jacob-long.com/reference/get_wave.md)
  : Retrieve panel_data metadata
- [`line_plot()`](https://panelr.jacob-long.com/reference/line_plot.md)
  : Plot trends in longitudinal variables
- [`heise()`](https://panelr.jacob-long.com/reference/heise.md) :
  Estimate Heise stability and reliability coefficients

## Datasets

- [`WageData`](https://panelr.jacob-long.com/reference/WageData.md) :
  Earnings data from the Panel Study of Income Dynamics
- [`teen_poverty`](https://panelr.jacob-long.com/reference/teen_poverty.md)
  : National Longitudinal Survey of Youth teenage women poverty data
- [`nlsy`](https://panelr.jacob-long.com/reference/nlsy.md) : National
  Longitudinal Survey of Youth data
