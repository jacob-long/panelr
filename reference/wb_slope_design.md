# Design contrasts for conditional within-between slopes

Integration hook for the S3 methods in `interactions::sim_slopes()`.
Uses the same hypothetical-grid transformation as interaction plots. No
models are refitted, and no person means are recomputed from the grid.

## Usage

``` r
wb_slope_design(
  model,
  pred,
  grid,
  cond.int = FALSE,
  check_support = TRUE,
  linear.moderator = NULL
)
```

## Arguments

- model:

  A fitted `wbm` or `wbgee` model, optionally prepared by
  [`prepare_wb_plot()`](https://panelr.jacob-long.com/reference/prepare_wb_plot.md)
  to use within-person display units.

- pred:

  Character name of an untransformed numeric focal predictor.

- grid:

  A data frame containing all predictor values for each requested
  profile, on the selected display scale. Without plotting metadata,
  inputs use original units and original levels.

- cond.int:

  Will conditional intercepts be reported? If so, their location at
  `pred = 0` on the display scale is checked for extrapolation.
  Intercept design rows and offsets are returned regardless.

- check_support:

  Warn about out-of-range profile values? Internal unit steps used to
  calculate contrasts never trigger extrapolation warnings.

- linear.moderator:

  Optional moderator name that must enter linearly, as required for
  analytic Johnson-Neyman regions.

## Value

A list containing slope contrasts (`L`), intercept design rows (`X0`),
intercept offsets, fixed-effect coefficients, their covariance matrix,
residual degrees of freedom, and the number of observations.

## Details

Slopes are fixed-effect, link-scale slopes conditional on reference
person means and interaction-centering quantities. They are not
derivatives of an entire recomputed panel history, nor averages over
random effects. The Gaussian identity link gives response-scale slopes.
Predictor units follow the display scale even when the fitted model is
standardized. Nonlinear focal terms, predictor-dependent offsets, basis
expansions, lagged/detrended models, and rank-deficient fits are not
supported.
