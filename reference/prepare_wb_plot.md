# Prepare component-aware interaction plots

Integration hook for `interactions::interact_plot()` and
`interactions::cat_plot()`. Plotting packages should call this before
choosing predictor ranges or moderator values. The fitted object is not
modified.

## Usage

``` r
prepare_wb_plot(
  model,
  data,
  pred,
  modx = NULL,
  mod2 = NULL,
  plot.scale = c("within", "original"),
  modx.values = NULL,
  mod2.values = NULL,
  at = NULL
)
```

## Arguments

- model:

  A fitted `wbm` or `wbgee` model.

- data:

  Original fitting data, optionally a subset of its observations. Panel
  identifiers must be present. Extra columns are allowed.

- pred, modx, mod2:

  Character names of the predictor and moderators.

- plot.scale:

  `"within"` (default) displays numeric within-person terms as
  deviations in their original units. `"original"` displays raw levels,
  conditional on fixed reference person means. Constant terms retain
  their original scale in either mode. Models without within-person
  centering are unchanged.

- modx.values, mod2.values:

  Moderator values on the selected display scale.

- at:

  Fixed values on the display scale. Overrides of `imean(...)` columns
  are always in original units.

## Value

A list containing display data, a temporary model with plotting
metadata, moderator values, default labels, and a caption.
