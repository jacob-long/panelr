# Make model frames for panel_data objects

This is similar to model.frame, but is designed specifically for
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
data frames. It's a workhorse in
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) but may be
useful in scripting use as well.

## Usage

``` r
model_frame(formula, data)
```

## Arguments

- formula:

  A formula. Note that to get an individual-level mean with incomplete
  data (e.g., panel attrition), you should use `imean()` rather than
  [`mean()`](https://rdrr.io/r/base/mean.html).

- data:

  A
  [`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  frame.

## Value

A
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
frame with only the columns needed to fit a model as described by the
formula.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
model_frame(lwage ~ wks + exp, data = wages)
#> # Panel data:    4,165 × 5
#> # Entities:      id [595]
#> # Wave variable: t [1, 2, 3, ... (7 waves)]
#>    id        t lwage   wks   exp
#>    <fct> <dbl> <dbl> <dbl> <dbl>
#>  1 1         1  5.56    32     3
#>  2 1         2  5.72    43     4
#>  3 1         3  6.00    40     5
#>  4 1         4  6.00    39     6
#>  5 1         5  6.06    42     7
#>  6 1         6  6.17    35     8
#>  7 1         7  6.24    32     9
#>  8 2         1  6.16    34    30
#>  9 2         2  6.21    27    31
#> 10 2         3  6.26    33    32
#> # ℹ 4,155 more rows
```
