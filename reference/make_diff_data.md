# Generate differenced and asymmetric effects data

This is an interface to the internal functions that process data for
[`fdm()`](https://panelr.jacob-long.com/reference/fdm.md),
[`asym()`](https://panelr.jacob-long.com/reference/asym.md), and
[`asym_gee()`](https://panelr.jacob-long.com/reference/asym_gee.md).

## Usage

``` r
make_diff_data(
  formula,
  data,
  id = NULL,
  wave = NULL,
  use.wave = FALSE,
  min.waves = 1,
  weights = NULL,
  offset = NULL,
  asym = FALSE,
  cumulative = FALSE,
  escape.names = FALSE,
  ...
)
```

## Arguments

- formula:

  Model formula. See details for crucial info on `panelr`'s formula
  syntax.

- data:

  The data, either a `panel_data` object or `data.frame`.

- id:

  If `data` is not a `panel_data` object, then the name of the
  individual id column as a string. Otherwise, leave as NULL, the
  default.

- wave:

  If `data` is not a `panel_data` object, then the name of the panel
  wave column as a string. Otherwise, leave as NULL, the default.

- use.wave:

  Should the wave be included as a predictor? Default is FALSE.

- min.waves:

  What is the minimum number of waves an individual must have
  participated in to be included in the analysis? Default is `2` and any
  valid number is accepted. `"all"` is also acceptable if you want to
  include only complete panelists.

- weights:

  If using weights, either the name of the column in the data that
  contains the weights or a vector of the weights.

- offset:

  this can be used to specify an *a priori* known component to be
  included in the linear predictor during fitting. This should be `NULL`
  or a numeric vector of length equal to the number of cases. One or
  more [`offset`](https://rdrr.io/r/stats/offset.html) terms can be
  included in the formula instead or as well, and if more than one is
  specified their sum is used. See
  [`model.offset`](https://rdrr.io/r/stats/model.extract.html).

- asym:

  Return asymmetric effects transformed data? Default is FALSE.

- cumulative:

  Return cumulative positive/negative differences, most useful for fixed
  effects estimation and/or generalized linear models? Default is FALSE.

- escape.names:

  Return only syntactically valid variable names? Default is FALSE.

- ...:

  Ignored.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
make_diff_data(wks ~ lwage + union, data = wages)
#> # Panel data:    3,570 × 5
#> # Entities:      id [595]
#> # Wave variable: t [2, 3, 4, ... (6 waves)]
#>    id        t   wks  lwage union
#>    <fct> <dbl> <dbl>  <dbl> <dbl>
#>  1 1         2    11 0.160      0
#>  2 1         3    -3 0.276      0
#>  3 1         4    -1 0          0
#>  4 1         5     3 0.0650     0
#>  5 1         6    -7 0.112      0
#>  6 1         7    -3 0.0704     0
#>  7 2         2    -7 0.0513     0
#>  8 2         3     6 0.0488     1
#>  9 2         4    -3 0.281     -1
#> 10 2         5     0 0.153      0
#> # ℹ 3,560 more rows
 
```
