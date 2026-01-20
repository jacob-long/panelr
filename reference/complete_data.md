# Filter out entities with too few observations

This function allows you to define a minimum number of waves/periods and
exclude all individuals with fewer observations than that.

## Usage

``` r
complete_data(data, ..., formula = NULL, vars = NULL, min.waves = "all")
```

## Arguments

- data:

  A
  [`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  frame.

- ...:

  Optionally, unquoted variable names/expressions separated by commas to
  be passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  Otherwise, all columns are included if `formula` and `vars` are also
  NULL.

- formula:

  A formula, like the one you'll be using to specify your model.

- vars:

  As an alternative to formula, a vector of variable names.

- min.waves:

  What is the minimum number of observations to be kept? Default is
  `"all"`, but it can be any number.

## Value

A `panel_data` frame.

## Details

If `...` (that is, unquoted variable name(s)) are included, then
`formula` and `vars` are ignored. Likewise, `formula` takes precedence
over `vars`. These are just different methods for selecting variables
and you can choose whichever you prefer/are comfortable with. `...`
corresponds with the "tidyverse" way, `formula` is useful for
programming or working with model formulas, and `vars` is a "standard"
evaluation method for when you are working with strings.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
complete_data(wages, wks, lwage, min.waves = 3)
#> # Panel data:    4,165 × 14
#> # Entities:      id [595]
#> # Wave variable: t [1, 2, 3, ... (7 waves)]
#>    id        t   exp   wks   occ   ind south  smsa    ms   fem union    ed   blk
#>    <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 1         1     3    32     0     0     1     0     1     0     0     9     0
#>  2 1         2     4    43     0     0     1     0     1     0     0     9     0
#>  3 1         3     5    40     0     0     1     0     1     0     0     9     0
#>  4 1         4     6    39     0     0     1     0     1     0     0     9     0
#>  5 1         5     7    42     0     1     1     0     1     0     0     9     0
#>  6 1         6     8    35     0     1     1     0     1     0     0     9     0
#>  7 1         7     9    32     0     1     1     0     1     0     0     9     0
#>  8 2         1    30    34     1     0     0     0     1     0     0    11     0
#>  9 2         2    31    27     1     0     0     0     1     0     0    11     0
#> 10 2         3    32    33     1     1     0     0     1     0     1    11     0
#> # ℹ 4,155 more rows
#> # ℹ 1 more variable: lwage <dbl>
```
