# Scan for gaps in panel data

This function identifies which entity-wave combinations are missing in a
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
object.

## Usage

``` r
scan_gaps(data)
```

## Arguments

- data:

  A `panel_data` frame.

## Value

A tibble with columns for the id variable and wave variable, showing
which combinations are missing. If there are no gaps, returns a tibble
with zero rows.

## See also

[`has_gaps()`](https://panelr.jacob-long.com/reference/has_gaps.md),
[`balance_panel()`](https://panelr.jacob-long.com/reference/balance_panel.md)

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

# Create data with gaps
wages_gaps <- wages[!(wages$t == 3 & wages$id == wages$id[1]), ]
scan_gaps(wages_gaps)
#> # A tibble: 1 × 2
#>   id        t
#>   <fct> <dbl>
#> 1 1         3
```
