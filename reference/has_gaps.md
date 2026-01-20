# Check if panel data has gaps

This function checks whether a
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
object has implicit gaps (missing rows for some entity-wave
combinations).

## Usage

``` r
has_gaps(data)
```

## Arguments

- data:

  A `panel_data` frame.

## Value

A logical value. `TRUE` if there are gaps, `FALSE` otherwise.

## See also

[`scan_gaps()`](https://panelr.jacob-long.com/reference/scan_gaps.md),
[`balance_panel()`](https://panelr.jacob-long.com/reference/balance_panel.md)

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
has_gaps(wages)  # FALSE (complete data)
#> [1] FALSE

# Create data with gaps
wages_gaps <- wages[wages$t != 3 | wages$id != wages$id[1], ]
has_gaps(wages_gaps)  # TRUE
#> [1] TRUE
```
