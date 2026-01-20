# Balance panel data by filling gaps

This function makes implicit missing values explicit by adding rows with
NA values for entity-wave combinations that are not present in the data.

## Usage

``` r
balance_panel(data, ...)
```

## Arguments

- data:

  A `panel_data` frame.

- ...:

  Optional fill values specified as `column = value`. Any columns not
  specified will be filled with `NA`.

## Value

A `panel_data` frame with all entity-wave combinations present.

## Details

Panel data often has implicit gaps where certain entities are not
observed in certain waves. This function makes these gaps explicit by
adding rows filled with NA values (or custom values if specified).

This is the inverse operation of removing incomplete cases. It can be
useful for:

- Visualizing the pattern of missing data

- Using functions that require complete (balanced) panels

- Explicit handling of missing waves in models

## See also

[`has_gaps()`](https://panelr.jacob-long.com/reference/has_gaps.md),
[`scan_gaps()`](https://panelr.jacob-long.com/reference/scan_gaps.md),
[`complete_data()`](https://panelr.jacob-long.com/reference/complete_data.md)

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

# Create data with gaps
wages_gaps <- wages[!(wages$t == 3 & wages$id == wages$id[1]), ]
nrow(wages_gaps)  # Missing one row
#> [1] 4164

# Balance the panel (add NA row)
wages_balanced <- balance_panel(wages_gaps)
nrow(wages_balanced)  # Back to full size
#> [1] 4165

# Balance with custom fill values
wages_balanced <- balance_panel(wages_gaps, wks = 0, union = 0)
```
