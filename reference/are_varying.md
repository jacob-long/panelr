# Check if variables are constant or variable over time.

This function is designed for use with
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
objects.

## Usage

``` r
are_varying(data, ..., type = "time")
```

## Arguments

- data:

  A data frame, typically of
  [`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  class.

- ...:

  Variable names. If none are given, all variables are checked.

- type:

  Check for variance over time or across individuals? Default is
  `"time"`. `"individual"` considers variables like age to be
  non-varying because everyone ages at the same speed.

## Value

A named logical vector. If TRUE, the variable is varying.

## Examples

``` r
wages <- panel_data(WageData, id = id, wave = t)
wages %>% are_varying(occ, ind, fem, blk)
#>   occ   ind   fem   blk 
#>  TRUE  TRUE FALSE FALSE 
```
