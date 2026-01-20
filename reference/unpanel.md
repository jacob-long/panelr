# Convert panel_data to regular data frame

This convenience function removes the special features of `panel_data`.

## Usage

``` r
unpanel(panel)
```

## Arguments

- panel:

  A `panel_data` object.

## Value

An ungrouped `tibble`.

## Examples

``` r
data("WageData") 
wages <- panel_data(WageData, id = id, wave = t)
wages_non_panel <- unpanel(wages)
```
