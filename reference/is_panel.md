# Check if object is panel_data

This is a convenience function that checks whether an object is a
`panel_data` object.

## Usage

``` r
is_panel(x)
```

## Arguments

- x:

  Any object.

## Examples

``` r
 data("WageData")
 is_panel(WageData) # FALSE
#> [1] FALSE
 wages <- panel_data(WageData, id = id, wave = t)
 is_panel(wages) # TRUE
#> [1] TRUE
```
