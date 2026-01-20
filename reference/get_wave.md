# Retrieve panel_data metadata

`get_id()`, `get_wave()`, and `get_periods()` are extractor functions
that can be used to retrieve the names of the id and wave variables or
time periods of a `panel_data` frame.

## Usage

``` r
get_wave(data)

get_id(data)

get_periods(data)
```

## Arguments

- data:

  A `panel_data` frame

## Value

A `panel_data` frame

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
get_wave(wages)
#> [1] "t"
get_id(wages)
#> [1] "id"
get_periods(wages)
#> [1] 1 2 3 4 5 6 7
```
