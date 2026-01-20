# Estimate Heise stability and reliability coefficients

This function uses three waves of data to estimate stability and
reliability coefficients as described in Heise (1969).

## Usage

``` r
heise(data, ..., waves = NULL)
```

## Arguments

- data:

  A `panel_data` frame.

- ...:

  unquoted variable names that are passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)

- waves:

  Which 3 waves should be used? If NULL (the default), the first,
  middle, and last waves are used.

## Value

A `tibble` with reliability (`rel`), waves 1-3 stability (`stab13`),
waves 1-2 stability (`stab12`), and waves 2-3 stability (`stab23`) and
the variable these values refer to (`var`).

## References

Heise, D. R. (1969). Separating reliability and stability in test-retest
correlation. *American Sociological Review*, *34*, 93–101.
https://doi.org/10.2307/2092790

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
heise(wages, wks, lwage) # will use waves 1, 4, and 7 by default
#> Using waves 1, 4, and 7
#> # A tibble: 2 × 5
#>   var   rel[,1] stab13[,1] stab12[,1] stab23[,1]
#>   <chr>   <dbl>      <dbl>      <dbl>      <dbl>
#> 1 wks     0.417      0.374      0.497      0.752
#> 2 lwage   0.922      0.871      0.918      0.949
```
