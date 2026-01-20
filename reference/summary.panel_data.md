# Summarize panel data frames

`summary` method for `panel_data` objects.

## Usage

``` r
# S3 method for class 'panel_data'
summary(object, ..., by.wave = TRUE, by.id = FALSE, skim_with = NULL)
```

## Arguments

- object:

  A `panel_data` frame.

- ...:

  Optionally, unquoted variable names/expressions separated by commas to
  be passed to
  [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).
  Otherwise, all columns are included.

- by.wave:

  (if `skimr` is installed) Separate descriptives by wave? Default is
  TRUE.

- by.id:

  (if `skimr` is installed) Separate descriptives by entity? Default is
  FALSE. Be careful if you have a large number of entities as the output
  will be massive.

- skim_with:

  A closure from
  [`skimr::skim_with()`](https://docs.ropensci.org/skimr/reference/skim_with.html).
  If set, skim

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
summary(wages, lwage, exp, wks)
#> Loading required namespace: skimr
#> 
#> ── Variable type: numeric ──────────────────────────────────────────────────────
#>    skim_variable t n_missing complete_rate  mean     sd    p0   p25   p50   p75
#>  1 lwage         1         0             1  6.38  0.388  5.01  6.12  6.42  6.65
#>  2 lwage         2         0             1  6.47  0.363  5.01  6.24  6.53  6.75
#>  3 lwage         3         0             1  6.60  0.447  4.61  6.33  6.61  6.86
#>  4 lwage         4         0             1  6.70  0.441  5.08  6.44  6.72  6.96
#>  5 lwage         5         0             1  6.79  0.424  5.27  6.51  6.80  7.04
#>  6 lwage         6         0             1  6.86  0.424  5.66  6.60  6.91  7.11
#>  7 lwage         7         0             1  6.95  0.438  5.68  6.68  6.98  7.21
#>  8 exp           1         0             1 16.9  10.8    1     7    15    26   
#>  9 exp           2         0             1 17.9  10.8    2     8    16    27   
#> 10 exp           3         0             1 18.9  10.8    3     9    17    28   
#> 11 exp           4         0             1 19.9  10.8    4    10    18    29   
#> 12 exp           5         0             1 20.9  10.8    5    11    19    30   
#> 13 exp           6         0             1 21.9  10.8    6    12    20    31   
#> 14 exp           7         0             1 22.9  10.8    7    13    21    32   
#> 15 wks           1         0             1 46.3   6.25   6    46    48    50   
#> 16 wks           2         0             1 47.0   5.13  11    47    49    50   
#> 17 wks           3         0             1 47.0   4.77  20    47    49    50   
#> 18 wks           4         0             1 47.2   4.46   8    47    48    50   
#> 19 wks           5         0             1 47.0   4.89   6    47    48    50   
#> 20 wks           6         0             1 46.7   4.98   6    46    48    50   
#> 21 wks           7         0             1 46.5   5.19   5    46    48    49   
#>     p100 hist 
#>  1  6.91 ▁▂▃▇▇
#>  2  6.91 ▁▁▂▅▇
#>  3  8.27 ▁▂▇▃▁
#>  4  8.52 ▁▃▇▂▁
#>  5  8.10 ▁▂▇▅▁
#>  6  8.16 ▁▃▇▃▁
#>  7  8.54 ▁▅▇▂▁
#>  8 45    ▇▇▅▅▁
#>  9 46    ▇▇▅▅▁
#> 10 47    ▇▇▅▅▁
#> 11 48    ▇▇▅▅▁
#> 12 49    ▇▇▅▅▁
#> 13 50    ▇▇▅▅▁
#> 14 51    ▇▇▅▅▁
#> 15 52    ▁▁▁▁▇
#> 16 52    ▁▁▁▁▇
#> 17 52    ▁▁▁▁▇
#> 18 52    ▁▁▁▁▇
#> 19 52    ▁▁▁▁▇
#> 20 52    ▁▁▁▁▇
#> 21 52    ▁▁▁▁▇
```
