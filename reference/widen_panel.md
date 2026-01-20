# Convert long panel data to wide format

This function takes
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
objects as input as converts them to wide format for use in SEM and
other situations when such a format is needed.

## Usage

``` r
widen_panel(data, separator = "_", ignore.attributes = FALSE, varying = NULL)
```

## Arguments

- data:

  The `panel_data` frame.

- separator:

  When the variables are labeled with the wave number, what should
  separate the variable name and wave number? By default, it is "\_". In
  other words, a variable named `var` will be `var_1`, `var_2`, and so
  on in the wide data frame.

- ignore.attributes:

  If the `data` was created by
  [`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md),
  it stores information about which variables vary over time and which
  are constants. Sometimes, though, this information is not accurate (
  it is only based on the wide data's variable names) and you may want
  to force this function to check again based on the actual values of
  the variables.

- varying:

  If you want to skip the checks for whether variables are varying and
  specify yourself, as is done with
  [`stats::reshape()`](https://rdrr.io/r/stats/reshape.html), you can
  supply them as a vector here.

## Value

A data.frame with 1 row per respondent.

## Details

This is a wrapper for
[`stats::reshape()`](https://rdrr.io/r/stats/reshape.html), which is
renowned for being pretty confusing to use. This function automatically
detects which of the variables vary over time and which don't, not
appending wave information to constants.

## See also

[`reshape`](https://rdrr.io/r/stats/reshape.html)

## Examples

``` r
wages <- panel_data(WageData, id = id, wave = t)
wide_wages <- widen_panel(wages)
```
