# Tidy methods for `fdm` and `asym` models

`panelr` provides methods to access `fdm` and `asym` data in a tidy
format

## Usage

``` r
# S3 method for class 'asym'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'fdm'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'fdm'
glance(x, ...)
```

## Arguments

- x:

  An `fdm` or `asym` object.

- conf.int:

  Logical indicating whether or not to include a confidence interval in
  the tidied output. Defaults to `FALSE`.

- conf.level:

  The confidence level to use for the confidence interval if
  `conf.int = TRUE`. Must be strictly greater than 0 and less than 1.
  Defaults to 0.95, which corresponds to a 95 percent confidence
  interval.

- ...:

  Ignored

## Examples

``` r
if (requireNamespace("clubSandwich")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  model <- fdm(lwage ~ wks + union, data = wages)
  if (requireNamespace("generics")) {
    generics::tidy(model)
  }
}
#> # A tibble: 3 × 5
#>   estimate std.error statistic   p.value term       
#>      <dbl>     <dbl>     <dbl>     <dbl> <chr>      
#> 1 0.0967    0.00176     54.9   1.09e-234 (Intercept)
#> 2 0.000461  0.000974     0.473 6.36e-  1 wks        
#> 3 0.0199    0.0221       0.904 3.67e-  1 union      
```
