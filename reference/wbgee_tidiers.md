# Tidy methods for `wbgee` models

`panelr` provides methods to access `wbgee` data in a tidy format

## Usage

``` r
# S3 method for class 'asym_gee'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'wbgee'
tidy(x, conf.int = FALSE, conf.level = 0.95, ...)

# S3 method for class 'wbgee'
glance(x, ...)
```

## Arguments

- x:

  A `wbgee` object.

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
if (requireNamespace("geepack")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  model <- wbgee(lwage ~ lag(union) + wks, data = wages)
  if (requireNamespace("generics")) {
    generics::tidy(model)
  }
}
#> # A tibble: 5 × 6
#>   group   estimate std.error statistic p.value term             
#>   <chr>      <dbl>     <dbl>     <dbl>   <dbl> <chr>            
#> 1 within   0.0145    0.0194      0.745  0.456  lag(union)       
#> 2 within  -0.00119   0.00150    -0.790  0.430  wks              
#> 3 between  6.16      0.277      22.2    0      (Intercept)      
#> 4 between  0.0309    0.0335      0.923  0.356  imean(lag(union))
#> 5 between  0.0116    0.00576     2.01   0.0448 imean(wks)       
```
