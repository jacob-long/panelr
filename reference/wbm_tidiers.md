# Tidy methods for `wbm` models

`panelr` provides methods to access `wbm` data in a tidy format

## Usage

``` r
# S3 method for class 'wbm'
tidy(
  x,
  conf.int = FALSE,
  conf.level = 0.95,
  effects = c("fixed", "ran_pars"),
  conf.method = "Wald",
  ran_prefix = NULL,
  ...
)

# S3 method for class 'wbm'
glance(x, ...)

# S3 method for class 'summ.wbm'
glance(x, ...)

# S3 method for class 'summ.wbm'
tidy(x, ...)
```

## Arguments

- x:

  An object of class `merMod`, such as those from `lmer`, `glmer`, or
  `nlmer`

- conf.int:

  whether to include a confidence interval

- conf.level:

  confidence level for CI

- effects:

  A character vector including one or more of "fixed" (fixed-effect
  parameters); "ran_pars" (variances and covariances or standard
  deviations and correlations of random effect terms); "ran_vals"
  (conditional modes/BLUPs/latent variable estimates); or "ran_coefs"
  (predicted parameter values for each group, as returned by
  `coef.merMod`.

- conf.method:

  method for computing confidence intervals (see
  [`lme4::confint.merMod`](https://rdrr.io/pkg/lme4/man/confint.merMod.html))

- ran_prefix:

  a length-2 character vector specifying the strings to use as prefixes
  for self- (variance/standard deviation) and cross-
  (covariance/correlation) random effects terms

- ...:

  Additional arguments (passed to `confint.merMod` for `tidy`;
  `augment_columns` for `augment`; ignored for `glance`)

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
model <- wbm(lwage ~ lag(union) + wks, data = wages)
if (requireNamespace("broom.mixed")) {
  broom.mixed::tidy(model)
}
#> # A tibble: 7 × 6
#>   group    estimate std.error statistic    p.value term             
#>   <chr>       <dbl>     <dbl>     <dbl>      <dbl> <chr>            
#> 1 within    0.0528    0.0250      2.11   3.50e-  2 lag(union)       
#> 2 within   -0.00166   0.00108    -1.54   1.25e-  1 wks              
#> 3 between   6.14      0.247      24.8    1.40e-125 (Intercept)      
#> 4 between   0.0168    0.0374      0.449  6.53e-  1 imean(lag(union))
#> 5 between   0.0125    0.00518     2.41   1.60e-  2 imean(wks)       
#> 6 id        0.388    NA          NA     NA         sd__(Intercept)  
#> 7 Residual  0.233    NA          NA     NA         sd__Observation  
```
