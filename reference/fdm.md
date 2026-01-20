# Estimate first differences models using GLS

The function fits first difference models using GLS estimation.

## Usage

``` r
fdm(
  formula,
  data,
  id = NULL,
  wave = NULL,
  use.wave = FALSE,
  min.waves = 1,
  variance = c("toeplitz-1", "constrained", "unconstrained"),
  error.type = c("CR2", "CR1S"),
  ...
)
```

## Arguments

- formula:

  Model formula. See details for crucial info on `panelr`'s formula
  syntax.

- data:

  The data, either a `panel_data` object or `data.frame`.

- id:

  If `data` is not a `panel_data` object, then the name of the
  individual id column as a string. Otherwise, leave as NULL, the
  default.

- wave:

  If `data` is not a `panel_data` object, then the name of the panel
  wave column as a string. Otherwise, leave as NULL, the default.

- use.wave:

  Should the wave be included as a predictor? Default is FALSE.

- min.waves:

  What is the minimum number of waves an individual must have
  participated in to be included in the analysis? Default is `2` and any
  valid number is accepted. `"all"` is also acceptable if you want to
  include only complete panelists.

- variance:

  One of `"toeplitz-1"`, `"constrained"`, or `"unconstrained"`. The
  toeplitz variance specification estimates a single error variance and
  a single lag-1 error correlation with other lags having zero
  correlation. The constrained model assumes no autocorrelated errors or
  heteroskedastic errors. The unconstrained option allows separate
  variances for every period as well as every lag of autocorrelation.
  This can be very computationally taxing as periods increase and will
  be inefficient when not necessary. See Allison (2019) for more.

- error.type:

  Either "CR2" or "CR1S". See the `clubSandwich` package for more
  details.

- ...:

  Ignored.

## References

Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
*Socius*, *5*, 1-12. https://doi.org/10.1177/2378023119826441

## Examples

``` r
if (requireNamespace("clubSandwich")) {
  data("teen_poverty")
  # Convert to long format
  teen <- long_panel(teen_poverty, begin = 1, end = 5)
  model <- fdm(hours ~ lag(pov) + spouse, data = teen)
  summary(model)
}
#> Loading required namespace: clubSandwich
#> Registered S3 method overwritten by 'clubSandwich':
#>   method    from    
#>   bread.mlm sandwich
#> MODEL INFO:
#> Entities: 1151
#> Time periods: 3-5
#> Dependent variable: hours
#> Variance structure: toeplitz-1 (theta = -0.51) 
#> 
#> MODEL FIT:
#> AIC = 28771.97, BIC = 28802.7
#>  
#> Standard errors: CR2 
#> ------------------------------------------------
#>                      Est.   S.E.   t val.      p
#> ----------------- ------- ------ -------- ------
#> (Intercept)          2.09   0.19    10.89   0.00
#> lag_pov             -1.62   0.56    -2.91   0.00
#> spouse              -2.38   1.26    -1.89   0.06
#> ------------------------------------------------
```
