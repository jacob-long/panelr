# Panel regression models fit with GEE

Fit "within-between" and several other regression variants for panel
data via generalized estimating equations.

## Usage

``` r
wbgee(
  formula,
  data,
  id = NULL,
  wave = NULL,
  model = "w-b",
  cor.str = c("ar1", "exchangeable", "unstructured"),
  detrend = FALSE,
  use.wave = FALSE,
  wave.factor = FALSE,
  min.waves = 2,
  family = gaussian,
  balance.correction = FALSE,
  dt.random = TRUE,
  dt.order = 1,
  weights = NULL,
  offset = NULL,
  interaction.style = c("double-demean", "demean", "raw"),
  scale = FALSE,
  scale.response = FALSE,
  n.sd = 1,
  calc.fit.stats = TRUE,
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

- model:

  One of `"w-b"`, `"within"`, `"between"`, `"contextual"`. See details
  for more on these options.

- cor.str:

  Any correlation structure accepted by
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html).
  Default is "ar1", most useful alternative is "exchangeable".
  "unstructured" may cause problems due to its computational complexity.

- detrend:

  Adjust within-subject effects for trends in the predictors? Default is
  FALSE, but some research suggests this is a better idea (see Curran
  and Bauer (2011) reference).

- use.wave:

  Should the wave be included as a predictor? Default is FALSE.

- wave.factor:

  Should the wave variable be treated as an unordered factor instead of
  continuous? Default is FALSE.

- min.waves:

  What is the minimum number of waves an individual must have
  participated in to be included in the analysis? Default is `2` and any
  valid number is accepted. `"all"` is also acceptable if you want to
  include only complete panelists.

- family:

  Use this to specify GLM link families. Default is `gaussian`, the
  linear model.

- balance.correction:

  Correct between-subject effects for unbalanced panels following the
  procedure in Curran and Bauer (2011)? Default is FALSE.

- dt.random:

  Should the detrending procedure be performed with a random slope for
  each entity? Default is TRUE but for short panels FALSE may be better,
  fitting a trend for all entities.

- dt.order:

  If detrending using `detrend`, what order polynomial would you like to
  specify for the relationship between time and the predictors? Default
  is 1, a linear model.

- weights:

  If using weights, either the name of the column in the data that
  contains the weights or a vector of the weights.

- offset:

  this can be used to specify an *a priori* known component to be
  included in the linear predictor during fitting. This should be `NULL`
  or a numeric vector of length equal to the number of cases. One or
  more [`offset`](https://rdrr.io/r/stats/offset.html) terms can be
  included in the formula instead or as well, and if more than one is
  specified their sum is used. See
  [`model.offset`](https://rdrr.io/r/stats/model.extract.html).

- interaction.style:

  The best way to calculate interactions in within models is in some
  dispute. The conventional way (`"demean"`) is to first calculate the
  product of the variables involved in the interaction before those
  variables have their means subtracted and then subtract the mean of
  the product from the product term (see Schunk and Perales (2017)).
  Giesselmann and Schmidt-Catran (2020) show this method carries
  between-entity differences that within models are designed to model
  out. They suggest an alternate method (`"double-demean"`) in which the
  product term is first calculated using the de-meaned lower-order
  variables and then the subject means are subtracted from this product
  term. Another option is to simply use the product term of the
  de-meaned variables (`"raw"`), but Giesselmann and
  Schmidt-Catran (2020) show this method biases the results towards zero
  effect. The default is `"double-demean"` but if emulating other
  software is the goal, `"demean"` might be preferred.

- scale:

  If `TRUE`, reports standardized regression coefficients by scaling and
  mean-centering input data (the latter can be changed via the
  `scale.only` argument). Default is `FALSE`.

- scale.response:

  Should the response variable also be rescaled? Default is `FALSE`.

- n.sd:

  How many standard deviations should you divide by for standardization?
  Default is 1, though some prefer 2.

- calc.fit.stats:

  Calculate fit statistics? Default is TRUE, but occasionally
  poor-fitting models might trip up here.

- ...:

  Additional arguments provided to
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html).

## Value

A `wbgee` object, which inherits from `geeglm`.

## Details

See the documentation for
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) for many
details on formula syntax and other arguments.

## References

Allison, P. (2009). *Fixed effects regression models*. Thousand Oaks,
CA: SAGE Publications. https://doi.org/10.4135/9781412993869.d33

Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects
modeling of time-series cross-sectional and panel data. *Political
Science Research and Methods*, *3*, 133–153.
https://doi.org/10.1017/psrm.2014.7

Curran, P. J., & Bauer, D. J. (2011). The disaggregation of
within-person and between-person effects in longitudinal models of
change. *Annual Review of Psychology*, *62*, 583–619.
https://doi.org/10.1146/annurev.psych.093008.100356

Giesselmann, M., & Schmidt-Catran, A. W. (2020). Interactions in fixed
effects regression models. *Sociological Methods & Research*, 1–28.
https://doi.org/10.1177/0049124120914934

McNeish, D. (2019). Effect partitioning in cross-sectionally clustered
data without multilevel models. *Multivariate Behavioral Research*,
Advance online publication.
https://doi.org/10.1080/00273171.2019.1602504

McNeish, D., Stapleton, L. M., & Silverman, R. D. (2016). On the
unnecessary ubiquity of hierarchical linear modeling. *Psychological
Methods*, *22*, 114-140. https://doi.org/10.1037/met0000078

Schunck, R., & Perales, F. (2017). Within- and between-cluster effects
in generalized linear mixed models: A discussion of approaches and the
`xthybrid` command. *The Stata Journal*, *17*, 89–115.
https://doi.org/10.1177/1536867X1701700106

## Author

Jacob A. Long

## Examples

``` r
if (requireNamespace("geepack")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  model <- wbgee(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
           data = wages)
  summary(model)
}
#> MODEL INFO:
#> Entities: 595
#> Time periods: 2-7
#> Dependent variable: lwage
#> Model type: Linear GEE
#> Variance: ar1 (alpha = 0.85)
#> Specification: within-between
#> 
#> MODEL FIT:
#> QIC = 655.54, QICu = 653.36, CIC = 9.09
#> 
#> WITHIN EFFECTS:
#> -----------------------------------------------
#>                     Est.   S.E.   z val.      p
#> ---------------- ------- ------ -------- ------
#> lag(union)          0.02   0.02     0.98   0.33
#> wks                -0.00   0.00    -0.82   0.41
#> -----------------------------------------------
#> 
#> BETWEEN EFFECTS:
#> ------------------------------------------------------
#>                            Est.   S.E.   z val.      p
#> ----------------------- ------- ------ -------- ------
#> (Intercept)                6.61   0.24    27.12   0.00
#> imean(lag(union))         -0.01   0.03    -0.40   0.69
#> imean(wks)                 0.00   0.01     0.75   0.45
#> blk                       -0.23   0.06    -3.86   0.00
#> fem                       -0.43   0.05    -8.94   0.00
#> ------------------------------------------------------
#> 
#> CROSS-LEVEL INTERACTIONS:
#> ---------------------------------------------------
#>                         Est.   S.E.   z val.      p
#> -------------------- ------- ------ -------- ------
#> lag(union):blk         -0.11   0.05    -2.22   0.03
#> ---------------------------------------------------
#> 
```
