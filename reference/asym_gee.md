# Asymmetric effects models fit with GEE

Fit "within-between" and several other regression variants for panel
data via generalized estimating equations.

## Usage

``` r
asym_gee(
  formula,
  data,
  id = NULL,
  wave = NULL,
  cor.str = c("ar1", "exchangeable", "unstructured"),
  use.wave = FALSE,
  wave.factor = FALSE,
  min.waves = 1,
  family = gaussian,
  weights = NULL,
  offset = NULL,
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

- cor.str:

  Any correlation structure accepted by
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html).
  Default is "ar1", most useful alternative is "exchangeable".
  "unstructured" may cause problems due to its computational complexity.

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

- ...:

  Additional arguments provided to
  [`geepack::geeglm()`](https://rdrr.io/pkg/geepack/man/geeglm.html).

## Value

An `asym_gee` object, which inherits from `wbgee` and `geeglm`.

## Details

See the documentation for
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) for many
details on formula syntax and other arguments.

## References

Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
*Socius*, *5*, 1-12. https://doi.org/10.1177/2378023119826441

McNeish, D. (2019). Effect partitioning in cross-sectionally clustered
data without multilevel models. *Multivariate Behavioral Research*,
Advance online publication.
https://doi.org/10.1080/00273171.2019.1602504

McNeish, D., Stapleton, L. M., & Silverman, R. D. (2016). On the
unnecessary ubiquity of hierarchical linear modeling. *Psychological
Methods*, *22*, 114-140. https://doi.org/10.1037/met0000078

## Author

Jacob A. Long

## Examples

``` r
if (requireNamespace("geepack")) {
  data("WageData")
  wages <- panel_data(WageData, id = id, wave = t)
  model <- asym_gee(lwage ~ lag(union) + wks, data = wages)
  summary(model)
}
#> Loading required namespace: geepack
#> MODEL INFO:
#> Entities: 595
#> Time periods: 3-7
#> Dependent variable: lwage
#> Model family: Linear 
#> Variance: ar1 (alpha = -0.3)
#> Specification: Asymmetric effects (via GEE)
#> 
#> MODEL FIT:
#> QIC = 121.63, QICu = 117.85, CIC = 6.89
#> 
#> ------------------------------------------------
#>                      Est.   S.E.   z val.      p
#> ----------------- ------- ------ -------- ------
#> (Intercept)          0.10   0.00    39.46   0.00
#> +lag(union)          0.02   0.02     1.13   0.26
#> -lag(union)         -0.00   0.02    -0.08   0.94
#> +wks                -0.00   0.00    -0.45   0.65
#> -wks                -0.00   0.00    -0.49   0.63
#> ------------------------------------------------
#> 
#> Tests of asymmetric effects:
#> -------------------------------
#>                    chi^2      p
#> ---------------- ------- ------
#> lag(union)          0.66   0.42
#> wks                 1.10   0.29
#> -------------------------------
```
