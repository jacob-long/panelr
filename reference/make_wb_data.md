# Prepare data for within-between modeling

This function allows users to make the changes to their data that occur
in [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) without
having to fit the model.

## Usage

``` r
make_wb_data(
  formula,
  data,
  id = NULL,
  wave = NULL,
  model = "w-b",
  detrend = FALSE,
  use.wave = FALSE,
  wave.factor = FALSE,
  min.waves = 2,
  balance.correction = FALSE,
  dt.random = TRUE,
  dt.order = 1,
  weights = NULL,
  offset = NULL,
  interaction.style = c("double-demean", "demean", "raw"),
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

- ...:

  Additional arguments provided to
  [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html),
  [`lme4::glmer()`](https://rdrr.io/pkg/lme4/man/glmer.html), or
  [`lme4::glmer.nb()`](https://rdrr.io/pkg/lme4/man/glmer.nb.html).

## Value

A `panel_data` object with the requested specification.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
make_wb_data(lwage ~ wks + union | fem, data = wages)
#> # Panel data:    4,165 × 8
#> # Entities:      id [595]
#> # Wave variable: t [1, 2, 3, ... (7 waves)]
#>    id        t lwage   wks  union   fem `imean(wks)` `imean(union)`
#>    <fct> <dbl> <dbl> <dbl>  <dbl> <dbl>        <dbl>          <dbl>
#>  1 1         1  5.56 -5.57  0         0         37.6          0    
#>  2 1         2  5.72  5.43  0         0         37.6          0    
#>  3 1         3  6.00  2.43  0         0         37.6          0    
#>  4 1         4  6.00  1.43  0         0         37.6          0    
#>  5 1         5  6.06  4.43  0         0         37.6          0    
#>  6 1         6  6.17 -2.57  0         0         37.6          0    
#>  7 1         7  6.24 -5.57  0         0         37.6          0    
#>  8 2         1  6.16  2.43 -0.143     0         31.6          0.143
#>  9 2         2  6.21 -4.57 -0.143     0         31.6          0.143
#> 10 2         3  6.26  1.43  0.857     0         31.6          0.143
#> # ℹ 4,155 more rows
```
