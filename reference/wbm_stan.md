# Bayesian estimation of within-between models

A near-equivalent of
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) that instead
uses Stan, via rstan and brms.

## Usage

``` r
wbm_stan(
  formula,
  data,
  id = NULL,
  wave = NULL,
  model = "w-b",
  detrend = FALSE,
  use.wave = FALSE,
  wave.factor = FALSE,
  min.waves = 2,
  model.cor = FALSE,
  family = gaussian,
  fit_model = TRUE,
  balance.correction = FALSE,
  dt.random = TRUE,
  dt.order = 1,
  chains = 3,
  iter = 2000,
  scale = FALSE,
  save_ranef = FALSE,
  interaction.style = c("double-demean", "demean", "raw"),
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

- model.cor:

  Do you want to model residual autocorrelation? This is often
  appropriate for linear models (`family = gaussian`). Default is FALSE
  to be consistent with
  [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md), reduce
  runtime, and avoid warnings for non-linear models.

- family:

  Use this to specify GLM link families. Default is `gaussian`, the
  linear model.

- fit_model:

  Fit the model? Default is TRUE. If FALSE, only the model code is
  returned.

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

- chains:

  How many Markov chains should be used? Default is 3, to leave you with
  one unused thread if you're on a typical dual-core machine.

- iter:

  How many iterations, including warmup? Default is 2000, leaving 1000
  per chain after warmup. For some models and data, you may need quite a
  few more.

- scale:

  Standardize predictors? This can speed up model fit. Default is FALSE.

- save_ranef:

  Save random effect estimates? This can be crucial for predicting from
  the model and for certain post-estimation procedures. On the other
  hand, it drastically increases the size of the resulting model.
  Default is FALSE.

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

  Additional arguments passed on to
  [`brms::brm()`](https://paulbuerkner.com/brms/reference/brm.html).
  This can include specification of priors.

## Value

A `wbm_stan` object, which is a list containing a `model` object with
the `brm` model and a `stan_code` object with the model code.

If `fit_model = FALSE`, instead a list is returned containing a
`stan_code` object and a `stan_data` object, leaving you with the tools
you need to run the model yourself using `rstan`.

## Details

See [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) for
details on the formula syntax, model types, and some other stuff.

## See also

[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md)

## Author

Jacob A. Long

## Examples

``` r
if (FALSE) { # \dontrun{
 data("WageData")
 wages <- panel_data(WageData, id = id, wave = t)
 model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
           data = wages, chains = 1, iter = 2000)
 summary(model)
} # }
```
