# Number of observations used in `wbm` models

This S3 method allows you to retrieve either the number of observations
or number of entities in the data used to fit `wbm` objects.

## Usage

``` r
# S3 method for class 'wbm'
nobs(object, entities = TRUE, ...)
```

## Arguments

- object:

  a fitted model object.

- entities:

  Should `nobs` return the number of entities in the panel or the number
  of rows in the `panel_data` frame? Default is TRUE, returning the
  number of entities.

- ...:

  further arguments to be passed to methods.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
model <- wbm(lwage ~ lag(union) + wks, data = wages)
nobs(model)
#> [1] 595
```
