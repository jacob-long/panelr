# Retrieve model formulas from `wbm` objects

This S3 method allows you to retrieve the formula used to fit `wbm`
objects.

## Usage

``` r
# S3 method for class 'wbm'
formula(x, raw = FALSE, ...)
```

## Arguments

- x:

  A `wbm` model.

- raw:

  Return the formula used in the call to `lmerMod`/`glmerMod`? Default
  is FALSE.

- ...:

  further arguments passed to or from other methods.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
model <- wbm(lwage ~ lag(union) + wks, data = wages)
# Returns the original model formula rather than the one sent to lme4
formula(model)
#> lwage ~ lag(union) + wks
#> <environment: 0x55f89ba5de08>
```
