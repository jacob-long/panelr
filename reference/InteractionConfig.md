# Interaction configuration

S3 class to encapsulate interaction processing settings. Replaces the
scattered boolean flags (demean.ints, old.ints, detrend).

## Usage

``` r
InteractionConfig(
  style = c("double-demean", "demean", "raw"),
  model_type = "w-b",
  detrend = FALSE
)
```

## Arguments

- style:

  Character: "double-demean", "demean", or "raw"

- model_type:

  Character: model type (e.g., "w-b", "within", "between")

- detrend:

  Logical: whether detrending is being used

## Value

An InteractionConfig S3 object
