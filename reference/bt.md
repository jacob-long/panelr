# Add backticks to names

Add backticks to variable names for use in formulas or expressions.
Handles NULL input and avoids double-backticking.

## Usage

``` r
bt(x)
```

## Arguments

- x:

  A character vector of variable names (or NULL)

## Value

A character vector with backticks added, or NULL if input was NULL
