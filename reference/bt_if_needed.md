# Conditionally add backticks based on syntax validity

Add backticks only if the name is not a valid R syntactic name.

## Usage

``` r
bt_if_needed(x, data = NULL)
```

## Arguments

- x:

  A character string

- data:

  Optional data frame to check if x exists as a column name

## Value

The name, potentially backticked
