# Reconstruct a basis function call with a modified variable

Reconstruct a basis function call with a modified variable

## Usage

``` r
reconstruct_basis_call(term, new_var, attrs = NULL)
```

## Arguments

- term:

  Original term like "ns(age, df=3)"

- new_var:

  New variable expression like "age - imean(age)"

- attrs:

  Optional list of attributes to add as arguments

## Value

Character string of the new call
