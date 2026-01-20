# Evaluate a basis function on pooled data and extract attributes

Evaluate a basis function on pooled data and extract attributes

## Usage

``` r
evaluate_basis_term(term, data)
```

## Arguments

- term:

  Character string like "ns(age, df=3)"

- data:

  Ungrouped data frame

## Value

List with components:

- result: The evaluated matrix

- attrs: Named list of reproducible attributes

- ncol: Number of columns

- fn_name: The function name

- var_name: The primary variable name
