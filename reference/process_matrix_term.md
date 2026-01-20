# Process a matrix term for within-between decomposition

This is the main entry point for handling basis functions in formulas.
It evaluates the term on pooled data, extracts attributes, and creates
both within and between versions of the term.

## Usage

``` r
process_matrix_term(term, data)
```

## Arguments

- term:

  Character string like "ns(age, df=3)"

- data:

  panel_data frame

## Value

List with:

- data: Updated data frame with expanded columns

- within_cols: Character vector of within column names

- between_cols: Character vector of between column names

- var_name: Original variable name

- fn_name: Function name
