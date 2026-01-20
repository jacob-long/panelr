# Expand matrix terms into data columns

This function takes the processed matrix_terms from wb_formula_parser
and creates the actual columns in the data frame for within and between
components.

## Usage

``` r
expand_matrix_terms_in_data(matrix_terms, data)
```

## Arguments

- matrix_terms:

  List of processed matrix term info

- data:

  panel_data frame

## Value

List with:

- data: Updated data frame with expanded columns

- within_cols: All within column names

- between_cols: All between column names
