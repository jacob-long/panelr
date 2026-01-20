# Lightweight panel_data constructor

Internal helper function for fast reconstruction of panel_data objects.
Unlike
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md),
this does NOT validate, sort, or set up grouping by default. It simply
attaches attributes and class. Use for fast reconstruction after
operations that preserve the panel structure.

## Usage

``` r
build_panel_data(
  x,
  id,
  wave,
  periods = NULL,
  reshaped = NULL,
  varying = NULL,
  constants = NULL,
  validate_order = FALSE
)
```

## Arguments

- x:

  A data frame to convert

- id:

  Name of the id column (string)

- wave:

  Name of the wave column (string)

- periods:

  Vector of time periods (optional)

- reshaped:

  Logical indicating if data was reshaped (optional)

- varying:

  Character vector of varying variable names (optional)

- constants:

  Character vector of constant variable names (optional)

- validate_order:

  If TRUE, check if data is sorted and re-sort if not. Default FALSE for
  speed. Set TRUE when row order might have changed.

## Value

A panel_data object

## Details

Set `validate_order = TRUE` to check if data is sorted and fix if
needed. The check is O(n); sorting only happens if data is actually
unsorted.
