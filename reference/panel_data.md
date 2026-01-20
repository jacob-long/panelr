# Create panel data frames

Format your data for use with panelr.

## Usage

``` r
panel_data(data, id = id, wave = wave, ...)

as_pdata.frame(data)

as_panel_data(data, ...)

# Default S3 method
as_panel_data(data, id = id, wave = wave, ...)

# S3 method for class 'pdata.frame'
as_panel_data(data, ...)

as_panel(data, ...)
```

## Arguments

- data:

  A data frame.

- id:

  The name of the column (unquoted) that identifies
  participants/entities. A new column will be created called `id`,
  overwriting any column that already has that name.

- wave:

  The name of the column (unquoted) that identifies waves or periods. A
  new column will be created called `wave`, overwriting any column that
  already has that name.

- ...:

  Attributes for adding onto this method. See
  [`tibble::new_tibble()`](https://tibble.tidyverse.org/reference/new_tibble.html)
  for a run-through of the logic.

## Value

A `panel_data` object.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
```
