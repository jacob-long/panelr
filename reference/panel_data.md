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

## Details

When the first input to
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
is a `panel_data` object, the result retains its panel identifiers and
is sorted by entity and wave. Source labels requested with `.id` stay
attached to their original rows. Direct vctrs operations preserve the
requested row order; after reordering rows, call `panel_data()` again
before computing lags or leads.

`as_panel_data()` also accepts
[`plm::pdata.frame()`](https://rdrr.io/pkg/plm/man/pdata.frame.html)
objects. If their index columns have been removed, they are restored
from the saved panel index.

## Examples

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
```
