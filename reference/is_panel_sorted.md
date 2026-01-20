# Check if panel data is properly sorted

Internal function that checks if a data frame is sorted by id (grouped),
then by wave within each id. This is O(n) - just one pass through the
data, much cheaper than O(n log n) sorting.

## Usage

``` r
is_panel_sorted(x, id, wave)
```

## Arguments

- x:

  A data frame

- id:

  Name of the id column (string)

- wave:

  Name of the wave column (string)

## Value

TRUE if properly sorted, FALSE otherwise
