# Convert wide panels to long format

This function takes wide format panels as input and converts them to
long format.

## Usage

``` r
long_panel(
  data,
  prefix = NULL,
  suffix = NULL,
  begin = NULL,
  end = NULL,
  id = "id",
  wave = "wave",
  periods = NULL,
  label_location = c("end", "beginning"),
  as_panel_data = TRUE,
  match = ".*",
  use.regex = FALSE,
  check.varying = TRUE
)
```

## Arguments

- data:

  The wide data frame.

- prefix:

  What character(s) go before the period indicator? If none, set this
  argument to NULL.

- suffix:

  What character(s) go after the period indicator? If none, set this
  argument to NULL.

- begin:

  What is the label for the first period? Could be `1`, `"A"`, or
  anything that can be sequenced.

- end:

  What is the label for the final period? Could be `2`, `"B"`, or
  anything that can be sequenced and lies further along the sequence
  than the `begin` argument.

- id:

  The name of the ID variable as a string. If there is no ID variable,
  then this will be the name of the newly-created ID variable.

- wave:

  This will be the name of the newly-created wave variable.

- periods:

  If you period indicator does not lie in a sequence or is not
  understood by the function, then you can supply them as a vector
  instead. For instance, you could give `c("one","three","five")` if
  your variables are labeled `var_one`, `var_three`, and `var_five`.

- label_location:

  Where does the period label go on the variable? If the variables are
  labeled like `var_1`, `var_2`, etc., then it is `"end"`. If the labels
  are more like `A_var`, `B_var`, and so on, then it is `"beginning"`.

- as_panel_data:

  Should the return object be a
  [`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
  object? Default is TRUE.

- match:

  The regex that will match the part of the variable names other than
  the wave indicator. By default it will match any character any amount
  of times. Sometimes you might know that the variable names should
  start with a digit, for instance, and you might use `"\\d.*"` instead.

- use.regex:

  Should the `begin` and `end` arguments be treated as regular
  expressions? Default is FALSE.

- check.varying:

  Should the function check to make sure that every variable in the wide
  data with a wave indicator is actually time-varying? Default is TRUE,
  meaning that a constant like "race_W1" only measured in wave 1 will be
  defined in each wave in the long data. With very large datasets,
  however, sometimes setting this to FALSE can save memory.

## Value

Either a `data.frame` or `panel_data` frame.

## Details

There is no easy way to convert panel data from wide to long format
because the both formats are basically non-standard for other
applications. This function can handle the common case in which the wide
data frame has a regular labeling system for each period. The key thing
is providing enough information for the function to understand the
pattern.

In the end, this function calls
[`stats::reshape()`](https://rdrr.io/r/stats/reshape.html) but should be
easier to use and able to handle more situations, such as when the label
occurs at the beginning of the variable name. Also, just as important,
this function has built-in utilities to handle unbalanced data — when
variables occur more than once but every single period, which breaks
[`stats::reshape()`](https://rdrr.io/r/stats/reshape.html).

## See also

[`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md)

## Examples

``` r
## We need a wide data frame, so we will make one from the long-format 
## data included in the package.

# Convert WageData to panel_data object
wages <- panel_data(WageData, id = id, wave = t)
# Convert wages to wide format
wide_wages <- widen_panel(wages)

# Note: wide_wages has variables in the following format:
# var1_1, var1_2, var1_3, var2_1, var2_2, var2_3, etc.
if (FALSE) { # \dontrun{
long_wages <- long_panel(wide_wages, prefix = "_", begin = 1, end = 7,
                         id = "id", label_location = "end")
} # }
# Note that in this case, the prefix and label_location arguments are
# the defaults but are included just for clarity.

```
