# panelr 0.3.3

More tweaks to `widen_panel`, giving users the option to opt out of the 
feature introduced in `0.3.2` that stores data about varying and constant
variables from `long_panel`. Since poor data labeling in the original wide
data can cause those stored attributes to be wrong, users can use 
`ignore.attributes = TRUE` with `widen_panel` to force checking for varying
variables with `are_varying`. Users can now also supply a vector of varying
variables, similar to `reshape` in base R.

# panelr 0.3.2

This small update adds an enhancement to `long_panel` and `widen_panel`. If
you start with wide data, convert it to long format, and then want to convert
back to wide, the `panel_data` object in long format will cache information
about the variables to drastically speed up `widen_panel` when you run it again.

Additionally, `are_varying` was sped up by about 50%, though it slows 
`widen_panel` down for data with many variables.

# panelr 0.3.1

Tiny bugfixes:

* `long_panel` would error when supplied a `tibble` rather than a base 
`data.frame`.
* There were namespace issues related to the `magrittr` operators used 
internally.

# panelr 0.3.0

New functions:

* `widen_panel` converts your `panel_data` object to wide format, with one row 
per entity. This can be useful for SEM analysis and some other things.
* `long_panel` does a much more difficult thing, which is convert wide-formatted
data to the more conventional long panel data format. It contains several 
means for parsing the variable names of the wide formatted data to produce
a sensible long data frame with all the time-variant variables accounted for
properly. Unlike `reshape`, it can deal with unbalanced data.
* `are_varying` is a function that can let you check whether variables in 
long-formatted panel data vary over time or not.

# panelr 0.2.0

New feature:

* `detrend` and `balance_correction` arguments were added to `wbm` to implement
the procedures described in Curran and Bauer (2011). These, respectively,
account for over-time trends in the predictors and correcting between-subject
effects when panels are unbalanced.

# panelr 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Added infrastructure for CRAN submission.
* Improved documentation and added references.
* Added README.

# panelr 0.1.0

* Got things working such that it can be shared outside the maintainer's own
computers.
* Added WageData example, documentation, etc.
* Unit testing and automated tests through Travis and Appveyor.

