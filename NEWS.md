# panelr 0.5.1

New feature:
* `are_varying()` can now also assess individual-leve variation, so using 
the `type = "individual"` argument you can instead assess variables like age 
that vary over time but change equally for every case.

Bugfixes:
* You now can add the `wave` variable to `wbm()` in the formula without
running into cryptic errors.
* Fixed a problem in which transformed variables (like `lag(x)`) could not be
included as a user-specified random effect. Pre-0.5.0, these could be included
if they were surrounded by backticks, but now that hack is unnecessary and 
does not work.
* `make_wb_data()` is now updated to work with other internal updates 
introduced in 0.5.0.
* `long_panel()` was never really working right when the source data's labels
were located at the beginning (i.e., `label_location = "beginning"`). It is
now much more robust.


# panelr 0.5.0

Starting to polish things up for CRAN.

Key changes:

* `panel_data` frames now always place the `id` and `wave` columns first (in 
that order).
* `wbm()` can now handle time-varying factors appropriately. Do note that it
only uses treatment contrasts, however. (#8)
* There is a new function, `line_plot()`, to help you explore trends in data.
It's a little rough around the edges for now.
* Summaries of `wbm` objects are now a bit more streamlined and nice-looking.
* There are now `tidy()` and `glance()` methods (from the `broom` package) for
`wbm` objects. (#4)
* `as_panel_data()` is an alias for `panel_data()` when supplying a data frame
and an S3 method otherwise. It can be used to convert `pdata.frame` objects
from the `plm` package to `panel_data`.
* Formulas provided to `wbm()` are now converted to `Formula` objects to make
working with their multiple parts easier (see the `Formula` package for more 
info).
* There is now a `summary` method for `panel_data` frames, which works best
if you have `skimr` installed. You can use `dplyr::select()` style syntax
to select which variables you want to describe and choose to get descriptives
by wave and/or entity.

# panelr 0.4.1

This version has switched the default degrees of freedom calculation for
linear `wbm` models to Satterthwaite, which are more computationally efficient
and less prone to breaking R. They are also calculated on a per-variable basis. 
Kenward-Roger standard errors and degrees of freedom can be requested with
the `t.df = "Kenward-Roger"` argument.

# panelr 0.4.0 

This version includes some major under-the-hood changes, converting from an 
S3 object representation to S4. This allows the `wbm` objects to formally be
extensions of `merMod` objects, meaning any method that could apply to `wbm` but
isn't formally implemented will fall back to the `merMod` implementation.

The `panel_data` class no longer hardcodes the id and wave variables as "id"
and "wave". Instead, they remain whatever they are named and the `panelr`
functions will simply know which variables are these special ones.

A new function, `make_wb_data`, allows users to do the data prepping that
`wbm` does internally without having to use all the modeling choices made by
`wbm`.

# panelr 0.3.4

A series of helper functions have been added to make `wbm` objects behave
more like regular model objects. Now `update`, `formula`, `terms`, 
`model.frame`, `coef`, `predict`, and several more are defined for `wbm`.

The `summary` function for `wbm` has been refined and had some minor bugs 
squished.

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

