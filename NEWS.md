# panelr 1.0.0.9000

This is a point release, albeit without breaking changes. There are numerous
long-delayed improvements to performance as well as some new features.

## Upgrades

* **vctrs integration**: Added vctrs support for `panel_data` objects. This
  improves attribute preservation during tidyverse operations and provides
  better type-safe coercion. New methods include `vec_restore.panel_data`,
  `vec_proxy.panel_data`, `vec_ptype2`, and `vec_cast` methods.

* **Panel balancing functions**: Three new functions help work with unbalanced
  panel data:
  - `has_gaps()`: Check if panel data has implicit missing rows
  - `scan_gaps()`: Identify which entity-wave combinations are missing
  - `balance_panel()`: Make implicit gaps explicit by adding NA rows for
    missing entity-wave combinations. Optionally fill with custom values.

* `wbm()` now supports matrix-returning basis expansion terms in the time-varying
  part of the formula, including `splines::ns()`, `splines::bs()`, and
  `stats::poly()`. These terms are expanded into within- and between-person
  components during data preparation. ([#36](https://github.com/jacob-long/panelr/issues/36))

## Internal improvements

* New `build_panel_data()` internal helper provides fast reconstruction of
  `panel_data` objects without full re-validation, improving performance of
  operations that preserve panel structure (~8x faster than full reconstruction).

* New `is_panel_sorted()` internal function provides O(n) validation that data
  is properly sorted by id then wave, avoiding unnecessary O(n log n) sorting.
  If data is out of order, it is automatically re-sorted.

* Updated `reconstruct.panel_data()` to use the lightweight `build_panel_data()`
  helper for faster attribute restoration.

* Removed deprecated dplyr method overrides (`arrange_`, `mutate_`, `summarise_`,
  `summarize_`, `slice_`). These were deprecated in dplyr 0.7.0 (2017) and are
  no longer needed.

* Refactored interaction effects processing in `wbm()` and related functions.
The scattered boolean flags (`demean.ints`, `old.ints`, `detrend`) are now
encapsulated in an `InteractionConfig` object for cleaner conditional logic.

* Added `WBFormula` S3 class for structured representation of parsed formulas
(groundwork for future improvements).


## Other changes

* Several improvements to formula parsing in the modeling functions. It is
now possible to have random effects for factors, have multiple, non-correlated
random effects, random effects for which there are no fixed effects, and several
edge cases relating to non-syntactic variables have been corrected.
([#56](https://github.com/jacob-long/panelr/issues/56),
 [#54](https://github.com/jacob-long/panelr/issues/54))
 * Models should now be compatible with `sim_slopes()` and `johnson_neyman()`
 from the `interactions` package. Note that this compatibility is pending an
 update to `jtools`.
 ([#57](https://github.com/jacob-long/panelr/issues/57))
 * `long_panel()` has been substantially sped up. ([#51](https://github.com/jacob-long/panelr/issues/51))
 
 
# panelr 0.7.8

* Includes a back-end update for continued compatibility with `tibble`. (#55)
* Fixes a testing issue that caused the package's removal from CRAN.

# panelr 0.7.7

* Includes back-end changes for compatibility with recent updates to `skimr`,  `dplyr`, and `purrr`. Users should not notice any change in behavior from these.
* When users do not want `long_panel()` to check whether values are varying,
it no longer performs a series of operations that are rendered unnecessary. This
will slightly speed up performance when `check.varying = FALSE`. (#44)
* Very small performance improvement when using `long_panel()` and 
`are_varying()` functions.

# panelr 0.7.6

Compatibility update for upcoming changes to the `clubSandwich`. Thanks to 
James Pustejovksy for submitting the necessary fixes.

# panelr 0.7.5

Re-release: There are no changes, but `panelr` was removed from CRAN because
one of the packages it depended on had also been removed. That package is 
now back on CRAN, so `panelr` will return as well.

# panelr 0.7.4

Re-release: There are no changes, but `panelr` was removed from CRAN because
one of the packages it depended on had also been removed. That package is 
now back on CRAN, so `panelr` will return as well.

# panelr 0.7.3

Bugfixes:

* Fixes several problems induced by recent `dplyr` updates. (#28, #29)
* Allow dates to serve as the `wave` variable.
* Fixes issues related to upcoming update to the `broom` package. (#30)
* Updated citation to Giesselmann and Schmidt-Catran's (2020) article,
no longer just a working paper. Thanks to Marco for letting me know.

# panelr 0.7.2

Bugfix:

* `long_panel()` now handles numeric waves correctly when the input data are
unbalanced.
* Fixed bug related to changes in `brms` package's interface for autocorrelated
errors.
* Eliminated warning from update to `tidyr` package.

# panelr 0.7.1

Bugfixes: 

* Multi-part random effects specification is supported in `wbm()` (#14; thanks
@strengejacke).
* Improved support for labelled data (#12).
* Conversion from `pdata.frame` to `panel_data` has been fixed.
* Added `interaction.style` argument to `make_wb_data()`.
* The behavior `predict.wbm()` and `predict.wbgee()` has been improved. Notably,
the DV does not need to be included in `newdata` and the ID variable is only
required when necessary.
* Fixed error with custom random effect and interaction terms (#18).

# panelr 0.7.0

Lots of new stuff! CRAN coming soon as well.

* `wbgee()` works just like `wbm()`, except uses GEE (via the `geepack` package)
for estimation. This can give you more trustworthy results under some 
circumstances and is much less likely to have convergence problems.
* `fdm()` estimates first differences models via GLS (from the `nlme` package).
* `asym()` estimates the linear asymmetric effects model described by 
[Allison (2019)](https://journals.sagepub.com/doi/10.1177/2378023119826441)
via first differences.
* `asym_gee()` estimates a similar asymmetric effects model to the one using
cumulative differences described in Allison (2019), but using GEE rather than
conditional logit. 
* `heise()` produces stability and reliability estimates via the popular 
method described in Heise (1969).
* Two new datasets have been added (`nlsy` and `teen_poverty`).
* A new vignette gives a relatively detailed discussion of the models
implemented in the package.

# panelr 0.6.0

New stuff:
* There is now a vignette to walk users through the process of reshaping panel
data.
* There is now more sophisticated handling of interactions between time-varying
variables in line with the recommendations of [Giesselmann and Schmidt-Catran
(2018)](https://ideas.repec.org/p/diw/diwwpp/dp1748.html).
* `are_varying()` can now also assess individual-level variation, so using 
the `type = "individual"` argument you can instead assess variables like age 
that vary over time but change equally for every case.
* `wbm()` can now handle transformed dependent variables (e.g. `log(y)`). 
Transformations on the right-hand side of the equation were always supported.
* `panel_data` objects are now quite a bit more difficult to break by 
accidentally subsetting the ID and wave columns out of existence. Now, 
subsetting via `data[]`, `select()` and implicitly via `transmute()` will 
never remove the ID and wave columns. You will also be warned if you `arrange()`
a `panel_data` object since it will generally break `lag()` functions.
* `panel_data` objects now store information about what the periods are for
the data, which you can access with the `get_periods()` function. For example,
if the waves in your data are the numbers 1 through 7, that's what you'll get.
This is more useful when the periods are irregular, such as if the waves are
the years of a biennial survey.

Bugfixes:
* The way lagged predictors are mean-centered is now consistent with the 
conventional fixed effects estimator. Results may change non-trivially
due to this change. Previously, the mean used for mean-centering was based on
all waves of data, but now it is based on all waves except the number of lags
away from the latest wave. 
* Detrending has also been tweaked to work comparably with the changes to 
the mean-centering.
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
* `wbm()`'s `wave.factor` argument had become non-functional for some time but
is now fixed.

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
