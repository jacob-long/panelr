# WBFormula class for within-between model formula representation

S3 class that represents a parsed within-between formula. This provides
a structured intermediate representation between the user-specified
formula and the final lme4 formula.

Constructor for the WBFormula S3 class.

## Usage

``` r
WBFormula(
  raw_formula,
  dv,
  varying = character(0),
  constants = character(0),
  v_info = NULL,
  wint_labs = NULL,
  cint_labs = NULL,
  bint_labs = NULL,
  ranefs = NULL,
  data = NULL,
  allvars = NULL,
  conds = NULL,
  matrix_terms = NULL
)
```

## Arguments

- raw_formula:

  The original Formula object

- dv:

  The dependent variable name

- varying:

  Character vector of time-varying predictor terms

- constants:

  Character vector of time-invariant predictor terms

- v_info:

  Tibble with columns: term, root, lag, meanvar

- wint_labs:

  Character vector of within x within interaction labels

- cint_labs:

  Character vector of cross-level interaction labels

- bint_labs:

  Character vector of between x between interaction labels

- ranefs:

  Character vector of random effects specifications

- data:

  The data frame (with any expanded factors)

- allvars:

  Character vector of all variables needed (passed from parser)

- conds:

  Integer number of formula conditions/parts

- matrix_terms:

  Optional list of metadata for matrix-returning terms detected in the
  varying part of the formula (e.g.,
  [`splines::ns()`](https://rdrr.io/r/splines/ns.html),
  [`splines::bs()`](https://rdrr.io/r/splines/bs.html),
  [`stats::poly()`](https://rdrr.io/r/stats/poly.html)). These terms are
  expanded into within- and between- component columns during data
  preparation.

## Value

A WBFormula S3 object
