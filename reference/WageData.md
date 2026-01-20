# Earnings data from the Panel Study of Income Dynamics

These data come from the years 1976-1982 in the Panel Study of Income
Dynamics (PSID), with information about the demographics and earnings of
595 individuals.

## Usage

``` r
WageData
```

## Format

A data frame with 4165 rows and 14 variables:

- id:

  Unique identifier for each survey respondent

- t:

  A number corresponding to each wave of the survey, 1 through 7

- wks:

  Weeks worked in the past year

- lwage:

  Natural logarithm of earnings in the past year

- union:

  Binary indicator whether respondent is a member of union (1 = union
  member)

- ms:

  Binary indicator for whether respondent is married (1 = married)

- occ:

  Binary indicator for whether respondent is a blue collar (= 0) or
  white collar (= 1) worker.

- ind:

  Binary indicator for whether respondent works in manufacturing (= 1)

- south:

  Binary indicator for whether respondent lives in the South (= 1)

- smsa:

  Binary indicator for whether respondent lives in a standard
  metropolitan area (SMSA; = 1)

- fem:

  Binary indicator for whether respondent is female (= 1)

- blk:

  Binary indicator for whether respondent is African-American (= 1)

- ed:

  Years of education

- exp:

  Years in the workforce.

## Source

These data are all over the place. This particular file was downloaded
from Richard Williams at
<https://www3.nd.edu/~rwilliam/statafiles/wages.dta>, though he doesn't
claim ownership of these data.

The data were shared as a supplement to Baltagi (2005) at
<https://www.wiley.com/legacy/wileychi/baltagi3e/data_sets.html>.

They were also shared as a supplement to Greene (2008) at
<https://pages.stern.nyu.edu/~wgreene/Text/Edition6/tablelist6.htm>.

The data are also available in numerous other locations, including in
slightly different formats as
[`Wages`](https://rdrr.io/pkg/plm/man/Wages.html) in the plm package and
[`PSID7682`](https://rdrr.io/pkg/AER/man/PSID7682.html) in the `AER`
package.
