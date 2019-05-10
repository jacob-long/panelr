
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/jacob-long/panelr.svg?branch=master)](https://travis-ci.org/jacob-long/panelr)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/jacob-long/panelr?branch=master&svg=true)](https://ci.appveyor.com/project/jacob-long/panelr)
[![Coverage
Status](https://img.shields.io/codecov/c/github/jacob-long/panelr/master.svg)](https://codecov.io/github/jacob-long/panelr?branch=master)

# panelr

This is an R package designed to aid in the analysis of panel data,
designs in which the same group of respondents/entities are
contacted/measured multiple times. `panelr` provides some useful
infrastructure, like a `panel_data` object class, as well as automating
some emerging methods for analyses of these data.

`wbm()` automates the “within-between” (also known as “between-within”
and “hybrid”) specification that combines the desirable aspects of both
fixed effects and random effects econometric models and fits them using
the `lme4` package in the backend. Bayesian estimation of these models
is supported by interfacing with the `brms` package (`wbm_stan()`) and
GEE estimation via `geepack` (`wbgee()`).

It also automates the fairly new “asymmetric effects” specification
described by [Allison
(2019)](http://journals.sagepub.com/doi/10.1177/2378023119826441) and
supports estimation via GLS for linear asymmetric effects models
(`asym()`) and via GEE for non-Gaussian models (`asym_gee()`).

## Installation

At the moment, `panelr` is only available through Github. A submission
to CRAN is coming soon.

``` r
install.packages("devtools")
devtools::install_github("jacob-long/panelr")
```

## Usage

### `panel_data` frames

While not strictly required, the best way to start is to declare your
data as panel data. I’ll load the example data `WageData` to
demonstrate.

``` r
library(panelr)
data("WageData")
colnames(WageData)
```

``` 
 [1] "exp"   "wks"   "occ"   "ind"   "south" "smsa"  "ms"    "fem"  
 [9] "union" "ed"    "blk"   "lwage" "t"     "id"   
```

The two key variables here are `t` and `id`. `t` is the wave of the
survey the row of the data refers to while `id` is the survey
respondent. This is a perfectly balanced data set, so there are 7
observations for each of the 595 respondents. We will use those two
pieces of information to create a `panel_data` object.

``` r
wages <- panel_data(WageData, id = id, wave = t)
wages
```

    # Panel data:    4,165 x 14
    # entities:      id [595]
    # wave variable: t [1, 2, 3, ... (7 waves)]
       id        t   exp   wks   occ   ind south  smsa    ms   fem union    ed
       <fct> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
     1 1         1     3    32     0     0     1     0     1     0     0     9
     2 1         2     4    43     0     0     1     0     1     0     0     9
     3 1         3     5    40     0     0     1     0     1     0     0     9
     4 1         4     6    39     0     0     1     0     1     0     0     9
     5 1         5     7    42     0     1     1     0     1     0     0     9
     6 1         6     8    35     0     1     1     0     1     0     0     9
     7 1         7     9    32     0     1     1     0     1     0     0     9
     8 2         1    30    34     1     0     0     0     1     0     0    11
     9 2         2    31    27     1     0     0     0     1     0     0    11
    10 2         3    32    33     1     1     0     0     1     0     1    11
    # ... with 4,155 more rows, and 2 more variables: blk <dbl>, lwage <dbl>

We have to tell `panel_data()` which column refers to the unique
identifiers for respondents/entities (the latter when you have something
like countries or companies instead of people) and which column refers
to the period/wave of data collection.

Note that the resulting `panel_data` object will remember which of the
columns is the ID column and which is the wave column. It will also
fight you a bit when you do things that might have the side effect of
dropping those columns or putting them out of time order. `panel_data`
frames are modified tibbles ([`tibble`
package](http://tibble.tidyverse.org/)) that are grouped by entity
(i.e., the ID column).

`panel_data` frames are meant to play nice with the
[`tidyverse`](tidyverse.org). Here’s a quick sample of how a tidy
workflow with `panelr` can work:

``` r
library(dplyr)
```

``` 

Attaching package: 'dplyr'
```

    The following objects are masked from 'package:stats':
    
        filter, lag

    The following objects are masked from 'package:base':
    
        intersect, setdiff, setequal, union

``` r
data("WageData")
# Create `panel_data` object
wages <- panel_data(WageData, id = id, wave = t) %>%
  # Pass to mutate, which will calculate statistics groupwise when appropriate
  mutate(
    wage = exp(lwage), # reverse transform the log wage variable
    mean_wage_individual = mean(wage), # means calculated separately by entity
    lag_wage = lag(wage) # mutate() will calculate lagged values correctly
  ) %>%
  # Use `panelr`'s complete_data() to filter for entities that have
  # enough observations
  complete_data(wage, union, min.waves = 5) %>% # drop if there aren't 5 completions
  # You can use unpanel() if you need to do rowwise or columnwise operations
  unpanel() %>%
  mutate(
    mean_wage_grand = mean(wage)
  ) %>%
  # You'll need to convert back to panel_data if you want to keep using panelr functions
  panel_data(id = id, wave = t)
```

### `wbm()` — the within-between model

Anyone can fit a within-between model without the use of this package as
it is just a particular specification of a multilevel model. With that
said, it’s something that will require some programming and could be
rather prone to error. In the best case, it is cumbersome and
inefficient to create the necessary variables.

`wbm()` is the primary model-fitting function that you’ll use from this
package and it fits within-between models for you, utilizing
[`lme4`](https://cran.r-project.org/web/packages/lme4/index.html) as a
backend for estimation.

A three-part model syntax is used that goes like this:

`dv ~ varying_variables | invariant_variables |
cross_level_interactions/random effects`

It works like a typical formula otherwise. The bars just tell `panelr`
how to treat the variables. Note also that you can specify random slopes
using `lme4`-style syntax in the third part of the formula as well. A
random intercept for the ID variable is included by default and doesn’t
need to be specified in the formula.

Lagged variables are supported as well through the `lag()` function.
Unlike base R, `panelr` lags the variables correctly — wave 1
observations will have NA values for the lagged variable rather than
taking the final wave value of the previous entity.

Here we will specify a model using the `wages` data. We will predict
logged wages (`lwage`) using two time-varying variables — lagged union
membership (`union`) and contemporaneous weeks worked (`wks`) — along
with a time-invariant predictor, a binary indicator for black race
(`blk`). For demonstrative purposes, we’ll fit a random slope for
`lag(union)` and a cross-level interaction between `blk` and `wks`.

``` r
model <- wbm(lwage ~ lag(union) + wks | blk | blk * wks + (lag(union) | id), data = wages)
summary(model)
```

    MODEL INFO:
    Entities: 595
    Time periods: 2-7
    Dependent variable: lwage
    Model type: Linear mixed effects
    Specification: within-between
    
    MODEL FIT:
    AIC = 1427.04, BIC = 1495.03
    Pseudo-R² (fixed effects) = 0.05
    Pseudo-R² (total) = 0.75
    Entity ICC = 0.73
    
    WITHIN EFFECTS:
    ---------------------------------------------------------
                        Est.   S.E.   t val.      d.f.      p
    ---------------- ------- ------ -------- --------- ------
    lag(union)          0.04   0.04     1.24     88.17   0.22
    wks                -0.00   0.00    -1.51   2948.04   0.13
    ---------------------------------------------------------
    
    BETWEEN EFFECTS:
    ---------------------------------------------------------------
                               Est.   S.E.   t val.     d.f.      p
    ----------------------- ------- ------ -------- -------- ------
    (Intercept)                6.20   0.24    25.89   571.97   0.00
    imean(lag(union))          0.03   0.04     0.72   593.27   0.47
    imean(wks)                 0.01   0.01     2.30   571.29   0.02
    blk                       -0.35   0.06    -5.65   591.87   0.00
    ---------------------------------------------------------------
    
    CROSS-LEVEL INTERACTIONS:
    ------------------------------------------------------
                     Est.   S.E.   t val.      d.f.      p
    ------------- ------- ------ -------- --------- ------
    wks:blk         -0.00   0.00    -1.06   2956.56   0.29
    ------------------------------------------------------
    
    p values calculated using Satterthwaite d.f.
     
    RANDOM EFFECTS:
    -------------------------------------
      Group      Parameter     Std. Dev. 
    ---------- -------------- -----------
        id      (Intercept)     0.3785   
        id       lag(union)      0.24    
     Residual                   0.2291   
    -------------------------------------

Note that `imean()` is an internal function that calculates the
individual-level mean, which represents the between-subjects effects of
the time-varying predictors. The within effects are the time-varying
predictors at the occasion level with the individual-level mean
subtracted. If you want the model specified such that the occasion level
predictors do not have the mean subtracted, use the `model =
"contextual"` argument. The “contextual” label refers to the way these
terms are normally interpreted when it is specified that way.

You may also use `model = "between"` to fit what econometricians call
the random effects model, which does not disaggregate the within- and
between-entity variation.

### `widen_panel()` and `long_panel()`

Two functions that should cover your bases for the tricky business of
**reshaping** panel data are included. Sometimes, like for doing
SEM-based analyses, you need your data in wide format — i.e., one row
per entity. `widen_panel()` makes that easy and should require minimal
trial and error or thinking.

Perhaps more often, your raw data are already in wide format and you
need to get it into long format to do cool stuff like use `wbm()`. That
can be very tricky, but `long_panel()` (I didn’t think
`lengthen_panel()` or `longen_panel()` quite worked as names) should
cover most situations. You tell it what the labels for periods are
(e.g., does it range from `1` to `5`, `"A"` to `"E"`, or something
else?), where they are located (before or after the variable’s name?),
and what kinds of formatting go before/after it. Check out the vignette
for more details and some worked examples.

## Contributing

I’m happy to receive bug reports, suggestions, questions, and (most of
all) contributions to fix problems and add features. I prefer you use
the Github issues system over trying to reach out to me in other ways.
Pull requests for contributions are encouraged.

Please note that this project is released with a [Contributor Code of
Conduct](CONDUCT.md). By participating in this project you agree to
abide by its terms.

## License

The source code of this package is licensed under the [MIT
License](http://opensource.org/licenses/mit-license.php).
