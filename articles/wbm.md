# Introduction to the \`panelr\` package

The `panelr` package contributes two categories of things:

1.  A `panel_data` object and some tools to create/manipulate them.
2.  A series of regression modeling functions for panel data.

## `panel_data` frames

Check out the other vignette for a lot of detail on how to take your raw
data and reshape it into a `panel_data` format. Here’s a short version,
using some example data provided by this package.

     [38;5;246m# A tibble: 1,151 × 28 [39m
          id  pov1 mother1 spouse1 inschool1 hours1  pov2 mother2 spouse2 inschool2
        [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m    [3m [38;5;246m<dbl> [39m [23m    [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m    [3m [38;5;246m<dbl> [39m [23m    [3m [38;5;246m<dbl> [39m [23m      [3m [38;5;246m<dbl> [39m [23m
     [38;5;250m 1 [39m    22     1       0       0         1     21     0       0       0         1
     [38;5;250m 2 [39m    75     0       0       0         1      8     0       0       0         1
     [38;5;250m 3 [39m    92     0       0       0         1     30     0       0       0         1
     [38;5;250m 4 [39m    96     0       0       0         0     19     1       1       0         0
     [38;5;250m 5 [39m   141     0       0       0         1      0     0       0       0         1
     [38;5;250m 6 [39m   161     0       0       0         1      0     0       0       0         1
     [38;5;250m 7 [39m   220     0       0       0         1      6     0       0       0         1
     [38;5;250m 8 [39m   229     0       0       0         1      0     1       0       0         1
     [38;5;250m 9 [39m   236     0       0       0         1      0     0       0       0         1
     [38;5;250m10 [39m   240     0       0       0         1     18     1       0       0         1
     [38;5;246m# ℹ 1,141 more rows [39m
     [38;5;246m# ℹ 18 more variables: hours2 <dbl>, pov3 <dbl>, mother3 <dbl>, spouse3 <dbl>, [39m
     [38;5;246m#   inschool3 <dbl>, hours3 <dbl>, pov4 <dbl>, mother4 <dbl>, spouse4 <dbl>, [39m
     [38;5;246m#   inschool4 <dbl>, hours4 <dbl>, age <dbl>, black <dbl>, pov5 <dbl>, [39m
     [38;5;246m#   mother5 <dbl>, spouse5 <dbl>, inschool5 <dbl>, hours5 <dbl> [39m

These data come from a subset of young women surveyed as part of the
National Longitudinal Survey of Youth starting in 1979. The
`teen_poverty` data come in “wide” format, meaning there is one row per
respondent and each of the repeated measures is in a separate column for
each wave.

We need to convert this to “long” format, in which you have one row for
each respondent in each wave of the 5-wave survey. We’ll use
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
for that.

``` r
teen <- long_panel(teen_poverty, begin = 1, end = 5, label_location = "end")
teen
```

     [38;5;246m# Panel data:    5,755 × 9 [39m
     [38;5;246m# Entities:      id [1,151] [39m
     [38;5;246m# Wave variable: wave [1, 2, 3, ... (5 waves)] [39m
       id     wave   age black   pov mother spouse inschool hours
        [3m [38;5;246m<fct> [39m [23m  [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m   [3m [38;5;246m<dbl> [39m [23m     [3m [38;5;246m<dbl> [39m [23m  [3m [38;5;246m<dbl> [39m [23m
     [38;5;250m 1 [39m 22        1    16     0     1      0      0        1    21
     [38;5;250m 2 [39m 22        2    16     0     0      0      0        1    15
     [38;5;250m 3 [39m 22        3    16     0     0      0      0        1     3
     [38;5;250m 4 [39m 22        4    16     0     0      0      0        1     0
     [38;5;250m 5 [39m 22        5    16     0     0      0      0        1     0
     [38;5;250m 6 [39m 75        1    17     0     0      0      0        1     8
     [38;5;250m 7 [39m 75        2    17     0     0      0      0        1     0
     [38;5;250m 8 [39m 75        3    17     0     0      0      0        1     0
     [38;5;250m 9 [39m 75        4    17     0     0      0      0        1     4
     [38;5;250m10 [39m 75        5    17     0     1      0      0        1     0
     [38;5;246m# ℹ 5,745 more rows [39m

Now we have a `panel_data` object! It is a special version of a
`tibble`, which is itself a special kind of `data.frame`. `panel_data`
objects work very hard to make sure you never accidentally drop the
variables that are the identifiers for each respondent and the
indicators for which wave the row corresponds to. `panel_data` objects
also try to stay in order by ID and wave.

Note that if your raw data are already in long format, you can use the
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md)
function to convert them to `panel_data` format.

``` r
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)
```

`panel_data` frames are designed to work with `tidyverse` packages,
particularly `dplyr`. When used inside
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
functions like
[`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html) work
properly by taking the previous value for the specific respondent. If
you ever need to do something that is easier to do with a “regular” data
frame, you can just use the
[`unpanel()`](https://panelr.jacob-long.com/reference/unpanel.md)
function to convert the `panel_data` frame back to normal.

## Regression models

### Within-between models

The original motivation to create this package was to automate the
process of fitting “within-between” models, sometimes called
“between-within” or “hybrid” models (see Allison, 2009; Bell & Jones,
2015). These combine the benefits of what econometricians call “fixed
effects” models — robustness to time-invariant confounding chief among
them — as well as what they call “random effects” models, which allow
the inclusion of time-invariant coefficients. Within-between models
include coefficients that are identical to the fixed effects equivalent,
but the flexibility to also include the random effects and other
time-invariant predictors (this was noticed by Mundlak, 1978). They are
fit via multilevel models which allow for some other nice possibilities
like inclusion of random slopes and generalized linear model
specifications.

From here, I’ll give a somewhat technical description of these models.
If you just want to look at how to estimate them in R, skip ahead to the
next mini-section.

Note that fixed effects models can be fit using individual demeaning.
That is, you can subtract the entity’s own mean for each predictor and
the dependent variable and fit a model via OLS that is equivalent to the
so-called least squares dummy variable approach (in which dummy
variables for every entity ID are included as predictors).

Let’s get a bit more technical. We have entities $i = 1,...,n$ who are
measured at times $t = 1,...,T$. We have as our dependent variable
$y_{it}$, the variable $y$ for individual $i$ at time $t$. We have
predictors that vary over time $x_{it}$, variables that do not vary over
time $z_{i}$, and variables we did not measure that do not vary over
time $\alpha_{i}$ as well as random error $\epsilon_{it}$

The fixed effects model, then, looks like this:

$$y_{it} = \mu_{t} + \beta_{1}x_{it} + \gamma z_{i} + \alpha_{i} + \epsilon_{it}$$

Although $\alpha_{i}$ is not observed, it can be estimated by including
a dummy variable for each $i$. The $\gamma$ is undefined because the
$z_{i}$ are perfectly collinear with the $\alpha_{i}$ dummy variables.

The individual-mean-centered version of the fixed effects models is
based on calculating a mean of $y$ and $x$ for each $i$ — so
$\bar{y_{i}}$ and $\bar{x_{i}}$ and subtracting it from each $y_{it}$
and $x_{it}$. The model can be expressed like this, including
$\bar{z_{i}}$ and $\bar{\alpha_{i}}$ for demonstration:

$$y_{it} - \bar{y_{i}} = \mu_{t} + \beta_{1}\left( x_{it} - \bar{x_{i}} \right) + \left( z_{i} - \bar{z_{i}} = 0 \right) + \left( \alpha_{i} - \bar{\alpha_{i}} = 0 \right) + \left( \epsilon_{it} - \bar{\epsilon_{i}} \right)$$

By de-meaning everything, all the time-invariant variables drop out:

$$y_{it} - \bar{y_{i}} = \mu_{t} + \beta_{1}\left( x_{it} - \bar{x_{i}} \right) + \left( \epsilon_{it} - \bar{\epsilon_{i}} \right)$$

This is often called the “within” estimator. You can take these
de-meaned variables and fit an OLS regression and get valid estimates
(with some adjustments to the standard errors).

You can also do something slightly different and get the same results
with multilevel models. Take this, for example:

$$y_{it} = \beta_{0i} + \beta_{1}\left( x_{it} - \bar{x_{i}} \right) + \left( \epsilon_{it} - \bar{\epsilon_{i}} \right)$$

Where $\beta_{0i}$ is a random intercept estimated for each $i$. This is
equivalent to subtracting $\bar{y_{i}}$ in terms of the estimation of
$\beta_{1}$. But in the multilevel modeling framework, we can include
those time-invariant $z_{i}$ as well. Conceptually, they are basically
being included in a model predicting $\beta_{0i}$:

$$\beta_{0i} = \beta_{0} + \gamma z_{i} + u_{0i}$$

Where $u_{0i}$ is the random error of the model predicting $\beta_{0i}$.

In fact, we can include the $\bar{x_{i}}$ in our multilevel model as
well and they are used just like the $z_{i}$:

$$\beta_{0i} = \beta_{0} + \beta_{2}\bar{x_{i}} + \gamma z_{i} + u_{0i}$$

Now we can substitute into the previous multilevel equation and we have
our within-between model:

$$y_{it} = \beta_{0} + \beta_{1}\left( x_{it} - \bar{x_{i}} \right) + \beta_{2}\bar{x_{i}} + \gamma z_{i} + u_{0i} + \epsilon_{it}$$

The $\beta_{1}$ has the same interpretation as in the fixed effects
model, these are the effects of within-entity deviations of $x$ on
within-entity deviations of $y$. The $\beta_{2}$ is basically predicting
the $\bar{y_{i}}$, however, so these coefficients are helpful for
predicting differences in mean levels across entities. The same is true
for the $z_{i}$.

A similar model that I call the “contextual” model because this is how
it is often interpreted (see, e.g., Raudenbush & Bryk, 2002). Here we do
not demean the $x_{i}$:

$$y_{it} = \beta_{0} + \beta_{1}x_{it} + \beta_{2}\bar{x_{i}} + \gamma z_{i} + u_{0i} + \epsilon_{it}$$

Believe it or not, the $\beta_{1}$ is unchanged in this model; it is the
$\beta_{2}$ that changes. The interpretation of $\beta_{2}$ becomes a
the *difference* between the within- and between-entities effects. A
significant coefficient for $\beta_{2}$ means significant differences
between the within- and between-entity effects. For those who are
familiar, this is like a variable-by-variable Hausman test.
Substantively, $\beta_{2}$ is often interpreted as a *contextual*
effect.

From this framework, we can do cross-level interactions, random slopes,
generalized linear models, and all kinds of interesting stuff.

#### A note on interactions

In the fixed effects framework, it is generally considered wrong to
operationalize an interaction between two time-varying variables (let’s
call them $w$ and $x$) by taking the product of their
individual-demeaned forms. That is, you are **not** supposed to generate
the interaction term $xw_{it}$ by doing this:

#### A note on splines and other basis expansions

[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) supports
common basis expansion functions in the time-varying part of the formula
that return multiple columns, including
[`splines::ns()`](https://rdrr.io/r/splines/ns.html),
[`splines::bs()`](https://rdrr.io/r/splines/bs.html), and
[`stats::poly()`](https://rdrr.io/r/stats/poly.html).

For a nonlinear term like `ns(exp, df = 3)`, `panelr` expands the term
into multiple within and between columns:

- **Within component**: compute spline bases on the within-person
  deviations `exp_it - expbar_i`, and then de-mean each resulting basis
  column within person (a double-demean analogue for nonlinear terms).
- **Between component**: compute spline bases on the person means
  `expbar_i`.

This approach avoids the error you would get if spline knots were chosen
within each person due to grouped
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).

``` r
library(panelr)
library(splines)
data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

mod_spline <- wbm(lwage ~ ns(exp, df = 3) | blk, data = wages)
mod_spline
```

    Linear mixed model fit by REML ['wblm']
    Formula: lwage ~ ns(exp, df = 3) | blk
       Data: wages
    REML criterion at convergence: -1586.533
    Random effects:
     Groups   Name        Std.Dev.
     id       (Intercept) 0.3668  
     Residual             0.1532  
    Number of obs: 4165, groups:  id, 595
    Fixed Effects:
    (Intercept)    ns_exp.w1    ns_exp.w2          blk    ns_exp.b1    ns_exp.b2  
        6.58202    -65.06281    -24.85345     -0.32938      0.49589     -0.00382  
      ns_exp.b3  
       -0.31440  
    fit warnings:
    fixed-effect model matrix is rank deficient so dropping 1 column / coefficient

Note: for [`bs()`](https://rdrr.io/r/splines/bs.html), after demeaning
some values may fall outside the original boundary knots, which can
produce a warning about ill-conditioned bases.

$$xw_{it} = \left( x_{it} - \bar{x_{i}} \right) \times \left( w_{it} - \bar{w_{i}} \right)$$

Instead, the conventional wisdom goes, you should first take the product
of the observed variables and subtract the individual-level mean of that
product, like so:

$$xw_{it} = x_{it}w_{it} - {\overline{xw}}_{i}$$

Where ${\overline{xw}}_{i}$ can also be expressed as
$\frac{\sum_{t = 1}^{T_{i}}{x_{it}w_{it}}}{T_{i}}$, the sum of all
products for each $i$ divided by the number of time points for each $i$,
$T_{i}$.

[Giesselmann and Schmidt-Catran
(2020)](https://doi.org/10.1177/0049124120914934) show that this
conventional method for generating $xw_{it}$ does not have the
unbiasedness that the individual terms do. I’ll leave it to them to
explain why exactly this is, but the solution is to start with the
first, wrong version of $xw_{it}$, which I’ll call $xw_{it}^{*}$, and
subtract *its* mean too:

\$\$ xw\_{it}^\* = (x\_{it} - \bar{x\_{i}}) \times (w\_{it} - \bar{w_i})
\\ xw\_{it} = xw\_{it}^\* - \overline{xw_i^\*} \$\$

I call this the “double-demeaning” approach to interactions, in contrast
to the one-time demeaning in the conventional approach. By default,
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) calculates
interactions via the double-demeaning method. You can change this via
the `interaction.style` argument if you need your results to match other
software.

### Fitting within-between models

The workhorse function for within-between models is
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md), which is
built on top of `lme4`’s `lmerMod()` and `glmerMod()`. It is not so hard
to understand how to treat your data to estimate within-between models,
but the programming can be a challenge to those who aren’t skilled with
R (or whatever else they might use) and is error-prone in any case.

The main thing to know in order to use
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md) is how the
model formula works, because it’s a little different from your typical
regression model. It is split into up to 3 parts, each for a different
kind of variable. Each part is separated by a `|`. The pattern is like
this:

    dependent ~ time_varying | time_invariant | cross_lev_interactions + (random_slopes | id)

So you start with your dependent variable on the left-hand side like
normal and then what comes next are variables that vary over time. You
will only get within-entity estimates for these variables. Next are
time-invariant variables; the between-entity terms for the time-varying
variables are added automatically so no need to try to include them
here. Finally, in the third part you can specify cross-level
interactions (i.e., within-entity by between-entity/time-invariant) as
well as additional random effects terms using the `lme4`-style syntax.
By default, `(1 | id)` (or whatever the ID variable is) is added
internally for a random intercept so you do not need to include it
yourself.

Let’s walk through an example with the `wages` data we looked at briefly
earlier. We’ll predict the logarithm of wages (`lwage`) using weeks
worked (`wks`), union membership (`union`), marital status (`ms`), blue
(vs. white) collar job status (`occ`), black race (`blk`), and female
sex (`fem`).

``` r
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mwithin-between

     [1mMODEL FIT: [22m
     [3mAIC =  [23m2036.78,  [3mBIC =  [23m2119.13
     [3mPseudo-R² (fixed effects) =  [23m0.27
     [3mPseudo-R² (total) =  [23m0.69 [3m
    Entity ICC =  [23m0.57

     [1mWITHIN EFFECTS:
     [22m----------------------------------------------------
                   Est.   S.E.   t val.      d.f.      p
    ----------- ------- ------ -------- --------- ------
    wks            0.00   0.00     1.06   3566.00   0.29
    union          0.06   0.03     2.53   3566.00   0.01
    ms            -0.08   0.03    -2.57   3566.00   0.01
    occ           -0.08   0.02    -3.32   3566.00   0.00
    ----------------------------------------------------

     [1mBETWEEN EFFECTS:
     [22m----------------------------------------------------------
                          Est.   S.E.   t val.     d.f.      p
    ------------------ ------- ------ -------- -------- ------
    (Intercept)           6.30   0.20    30.85   588.00   0.00
    imean(wks)            0.01   0.00     2.25   588.00   0.02
    imean(union)          0.15   0.03     4.67   588.00   0.00
    imean(ms)             0.17   0.05     3.07   588.00   0.00
    imean(occ)           -0.41   0.03   -13.31   588.00   0.00
    blk                  -0.15   0.05    -2.81   588.00   0.01
    fem                  -0.32   0.06    -4.96   588.00   0.00
    ----------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.2992   
     Residual                  0.2589   
    ------------------------------------

As you can see, the output distinguishes within- and between-entity
effects. When you see `imean()` around a variable, that is the
between-entity effect represented as the individual mean.

Here, we see there seems to be a wage penalty for switching from white
collar to blue collar work (`occ`) and although married people earn more
(`imean(ms)`), just becoming married (`ms`) coincides with a drop in
earnings. We also see a boost in earnings from joining a union
(`union`).

Maybe we think the timing of the marriage effect is off and the true
effect occurs the time period after a person becomes married. We can ask
for the lagged effect using
[`lag()`](https://dplyr.tidyverse.org/reference/lead-lag.html).

``` r
model <- wbm(lwage ~ wks + union + lag(ms) + occ | blk + fem, data = wages)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m2-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mwithin-between

     [1mMODEL FIT: [22m
     [3mAIC =  [23m1247.06,  [3mBIC =  [23m1327.41
     [3mPseudo-R² (fixed effects) =  [23m0.28
     [3mPseudo-R² (total) =  [23m0.74 [3m
    Entity ICC =  [23m0.64

     [1mWITHIN EFFECTS:
     [22m------------------------------------------------------
                     Est.   S.E.   t val.      d.f.      p
    ------------- ------- ------ -------- --------- ------
    wks             -0.00   0.00    -1.43   2999.92   0.15
    union            0.05   0.03     1.82   2991.78   0.07
    lag(ms)         -0.04   0.03    -1.19   2971.04   0.23
    occ             -0.06   0.02    -2.56   2992.05   0.01
    ------------------------------------------------------

     [1mBETWEEN EFFECTS:
     [22m------------------------------------------------------------
                            Est.   S.E.   t val.     d.f.      p
    -------------------- ------- ------ -------- -------- ------
    (Intercept)             6.36   0.21    30.51   588.00   0.00
    imean(wks)              0.01   0.00     2.25   588.00   0.02
    imean(union)            0.15   0.03     4.48   588.00   0.00
    imean(lag(ms))          0.15   0.06     2.80   588.01   0.01
    imean(occ)             -0.41   0.03   -13.16   588.02   0.00
    blk                    -0.15   0.05    -2.92   587.99   0.00
    fem                    -0.33   0.06    -5.17   588.01   0.00
    ------------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.3068   
     Residual                  0.2325   
    ------------------------------------

Well that doesn’t change the direction of the estimate, but it also
moved it sufficiently close to 0 that we can’t say much about it one way
or another.

Keep in mind that you do not have to stick to linear models. Using the
`family` argument (just like
[`glm()`](https://rdrr.io/r/stats/glm.html)), you can estimate logit
(`family = binomial`), probit (`family = binomal(link = "probit")`),
poisson (`family = poisson`), or other model families and links as
needed.

#### Growth curves

Now maybe we want to include an effect of time since wages tend to go up
for everyone, on average, over time. We can just include the time
variable in the formula or set `use.wave` to `TRUE`.

``` r
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages, use.wave = TRUE)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mwithin-between

     [1mMODEL FIT: [22m
     [3mAIC =  [23m-1688.87,  [3mBIC =  [23m-1600.19
     [3mPseudo-R² (fixed effects) =  [23m0.44
     [3mPseudo-R² (total) =  [23m0.89 [3m
    Entity ICC =  [23m0.8

     [1mWITHIN EFFECTS:
     [22m----------------------------------------------------
                   Est.   S.E.   t val.      d.f.      p
    ----------- ------- ------ -------- --------- ------
    wks            0.00   0.00     1.91   3565.00   0.06
    union          0.03   0.02     2.29   3565.00   0.02
    ms            -0.03   0.02    -1.71   3565.00   0.09
    occ           -0.03   0.01    -1.81   3565.00   0.07
    ----------------------------------------------------

     [1mBETWEEN EFFECTS:
     [22m-----------------------------------------------------------
                          Est.   S.E.   t val.      d.f.      p
    ------------------ ------- ------ -------- --------- ------
    (Intercept)           5.91   0.20    28.95    588.64   0.00
    imean(wks)            0.01   0.00     2.25    588.00   0.02
    imean(union)          0.15   0.03     4.67    588.00   0.00
    imean(ms)             0.17   0.05     3.07    588.00   0.00
    imean(occ)           -0.41   0.03   -13.31    588.00   0.00
    blk                  -0.15   0.05    -2.81    588.00   0.01
    fem                  -0.32   0.06    -4.96    588.00   0.00
    t                     0.10   0.00    81.29   3565.00   0.00
    -----------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.3094   
     Residual                  0.1533   
    ------------------------------------

Including `t` wipes out some of those previously observed effects.
Believe it or not, we just fit a growth curve model!

Now, we might think people have different trajectories. We can include
that as a random slope, which will go in the third part of the formula.

``` r
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem | (t | id), use.wave = TRUE, data = wages)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mwithin-between

     [1mMODEL FIT: [22m
     [3mAIC =  [23m-2064.42,  [3mBIC =  [23m-1963.07
     [3mPseudo-R² (fixed effects) =  [23m0.43
     [3mPseudo-R² (total) =  [23m0.92 [3m
    Entity ICC =  [23m0.84

     [1mWITHIN EFFECTS:
     [22m----------------------------------------------------
                   Est.   S.E.   t val.      d.f.      p
    ----------- ------- ------ -------- --------- ------
    wks            0.00   0.00     1.47   3498.40   0.14
    union          0.02   0.01     1.61   3561.26   0.11
    ms            -0.04   0.02    -1.97   3416.40   0.05
    occ           -0.02   0.01    -1.42   3563.73   0.16
    ----------------------------------------------------

     [1mBETWEEN EFFECTS:
     [22m----------------------------------------------------------
                          Est.   S.E.   t val.     d.f.      p
    ------------------ ------- ------ -------- -------- ------
    (Intercept)           5.93   0.20    29.69   588.53   0.00
    imean(wks)            0.01   0.00     2.12   587.97   0.03
    imean(union)          0.16   0.03     4.90   587.97   0.00
    imean(ms)             0.17   0.05     3.25   588.06   0.00
    imean(occ)           -0.39   0.03   -13.16   588.01   0.00
    blk                  -0.13   0.05    -2.49   588.00   0.01
    fem                  -0.31   0.06    -4.90   588.07   0.00
    t                     0.10   0.00    54.67   594.66   0.00
    ----------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.3057   
        id           t         0.03499  
     Residual                  0.1334   
    ------------------------------------

And now we have a latent growth curve model. The general effect on the
other coefficients is more uncertainty and attenuated estimates. It’s
worth keeping in mind that it is sometimes wrong to use a growth curve
model like this if you think the variables in your model *cause* the
time trend; if you think wages are going up because more people are
moving into white collar work, then including the growth curve will make
it harder for you to see the true effect of `occ`.

#### Contextual, within, and random effects specifications

By default, [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md)
does as the name suggests. But if you’d rather have the contextual model
described earlier, in which the means are not subtracted from the time
varying variables, that’s an option too.

``` r
model <- wbm(lwage ~ wks + union + ms + occ | blk + fem, data = wages, model = "contextual")
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mcontextual

     [1mMODEL FIT: [22m
     [3mAIC =  [23m2036.78,  [3mBIC =  [23m2119.13
     [3mPseudo-R² (fixed effects) =  [23m0.27
     [3mPseudo-R² (total) =  [23m0.69 [3m
    Entity ICC =  [23m0.57

     [1mWITHIN EFFECTS:
     [22m----------------------------------------------------
                   Est.   S.E.   t val.      d.f.      p
    ----------- ------- ------ -------- --------- ------
    wks            0.00   0.00     1.06   3566.00   0.29
    union          0.06   0.03     2.53   3566.00   0.01
    ms            -0.08   0.03    -2.57   3566.00   0.01
    occ           -0.08   0.02    -3.32   3566.00   0.00
    ----------------------------------------------------

     [1mCONTEXTUAL EFFECTS:
     [22m-----------------------------------------------------------
                          Est.   S.E.   t val.      d.f.      p
    ------------------ ------- ------ -------- --------- ------
    (Intercept)           6.30   0.20    30.85    588.00   0.00
    imean(wks)            0.01   0.00     1.93    660.18   0.05
    imean(union)          0.09   0.04     2.15   1411.72   0.03
    imean(ms)             0.25   0.06     3.95   1047.31   0.00
    imean(occ)           -0.33   0.04    -8.55   1401.80   0.00
    blk                  -0.15   0.05    -2.81    588.00   0.01
    fem                  -0.32   0.06    -4.96    588.00   0.00
    -----------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.2992   
     Residual                  0.2589   
    ------------------------------------

Now the individual means have a new interpretation as the difference in
effect compared to the within-entity estimates.

If you don’t want to use any of the time-invariant variables, you can
also just ask for the “within” estimator:

``` r
model <- wbm(lwage ~ wks + union + ms + occ, data = wages, model = "within")
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear mixed effects
     [3mSpecification:  [23mwithin

     [1mMODEL FIT: [22m
     [3mAIC =  [23m2266.01,  [3mBIC =  [23m2310.35
     [3mPseudo-R² (fixed effects) =  [23m0
     [3mPseudo-R² (total) =  [23m0.69 [3m
    Entity ICC =  [23m0.69

    ----------------------------------------------------------
                         Est.   S.E.   t val.      d.f.      p
    ----------------- ------- ------ -------- --------- ------
    (Intercept)          6.68   0.02   413.08    594.00   0.00
    wks                  0.00   0.00     1.06   3566.00   0.29
    union                0.06   0.03     2.53   3566.00   0.01
    ms                  -0.08   0.03    -2.57   3566.00   0.01
    occ                 -0.08   0.02    -3.32   3566.00   0.00
    ----------------------------------------------------------

     [3mp values calculated using Satterthwaite d.f.
     [23m 
     [1mRANDOM EFFECTS:
     [22m------------------------------------
      Group      Parameter    Std. Dev. 
    ---------- ------------- -----------
        id      (Intercept)    0.3819   
     Residual                  0.2589   
    ------------------------------------

This can help declutter your output when you really just don’t care
about the between-subjects effects.

### Using GEE to fit within-between models

You don’t have to estimate these models using multilevel models and in
fact you may get better inferences by avoiding some of the assumptions
inherent to multilevel modeling (see McNeish, 2019). You can use the
semiparametric generalized estimating equations (GEE) approach to
estimation, with the main tradeoff being that you can no longer use
random slopes or anything like that. But if you only care about the
average effects across all entities, GEE can be a better approach that
doesn’t require you to be right about the distribution of effects and
several other assumptions.

[`wbgee()`](https://panelr.jacob-long.com/reference/wbgee.md) builds on
`geeglm()` from the `geepack` package and works just like
[`wbm()`](https://panelr.jacob-long.com/reference/wbm.md).

``` r
model <- wbgee(lwage ~ wks + union + ms + occ | blk + fem, data = wages)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m1-7
     [3mDependent variable:  [23mlwage
     [3mModel type: [23m Linear GEE
     [3mVariance:  [23mar1 (alpha = 0.79)
     [3mSpecification:  [23mwithin-between

     [1mMODEL FIT: [22m
     [3mQIC =  [23m672.19,  [3mQICu =  [23m669.6,  [3mCIC =  [23m12.29

     [1mWITHIN EFFECTS:
     [22m------------------------------------------
                   Est.   S.E.   z val.      p
    ----------- ------- ------ -------- ------
    wks            0.00   0.00     0.07   0.94
    union          0.03   0.02     1.32   0.19
    ms            -0.08   0.03    -2.78   0.01
    occ           -0.03   0.02    -1.64   0.10
    ------------------------------------------

     [1mBETWEEN EFFECTS:
     [22m-------------------------------------------------
                          Est.   S.E.   z val.      p
    ------------------ ------- ------ -------- ------
    (Intercept)           6.29   0.22    28.48   0.00
    imean(wks)            0.01   0.00     2.09   0.04
    imean(union)          0.16   0.03     4.74   0.00
    imean(ms)             0.17   0.06     3.01   0.00
    imean(occ)           -0.41   0.03   -12.95   0.00
    blk                  -0.14   0.05    -2.90   0.00
    fem                  -0.30   0.06    -4.89   0.00
    -------------------------------------------------

This gives us more conservative estimates, in general. Note that by
default, [`wbgee()`](https://panelr.jacob-long.com/reference/wbgee.md)
uses an AR-1 working error correlation structure in estimation. This
makes sense in general but at times it may make sense to use
“exchangeable” as the argument to `cor.str` which assumes all
within-entity correlations are equal regardless of time lag. Other
options include “unstructured”, which can be very computationally
intensive, and “independence,” assuming no correlation within entities.

Like [`wbm()`](https://panelr.jacob-long.com/reference/wbm.md), you can
do generalized linear models via the `family` argument. It is for these
generalized linear models that GEEs are likely to stand out the most in
terms of added benefit above and beyond the multilevel models, although
this is not a well-tested question to my knowledge.

### Asymmetric effects

Sometimes, theory may suggest that increases in a variable have a
different effect than decreases in a variable. For instance, getting
married and getting divorced are probably not equivalent (in the sense
that one is the exact opposite of the other) in their effects on other
outcomes. Allison (2019) described a method for estimating models with
asymmetric effects based on first differences.

First, you take first differences:

$$y_{it} - y_{it - 1} = \left( \mu_{t} - \mu_{t - 1} \right) + \beta\left( x_{it} - x_{it - 1} \right) + \left( \epsilon_{it} - \epsilon_{it - 1} \right)$$

We need a slightly different model for asymmetric effects in which we
decompose the differences into positive and negative variables.

Our asymmetric effects model will be:

$$y_{it} - y_{it - 1} = \left( \mu_{t} - \mu_{t - 1} \right) + \beta^{+}x_{it}^{+} + \beta^{-}x_{it}^{-} + \left( \epsilon_{it} - \epsilon_{it - 1} \right)$$

Where

\$\$ x\_{it}^+ = x\_{it} - x\_{it -1} \text{ if } (x\_{it} - x\_{it -1})
\> 0, \text{otherwise } 0 \\ x\_{it}^- = -(x\_{it} - x\_{it -1}) \text{
if } (x\_{it} - x\_{it -1}) \< 0, \text{otherwise } 0 \$\$

In other words, if the difference is positive, it becomes part of the
$x_{it}^{+}$ and if it is negative, it is multiplied by -1 to be made
positive and is made part of the $x_{it}^{-}$ variable. If the effects
are symmetric, $\beta^{+} = - \beta^{-}$.

After fitting the model via GLS, we can then do a test of the contrasts
of the $\beta^{+}$ and $\beta^{-}$ coefficients as a formal way to
assess the presence of asymmetric effects.

Here’s how it works with the `panelr` function,
[`asym()`](https://panelr.jacob-long.com/reference/asym.md).

``` r
model <- asym(lwage ~ ms + occ + union + wks, data = wages)
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m595
     [3mTime periods:  [23m2-7
     [3mDependent variable:  [23mlwage
     [3mModel type:  [23mLinear asymmetric effects (first differences)
     [3mVariance structure:  [23mtoeplitz-1 (theta = -0.44) 

     [3mStandard errors: [23m Cluster-robust (CR2) 
    ------------------------------------------------
                         Est.   S.E.   t val.      p
    ----------------- ------- ------ -------- ------
    (Intercept)          0.10   0.00    41.12   0.00
    +ms                 -0.04   0.02    -1.95   0.05
    -ms                  0.04   0.04     1.23   0.22
    +occ                -0.02   0.02    -0.97   0.33
    -occ                 0.03   0.02     1.17   0.24
    +union               0.01   0.02     0.64   0.52
    -union              -0.03   0.03    -1.11   0.27
    +wks                 0.00   0.00     0.48   0.63
    -wks                -0.00   0.00    -0.35   0.72
    ------------------------------------------------

     [1mTests of asymmetric effects:
     [22m--------------------------
                  chi^2      p
    ----------- ------- ------
    ms             0.01   0.92
    occ            0.19   0.66
    union          0.44   0.51
    wks            0.00   1.00
    --------------------------

As you can see, in a model comparable to our within-between model from
earlier, the effects seem quite symmetric.

Let’s look at the `teen` data from earlier, where `spouse` indicates
whether the respondent is living with a spouse, `inschool` indicates
whether the respondent is enrolled in school, and `hours` is the hours
worked in the week of the survey.

``` r
summary(asym(hours ~ spouse + inschool, data = teen))
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m1151
     [3mTime periods:  [23m2-5
     [3mDependent variable:  [23mhours
     [3mModel type:  [23mLinear asymmetric effects (first differences)
     [3mVariance structure:  [23mtoeplitz-1 (theta = -0.54) 

     [3mStandard errors: [23m Cluster-robust (CR2) 
    ------------------------------------------------
                         Est.   S.E.   t val.      p
    ----------------- ------- ------ -------- ------
    (Intercept)          1.16   0.15     7.74   0.00
    +spouse             -4.71   1.15    -4.09   0.00
    -spouse             -0.61   2.20    -0.28   0.78
    +inschool           -5.65   1.29    -4.38   0.00
    -inschool            7.66   0.69    11.09   0.00
    ------------------------------------------------

     [1mTests of asymmetric effects:
     [22m-----------------------------
                     chi^2      p
    -------------- ------- ------
    spouse            6.21   0.01
    inschool          2.14   0.14
    -----------------------------

Here we see an asymmetric effect of marriage: gaining a spouse
corresponds with fewer hours worked, but there’s no effect on work hours
when a spouse is lost. You can see in the lower table that this
difference in coefficients is associated with a fairly low *p* value.
There is only weak evidence of an asymmetric effect for entering/leaving
school.

#### Asymmetric effects for generalized linear models

The downside to the first differences method is that it does not
generalize to non-continuous dependent variables — you can’t run a logit
model with a differenced binary outcome. Allison (2019) showed that you
can do a modified form for such situations.

Instead of including the $x_{it}^{+}$ and $x_{it}^{-}$ as predictors,
you instead create new variables $z_{it}^{+}$ and $z_{it}^{-}$ that are
the cumulative sum of all differences prior to time $t$.

\$\$ z\_{it}^+ = \sum\_{s = 1}^{t}{x\_{is}^+} \\ z\_{it}^- = \sum\_{s =
1}^{t}{x\_{is}^-} \\ \$\$

Note that at $t = 1$, both are set to 0. I’ll leave the details as to
*why* this works to the manuscript, but he shows that we’re left with
the following equation:

$$y_{it} = \mu_{t} + \beta^{+}z_{it}^{+} + \beta^{-}z_{it}^{-} + \alpha_{i} + \epsilon_{it}$$

So we can treat this like a fixed effects model in which we just need to
address the $\alpha_{i}$. For situations like this that call for a
conditional logit, as Allison used in his paper, another option is the
GEE with logit link.

Let’s try with the `teen` data, which also appears in Allison (2019).
Here our outcome variable is `pov`, poverty, and there’s a new
predictor, `mother`, an indicator for whether the respondent has ever
had any children.

``` r
model <- asym_gee(pov ~ mother + spouse + inschool + hours, data = teen, family = binomial(link = "logit"), 
                  use.wave = TRUE, wave.factor = TRUE)
```

     [36mmother does not decrease over time so -mother is not included in the
    model. [39m

``` r
summary(model)
```

     [1mMODEL INFO:
     [22m [3mEntities:  [23m1151
     [3mTime periods:  [23m2-5
     [3mDependent variable:  [23mpov
     [3mModel family:  [23mbinomial,  [3mLink:  [23mlogit
     [3mVariance:  [23mar1 (alpha = 0.33)
     [3mSpecification:  [23mAsymmetric effects (via GEE)

     [1mMODEL FIT: [22m
     [3mQIC =  [23m5898.64,  [3mQICu =  [23m5897.59,  [3mCIC =  [23m11.52

    ------------------------------------------------
                         Est.   S.E.   z val.      p
    ----------------- ------- ------ -------- ------
    (Intercept)         -0.37   0.06    -5.78   0.00
    +mother              0.72   0.11     6.63   0.00
    +spouse             -0.70   0.14    -5.14   0.00
    -spouse              0.43   0.25     1.71   0.09
    +inschool           -0.02   0.16    -0.15   0.88
    -inschool           -0.01   0.09    -0.09   0.93
    +hours              -0.02   0.00    -8.17   0.00
    -hours               0.01   0.00     1.61   0.11
    wave3                0.01   0.08     0.09   0.93
    wave4                0.12   0.08     1.48   0.14
    wave5                0.13   0.09     1.37   0.17
    ------------------------------------------------

     [1mTests of asymmetric effects:
     [22m-----------------------------
                     chi^2      p
    -------------- ------- ------
    spouse            1.10   0.29
    inschool          0.04   0.85
    hours            25.57   0.00
    -----------------------------

The results are broadly similar in terms of coefficient estimates to
those obtained by Allison. Unlike Allison, we do not have good evidence
of an asymmetric effect in the case of `spouse` but we do have one in
the case of `hours`. Note that `mother` never goes down so the negative
version of this variable is dropped from the model with a message. To
match Allison, I also used `use.wave` to include the wave variable and
`wave.factor` to make it a factor variable.

## References

Allison, P. D. (2009). Fixed effects regression models. Thousand Oaks,
CA: SAGE Publications. <https://doi.org/10.4135/9781412993869.d33>

Allison, P. D. (2019). Asymmetric fixed-effects models for panel data.
*Socius*, *5*, 1–12. <https://doi.org/10.1177/2378023119826441>

Bell, A., & Jones, K. (2015). Explaining fixed effects: Random effects
modeling of time-series cross-sectional and panel data. *Political
Science Research and Methods*, *3*, 133–153.
<https://doi.org/10.1017/psrm.2014.7>

Giesselmann, M., & Schmidt-Catran, A. W. (2020). Interactions in fixed
effects regression models. *Sociological Methods & Research*, 1–28.
<https://doi.org/10.1177/0049124120914934>

McNeish, D. (2019). Effect partitioning in cross-sectionally clustered
data without multilevel models. *Multivariate Behavioral Research*,
Advance online publication.
<https://doi.org/10.1080/00273171.2019.1602504>
