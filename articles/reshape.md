# Reshaping panel data with \`long_panel()\` and \`widen_panel()\`

One of the initial challenges a data analyst is likely to face with
panel data is getting it into a format suitable for analysis. Most
regression analyses for panel data require the data to be in **long**
format. That means there is a row for each entity (e.g., person) at each
time point. If I conducted a 3-wave panel survey of 300 people, each of
whom responded to all 3 waves, the long format of these data would have
900 rows (300 respondents x 3 waves).

For example, the following is how long data look, where `id` is the
identifier for each entity, `wave` is the indicator of the time point,
and `Q1`/`Q2` are measures repeated at each time point.

| id  | wave |   Q1 |   Q2 |
|:----|-----:|-----:|-----:|
| 1   |    1 |  1.0 |  5.0 |
| 1   |    2 |  1.5 |  7.5 |
| 1   |    3 |  2.0 | 10.0 |
| 2   |    1 |  5.0 | 14.0 |
| 2   |    2 |  4.0 | 10.5 |
| 2   |    3 |  3.0 |  7.0 |
| 3   |    1 | 15.0 |  8.0 |
| 3   |    2 | 12.0 | 12.0 |
| 3   |    3 |  9.0 | 16.0 |

**Wide** data, on the other hand, have only one row per entity and a
separate column for each measure and time point. The same data above in
wide format look like this:

| id  | Q1_W1 | Q1_W2 | Q1_W3 | Q2_W1 | Q2_W2 | Q2_W3 |
|:----|------:|------:|------:|------:|------:|------:|
| 1   |     1 |   1.5 |     2 |     5 |   7.5 |    10 |
| 2   |     5 |   4.0 |     3 |    14 |  10.5 |     7 |
| 3   |    15 |  12.0 |     9 |     8 |  12.0 |    16 |

Here you differentiate between waves by looking at the column name,
which in this case end in “\_W” and then the wave indicator. Some
analyses prefer the data in this format, like structural equation
models.

`panelr` considers the native format of panel data to be *long* and
provides the `panel_data` class to keep your data tidy in the long
format. Of course, sometimes your raw data aren’t in long format and
need to be “reshaped” from wide to long. In other cases, you have long
format data but need to get it into wide format for some reason or
another. `panelr` provides tools to help with these situations.

There are some other tools, including ones that `panelr` uses
internally, that can manage these situations. However, they tend to be
some combination of confusing, inflexible, or too general to be easily
used for these purposes by non-experts.

## From wide to long

In my experience, survey contractors (i.e., the people you pay to carry
out panel surveys) like to provide the data in wide format. As a general
rule, the conversion of data from wide to long is much more difficult
than the inverse. When preparing to reshape data from wide to long
format, you’ll need to answer some questions relating to how the
column/variable names distinguish the variable name from the time
indicator:

- What are the time indicators: numbers, letters, something else?
- Are the time labels at the beginning or end of the column name?
- Are there prefixes or suffixes surrounding the time indicator (e.g., a
  name like `W1_variable` has both prefix (`W`) and suffix (`_`)).

One key assumption is that variables labeled with a pattern such as
`Q1_W1`, `Q1_W2`, and so on refer to the *same measure* at different
times. I’ve encountered datasets in which `Q1` might refer to a
different measure at each time point and this is not a problem that can
be handled in an automated way.

With that warning out of the way, let’s look at a couple examples.

### Wave indicators at the end of variable names

Let’s return to the wide data we looked at earlier.

| id  | Q1_W1 | Q1_W2 | Q1_W3 | Q2_W1 | Q2_W2 | Q2_W3 |
|:----|------:|------:|------:|------:|------:|------:|
| 1   |     1 |   1.5 |     2 |     5 |   7.5 |    10 |
| 2   |     5 |   4.0 |     3 |    14 |  10.5 |     7 |
| 3   |    15 |  12.0 |     9 |     8 |  12.0 |    16 |

Here we can see that the time indicators are at the *end* of the
variable names (`_W1`), time indicators of 1, 2, and 3, and a *prefix*
of `_W`. With that in mind, we can use
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
to convert the data to long format.

``` r
long_panel(wide, prefix = "_W", begin = 1, end = 3, label_location = "end")
```

| id  | wave |   Q1 |   Q2 |
|:----|-----:|-----:|-----:|
| 1   |    1 |  1.0 |  5.0 |
| 1   |    2 |  1.5 |  7.5 |
| 1   |    3 |  2.0 | 10.0 |
| 2   |    1 |  5.0 | 14.0 |
| 2   |    2 |  4.0 | 10.5 |
| 2   |    3 |  3.0 |  7.0 |
| 3   |    1 | 15.0 |  8.0 |
| 3   |    2 | 12.0 | 12.0 |
| 3   |    3 |  9.0 | 16.0 |

Perfect! The first argument, `w`, was the wide data. `prefix` is
self-explanatory. `begin` and `end` refer to the range of the time
indicators, since they are consecutive. You can instead use
`periods = c(1, 2, 3)` if you prefer. That’s especially true if you have
non-consecutive time indicators like a biannual survey that uses the
year as an indicator, like `periods = c(1990, 1992, 1994)`.

### Comparing with base R

I should note that base R has a function,
[`reshape()`](https://rdrr.io/r/stats/reshape.html) that can work in
this situation without making you pull your hair out too much:

``` r
reshape(as.data.frame(wide), sep = "_W", times = c(1, 2, 3), direction = "long",
        varying = c("Q1_W1", "Q1_W2", "Q1_W3", "Q2_W1", "Q2_W2", "Q2_W3"))
```

|     | id  | time |   Q1 |   Q2 |
|:----|:----|-----:|-----:|-----:|
| 1.1 | 1   |    1 |  1.0 |  5.0 |
| 2.1 | 2   |    1 |  5.0 | 14.0 |
| 3.1 | 3   |    1 | 15.0 |  8.0 |
| 1.2 | 1   |    2 |  1.5 |  7.5 |
| 2.2 | 2   |    2 |  4.0 | 10.5 |
| 3.2 | 3   |    2 | 12.0 | 12.0 |
| 1.3 | 1   |    3 |  2.0 | 10.0 |
| 2.3 | 2   |    3 |  3.0 |  7.0 |
| 3.3 | 3   |    3 |  9.0 | 16.0 |

You can see how frustrating that could be if you had many more variables
— it wouldn’t be unusual to have hundreds of columns in the wide format,
not all of which would be variables that vary over time (e.g., race).
Truth be told,
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
uses [`reshape()`](https://rdrr.io/r/stats/reshape.html) internally, but
only after a lot of processing. Other options include the `reshape2` and
`tidyr` packages, but they are not purpose-built for the panel setting
and therefore can have a learning curve to avoid having data that end up
a bit *too* long.

### A more challenging example

Here’s a wide dataset with what is usually a trickier format to handle
due to limitations of
[`reshape()`](https://rdrr.io/r/stats/reshape.html).

| WA_Q1 | WB_Q1 | WC_Q1 | WA_Q2 | WC_Q2 |
|------:|------:|------:|------:|------:|
|     1 |   1.5 |     2 |     5 |    10 |
|     5 |   4.0 |     3 |    14 |     7 |
|    15 |  12.0 |     9 |     8 |    16 |

Key characteristics:

- Wave indicators are at the *beginning* of the variable names.
- The time indicators are *letters*, from A to C.
- There is a prefix (`W`) and suffix (`_`).

While you don’t have to recognize this to use the function properly,
notice that in this case `Q2` was only measured at times A and C. This
can add considerable difficulty to when trying to reshape data “by
hand.”

``` r
long_panel(wide, prefix = "W", suffix = "_", label_location = "beginning",
           begin = "A", end = "C")
```

| id  | wave |   Q1 |  Q2 |
|:----|:-----|-----:|----:|
| 1   | A    |  1.0 |   5 |
| 1   | B    |  1.5 |  NA |
| 1   | C    |  2.0 |  10 |
| 2   | A    |  5.0 |  14 |
| 2   | B    |  4.0 |  NA |
| 2   | C    |  3.0 |   7 |
| 3   | A    | 15.0 |   8 |
| 3   | B    | 12.0 |  NA |
| 3   | C    |  9.0 |  16 |

Just what we were looking for. Note that `panel_data` objects must have
an ordered wave variable, but `long_data()` understands how to order
letters and handles that for you. The missingness in `Q2` is by design,
since it wasn’t measured in wave B.

Another issue that can come up is the treatment of constants — that is,
variables that do not change over time. The best wide data should come
labeled in a way that makes it clear the constants are constants. For
instance, a variable signifying race wouldn’t be called `race_W1`, but
instead just `race`.
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
automatically checks your data for variables that are labeled as if they
vary over time but actually do not.

For instance, data that start by looking like this:

|  id | Q1_W1 | Q1_W2 | Q1_W3 | race_W1 |
|----:|------:|------:|------:|:--------|
|   1 |     1 |   1.5 |     2 | white   |
|   2 |     4 |   3.0 |     2 | black   |
|   3 |    15 |  12.0 |     9 | white   |

Can easily end up shaped like this:

|  id | wave | race  |   Q1 |
|----:|-----:|:------|-----:|
|   1 |    1 | white |  1.0 |
|   1 |    2 | NA    |  1.5 |
|   1 |    3 | NA    |  2.0 |
|   2 |    1 | black |  4.0 |
|   2 |    2 | NA    |  3.0 |
|   2 |    3 | NA    |  2.0 |
|   3 |    1 | white | 15.0 |
|   3 |    2 | NA    | 12.0 |
|   3 |    3 | NA    |  9.0 |

But obviously just because the wide data marked `race` with a wave
label, that doesn’t mean it was unknown in the other waves. You’ll get
the right result with
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md):

``` r
long_panel(wide, prefix = "_W", label_location = "end", begin = 1, end = 3)
```

| id  | wave |   Q1 | race  |
|:----|-----:|-----:|:------|
| 1   |    1 |  1.0 | white |
| 1   |    2 |  1.5 | white |
| 1   |    3 |  2.0 | white |
| 2   |    1 |  4.0 | black |
| 2   |    2 |  3.0 | black |
| 2   |    3 |  2.0 | black |
| 3   |    1 | 15.0 | white |
| 3   |    2 | 12.0 | white |
| 3   |    3 |  9.0 | white |

### Other details

If you have an ID variable in the wide data, you can pass the name of
that variable to
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
with the `id` argument, which is `"id"` by default. If there is no
variable with the name you give to `id`, one will be created. You can
also choose the name of the wave variable via `wave`, which is `"wave"`
by default.

You can also choose not to have the output of
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
be a `panel_data` object by setting `as_panel_data` to `FALSE`.

### Advanced options

There are some other options available to you for tougher cases. For
instance, when `use.regex` is `TRUE`, the arguments for `prefix` and
`suffix` are treated as regular expressions for more complicated
patterns.

Internally, time-varying variables are detected by the presence of
`prefix`, one of the time periods, and `suffix` in the variable name.
The “root” variable without the indicator is whatever is left.
Sometimes, though, this can cause false matches. Here’s an example I
have encountered. My wide data looked like this:

| CaseID | Consent |  A1 |  B1 |  C1 |
|-------:|:--------|----:|----:|----:|
|      1 | TRUE    |   5 |   4 |   3 |
|      2 | TRUE    |   6 |   7 |   8 |
|      3 | TRUE    |  10 |   8 |   6 |

My ID variable was called `CaseID` and the periods — which were A, B,
and C — were labeled at the *beginning* of the column names. Following
the earlier examples, this will confuse
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md):

``` r
long_panel(wide, begin = "A", end = "C", label_location = "beginning", id = "CaseID")
```

| CaseID | wave |   1 | onsent |
|:-------|:-----|----:|:-------|
| 1      | A    |   5 | TRUE   |
| 1      | B    |   4 | TRUE   |
| 1      | C    |   3 | TRUE   |
| 2      | A    |   6 | TRUE   |
| 2      | B    |   7 | TRUE   |
| 2      | C    |   8 | TRUE   |
| 3      | A    |  10 | TRUE   |
| 3      | B    |   8 | TRUE   |
| 3      | C    |   6 | TRUE   |

See what happened? The `Consent` variable in the wide data looked just
like a constant variable that was measured at time point C. This isn’t
the end of the world, but errors like this can be more confusing and
damaging in other scenarios. Fortunately, I knew more about the labeling
of the time-varying variables than what I told
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md).
Yes, there is A/B/C at the beginning with no prefix/suffix, but also
each time-varying item has a *number* that comes after A/B/C.

[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
offers the argument `match` for situations like these. This is the
regular expression used to match and then capture the variable name sans
time indicator. By default, `match` is `".*"`, meaning any character any
number of times. To reflect what I know about these data, I change it to
`"\\d+.*"`, meaning at least one digit following by any number of other
characters.

``` r
long_panel(wide, begin = "A", end = "C", label_location = "beginning", 
           id = "CaseID", match = "\\d+.*")
```

| CaseID | wave | Consent |   1 |
|:-------|:-----|:--------|----:|
| 1      | A    | TRUE    |   5 |
| 1      | B    | TRUE    |   4 |
| 1      | C    | TRUE    |   3 |
| 2      | A    | TRUE    |   6 |
| 2      | B    | TRUE    |   7 |
| 2      | C    | TRUE    |   8 |
| 3      | A    | TRUE    |  10 |
| 3      | B    | TRUE    |   8 |
| 3      | C    | TRUE    |   6 |

Now it rightly ignores `Consent` as a variable that lacks a time
indicator. In general,
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
tries to protect you from having to use or even know how to use regular
expressions, but sometimes there’s no way around it.

## From long to wide

[`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md),
as you might expect, does the opposite of
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md).
This is generally an easier operation, thankfully.

[`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md)
expects a `panel_data` object. If your long data aren’t in that format,
it’s easy enough to just pass it to
[`panel_data()`](https://panelr.jacob-long.com/reference/panel_data.md).

To go through an example, let’s take a look at some long data.

| person | time |   Q1 |   Q2 | race  |
|:-------|-----:|-----:|-----:|:------|
| 1      |    1 |  1.0 |  5.0 | white |
| 1      |    2 |  1.5 |  7.5 | white |
| 1      |    3 |  2.0 | 10.0 | white |
| 2      |    1 |  5.0 | 14.0 | black |
| 2      |    2 |  4.0 | 10.5 | black |
| 2      |    3 |  3.0 |  7.0 | black |
| 3      |    1 | 15.0 |  8.0 | white |
| 3      |    2 | 12.0 | 12.0 | white |
| 3      |    3 |  9.0 | 16.0 | white |

Okay, so we have an ID variable (`person`), wave variable (`time`), two
time-varying variables (`Q1` and `Q2`), and a time-invariant variable
(`race`). The only difficulty here conceptually is how to automatically
know, without the domain knowledge about the substantive meaning of
these variables, which ones vary over time and which don’t. This is
simply a matter of
[`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md)
checking the variance of each (using the `panelr` function
[`are_varying()`](https://panelr.jacob-long.com/reference/are_varying.md)).
Note that in very wide datasets, or those with many individuals, this
can take a little while to happen.

``` r
widen_panel(long_data, separator = "_")
```

| person | race  | Q1_1 | Q2_1 | Q1_2 | Q2_2 | Q1_3 | Q2_3 |
|:-------|:------|-----:|-----:|-----:|-----:|-----:|-----:|
| 1      | white |    1 |    5 |  1.5 |  7.5 |    2 |   10 |
| 2      | black |    5 |   14 |  4.0 | 10.5 |    3 |    7 |
| 3      | white |   15 |    8 | 12.0 | 12.0 |    9 |   16 |

Pretty much all you need to worry about is how you want to label the
wide data. By default the `separator` argument is `"_"`.

There are only two other arguments. `varying` lets you specify which
variables in the long data vary over time. This can save you time
compared to having
[`widen_panel()`](https://panelr.jacob-long.com/reference/widen_panel.md)
check them all, but of course requires you to pass those variable names
along which can be more work than it’s worth at times.

`ignore.attributes` deals with the scenario in which you started with
wide data, used
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
to convert to long format, and now want to convert back to wide format.
[`long_panel()`](https://panelr.jacob-long.com/reference/long_panel.md)
stores information in the data frame about which variables vary over
time so that they don’t have to be checked all over again. If you’ve
made changes or think something went wrong, you can set
`ignore.attributes` to `TRUE` to force those checks all over again.
