
context("reconstructs")

library(panelr)
w <- panel_data(WageData, id = id, wave = t)

library(dplyr)
test_that("dplyr functions return panel_data objects", {
  expect_s3_class(mutate(w, gender = fem), "panel_data")
  expect_s3_class(transmute(w, gender = fem), "panel_data")
  expect_s3_class(summarize(w, mean_wg = mean(lwage)), "tbl_df")
  expect_s3_class(filter(w, fem == 1), "panel_data")
  expect_warning(arrange(w, lwage))
  expect_s3_class(distinct(w, lwage), "tbl_df")
  suppressWarnings({
  expect_s3_class(full_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  expect_s3_class(inner_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  expect_s3_class(left_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  expect_s3_class(right_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  expect_s3_class(anti_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  expect_s3_class(semi_join(w, summarize(w, mean_wg = mean(lwage), by = "id")),
                  "panel_data")
  })
  expect_s3_class(select(w, lwage), "panel_data")
  expect_s3_class(slice(w, 3), "panel_data")
  expect_s3_class(group_by(w, id), "panel_data")
  suppressWarnings({
  expect_s3_class(mutate_(w, "gender" = "fem"), "panel_data")
  expect_s3_class(transmute_(w, "gender" = "fem"), "panel_data")
  expect_s3_class(summarize_(w, "mean_wg" = mean(w$lwage)), "tbl_df")
  expect_s3_class(summarise_(w, "mean_wg" = mean(w$lwage)), "tbl_df")
  expect_s3_class(filter_(w, "fem" == 1), "panel_data")
  expect_s3_class(slice_(w, 3), "panel_data")
  })
  expect_s3_class(w[names(w)], "panel_data")
})

context("widen_panel")

test_that("widen_panel works", {
  expect_s3_class(widen_panel(w), "data.frame")
})

context("long_panel")

w <- tibble::tribble(
  ~Q1_W1, ~Q1_W2, ~Q1_W3, 
  1,      1.5,     2,      
  5,      4,       3,    
  15,     12,      9,      
)

l <- panel_data(tibble::tribble(
  ~id, ~wave, ~Q1,
  "1",     1,   1,
  "1",     2, 1.5,
  "1",     3,   2,
  "2",     1,   5,
  "2",     2,   4,
  "2",     3,   3,
  "3",     1,  15,
  "3",     2,  12,
  "3",     3,   9
))

test_that("long_panel works (basic case)", {
  expect_equal(long_panel(w, prefix = "_W", begin = 1, end = 3), l)
})

w <- tibble::tribble(
  ~Q1_W1, ~Q1_W2, ~Q1_W3, ~Q2_W1, ~Q2_W3,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- panel_data(tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",     1,   1,   5,
  "1",     2, 1.5,  NA,
  "1",     3,   2,  10,
  "2",     1,   5,  14,
  "2",     2,   4,  NA,
  "2",     3,   3,   7,
  "3",     1,  15,   8,
  "3",     2,  12,  NA,
  "3",     3,   9,  16
))

test_that("long_panel works (unbalanced data)", {
  expect_equal(long_panel(w, prefix = "_W", begin = 1, end = 3), l)
})

w <- tibble::tribble(
  ~Q1_W2, ~Q1_W3, ~Q1_W4, ~Q2_W2, ~Q2_W4,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- panel_data(tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",     2,   1,   5,
  "1",     3, 1.5,  NA,
  "1",     4,   2,  10,
  "2",     2,   5,  14,
  "2",     3,   4,  NA,
  "2",     4,   3,   7,
  "3",     2,  15,   8,
  "3",     3,  12,  NA,
  "3",     4,   9,  16
))

test_that("long_panel works (unbalanced data, numeric waves not begin w/ 1)", {
  expect_equal(long_panel(w, prefix = "_W", begin = 2, end = 4), l)
})

w <- tibble::tribble(
  ~Q1_WA, ~Q1_WB, ~Q1_WC, ~Q2_WA, ~Q2_WC,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",   "A",   1,   5,
  "1",   "B", 1.5,  NA,
  "1",   "C",   2,  10,
  "2",   "A",   5,  14,
  "2",   "B",   4,  NA,
  "2",   "C",   3,   7,
  "3",   "A",  15,   8,
  "3",   "B",  12,  NA,
  "3",   "C",   9,  16
)

l$wave <- ordered(l$wave, c("A", "B", "C"))

test_that("long_panel works (character periods)", {
  expect_equal(long_panel(w, prefix = "_W", begin = "A", end = "C"), 
               panel_data(l))
})

w <- tibble::tribble(
  ~W1_Q1, ~W2_Q1, ~W3_Q1, ~W1_Q2, ~W3_Q2,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- panel_data(tibble::tribble(
    ~id, ~wave, ~Q1, ~Q2,
    "1",     1,   1,   5,
    "1",     2, 1.5,  NA,
    "1",     3,   2,  10,
    "2",     1,   5,  14,
    "2",     2,   4,  NA,
    "2",     3,   3,   7,
    "3",     1,  15,   8,
    "3",     2,  12,  NA,
    "3",     3,   9,  16
))

test_that("long_panel works (beginning label)", {
  expect_equal(long_panel(w, prefix = "W", suffix = "_", begin = 1, end = 3,
                          label_location = "beginning"), l)
})

w <- tibble::tribble(
  ~WA_Q1, ~WB_Q1, ~WC_Q1, ~WA_Q2, ~WC_Q2,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",   "A",   1,   5,
  "1",   "B", 1.5,  NA,
  "1",   "C",   2,  10,
  "2",   "A",   5,  14,
  "2",   "B",   4,  NA,
  "2",   "C",   3,   7,
  "3",   "A",  15,   8,
  "3",   "B",  12,  NA,
  "3",   "C",   9,  16
)

l$wave <- ordered(l$wave, c("A", "B", "C"))

test_that("long_panel works (beginning label/character periods)", {
  expect_equal(long_panel(w, prefix = "W", suffix = "_", begin = "A", end = "C",
                          label_location = "beginning"), panel_data(l))
})

w <- tibble::tribble(
  ~Q1_AW, ~Q1_BW, ~Q1_CW, ~Q2_AW, ~Q2_CW,
  1,      1.5,     2,      5,      10,
  5,      4,       3,      14,     7,
  15,     12,      9,      8,      16
)

l <- tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",   "A",   1,   5,
  "1",   "B", 1.5,  NA,
  "1",   "C",   2,  10,
  "2",   "A",   5,  14,
  "2",   "B",   4,  NA,
  "2",   "C",   3,   7,
  "3",   "A",  15,   8,
  "3",   "B",  12,  NA,
  "3",   "C",   9,  16
)

l$wave <- ordered(l$wave, c("A", "B", "C"))

test_that("long_panel works (prefix and suffix/character periods)", {
  expect_equal(long_panel(w, prefix = "_", suffix = "W", begin = "A", end = "C",
                          label_location = "end"), panel_data(l))
})

w <- tibble::tribble(
  ~AQ1, ~BQ1, ~CQ1, ~AQ2, ~CQ2,
  1,    1.5,  2,    5,    10,
  5,    4,    3,    14,   7,
  15,   12,   9,    8,    16
)

l <- tibble::tribble(
  ~id, ~wave, ~Q1, ~Q2,
  "1",   "A",   1,   5,
  "1",   "B", 1.5,  NA,
  "1",   "C",   2,  10,
  "2",   "A",   5,  14,
  "2",   "B",   4,  NA,
  "2",   "C",   3,   7,
  "3",   "A",  15,   8,
  "3",   "B",  12,  NA,
  "3",   "C",   9,  16
)

l$wave <- ordered(l$wave, c("A", "B", "C"))

test_that("long_panel works (beginning/no separators/character periods)", {
  expect_equal(long_panel(w, prefix = "", suffix = "",
                          label_location = "beginning", begin = "A", end = "C"),
               panel_data(l))
})

w <- tibble::tribble(
  ~qW2W1, ~qW2W2, ~qW2W12,
  1,     2,      3,
  5,     7,      9,
  11,    15,     19
)

l <- panel_data(tibble::tribble(
  ~id,  ~wave,  ~qW2,
    1,      1,     1,
    1,      2,     2,
    1,     12,     3,
    2,      1,     5,
    2,      2,     7, 
    2,     12,     9,
    3,      1,    11, 
    3,      2,    15,
    3,     12,    19
))

test_that("long_panel works (redundant match in varname, ambiguous waves)", {
  expect_equal(long_panel(w, prefix = "W", label_location = "end", 
                          periods = c(1, 2, 12)), l)
})

context("tibble printing")

test_that("print.panel_data works", {
  expect_output(print(w))
})

context("extractors")

library(lme4)
w <- panel_data(WageData, id = id, wave = t)
mod <- wbm(lwage ~ union, data = w, pvals = FALSE)

test_that("extractors work", {
  expect_silent(getCall(mod))
  expect_silent(predict(mod))
  expect_silent(simulate(mod))
  expect_silent(fixef(mod))
  expect_silent(ranef(mod))
  expect_silent(vcov(mod))
  expect_silent(model.frame(mod))
  expect_silent(nobs(mod))
  expect_silent(formula(mod))
  expect_silent(terms(mod))
  expect_silent(coef(mod))
  expect_silent(anova(mod))
  expect_silent(isGLMM(mod))
  expect_silent(isLMM(mod))
  expect_silent(isNLMM(mod))
})

context("panel_data")

test_that("summary.panel_data works", {
  expect_output(print(summary(w, lwage, blk)))
  expect_output(print(summary(w[1:14,], lwage, blk, by.id = TRUE, by.wave = FALSE)))
})

test_that("complete_data works", {
  expect_is(complete_data(w, lwage, blk), "panel_data")
  expect_is(complete_data(w, formula = ~ lwage + blk), "panel_data")
  expect_is(complete_data(w, vars = c("lwage", "blk")), "panel_data")
})

if (requireNamespace("plm")) {
  test_that("coercion to pdata.frame works", {
    expect_is(as_pdata.frame(w), "pdata.frame")
  })
  data(Males, package = "plm")
  males <- plm::pdata.frame(Males)
  test_that("coercion from pdata.frame works", {
    expect_is(as_panel_data(males), "panel_data")
  })
}

context("tidiers")

if (requireNamespace("broom")) {
  wb <- wbm(wks ~ union + lwage | blk, data = w)
  test_that("tidy works", {
    expect_is(tidy.wbm(wb), "tbl_df")
    expect_is(tidy.wbm(wb, conf.int = TRUE), "tbl_df")
  })
  test_that("glance works", {
    expect_is(glance.wbm(wb), "tbl_df")
  })
}


