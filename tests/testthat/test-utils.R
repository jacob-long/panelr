
context("reconstructs")

library(panelr)
w <- panel_data(WageData, id = id, wave = t)

library(dplyr)
test_that("dplyr functions return panel_data objects", {
  expect_s3_class(mutate(w, gender = fem), "panel_data")
  expect_s3_class(transmute(w, gender = fem), "panel_data")
  expect_s3_class(summarize(w, mean_wg = mean(lwage)), "tbl_df")
  expect_s3_class(filter(w, fem == 1), "panel_data")
  expect_s3_class(arrange(w, lwage), "panel_data")
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

wide <- widen_panel(w)

test_that("long_panel works", {
  expect_s3_class(long_panel(wide, begin = 1, end = 7), "panel_data")
})

wide <- wide[names(wide) %nin% c("occ_3", "lwage_5")]
test_that("long_panel handles unbalanced data", {
  expect_s3_class(long_panel(wide, begin = 1, end = 7), "panel_data")
})

context("tibble printing")

test_that("print.panel_data works", {
  expect_output(print(w))
})

context("extractors")

library(lme4)
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



