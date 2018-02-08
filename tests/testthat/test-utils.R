
context("reconstructs")

library(panelr)
w <- panel_data(WageData, id = id, wave = t)

library(dplyr)
test_that("dplyr functions return panel_data objects", {
  expect_s3_class(mutate(w, gender = fem), "panel_data")
  expect_s3_class(transmute(w, gender = fem), "panel_data")
  expect_s3_class(summarize(w, mean_wg = mean(lwage)), "tbl_df")
  expect_s3_class(summarise(w, mean_wg = mean(lwage)), "tbl_df")
  expect_s3_class(filter(w, fem == 1), "panel_data")
  expect_s3_class(arrange(w, lwage), "panel_data")
  expect_s3_class(distinct(w, lwage), "tbl_df")
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
  expect_s3_class(select(w, lwage), "panel_data")
  expect_s3_class(slice(w, 3), "panel_data")
  expect_s3_class(group_by(w, id), "panel_data")
  expect_s3_class(mutate_(w, "gender" = "fem"), "panel_data")
  expect_s3_class(transmute_(w, "gender" = "fem"), "panel_data")
  expect_s3_class(summarize_(w, "mean_wg" = mean(w$lwage)), "tbl_df")
  expect_s3_class(summarise_(w, "mean_wg" = mean(w$lwage)), "tbl_df")
  expect_s3_class(slice_(w, "fem" == 1), "panel_data")
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
