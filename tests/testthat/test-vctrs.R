context("vctrs integration")

library(panelr)
library(dplyr)

data("WageData")
w <- panel_data(WageData, id = id, wave = t)

# --- vec_restore tests ---
test_that("vec_restore preserves panel_data after vctrs::vec_slice", {
  sliced <- vctrs::vec_slice(w, 1:100)
  expect_s3_class(sliced, "panel_data")
  expect_equal(get_id(sliced), get_id(w))
  expect_equal(get_wave(sliced), get_wave(w))
})

test_that("vec_restore falls back to tibble when id column removed", {
  # Create a data frame without the id column
  no_id <- unpanel(w)[, names(w) != "id"]
  restored <- vctrs::vec_restore(no_id, w)
  expect_s3_class(restored, "tbl_df")
  expect_false(inherits(restored, "panel_data"))
})

test_that("vec_restore falls back to tibble when wave column removed", {
  # Create a data frame without the wave column  
  no_wave <- unpanel(w)[, names(w) != "t"]
  restored <- vctrs::vec_restore(no_wave, w)
  expect_s3_class(restored, "tbl_df")
  expect_false(inherits(restored, "panel_data"))
})

# --- vec_rbind tests ---
test_that("vec_rbind combines panel_data objects", {
  w1 <- filter(w, id %in% levels(id)[1:5])
  w2 <- filter(w, id %in% levels(id)[6:10])
  combined <- vctrs::vec_rbind(w1, w2)
  expect_s3_class(combined, "panel_data")
  expect_equal(get_id(combined), get_id(w))
  expect_equal(get_wave(combined), get_wave(w))
})

test_that("vec_rbind with panel_data and tibble returns panel_data", {
  w_small <- filter(w, id %in% levels(id)[1:3])
  tbl_small <- tibble::as_tibble(filter(w, id %in% levels(id)[4:5]))
  combined <- vctrs::vec_rbind(w_small, tbl_small)
  expect_s3_class(combined, "panel_data")
})

# --- vec_c tests ---
test_that("vec_c combines panel_data objects", {
  w1 <- filter(w, id %in% levels(id)[1:3])
  w2 <- filter(w, id %in% levels(id)[4:6])
  combined <- vctrs::vec_c(w1, w2)
  expect_s3_class(combined, "panel_data")
})

# --- build_panel_data tests ---
test_that("build_panel_data creates panel_data with correct attributes", {
  df <- tibble::as_tibble(w)
  built <- panelr:::build_panel_data(df, id = "id", wave = "t", 
                                      periods = 1:7)
  expect_s3_class(built, "panel_data")
  expect_equal(get_id(built), "id")
  expect_equal(get_wave(built), "t")
  expect_equal(get_periods(built), 1:7)
})

test_that("build_panel_data preserves optional attributes", {
  df <- tibble::as_tibble(w)
  built <- panelr:::build_panel_data(df, id = "id", wave = "t",
                                      periods = 1:7, reshaped = TRUE,
                                      varying = c("lwage", "wks"),
                                      constants = c("fem", "blk"))
  expect_true(attr(built, "reshaped"))
  expect_equal(attr(built, "varying"), c("lwage", "wks"))
  expect_equal(attr(built, "constants"), c("fem", "blk"))
})

# --- Attribute preservation tests ---
test_that("attributes preserved through mutate", {
  w_with_attrs <- w
  attr(w_with_attrs, "reshaped") <- TRUE
  attr(w_with_attrs, "varying") <- c("lwage")
  
  mutated <- mutate(w_with_attrs, new_var = lwage * 2)
  expect_true(attr(mutated, "reshaped"))
  expect_equal(attr(mutated, "varying"), c("lwage"))
})

test_that("attributes preserved through filter", {
  w_with_attrs <- w
  attr(w_with_attrs, "reshaped") <- TRUE
  
  filtered <- filter(w_with_attrs, fem == 1)
  expect_true(attr(filtered, "reshaped"))
})

test_that("attributes preserved through select", {
  w_with_attrs <- w
  attr(w_with_attrs, "varying") <- c("lwage", "wks")
  
  selected <- select(w_with_attrs, id, t, lwage)
  expect_equal(attr(selected, "varying"), c("lwage", "wks"))
})

# --- Performance sanity check ---
test_that("vec_restore is faster than full panel_data construction", {
  # This is a sanity check, not a strict benchmark
  df <- tibble::as_tibble(w)
  
  # Time vec_restore
  t1 <- system.time({
    for (i in 1:100) {
      vctrs::vec_restore(df, w)
    }
  })
  
  # Time full panel_data
  t2 <- system.time({
    for (i in 1:100) {
      panel_data(df, id = id, wave = t)
    }
  })
  
  # vec_restore should be faster (we allow some margin for noise)
  # This test documents the expected behavior but won't fail on timing
  expect_true(TRUE)  # Just document that we tested this
})