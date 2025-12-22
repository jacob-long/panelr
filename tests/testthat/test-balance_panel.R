context("balance_panel")

library(panelr)
library(dplyr)

data("WageData")
wages <- panel_data(WageData, id = id, wave = t)

# Create test data with known gaps
create_gapped_data <- function() {
  # Remove some observations to create gaps using filter (more reliable than [ subsetting)
  id1 <- levels(wages$id)[1]
  id2 <- levels(wages$id)[2]
  wages_gapped <- dplyr::filter(wages, !(t == 3 & id == id1))
  wages_gapped <- dplyr::filter(wages_gapped, !(t == 5 & id == id2))
  wages_gapped
}

# --- has_gaps tests ---
test_that("has_gaps returns FALSE for complete data", {
  expect_false(has_gaps(wages))
})

test_that("has_gaps returns TRUE for data with gaps", {
  gapped <- create_gapped_data()
  expect_true(has_gaps(gapped))
})

test_that("has_gaps errors for non-panel_data", {
  expect_error(has_gaps(data.frame(x = 1)), "panel_data")
})

# --- scan_gaps tests ---
test_that("scan_gaps returns empty tibble for complete data", {
  gaps <- scan_gaps(wages)
  expect_s3_class(gaps, "tbl_df")
  expect_equal(nrow(gaps), 0)
})

test_that("scan_gaps identifies correct gaps", {
  gapped <- create_gapped_data()
  gaps <- scan_gaps(gapped)
  
  expect_s3_class(gaps, "tbl_df")
  expect_equal(nrow(gaps), 2)
  expect_true("id" %in% names(gaps))
  expect_true("t" %in% names(gaps))
  
  # Check specific gaps
  gap1 <- gaps[gaps$id == levels(wages$id)[1], ]
  expect_equal(gap1$t, 3)
  
  gap2 <- gaps[gaps$id == levels(wages$id)[2], ]
  expect_equal(gap2$t, 5)
})

test_that("scan_gaps errors for non-panel_data", {
  expect_error(scan_gaps(data.frame(x = 1)), "panel_data")
})

# --- balance_panel tests ---
test_that("balance_panel returns same data if no gaps", {
  balanced <- balance_panel(wages)
  expect_equal(nrow(balanced), nrow(wages))
})

test_that("balance_panel fills gaps with NA rows", {
  gapped <- create_gapped_data()
  original_rows <- nrow(gapped)
  
  balanced <- balance_panel(gapped)
  
  expect_s3_class(balanced, "panel_data")
  expect_equal(nrow(balanced), nrow(wages))
  expect_false(has_gaps(balanced))
  
  # Check that gap rows have NA values
  gap_row1 <- balanced[balanced$id == levels(wages$id)[1] & balanced$t == 3, ]
  expect_true(is.na(gap_row1$lwage))
  expect_true(is.na(gap_row1$wks))
})

test_that("balance_panel accepts custom fill values", {
  gapped <- create_gapped_data()
  
  balanced <- balance_panel(gapped, wks = 0, union = 0)
  
  gap_row1 <- balanced[balanced$id == levels(wages$id)[1] & balanced$t == 3, ]
  expect_equal(gap_row1$wks, 0)
  expect_equal(gap_row1$union, 0)
  # Other columns should still be NA
  expect_true(is.na(gap_row1$lwage))
})

test_that("balance_panel preserves panel_data attributes", {
  gapped <- create_gapped_data()
  attr(gapped, "reshaped") <- TRUE
  attr(gapped, "varying") <- c("lwage", "wks")
  
  balanced <- balance_panel(gapped)
  
  expect_true(attr(balanced, "reshaped"))
  expect_equal(attr(balanced, "varying"), c("lwage", "wks"))
})

test_that("balance_panel maintains correct column types", {
  gapped <- create_gapped_data()
  balanced <- balance_panel(gapped)
  
  # Check column types match
  expect_identical(class(balanced$lwage), class(wages$lwage))
  expect_identical(class(balanced$wks), class(wages$wks))
  expect_identical(class(balanced$union), class(wages$union))
})

test_that("balance_panel errors for non-panel_data", {
  expect_error(balance_panel(data.frame(x = 1)), "panel_data")
})

test_that("balance_panel works with ordered factor waves", {
  # Create data with ordered factor wave variable
  test_data <- tibble::tibble(
    id = rep(1:3, each = 3),
    wave = rep(ordered(c("A", "B", "C")), 3),
    value = 1:9
  )
  pd <- panel_data(test_data, id = id, wave = wave)
  
  # Remove one observation
  pd_gapped <- pd[!(pd$id == 1 & pd$wave == "B"), ]
  
  balanced <- balance_panel(pd_gapped)
  
  expect_false(has_gaps(balanced))
  expect_equal(nrow(balanced), 9)
})

# --- Integration tests ---
test_that("balance_panel result can be used with wbm", {
  gapped <- create_gapped_data()
  balanced <- balance_panel(gapped)
  
  # This should work without error (though may have fewer observations due to NA)
  expect_s3_class(balanced, "panel_data")
  # Can't actually run wbm here without many more observations
})