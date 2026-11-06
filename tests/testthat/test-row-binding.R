test_that("bind_rows preserves panel metadata and source labels", {
  early <- panel_data(data.frame(id = c("b", "a"), t = 1, x = 1:2), wave = t)
  late <- panel_data(data.frame(id = c("b", "a"), t = 2, x = 3:4), wave = t)

  out <- dplyr::bind_rows(late = late, early = early, .id = "source")
  expect_s3_class(out, "panel_data")
  expect_identical(get_id(out), "id")
  expect_identical(get_wave(out), "t")
  expect_equal(get_periods(out), 1:2)
  expect_equal(out$source, ifelse(out$t == 1, "early", "late"))
  expect_equal(out$x, c(2L, 4L, 1L, 3L))
  expect_equal(lapply(dplyr::group_rows(out), identity), list(1:2, 3:4))
  expect_equal(dplyr::mutate(out, previous = dplyr::lag(x))$previous,
               c(NA_integer_, 2L, NA_integer_, 1L))
})

test_that("vctrs restoration does not reorder rows or their source labels", {
  early <- panel_data(data.frame(id = c("a", "b"), t = 1, x = 1:2), wave = t)
  late <- panel_data(data.frame(id = c("a", "b"), t = 2, x = 3:4), wave = t)
  out <- vctrs::vec_rbind(late = late, early = early, .names_to = "source")
  expect_equal(out$x, c(3L, 4L, 1L, 2L))
  expect_equal(out$source, c("late", "late", "early", "early"))
  expect_equal(get_periods(out), 1:2)
  expect_equal(vctrs::vec_slice(early, c(2, 1))$x, c(2L, 1L))
})

test_that("bind_rows handles empty panels and plain data frames", {
  early <- panel_data(data.frame(id = c("a", "b"), t = 1, x = 1:2), wave = t)
  late <- tibble::tibble(id = c("a", "b"), t = 2, x = 3:4)
  out <- dplyr::bind_rows(early[0, ], late, early)
  expect_s3_class(out, "panel_data")
  expect_equal(get_periods(out), 1:2)
  expect_equal(out$x, c(1L, 3L, 2L, 4L))
})
