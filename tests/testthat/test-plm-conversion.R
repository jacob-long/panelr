test_that("pdata.frame conversion restores dropped index columns", {
  skip_if_not_installed("plm")
  raw <- data.frame(person = rep(c("a", "b"), each = 3),
                    time = rep(c(1, 2, 4), 2), x = 11:16)
  for (drop_index in c(FALSE, TRUE)) {
    pd <- plm::pdata.frame(raw, index = c("person", "time"),
                           drop.index = drop_index)
    out <- suppressMessages(as_panel_data(pd))
    expect_s3_class(out, "panel_data")
    expect_identical(get_id(out), "person")
    expect_identical(get_wave(out), "time")
    expect_equal(as.character(out$person), raw$person)
    expect_equal(as.character(out$time), as.character(raw$time))
    expect_equal(out$x, raw$x)
    expect_false(inherits(out$x, "pseries"))
    expect_equal(dplyr::mutate(out, previous = dplyr::lag(x))$previous,
                 c(NA_integer_, 11L, 12L, NA_integer_, 14L, 15L))
    roundtrip <- as_pdata.frame(out)
    expect_equal(lapply(plm::index(roundtrip), as.character),
                 lapply(plm::index(pd), as.character))
  }
})

test_that("pdata.frame conversion restores either missing index separately", {
  skip_if_not_installed("plm")
  pd <- plm::pdata.frame(data.frame(person = rep(1:2, each = 2),
                                   time = rep(1:2, 2), x = 1:4),
                         index = c("person", "time"))
  for (missing_index in c("person", "time")) {
    partial <- pd[, setdiff(names(pd), missing_index), drop = FALSE]
    out <- suppressMessages(as_panel_data(partial))
    expect_equal(out$x, 1:4)
    expect_equal(as.character(out[[missing_index]]),
                 as.character(plm::index(pd)[[missing_index]]))
  }
})
