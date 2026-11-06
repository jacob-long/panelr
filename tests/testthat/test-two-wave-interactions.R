test_that("two-wave warnings distinguish within and cross-level interactions", {
  two <- panel_data(data.frame(
    id = rep(1:6, each = 2), wave = rep(1:2, 6), y = seq_len(12),
    x1 = c(1, 2, 2, 4, 3, 6, 4, 8, 5, 10, 6, 12),
    x2 = c(2, 1, 4, 2, 6, 3, 8, 4, 10, 5, 12, 6),
    z = rep(1:6, each = 2)
  ))
  expect_warning(within <- make_wb_data(y ~ x1 * x2, two),
                 "Double-demeaned interactions")
  expect_equal(within[["x1:x2"]], rep(0, nrow(two)))
  expect_warning(cross <- make_wb_data(y ~ x1 | z | x1 * z, two), NA)
  expect_true(any(cross[["x1:z"]] != 0))
  expect_warning(make_wb_data(y ~ x1 + x2, two), NA)
  expect_warning(make_wb_data(y ~ x1 * x2, two, interaction.style = "demean"), NA)
  expect_warning(make_wb_data(y ~ x1 * x2, two, interaction.style = "raw"), NA)
  expect_warning(make_wb_data(y ~ x1 * x2, two, model = "between"), NA)
})

test_that("warning is limited to the double-demeaning computation", {
  two <- panel_data(data.frame(id = rep(1:2, each = 2),
                               wave = rep(1:2, 2)))
  pf <- list(wint_labs = "`x1`*`x2`", cint_labs = NULL,
             varying = c("x1", "x2"))
  config <- panelr:::InteractionConfig(detrend = TRUE)
  expect_warning(panelr:::warn_two_wave_double_demean(two, "id", config, pf), NA)
  config <- panelr:::InteractionConfig()
  expect_warning(panelr:::warn_two_wave_double_demean(two[0, ], "id", config, pf), NA)
  three <- panel_data(data.frame(id = rep(1:2, each = 3), wave = rep(1:3, 2)))
  expect_warning(panelr:::warn_two_wave_double_demean(three, "id", config, pf), NA)
})
