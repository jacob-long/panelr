data("WageData")


# Defaults ----------------------------------------------------------------
context("asym_gee defaults")
wages <- WageData
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)
wb <- asym_gee(wks ~ union + lwage, data = wages)

test_that("asym_gee defaults work", {
  expect_s3_class(wb, "asym_gee")
})

test_that("asym_gee summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})


# Lags --------------------------------------------------------------------
context("asym_gee with lags")
wb <- asym_gee(wks ~ lag(union) + lag(lwage), data = wages)

test_that("asym_gee with lags works", {
  expect_s3_class(wb, "asym_gee")
})

test_that("asym_gee summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

context("asym_gee multiple lags")
wb <- asym_gee(wks ~ union + lag(union) + lag(lwage), data = wages)

test_that("asym_gee with multiple lags works", {
  expect_s3_class(wb, "asym_gee")
})

test_that("asym_gee summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

context("asym_gee non-standard lags")
wb <- asym_gee(wks ~ union + lag(union, 2) + lag(lwage), data = wages)

test_that("asym_gee with non-standard lags works", {
  expect_s3_class(wb, "asym_gee")
})
test_that("asym_gee summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

# Model types -------------------------------------------------------------
context("asym_gee with constant interaction")
wb <- asym_gee(wks ~ union + lag(lwage) * blk, data = wages)

test_that("asym_gee with contextual model works", {
  expect_s3_class(wb, "asym_gee")
})
test_that("asym_gee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

context("asym_gee with exchangeable variance")

test_that("asym_gee with within model works", {
  expect_s3_class(wb <- asym_gee(wks ~ union + lag(lwage), data = wages, 
                             cor.str = "exchangeable"), "asym_gee")
})
test_that("asym_gee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

context("asym_gee with unstructured variance")
wb <- asym_gee(wks ~ union + lag(lwage), data = wages, cor.str = "unstructured")

test_that("asym_gee with between model works", {
  expect_s3_class(wb, "asym_gee")
})
test_that("asym_gee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})


# Other options -----------------------------------------------------------
context("asym_gee with use.wave")
wb <- asym_gee(wks ~ union + lag(lwage) | blk, data = wages,
           use.wave = TRUE)

test_that("asym_gee with use.wave works", {
  expect_s3_class(wb, "asym_gee")
})
test_that("asym_gee summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

# Missing data ------------------------------------------------------------
context("asym_gee missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$t == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- asym_gee(wks ~ lag(union) + lag(lwage) | blk, data = wagesm)

test_that("asym_gee with defaults works", {
  expect_s3_class(wb, "asym_gee")
})
test_that("asym_gee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym_gee")
  expect_output(print(swb))
})

# factors -----------------------------------------------------------------
context("asym_gee w/ time-varying factors")
if (requireNamespace("plm")) {
  data(Males, package = "plm")
  males <- panel_data(Males, id = nr, wave = year)
  set.seed(2)
  # Cutting down on the time it takes to fit these models
  males <- filter(males, nr %in% sample(males$nr, 100))
  test_that("Models with time-varying factors work", {
    expect_s3_class(wbf <- asym_gee(wage ~ industry, data = males),
                    "asym_gee")
    expect_output(print(summary(wbf)))
  })
}


# tidiers -----------------------------------------------------------------
context("asym_gee tidiers")
if (requireNamespace("broom")) {
  expect_is(broom::tidy(wb <- asym_gee(wks ~ lag(union) + lag(lwage),
                                       data = wages), conf.int = TRUE), 
            "data.frame")
  expect_is(broom::glance(wb), "data.frame")
}
