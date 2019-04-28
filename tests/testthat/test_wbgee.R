data("WageData")

# Defaults ----------------------------------------------------------------
context("wbgee defaults")
wages <- WageData
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)
wb <- wbgee(wks ~ union + lwage | blk, data = wages)

test_that("wbgee defaults work", {
  expect_s3_class(wb, "wbgee")
})

test_that("wbgee summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})


# Lags --------------------------------------------------------------------
context("wbgee with lags")
wb <- wbgee(wks ~ lag(union) + lag(lwage) | blk, data = wages)

test_that("wbgee with lags works", {
  expect_s3_class(wb, "wbgee")
})

test_that("wbgee summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

context("wbgee multiple lags")
wb <- wbgee(wks ~ union + lag(union) + lag(lwage) | blk,
          data = wages)

test_that("wbgee with multiple lags works", {
  expect_s3_class(wb, "wbgee")
})

test_that("wbgee summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

context("wbgee non-standard lags")
wb <- wbgee(wks ~ union + lag(union, 2) + lag(lwage) | blk,
          data = wages)

test_that("wbgee with non-standard lags works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

# Model types -------------------------------------------------------------
context("wbgee with contextual model")
wb <- wbgee(wks ~ union + lag(lwage) | blk, data = wages,
          model = "contextual")

test_that("wbgee with contextual model works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (with contextual model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

context("wbgee with within model")

test_that("wbgee with within model works", {
  expect_warning(wb <- wbgee(wks ~ union + lag(lwage) | blk, data = wages,
                           model = "within"))
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (with within model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

context("wbgee with between-model")
wb <- wbgee(wks ~ union + lag(lwage) | blk, data = wages,
          model = "between")

test_that("wbgee with between model works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (with between model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})


# GLMs --------------------------------------------------------------------
context("wbgee as poisson glm")
wb <- suppressWarnings(wbgee(wks ~ union + lag(lwage) | fem, data = wages,
                           family = poisson))

test_that("wbgee with poisson family works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (as poisson glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})


# Other options -----------------------------------------------------------
context("wbgee with use.wave")
wb <- wbgee(wks ~ union + lag(lwage) | blk, data = wages,
          use.wave = TRUE)

test_that("wbgee with use.wave works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})

context("wbgee with weights")
wb <- wbgee(wks ~ union + lag(lwage) | blk, data = wages, weights = wts)

test_that("wbgee works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})


# Missing data ------------------------------------------------------------
context("wbgee missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$t == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- wbgee(wks ~ lag(union) + lag(lwage) | blk, data = wagesm)

test_that("wbgee with defaults works", {
  expect_s3_class(wb, "wbgee")
})
test_that("wbgee summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbgee")
  expect_output(print(swb))
})


# wbgee with detrending ---------------------------------------------------
context("wbgee with detrending")

# May not converge perfectly
wb1 <- suppressWarnings(wbgee(wks ~ union + lag(lwage) | blk,
                            data = wages, detrend = TRUE))
wb2 <- wbgee(wks ~ union + lag(lwage) | blk,
           data = wages, detrend = TRUE,
           balance.correction = TRUE)

test_that("wbgee works (detrend only)", {
  expect_s3_class(wb1, "wbgee")
})
test_that("wbgee works (w/ balance_correction)", {
  expect_s3_class(wb2, "wbgee")
})
test_that("wbgee summary works (detrend only)", {
  expect_s3_class(swb1 <- summary(wb1), "summary.wbgee")
  expect_output(print(swb1))
})
test_that("wbgee summary works (detrend only)", {
  expect_s3_class(swb2 <- summary(wb2), "summary.wbgee")
  expect_output(print(swb2))
})


# factors -----------------------------------------------------------------
context("wbgee with time-varying factors")
if (requireNamespace("plm")) {
  data(Males, package = "plm")
  males <- panel_data(Males, id = nr, wave = year)
  set.seed(2)
  # Cutting down on the time it takes to fit these models
  males <- filter(males, nr %in% sample(males$nr, 100))
  test_that("Models with time-varying factors work", {
    expect_s3_class(wbf <- wbgee(wage ~ industry + exper | ethn, data = males),
                    "wbgee")
    expect_output(print(summary(wbf)))
    expect_s3_class(wbf <- wbgee(wage ~ industry * exper | ethn, data = males),
                    "wbgee")
    expect_output(print(summary(wbf)))
    expect_s3_class(wbf <- wbgee(wage ~ industry * married | ethn,
                               data = males),
                    "wbgee")
    expect_output(print(summary(wbf)))
  })
}

# tidiers -----------------------------------------------------------------
context("wbgee tidiers")
if (requireNamespace("broom")) {
  expect_is(broom::tidy(wb <- wbgee(wks ~ lag(union) + lag(lwage),
                                       data = wages), conf.int = TRUE), 
            "data.frame")
  expect_is(broom::glance(wb), "data.frame")
}