data("WageData")


# Defaults ----------------------------------------------------------------
context("asym defaults")
wages <- WageData
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)
wb <- asym(wks ~ union + lwage, data = wages)

test_that("asym defaults work", {
  expect_s3_class(wb, "asym")
})

test_that("asym summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})


# Lags --------------------------------------------------------------------
context("asym with lags")
wb <- asym(wks ~ lag(union) + lag(lwage), data = wages)

test_that("asym with lags works", {
  expect_s3_class(wb, "asym")
})

test_that("asym summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

context("asym multiple lags")
wb <- asym(wks ~ union + lag(union) + lag(lwage), data = wages)

test_that("asym with multiple lags works", {
  expect_s3_class(wb, "asym")
})

test_that("asym summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

context("asym non-standard lags")
wb <- asym(wks ~ union + lag(union, 2) + lag(lwage), data = wages)

test_that("asym with non-standard lags works", {
  expect_s3_class(wb, "asym")
})
test_that("asym summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

# Model types -------------------------------------------------------------
context("asym with constant interaction")
wb <- asym(wks ~ union + lag(lwage) * blk, data = wages)

test_that("asym with contextual model works", {
  expect_s3_class(wb, "asym")
})
test_that("asym summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

context("asym with constrained variance")

test_that("asym with within model works", {
  expect_s3_class(wb <- asym(wks ~ union + lag(lwage), data = wages, 
                            variance = "constrained"), "asym")
})
test_that("asym summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

context("asym with unconstrained variance")
wb <- asym(wks ~ union + lag(lwage), data = wages, variance = "unconstrained")

test_that("asym with between model works", {
  expect_s3_class(wb, "asym")
})
test_that("asym summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})


# Other options -----------------------------------------------------------
context("asym with use.wave")
wb <- asym(wks ~ union + lag(lwage) | blk, data = wages,
          use.wave = TRUE)

test_that("asym with use.wave works", {
  expect_s3_class(wb, "asym")
})
test_that("asym summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

# Missing data ------------------------------------------------------------
context("asym missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$t == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- asym(wks ~ lag(union) + lag(lwage) | blk, data = wagesm)

test_that("asym with defaults works", {
  expect_s3_class(wb, "asym")
})
test_that("asym summary works", {
  expect_s3_class(swb <- summary(wb), "summary.asym")
  expect_output(print(swb))
})

# factors -----------------------------------------------------------------
context("asym w/ time-varying factors")
if (requireNamespace("plm")) {
  data(Males, package = "plm")
  males <- panel_data(Males, id = nr, wave = year)
  set.seed(2)
  # Cutting down on the time it takes to fit these models
  males <- filter(males, nr %in% sample(males$nr, 100))
  test_that("Models with time-varying factors work", {
    expect_s3_class(wbf <- asym(wage ~ industry, data = males),
                    "asym")
    expect_output(print(summary(wbf)))
  })
}


# tidiers -----------------------------------------------------------------
context("asym tidiers")
if (requireNamespace("broom")) {
  expect_is(broom::tidy(wb <- asym(wks ~ lag(union) + lag(lwage),
                                       data = wages), conf.int = TRUE), 
            "data.frame")
  expect_is(broom::glance(wb), "data.frame")
}
