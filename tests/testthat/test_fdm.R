data("WageData")


# Defaults ----------------------------------------------------------------
context("fdm defaults")
wages <- WageData
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)
wb <- fdm(wks ~ union + lwage, data = wages)

test_that("fdm defaults work", {
  expect_s3_class(wb, "fdm")
})

test_that("fdm summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})


# Lags --------------------------------------------------------------------
context("fdm with lags")
wb <- fdm(wks ~ lag(union) + lag(lwage), data = wages)

test_that("fdm with lags works", {
  expect_s3_class(wb, "fdm")
})

test_that("fdm summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

context("fdm multiple lags")
wb <- fdm(wks ~ union + lag(union) + lag(lwage), data = wages)

test_that("fdm with multiple lags works", {
  expect_s3_class(wb, "fdm")
})

test_that("fdm summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

context("fdm non-standard lags")
wb <- fdm(wks ~ union + lag(union, 2) + lag(lwage), data = wages)

test_that("fdm with non-standard lags works", {
  expect_s3_class(wb, "fdm")
})
test_that("fdm summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

# Model types -------------------------------------------------------------
context("fdm with constant interaction")
wb <- fdm(wks ~ union + lag(lwage) * blk, data = wages)

test_that("fdm with contextual model works", {
  expect_s3_class(wb, "fdm")
})
test_that("fdm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

context("fdm with constrained variance")

test_that("fdm with within model works", {
  expect_s3_class(wb <- fdm(wks ~ union + lag(lwage), data = wages, 
                            variance = "constrained"), "fdm")
})
test_that("fdm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

context("fdm with unconstrained variance")
wb <- fdm(wks ~ union + lag(lwage), data = wages, variance = "unconstrained")

test_that("fdm with between model works", {
  expect_s3_class(wb, "fdm")
})
test_that("fdm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})


# Other options -----------------------------------------------------------
context("fdm with use.wave")
wb <- fdm(wks ~ union + lag(lwage) | blk, data = wages,
            use.wave = TRUE)

test_that("fdm with use.wave works", {
  expect_s3_class(wb, "fdm")
})
test_that("fdm summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

# Missing data ------------------------------------------------------------
context("fdm missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$t == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- fdm(wks ~ lag(union) + lag(lwage) | blk, data = wagesm)

test_that("fdm with defaults works", {
  expect_s3_class(wb, "fdm")
})
test_that("fdm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.fdm")
  expect_output(print(swb))
})

# factors -----------------------------------------------------------------
context("fdm with time-varying factors")
if (requireNamespace("plm")) {
  data(Males, package = "plm")
  males <- panel_data(Males, id = nr, wave = year)
  set.seed(2)
  # Cutting down on the time it takes to fit these models
  males <- filter(males, nr %in% sample(males$nr, 100))
  test_that("Models with time-varying factors work", {
    expect_s3_class(wbf <- fdm(wage ~ industry, data = males),
                    "fdm")
    expect_output(print(summary(wbf)))
  })
}

# tidiers -----------------------------------------------------------------
context("fdm tidiers")
if (requireNamespace("broom.mixed")) {
  expect_is(generics::tidy(wb <- fdm(wks ~ lag(union) + lag(lwage),
                                       data = wages), conf.int = TRUE), 
            "data.frame")
  expect_is(generics::glance(wb), "data.frame")
}