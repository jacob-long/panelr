data("WageData")

context("Setup")
wages <- WageData
wages <- wages[1:203,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)

context("wbm defaults")
wb <- wbm(wks ~ union + lwage, data = wages)

test_that("wbm defaults work", {
  expect_s3_class(wb, "wbm")
})

test_that("wbm summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with lags")
wb <- wbm(wks ~ lag(union) + lag(lwage), data = wages)

test_that("wbm with lags works", {
  expect_s3_class(wb, "wbm")
})

test_that("wbm summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm multiple lags")
wb <- wbm(wks ~ union + lag(union) + lag(lwage),
       data = wages)

test_that("wbm with multiple lags works", {
  expect_s3_class(wb, "wbm")
})

test_that("wbm summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm non-standard lags")
wb <- wbm(wks ~ union + lag(union, 2) + lag(lwage),
          data = wages)

test_that("wbm with non-standard lags works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with use.wave")
wb <- wbm(wks ~ union + lag(lwage), data = wages,
       use.wave = TRUE)

test_that("wbm with use.wave works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with contextual estimator")
wb <- wbm(wks ~ union + lag(lwage), data = wages,
          estimator = "contextual")

test_that("wbm with contextual estimator works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with contextual estimator)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with within estimator")
wb <- wbm(wks ~ union + lag(lwage), data = wages,
          estimator = "within")

test_that("wbm with within estimator works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with within estimator)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with stability estimator")
wb <- wbm(wks ~ union + lag(lwage), data = wages,
          estimator = "stability")

test_that("wbm with stability estimator works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with stability estimator)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with between-estimator")
wb <- wbm(wks ~ union + lag(lwage), data = wages,
       estimator = "between")

test_that("wbm with between estimator works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (with between estimator)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm as binomial glm")
wb <- wbm(union ~ wks + lag(lwage), data = wages,
          family = binomial)

test_that("wbm with binomial family works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (as binomial glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm as negbinomial glm")
library(lme4)
wb <- suppressWarnings(wbm(wks ~ union + lag(lwage), data = wages,
          family = negbinomial))

test_that("wbm with negbinomial family works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (as negbinomial glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm pseudo-R2")
wb <- wbm(wks ~ union + lag(lwage), data = wages, pR2 = TRUE)

test_that("wbm works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm p-values on/off")
wb <- wbm(wks ~ union + lag(lwage), data = wages, pvals = TRUE)
wb2 <- wbm(wks ~ union + lag(lwage), data = wages, pvals = FALSE)

test_that("wbm works", {
  expect_s3_class(wb, "wbm")
  expect_s3_class(wb2, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
  expect_s3_class(swb2 <- summary(wb2), "summary.wbm")
  expect_output(print(swb2))
})

context("wbm with weights")
wb <- wbm(wks ~ union + lag(lwage), data = wages, weights = wts)

test_that("wbm works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("Missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$wave == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- wbm(wks ~ lag(union) + lag(lwage), data = wagesm)

test_that("wbm with defaults works", {
  expect_s3_class(wb, "wbm")
})
test_that("wbm summary works (as negbinomial glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

