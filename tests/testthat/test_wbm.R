data("WageData")


# Setup -------------------------------------------------------------------
context("Setup")
wages <- WageData
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
# Make it a panel data frame
wages <- panel_data(wages, id = id, wave = t)


# Defaults ----------------------------------------------------------------
context("wbm defaults")
wb <- wbm(wks ~ union + lwage | blk, data = wages)

test_that("wbm defaults work", {
  expect_s4_class(wb, "wbm")
})

test_that("wbm summary works (defaults)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# Lags --------------------------------------------------------------------
context("wbm with lags")
wb <- wbm(wks ~ lag(union) + lag(lwage) | blk, data = wages)

test_that("wbm with lags works", {
  expect_s4_class(wb, "wbm")
})

test_that("wbm summary works (with lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm multiple lags")
wb <- wbm(wks ~ union + lag(union) + lag(lwage) | blk,
       data = wages)

test_that("wbm with multiple lags works", {
  expect_s4_class(wb, "wbm")
})

test_that("wbm summary works (with multiple lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm non-standard lags")
wb <- wbm(wks ~ union + lag(union, 2) + lag(lwage) | blk,
          data = wages)

test_that("wbm with non-standard lags works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with non-standard lags)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

# Model types -------------------------------------------------------------
context("wbm with contextual model")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages,
          model = "contextual")

test_that("wbm with contextual model works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with contextual model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with within model")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages,
          model = "within")

test_that("wbm with within model works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with within model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with stability model")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages, model = "stability")

test_that("wbm with stability model works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with stability model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm with between-model")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages,
       model = "between")

test_that("wbm with between model works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with between model)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# GLMs --------------------------------------------------------------------
context("wbm as poisson glm")
wb <- wbm(wks ~ union + lag(lwage) | fem, data = wages,
          family = poisson)

test_that("wbm with poisson family works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (as poisson glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm as negbinomial glm")
library(lme4)
wb <- suppressWarnings(wbm(wks ~ union + lag(lwage) | blk, data = wages,
          family = negbinomial))

test_that("wbm with negbinomial family works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (as negbinomial glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# Other options -----------------------------------------------------------
context("wbm with use.wave")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages,
          use.wave = TRUE)

test_that("wbm with use.wave works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (with use.wave)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm pseudo-R2")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages, pR2 = TRUE)

test_that("wbm works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

context("wbm p-values on/off")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages, pvals = TRUE)
wb2 <- wbm(wks ~ union + lag(lwage) | blk, data = wages, pvals = FALSE)

test_that("wbm works", {
  expect_s4_class(wb, "wbm")
  expect_s4_class(wb2, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
  expect_s3_class(swb2 <- summary(wb2), "summary.wbm")
  expect_output(print(swb2))
})

context("wbm with weights")
wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages, weights = wts)

test_that("wbm works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# Missing data ------------------------------------------------------------
context("Missing data")
# Create some missing data
wagesm <- wages
missings <- sample(unique(wagesm$id),5)
inds <- which(wagesm$id %in% missings & wagesm$t == 7)
wagesm <- wagesm[!(1:length(wagesm$id) %in% inds),]
wb <- wbm(wks ~ lag(union) + lag(lwage) | blk, data = wagesm)

test_that("wbm with defaults works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works (as negbinomial glm)", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# Custom random effects ---------------------------------------------------
context("Custom random effects")

wb <- wbm(wks ~ union + lag(lwage) | blk | (union | id),
          data = wages, pvals = TRUE)

test_that("wbm works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

# wbm with detrending ---------------------------------------------------
context("wbm with detrending")

wb1 <- wbm(wks ~ union + lag(lwage) | blk | (union | id),
          data = wages, pvals = FALSE, detrend = TRUE)
wb2 <- wbm(wks ~ union + lag(lwage) | blk | (union | id),
           data = wages, pvals = FALSE, detrend = TRUE,
           balance_correction = TRUE)

test_that("wbm works (detrend only)", {
  expect_s4_class(wb1, "wbm")
})
test_that("wbm works (w/ balance_correction)", {
  expect_s4_class(wb2, "wbm")
})
test_that("wbm summary works (detrend only)", {
  expect_s3_class(swb1 <- summary(wb1), "summary.wbm")
  expect_output(print(swb1))
})
test_that("wbm summary works (detrend only)", {
  expect_s3_class(swb2 <- summary(wb2), "summary.wbm")
  expect_output(print(swb2))
})


# wbm_stan ----------------------------------------------------------------
context("wbm_stan")
model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE)

test_that("wbm_stan makes code and data", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | (blk | id),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE)

test_that("wbm_stan works w/ custom random effect", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

model <- wbm_stan(lwage ~ lag(union) + wks | blk, data = wages,
                  model = "within", fit_model = FALSE)
model2 <- wbm_stan(lwage ~ lag(union) + wks | blk, data = wages,
                  model = "between", fit_model = FALSE)
model3 <- wbm_stan(lwage ~ lag(union) + wks | blk, data = wages,
                   model = "contextual", fit_model = FALSE)

test_that("wbm_stan works w/ other models", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
  expect_s3_class(model2$stan_data, "standata")
  expect_s3_class(model2$stan_code, "brmsmodel")
  expect_s3_class(model3$stan_data, "standata")
  expect_s3_class(model3$stan_code, "brmsmodel")
})

model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | (blk | id),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE,
                  detrend = TRUE)

test_that("wbm_stan works w/ detrending", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | (blk | id),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE,
                  detrend = TRUE, balance_correction = TRUE)

test_that("wbm_stan works w/ balance correction", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

