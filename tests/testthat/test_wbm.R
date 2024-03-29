data("WageData")

# Defaults ----------------------------------------------------------------
context("wbm defaults")
WageData <- WageData %>% 
  mutate(
    south_names = factor(ifelse(south == 0, "north", "south")),
    union_names = factor(ifelse(union == 1, "yes", "no"))
  )
wages <- panel_data(WageData, id = id, wave = t)
wages <- wages[8:210,] # Reduce runtime
# Add arbitrary weights for testing
wages$wts <- runif(nrow(wages), 0.3, 3)
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

test_that("wbm with within model works", {
  expect_warning(wb <- wbm(wks ~ union + lag(lwage) | blk, data = wages,
                 model = "within"))
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
wb <- suppressWarnings(wbm(wks ~ union + lag(lwage) | fem, data = wages,
          family = poisson, nAGQ = 0L))

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
          family = negbinomial, nAGQ = 0L))

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
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})


# Custom random effects ---------------------------------------------------
context("Custom random effects")

wb <- wbm(wks ~ union + lag(lwage) | blk | (union | id), data = wages)

test_that("wbm works", {
  expect_s4_class(wb, "wbm")
})
test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

# Multiple random effects
test_that("wbm works with multiple random effects", {
  suppressWarnings({
    wb <- wbm(wks ~ union + lag(lwage) | blk | (union | id) + (lag(lwage) | id),
            data = wages)
  })
  expect_s4_class(wb, "wbm")
})

test_that("wbm works with factors in random effect", {
  suppressWarnings({
    wb <- wbm(wks ~ union_names + lag(lwage) | blk | (union_names | id) + 
              (lag(lwage) | id),
              data = wages)
  })
  expect_s4_class(wb, "wbm")
})

test_that("wbm works with lagged factors in random effect", {
  suppressWarnings({
    wb <- wbm(wks ~ union_names + lag(lwage) | blk | (lag(union_names) | id) + 
              (lwage | id),
              data = wages)
  })
  expect_s4_class(wb, "wbm")
})

test_that("wbm works with variable only in random effect", {
  suppressWarnings({
    wb <- wbm(wks ~ lag(lwage) | blk | (union_names | id) + 
              (lag(lwage) | id),
              data = wages)
  })
  expect_s4_class(wb, "wbm")
})

test_that("wbm summary works", {
  expect_s3_class(swb <- summary(wb), "summary.wbm")
  expect_output(print(swb))
})

# wbm with detrending ---------------------------------------------------
context("wbm with detrending")

# May not converge perfectly
wb1 <- suppressWarnings(wbm(wks ~ union + lag(lwage) | blk | (union | id),
           data = wages, pvals = FALSE, detrend = TRUE))
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


# factors -----------------------------------------------------------------
context("Time-varying factors")
if (requireNamespace("plm")) {
  data(Males, package = "plm")
  males <- panel_data(Males, id = nr, wave = year)
  set.seed(2)
  # Cutting down on the time it takes to fit these models
  males <- filter(males, nr %in% sample(males$nr, 100))
  test_that("Models with time-varying factors work", {
    expect_s4_class(wbf <- wbm(wage ~ industry + exper | ethn, data = males),
                    "wbm")
    expect_output(print(summary(wbf)))
    expect_s4_class(wbf <- wbm(wage ~ industry * exper | ethn, data = males),
                    "wbm")
    expect_output(print(summary(wbf)))
    expect_s4_class(wbf <- wbm(wage ~ industry + exper | ethn | industry * ethn,
                               data = males),
                    "wbm")
    expect_output(print(summary(wbf)))
  })
}


# wbm_stan ----------------------------------------------------------------
context("wbm_stan")

if (requireNamespace("brms")) {
model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE)

test_that("wbm_stan makes code and data", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

library(brms)

model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | (blk | id),
                  data = wages, chains = 1, iter = 2000, fit_model = FALSE)

test_that("wbm_stan works w/ custom random effect", {
  expect_s3_class(model$stan_data, "standata")
  expect_s3_class(model$stan_code, "brmsmodel")
})

model <- wbm_stan(lwage ~ lag(union) + wks, data = wages,
                  model = "within", fit_model = FALSE, model.cor = TRUE)
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
}

# predictions -----------------------------------------------------------------
context("wbm predictions")
model <- wbm(lwage ~ lag(union) + wks, data = wages)
test_that("wbm predictions work w/o newdata", {
  expect_is(predict(model), "numeric")
})

test_that("wbm predictions work w/ non-raw newdata", {
  # non-panel_data input
  expect_is(predict(model, newdata = data.frame(
    union = 1:4,
    wks = 40,
    lwage = 50,
    id = 1,
    t = 5
  )), "numeric")
  # without random effects
  expect_is(predict(model, newdata = data.frame(
    union = 1:4,
    wks = 40,
    lwage = 50,
    id = 1,
    t = 5
  ), re.form = ~0), "numeric")
  # panel_data input
  expect_is(predict(model, newdata = panel_data(data.frame(
    union = 1:4,
    wks = 40,
    lwage = 50,
    id = 1,
    t = 5
  ), id = id, wave = t, strict = FALSE)), "numeric")
  # without random effects
  expect_is(predict(model, newdata = panel_data(data.frame(
    union = 1:4,
    wks = 40,
    lwage = 50,
    id = 1,
    t = 5
  ), id = id, wave = t, strict = FALSE), re.form = ~0), "numeric")
})

test_that("wbm predictions work w/ raw newdata", {
  expect_is(predict(model, newdata = data.frame(
    `lag(union)` = -2:2,
    wks = 0,
    `imean(wks)` = 40,
    `imean(lag(union))` = 2,
    lwage = 50,
    id = 1,
    t = 5, check.names = FALSE
  ), raw = TRUE), "numeric")
  expect_is(predict(model, newdata = data.frame(
    `lag(union)` = -2:2,
    wks = 0,
    `imean(wks)` = 40,
    `imean(lag(union))` = 2,
    lwage = 50,
    id = 1,
    t = 5, check.names = FALSE
  ), raw = TRUE, re.form = ~0), "numeric")
})
