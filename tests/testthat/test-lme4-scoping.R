test_that("wbm defaults work inside a wrapper without changing caller bindings", {
  fit_model <- function() {
    weights <- "caller weights"
    offset <- "caller offset"
    model_formula <- wks ~ union + lwage | blk
    fit <- wbm(model_formula, data = WageData[1:210, ], id = "id", wave = "t",
               pR2 = FALSE)
    expect_identical(weights, "caller weights")
    expect_identical(offset, "caller offset")
    fit
  }

  fit <- fit_model()
  expect_s4_class(fit, "wblm")
  expect_equal(stats::weights(fit), rep(1, nobs(fit, entities = FALSE)))
  expect_equal(lme4::getME(fit, "offset"), rep(0, nobs(fit, entities = FALSE)))
})

test_that("wbm preserves covariance metadata for display and likelihoods", {
  fit <- wbm(wks ~ union + lwage | blk, data = WageData[1:210, ],
             id = "id", wave = "t", pR2 = FALSE)
  reference <- lme4::lmer(formula(fit, raw = TRUE), data = fit@frame)
  roundtrip <- to_merMod(fit)

  expect_output(methods::show(fit), "Linear mixed model")
  expect_equal(logLik(fit), logLik(reference))
  expect_equal(AIC(fit), AIC(reference))
  expect_equal(logLik(roundtrip), logLik(reference))
  for (name in setdiff(names(attributes(reference)),
                       c(methods::slotNames(reference), "class"))) {
    expect_identical(attr(fit, name), attr(reference, name))
    expect_identical(attr(roundtrip, name), attr(reference, name))
  }
})

test_that("wbm keeps weights and offsets aligned after lags and missing data", {
  d <- WageData[1:210, ]
  d$wts <- seq(0.75, 1.25, length.out = nrow(d))
  d$offs <- seq(-0.1, 0.1, length.out = nrow(d))
  d$lwage[23] <- NA_real_
  fit <- wbm(wks ~ union + lag(lwage) | blk, data = d, id = "id", wave = "t",
             weights = wts, offset = offs, pR2 = FALSE)
  frame <- as.data.frame(fit@frame)
  reference <- lme4::lmer(formula(fit, raw = TRUE), data = frame,
                           weights = .weights, offset = .offset)

  expect_lt(nobs(fit, entities = FALSE), nrow(d))
  expect_equal(stats::weights(fit), frame$.weights)
  expect_equal(lme4::getME(fit, "offset"), frame$.offset)
  expect_equal(lme4::fixef(fit), lme4::fixef(reference), tolerance = 1e-7)
})

test_that("wbm passes weights and offsets to Poisson models", {
  d <- WageData[1:210, ]
  d$wts <- seq(0.75, 1.25, length.out = nrow(d))
  d$offs <- seq(-0.1, 0.1, length.out = nrow(d))
  fit <- wbm(wks ~ union + lwage | blk, data = d, id = "id", wave = "t",
             weights = wts, offset = offs, family = poisson, nAGQ = 0L,
             pR2 = FALSE, pvals = FALSE)
  frame <- as.data.frame(fit@frame)
  reference <- lme4::glmer(formula(fit, raw = TRUE), data = frame, family = poisson,
                            weights = .weights, offset = .offset, nAGQ = 0L)

  expect_s4_class(fit, "wbglm")
  expect_equal(stats::weights(fit), frame$.weights)
  expect_equal(lme4::getME(fit, "offset"), frame$.offset)
  expect_equal(lme4::fixef(fit), lme4::fixef(reference), tolerance = 1e-7)
})

test_that("wbm passes weights and offsets to negative binomial models", {
  d <- WageData[1:210, ]
  d$wks <- as.integer((seq_len(nrow(d)) %% 11)^2)
  d$wts <- seq(0.75, 1.25, length.out = nrow(d))
  d$offs <- seq(-0.1, 0.1, length.out = nrow(d))
  fit <- wbm(wks ~ union + lwage | blk, data = d, id = "id", wave = "t",
             weights = wts, offset = offs, family = negbinomial, nAGQ = 0L,
             pR2 = FALSE, pvals = FALSE)

  expect_s4_class(fit, "wbglm")
  expect_equal(stats::weights(fit), fit@frame$.weights)
  expect_equal(lme4::getME(fit, "offset"), fit@frame$.offset)
})
