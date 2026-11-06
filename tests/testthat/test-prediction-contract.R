prediction_fixture <- function() {
  set.seed(4201)
  d <- data.frame(id = rep(seq_len(24), each = 6), wave = rep(1:6, 24))
  d$x <- rep(rnorm(24, 3, 1), each = 6) + rnorm(144)
  d$z <- rep(rnorm(24, 2, 1), each = 6) + rnorm(144)
  d$v <- rnorm(144)
  d$g <- rep(rnorm(24), each = 6)
  d$f <- factor(rep(rep(c("A", "B", "C"), 8), each = 6))
  d$y <- 2 + 1.2 * d$x - .5 * d$z + .9 * d$x * d$z +
    rep(rnorm(24), each = 6) + rnorm(144, sd = .5)
  d
}

prediction_fit <- function(formula, data = prediction_fixture(), ...) {
  suppressMessages(wbm(formula, panel_data(data), pR2 = FALSE, pvals = FALSE, ...))
}

fixed_prediction <- function(m) unname(drop(lme4::getME(m, "X") %*% lme4::fixef(m)))

test_that("interaction estimation and prediction obey the same algebra", {
  d <- prediction_fixture()
  cx <- d$x - ave(d$x, d$id)
  cz <- d$z - ave(d$z, d$id)
  expected <- list(`double-demean` = cx * cz - ave(cx * cz, d$id),
                   demean = d$x * d$z - ave(d$x * d$z, d$id), raw = cx * cz)
  for (style in names(expected)) {
    m <- prediction_fit(y ~ x * z, interaction.style = style)
    expect_equal(m@frame[["x:z"]], expected[[style]], tolerance = 1e-10)
    ref <- fixed_prediction(m)
    expect_equal(unname(predict(m, re.form = NA)), ref)
    expect_equal(predict(m), predict(to_merMod(m)))
    expect_equal(unname(predict(m, d, re.form = NA)), ref)
    expect_equal(unname(predict(m, m@frame, raw = TRUE, re.form = NA)), ref)
    pieces <- vapply(seq_len(nrow(d)), function(i) {
      unname(predict(m, m@frame[i, ], raw = TRUE, re.form = NA))
    }, numeric(1))
    expect_equal(pieces, ref)
    expect_identical(process_raw_newdata(m, m@frame), m@frame)
  }
})

test_that("panel prediction preserves order, roles, and does not require the response", {
  d <- prediction_fixture()
  m <- prediction_fit(y ~ x + g)
  new <- d[nrow(d):1, setdiff(names(d), "y")]
  expect_equal(unname(predict(m, new, re.form = NA)), rev(fixed_prediction(m)))
  expect_s3_class(formula(m), "Formula")
  expect_s3_class(formula(m, raw = TRUE), "formula")
  for (re in list(NA, ~0)) {
    expect_error(predict(m, d[, names(d) != "id"], re.form = re), "Panel predictions require")
  }
})

test_that("factor subsets and wave factors retain the training schema", {
  d <- prediction_fixture()
  m <- prediction_fit(y ~ x | f | x * f)
  rows <- which(d$f == "A")
  expect_equal(unname(predict(m, d[rows, ], re.form = NA)), fixed_prediction(m)[rows])
  new <- d[rows, ]
  new$f <- "new"
  expect_error(predict(m, new, re.form = NA), "New levels")
  w <- prediction_fit(y ~ x + z, use.wave = TRUE, wave.factor = TRUE)
  expect_equal(unname(predict(w, d, re.form = NA)), fixed_prediction(w))
  expect_error(predict(w, d[, names(d) != "wave"]), "must contain.*wave")
})

test_that("scaling and three-way interactions reproduce training predictions", {
  d <- prediction_fixture()
  for (style in c("double-demean", "demean", "raw")) {
    m <- prediction_fit(y ~ x * z * v, interaction.style = style, scale = TRUE)
    expect_equal(unname(predict(m, d, re.form = NA)), fixed_prediction(m), tolerance = 1e-8)
    expect_equal(unname(predict(m, m@frame, raw = TRUE, re.form = NA)), fixed_prediction(m))
  }
})

test_that("basis predictions reuse the fitted basis parameters", {
  d <- prediction_fixture()
  for (form in list(y ~ splines::ns(x, df = 3), y ~ poly(x, 2))) {
    m <- prediction_fit(form)
    expect_equal(unname(predict(m, d, re.form = NA)), fixed_prediction(m), tolerance = 1e-8)
    rows <- d$id <= 10
    expect_equal(unname(predict(m, d[rows, ], re.form = NA)), fixed_prediction(m)[rows], tolerance = 1e-8)
  }
})

test_that("simulation shares prediction preprocessing", {
  d <- prediction_fixture()
  m <- prediction_fit(y ~ x * z)
  a <- simulate(m, newdata = d, nsim = 2, seed = 43)
  b <- simulate(to_merMod(m), newdata = m@frame, nsim = 2, seed = 43)
  expect_equal(as.matrix(a), as.matrix(b))
})

test_that("fixed-effect uncertainty matches the design matrix", {
  m <- prediction_fit(y ~ x * z)
  X <- lme4::getME(m, "X")
  expected <- sqrt(rowSums((X %*% as.matrix(vcov(m))) * X))
  out <- predict(m, re.form = NA, se.fit = TRUE)
  expect_equal(unname(out$fit), fixed_prediction(m))
  expect_equal(out$se.fit, unname(expected))
  out2 <- predict(m, m@frame, raw = TRUE, re.form = NA, se.fit = TRUE)
  expect_equal(out2, out)
  expect_error(predict(m, use.re.var = TRUE), "does not give a valid")
})

test_that("hypothetical grids freeze interaction centering rather than recentering rows", {
  for (style in c("double-demean", "demean", "raw")) {
    m <- prediction_fit(y ~ x * z, interaction.style = style)
    grid <- data.frame(x = c(2, 4, 6), z = c(1, 2, 3))
    prepared <- prepare_wb_grid(m, grid)
    refs <- m@call_info$prediction$reference
    x <- grid$x - refs[["imean(x)"]]
    z <- grid$z - refs[["imean(z)"]]
    product <- if (style == "demean") grid$x * grid$z else x * z
    if (style != "raw") product <- product - refs[["imean(x:z)"]]
    expect_equal(prepared[["x:z"]], product)
    expect_equal(prepare_wb_grid(m, grid[1, ])[["x:z"]], product[1])
    expect_equal(prepared$x, x)
    expect_equal(prepared$z, z)
  }
})

test_that("interactions plots use correct cross-level fitted values and intervals", {
  skip_if_not_installed("interactions")
  m <- prediction_fit(y ~ x | g | x * g)
  plot <- interactions::interact_plot(m, pred = x, modx = g,
                                     modx.values = c(-1, 0, 1), interval = TRUE)
  pm <- plot$data
  refs <- m@call_info$prediction$reference
  x <- if (identical(attr(plot, "panelr")$scale, "within")) pm$x else pm$x - refs[["imean(x)"]]
  X <- cbind(1, refs[["imean(x)"]], x, pm$g,
             x * pm$g - refs[["imean(x:g)"]])
  colnames(X) <- c("(Intercept)", "`imean(x)`", "x", "g", "`x:g`")
  X <- X[, names(lme4::fixef(m)), drop = FALSE]
  expected <- drop(X %*% lme4::fixef(m))
  se <- sqrt(rowSums((X %*% as.matrix(vcov(m))) * X))
  critical <- qt(.975, df.residual(m))
  expect_equal(unname(pm$y), unname(expected))
  expect_equal(unname(pm$ymax), unname(expected + critical * se))
  expect_equal(unname(pm$ymin), unname(expected - critical * se))
  expect_silent(ggplot2::ggplot_build(plot))
})

test_that("interactions supports numeric three-way and factor moderators", {
  skip_if_not_installed("interactions")
  triple <- prediction_fit(y ~ x * z * v)
  plot <- interactions::interact_plot(triple, pred = x, modx = z, mod2 = v,
                                     modx.values = c(-1, 1), mod2.values = c(-1, 1))
  expect_gt(nrow(plot$data), 0)
  expect_true(all(is.finite(plot$data$y)))
  factor <- prediction_fit(y ~ x | f | x * f)
  plot <- interactions::interact_plot(factor, pred = x, modx = f, interval = TRUE)
  expect_equal(length(unique(plot$data$f)), 3)
  expect_true(all(is.finite(plot$data$y)))
})

test_that("GEE predictions and plot intervals share the same transformations", {
  skip_if_not_installed("geepack")
  d <- prediction_fixture()
  m <- wbgee(y ~ x * z, panel_data(d), calc.fit.stats = FALSE)
  ref <- unname(predict(m))
  expect_equal(unname(predict(m, d)), ref)
  expect_equal(unname(predict(m, m$frame, raw = TRUE)), ref)
  expect_equal(unname(predict(m, d[nrow(d):1, ])), rev(ref))
  pm <- jtools::make_predictions(m, pred = "x", at = list(z = 2), interval = TRUE)
  expect_true(all(pm$ymin <= pm$y & pm$y <= pm$ymax))
})

test_that("generalized models transform point predictions and uncertainty once", {
  d <- prediction_fixture()
  set.seed(817)
  d$y <- rpois(nrow(d), exp(.1 * d$x + .15 * d$z + .05 * d$x * d$z))
  m <- prediction_fit(y ~ x * z, data = d, family = poisson,
                      control = lme4::glmerControl(optimizer = "bobyqa"))
  eta <- fixed_prediction(m)
  expect_equal(unname(predict(m, re.form = NA)), eta)
  expect_equal(unname(predict(m, re.form = NA, type = "response")), exp(eta))
  expect_equal(unname(predict(m, d, re.form = NA, type = "response")), exp(eta))
  link <- predict(m, d, re.form = NA, se.fit = TRUE)
  response <- predict(m, d, re.form = NA, se.fit = TRUE, type = "response")
  expect_equal(response$se.fit, link$se.fit * exp(eta))
  expect_equal(predict(m, type = "response"), predict(to_merMod(m), type = "response"))
  skip_if_not_installed("interactions")
  plot <- interactions::interact_plot(m, pred = x, modx = z,
                                     modx.values = c(-1, 1), interval = TRUE)
  grid <- plot$data
  if (identical(attr(plot, "panelr")$scale, "within")) {
    for (v in c("x", "z")) grid[[v]] <- grid[[v]] + m@call_info$prediction$reference[[paste0("imean(", v, ")")]]
  }
  nd <- prepare_wb_grid(m, grid)
  expected <- predict(m, nd, raw = TRUE, re.form = NA, type = "response")
  expect_equal(unname(plot$data$y), unname(expected))
  expect_true(all(plot$data$ymin <= plot$data$y & plot$data$y <= plot$data$ymax))
})

test_that("offsets survive training and new-data prediction", {
  d <- prediction_fixture()
  d$exposure <- exp(d$v)
  m <- prediction_fit(y ~ x * z, data = d, offset = log(exposure))
  expected <- fixed_prediction(m) + log(d$exposure)
  expect_equal(unname(predict(m, re.form = NA)), expected)
  expect_equal(unname(predict(m, d, re.form = NA)), expected)
  expect_equal(unname(predict(m, m@frame, raw = TRUE, re.form = NA)), expected)
  new <- d
  new$exposure <- new$exposure * 2
  expect_equal(unname(predict(m, new, re.form = NA)), expected + log(2))
})

test_that("lagged and detrended panel predictions agree with fitted designs", {
  d <- prediction_fixture()
  lagged <- prediction_fit(y ~ lag(x) * z)
  p <- predict(lagged, d, re.form = NA)
  expect_true(all(is.na(p[d$wave == 1])))
  expect_equal(unname(p[d$wave != 1]), fixed_prediction(lagged))
  m <- prediction_fit(y ~ x * z, detrend = TRUE, dt.random = FALSE)
  expect_equal(unname(predict(m, d, re.form = NA)), fixed_prediction(m))
  expect_equal(as.numeric(tapply(m@frame[["x:z"]], m@frame$id, mean)), rep(0, 24), tolerance = 1e-8)
  expect_error(jtools::make_predictions(m, pred = "x"), "Automatic prediction grids")
})

test_that("missing values keep prediction and uncertainty rows aligned", {
  m <- prediction_fit(y ~ x * z)
  new <- m@frame
  new$x[c(2, 10)] <- NA_real_
  p <- predict(m, new, raw = TRUE, re.form = NA, se.fit = TRUE)
  expect_length(p$fit, nrow(new))
  expect_identical(unname(which(is.na(p$fit))), c(2L, 10L))
  expect_identical(is.na(p$se.fit), unname(is.na(p$fit)))
  omitted <- predict(m, new, raw = TRUE, re.form = NA, se.fit = TRUE, na.action = na.omit)
  expect_equal(unname(omitted$fit), unname(p$fit[-c(2, 10)]))
  expect_equal(omitted$se.fit, p$se.fit[-c(2, 10)])
})

test_that("bootstrap predictions return simulation draws", {
  m <- prediction_fit(y ~ x * z)
  set.seed(123)
  out <- predict(m, m@frame[1:3, ], raw = TRUE, re.form = NA,
                 se.fit = TRUE, boot = TRUE, sims = 5)
  expect_equal(dim(out$fit), c(5L, 3L))
  expect_true(all(is.finite(out$fit)))
})

test_that("random-slope predictions use the fitted random-effects design", {
  d <- prediction_fixture()
  m <- prediction_fit(y ~ x | g | x * g + (1 + x | id))
  expected <- fixed_prediction(m) + as.numeric(lme4::getME(m, "Z") %*% lme4::getME(m, "b"))
  expect_equal(unname(predict(m)), expected)
  expect_equal(unname(predict(m, re.form = ~(1 + x | id))), expected)
  expect_equal(unname(predict(m, d)), expected)
  new <- d
  new$id <- new$id + 24
  expect_equal(unname(predict(m, new, allow.new.levels = TRUE)), fixed_prediction(m))
  expect_error(predict(m, new, allow.new.levels = FALSE), "new levels")
})

test_that("all scalar model styles replay fitted transformations", {
  d <- prediction_fixture()
  for (model in c("within", "contextual", "random", "between")) {
    for (style in c("double-demean", "demean", "raw")) {
      m <- prediction_fit(y ~ x * z, model = model, interaction.style = style)
      expect_equal(unname(predict(m, d, re.form = NA)), fixed_prediction(m))
    }
  }
})

test_that("plot reference means can be changed explicitly", {
  m <- prediction_fit(y ~ x * z)
  at <- list(z = 2, `imean(x)` = 1, `imean(x:z)` = .2)
  pm <- jtools::make_predictions(m, pred = "x", pred.values = c(2, 3),
                                 at = at, interval = FALSE)
  expect_equal(pm[["imean(x)"]], c(1, 1))
  expect_equal(pm[["imean(x:z)"]], c(.2, .2))
  grid <- prepare_wb_grid(m, pm)
  expect_equal(grid[["x:z"]], (pm$x - 1) * (2 - m@call_info$prediction$reference[["imean(z)"]]) - .2)
  expect_equal(unname(pm$y), unname(predict(m, grid, raw = TRUE, re.form = NA)))
})

test_that("categorical plotting and GEE plotting render", {
  skip_if_not_installed("interactions")
  m <- prediction_fit(y ~ x | f | x * f)
  p <- interactions::cat_plot(m, pred = f, modx = x, modx.values = c(-1, 0, 1), interval = TRUE)
  expect_gt(nrow(p$data), 0)
  expect_true(all(is.finite(p$data$y)))
  expect_silent(ggplot2::ggplot_build(p))
  expect_error(interactions::interact_plot(m, pred = x, modx = f,
                                          partial.residuals = TRUE), "Partial residuals")
  skip_if_not_installed("geepack")
  gee <- wbgee(y ~ x | f | x * f, panel_data(prediction_fixture()), calc.fit.stats = FALSE)
  p <- interactions::interact_plot(gee, pred = x, modx = f, interval = TRUE)
  expect_true(all(is.finite(p$data$y)))
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("legacy fits fail clearly when preprocessing information is missing", {
  m <- prediction_fit(y ~ x * z)
  m@call_info$prediction <- NULL
  expect_error(predict(m, prediction_fixture()), "Refit this model")
  expect_equal(unname(predict(m, m@frame, raw = TRUE, re.form = NA)), fixed_prediction(m))
})

test_that("offset simulation and GEE prediction keep offset values", {
  d <- prediction_fixture()
  m <- prediction_fit(y ~ x * z, data = d, offset = v)
  a <- simulate(m, newdata = d, nsim = 2, seed = 43)
  new <- d
  new$v <- new$v + 2
  b <- simulate(m, newdata = new, nsim = 2, seed = 43)
  expect_equal(unname(as.matrix(b) - as.matrix(a)), matrix(2, nrow(d), 2))
  skip_if_not_installed("geepack")
  gee <- wbgee(y ~ x * z, panel_data(d), offset = v, calc.fit.stats = FALSE)
  expect_equal(unname(predict(gee, d)), unname(predict(gee)))
  expect_equal(unname(predict(gee, new) - predict(gee, d)), rep(2, nrow(d)))
})

test_that("constant moderators factor out of interaction centering", {
  for (style in c("double-demean", "demean", "raw")) {
    m <- prediction_fit(y ~ x | g | x * g, interaction.style = style)
    grid <- data.frame(x = c(1, 3, 5), g = c(-2, 0, 2))
    p <- prepare_wb_grid(m, grid)
    expected <- (grid$x - m@call_info$prediction$reference[["imean(x)"]]) * grid$g
    expect_equal(p[["x:g"]], expected)
    m <- prediction_fit(y ~ x + z | g | x:z:g, interaction.style = style)
    grid$z <- c(2, 4, 6)
    p <- prepare_wb_grid(m, grid)
    ref <- m@call_info$prediction$reference
    prod <- if (style == "demean") grid$x * grid$z else
      (grid$x - ref[["imean(x)"]]) * (grid$z - ref[["imean(z)"]])
    if (style != "raw") prod <- prod - ref[["imean(x:z)"]]
    expect_equal(p[["x:z:g"]], prod * grid$g)
  }
})
