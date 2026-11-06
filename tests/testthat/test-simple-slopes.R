slope_data <- function() {
  set.seed(918)
  d <- data.frame(id = rep(1:30, each = 6), wave = rep(1:6, 30))
  d$x <- rep(rnorm(30, 10), each = 6) + rnorm(180)
  d$z <- rep(rnorm(30, 20), each = 6) + rnorm(180)
  d$w <- rnorm(180)
  d$g <- rep(rnorm(30), each = 6)
  d$f <- factor(rep(rep(c("a", "b", "c"), 10), each = 6))
  wx <- d$x - ave(d$x, d$id)
  wz <- d$z - ave(d$z, d$id)
  d$y <- 2 + .7 * wx + .8 * wz + 1.2 * wx * wz +
    rep(rnorm(30), each = 6) + rnorm(180, sd = .3)
  d
}

slope_fit <- function(formula = y ~ x * z, data = slope_data(), ...) {
  suppressMessages(wbm(formula, panel_data(data), pR2 = FALSE, pvals = FALSE, ...))
}

slopes_available <- function() {
  skip_if_not_installed("interactions")
  skip_if(is.null(getS3method("sim_slopes", "wbm", optional = TRUE,
                              envir = asNamespace("interactions"))),
          "interactions needs the S3 slope methods")
}

test_that("the design contrast agrees with hand-derived slopes and variances", {
  for (scaled in c(FALSE, TRUE)) {
    for (style in c("double-demean", "demean", "raw")) {
      m <- slope_fit(scale = scaled, interaction.style = style)
      prep <- prepare_wb_plot(m, slope_data(), "x", "z")
      z <- c(-1, 0, 1)
      grid <- data.frame(x = 0, z = z)
      out <- wb_slope_design(prep$model, "x", grid)
      scale <- m@call_info$prediction$scaling
      sx <- if (is.null(scale$x)) 1 else scale$x[["scale"]]
      sxz <- if (is.null(scale[["x:z"]])) 1 else scale[["x:z"]][["scale"]]
      value <- if (style == "demean") z + m@call_info$prediction$reference[["imean(z)"]] else z
      L <- matrix(0, 3, length(out$beta), dimnames = list(NULL, names(out$beta)))
      L[, "x"] <- 1 / sx
      L[, "`x:z`"] <- value / sxz
      expect_equal(unname(out$L), unname(L), tolerance = 1e-10)
      expect_equal(unname(drop(out$L %*% out$beta)), unname(drop(L %*% lme4::fixef(m))))
      expect_equal(out$vcov, as.matrix(vcov(m)))
      expect_equal(out$nobs, nrow(m@frame))
    }
  }
})

test_that("native slopes match the displayed lines, without refitting", {
  slopes_available()
  m <- slope_fit()
  before <- m
  ss <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1),
                                 cond.int = TRUE, confint = TRUE)
  p <- interactions::interact_plot(m, x, z, modx.values = c(-1, 0, 1), interval = TRUE)
  for (i in seq_len(nrow(ss$slopes))) {
    rows <- p$data[p$data$z == ss$slopes[i, 1], ]
    slope <- diff(range(rows$y)) / diff(range(rows$x))
    slope <- slope * sign(tail(rows$y, 1) - rows$y[1])
    expect_equal(ss$slopes$Est.[i], unname(slope), tolerance = 1e-8)
  }
  expect_length(ss$mods, 0)
  expect_identical(ss$model, m)
  expect_identical(m, before)
  expect_equal(stats::nobs(ss), nrow(m@frame))
  expect_equal(generics::glance(ss)$N, nrow(m@frame))
  tidy <- generics::tidy(ss, conf.level = .9)
  expect_equal(tidy$conf.low, tidy$estimate - qt(.95, df.residual(m)) * tidy$std.error)
  expect_output(print(ss), "[Ww]ithin-person")
  expect_s3_class(plot(ss), "ggplot")
  expect_s3_class(interactions::probe_interaction(m, x, z), "probe_interaction")
})

test_that("original-scale values describe the same slopes and J-N boundaries", {
  slopes_available()
  m <- slope_fit()
  ref <- m@call_info$prediction$reference[["imean(z)"]]
  a <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1))
  b <- interactions::sim_slopes(m, x, z, modx.values = ref + c(-1, 0, 1),
                                plot.scale = "original")
  expect_equal(a$slopes[, -1], b$slopes[, -1], tolerance = 1e-8)
  expect_equal(unname(a$jn[[1]]$bounds + ref), unname(b$jn[[1]]$bounds), tolerance = 1e-8)
  standalone <- interactions::johnson_neyman(m, x, z, plot = FALSE)
  expect_equal(standalone$bounds, a$jn[[1]]$bounds)
  se <- sqrt(rowSums((a$contrasts[[1]] %*% as.matrix(vcov(m))) * a$contrasts[[1]]))
  expect_equal(a$slopes$S.E., unname(se))
  expect_equal(a$slopes$p, unname(2 * pt(-abs(a$slopes$Est. / se), df.residual(m))))
  expect_silent(ggplot2::ggplot_build(standalone$plot))
})

test_that("cross-level, factor, and three-way moderation preserve reference roles", {
  slopes_available()
  m <- slope_fit(y ~ x | g | x * g)
  ss <- interactions::sim_slopes(m, x, g, modx.values = c(-1, 0, 1))
  expect_equal(ss$slopes$Est., unname(fixef(m)["x"] + fixef(m)["`x:g`"] * ss$slopes[[1]]))
  m <- slope_fit(y ~ x | f | x * f)
  ss <- interactions::sim_slopes(m, x, f, johnson_neyman = FALSE)
  expected <- fixef(m)["x"] + c(0, fixef(m)["`x:fb`"], fixef(m)["`x:fc`"])
  expect_equal(ss$slopes$Est., unname(expected))
  m <- slope_fit(y ~ x * z * w)
  ss <- interactions::sim_slopes(m, x, z, mod2 = w, modx.values = c(-1, 0, 1),
                                 mod2.values = c(-1, 1))
  expect_length(ss$slopes, 2)
  expect_length(ss$jn, 2)
  for (j in 1:2) {
    z <- ss$slopes[[j]][[1]]
    w <- c(-1, 1)[j]
    expected <- fixef(m)["x"] + fixef(m)["`x:z`"] * z +
      fixef(m)["`x:w`"] * w + fixef(m)["`x:z:w`"] * z * w
    expect_equal(ss$slopes[[j]]$Est., unname(expected))
  }
  expect_output(print(ss), "2nd moderator")
  expect_equal(nrow(generics::tidy(ss)), 6)
})

test_that("GEE uses its fitted sandwich covariance and normal inference", {
  slopes_available()
  skip_if_not_installed("geepack")
  d <- slope_data()
  m <- wbgee(y ~ x * z, panel_data(d), calc.fit.stats = FALSE)
  ss <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1), cond.int = TRUE)
  L <- ss$contrasts[[1]]
  expect_equal(ss$slopes$Est., unname(drop(L %*% coef(m))))
  expect_equal(ss$slopes$S.E., unname(sqrt(rowSums((L %*% vcov(m)) * L))))
  expect_equal(ss$slopes$p, 2 * pnorm(-abs(ss$slopes$Est. / ss$slopes$S.E.)))
  expect_identical(attr(ss, "inference.df"), Inf)
  expect_equal(nobs(ss), nrow(m$frame))
})

test_that("generalized slopes explicitly use the link scale", {
  slopes_available()
  d <- slope_data()
  set.seed(202)
  d$y <- rpois(nrow(d), exp(.2 * (d$x - ave(d$x, d$id)) + .1 * (d$z - ave(d$z, d$id))))
  m <- suppressWarnings(slope_fit(data = d, family = poisson,
                                  control = lme4::glmerControl(optimizer = "bobyqa")))
  ss <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1))
  expect_identical(attr(ss, "inference.df"), Inf)
  expect_match(attr(ss, "panelr")$note, "log link scale")
  expect_equal(ss$slopes$Est., unname(fixef(m)["x"] + fixef(m)["`x:z`"] * c(-1, 0, 1)))
  expect_error(interactions::sim_slopes(m, x, z, outcome.scale = "response"), "link scale")
})

test_that("offsets, reference overrides, and named arguments retain their meanings", {
  slopes_available()
  d <- slope_data()
  m <- slope_fit(data = d, offset = g)
  ss <- interactions::sim_slopes(m, "x", "z", modx.values = c(-1, 0, 1),
                                 at = list(g = 2, `imean(x)` = 8), cond.int = TRUE,
                                 df = "normal", pvals = FALSE)
  expect_identical(attr(ss, "inference.df"), Inf)
  expect_match(attr(ss, "panelr")$note, "x = 8")
  expect_output(print(ss), "Conditional intercept")
  expect_s3_class(generics::tidy(ss), "tbl_df")
  base <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1),
                                   at = list(g = 0, `imean(x)` = 8), cond.int = TRUE)
  expect_equal(ss$ints$Est. - base$ints$Est., rep(2, 3))
  expect_equal(ss$slopes$Est., base$slopes$Est.)
  var <- "x"
  expect_s3_class(interactions::sim_slopes(m, !!rlang::sym(var), z, at = list(g = 0)), "sim_slopes")
})

test_that("unsupported slopes fail explicitly instead of silently changing estimands", {
  slopes_available()
  m <- slope_fit()
  expect_error(interactions::sim_slopes(m, x, z, robust = TRUE), "fitted covariance")
  expect_error(interactions::sim_slopes(m, x, z, v.cov = vcov), "fitted covariance")
  expect_error(interactions::sim_slopes(m, x, z, df = "satterthwaite"), "df must")
  expect_error(interactions::sim_slopes(m, x, z, at = list(z = 1)), "cannot set focal")
  expect_error(interactions::sim_slopes(m, x, z, at = list(`imean(x)` = c(1, 2))), "one value")
  expect_error(interactions::sim_slopes(m, x, z, control.fdr = TRUE), "FDR")
  expect_error(interactions::sim_slopes(m, x, z, random.option = TRUE), "Unsupported")
  expect_error(interactions::sim_slopes(m, x, z, modx.values = c(0, 0)), "distinct")
  m <- slope_fit(y ~ log(x) * z)
  expect_error(interactions::sim_slopes(m, x, z, plot.scale = "original"), "untransformed numeric focal")
  m <- slope_fit(y ~ x | f | x * f)
  expect_error(interactions::sim_slopes(m, f, x), "numeric focal")
  m <- slope_fit(y ~ lag(x) * z)
  expect_error(interactions::sim_slopes(m, x, z), "lags/leads")
  m <- slope_fit(y ~ x * log(z))
  expect_error(interactions::sim_slopes(m, x, z, plot.scale = "original"),
                "untransformed numeric moderator")
  expect_s3_class(interactions::sim_slopes(m, x, z, modx.values = c(19, 20, 21),
                                           plot.scale = "original", johnson_neyman = FALSE), "sim_slopes")
})

test_that("contrasts support random slopes, weights, and non-syntactic names", {
  slopes_available()
  d <- slope_data()
  d$weight <- rep(seq(.5, 2, length.out = 30), each = 6)
  m <- suppressWarnings(slope_fit(y ~ x * z | g | (1 + x | id), data = d, weights = weight))
  ss <- interactions::sim_slopes(m, x, z, modx.values = c(-1, 0, 1))
  expect_equal(ss$slopes$Est., unname(drop(ss$contrasts[[1]] %*% fixef(m))))
  expect_equal(ss$slopes$S.E., unname(sqrt(rowSums((ss$contrasts[[1]] %*% as.matrix(vcov(m))) * ss$contrasts[[1]]))))
  p <- interactions::interact_plot(m, x, z, modx.values = c(-1, 0, 1), interval = TRUE)
  expect_silent(ggplot2::ggplot_build(p))
  expect_equal(model.weights(model.frame(to_merMod(m))), m@frame$.weights)
  names(d)[names(d) == "x"] <- "work hours"
  m <- slope_fit(y ~ `work hours` * z | g, data = d)
  expect_s3_class(interactions::sim_slopes(m, `work hours`, z), "sim_slopes")
})

test_that("rank-deficient fits and focal offsets are rejected", {
  d <- slope_data()
  d$duplicate <- d$x
  m <- suppressMessages(slope_fit(y ~ x * z + duplicate, data = d))
  prep <- prepare_wb_plot(m, d, "x", "z")
  expect_error(wb_slope_design(prep$model, "x", data.frame(x = 0, z = 0, duplicate = 0)), "rank-deficient")
  m <- slope_fit(offset = x)
  prep <- prepare_wb_plot(m, slope_data(), "x", "z")
  expect_error(wb_slope_design(prep$model, "x", data.frame(x = 0, z = 0)), "predictor-dependent offsets")
})
