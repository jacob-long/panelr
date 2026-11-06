plot_fixture <- function() {
  set.seed(142)
  d <- data.frame(id = rep(1:20, each = 6), wave = rep(1:6, 20))
  d$x <- rep(rnorm(20, 20, 3), each = 6) + rnorm(120)
  d$z <- rep(rnorm(20, 10, 2), each = 6) + rnorm(120)
  d$g <- rep(rnorm(20), each = 6)
  d$f <- factor(rep(rep(c("a", "b"), 10), each = 6))
  d$y <- d$x + d$z + .5 * d$x * d$z + rep(rnorm(20), each = 6) + rnorm(120)
  d
}

plot_fit <- function(formula = y ~ x * z, data = plot_fixture(), ...) {
  suppressMessages(wbm(formula, panel_data(data), pR2 = FALSE, pvals = FALSE, ...))
}

skip_old_interactions <- function() {
  skip_if_not_installed("interactions")
  skip_if(!"plot.scale" %in% names(formals(interactions::interact_plot)),
          "Installed interactions lacks component-aware plots")
}

test_that("plot data use fitted within components in original units", {
  d <- plot_fixture()
  for (scaled in c(FALSE, TRUE)) {
    m <- plot_fit(scale = scaled)
    before <- m
    prep <- prepare_wb_plot(m, d, "x", "z")
    expect_equal(prep$data$x, d$x - ave(d$x, d$id))
    expect_equal(prep$data$z, d$z - ave(d$z, d$id))
    expect_equal(prep$data$y, d$y)
    expect_equal(prep$modx.values, c(-1, 0, 1) * sd(d$z - ave(d$z, d$id)))
    expect_identical(m, before)
    expect_null(attr(m, "panelr_plot"))
    expect_match(prep$x.label, "within-person deviation")
  }
})

test_that("within and original display grids give identical values and bands", {
  d <- plot_fixture()
  for (style in c("double-demean", "demean", "raw")) {
    m <- plot_fit(interaction.style = style, scale = TRUE)
    a <- prepare_wb_plot(m, d, "x", "z")
    b <- prepare_wb_plot(m, d, "x", "z", plot.scale = "original")
    grid <- data.frame(x = c(-1, 0, 1), z = c(1, 0, -1))
    original <- grid
    for (v in c("x", "z")) {
      original[[v]] <- grid[[v]] + m@call_info$prediction$reference[[paste0("imean(", v, ")")]]
    }
    pa <- jtools::make_predictions(a$model, pred = "x", new_data = grid, message = FALSE)
    pb <- jtools::make_predictions(b$model, pred = "x", new_data = original, message = FALSE)
    expect_equal(pa[c("y", "ymin", "ymax")], pb[c("y", "ymin", "ymax")])
    expect_equal(pa$x, grid$x)
    expect_equal(pb$x, original$x)
    expect_equal(prepare_wb_plot_grid(a$model, grid), prepare_wb_grid(m, original))
    expect_error(prepare_wb_plot_grid(a$model, grid, raw = TRUE), "raw = TRUE")
  }
})

test_that("plot data subsets do not recompute person means", {
  d <- plot_fixture()
  d$y[1] <- NA
  m <- plot_fit(data = d)
  data <- d[120:1, ]
  data$extra <- data$id
  prep <- prepare_wb_plot(m, data, "x", "z")
  expect_equal(prep$data$x, rev(m@frame$x))
  expect_equal(prep$data$extra, rev(as.numeric(as.character(m@frame$id))))
  one_wave <- d[d$wave == 3, ]
  prep <- prepare_wb_plot(m, one_wave, "x", "z")
  expect_equal(prep$data$x, m@frame$x[m@frame$wave == 3])
  expect_error(prepare_wb_plot(m, d[, -1], "x", "z"), "panel identifiers")
  d$x <- d$x + 1
  expect_error(prepare_wb_plot(m, d, "x", "z"), "changed model variables")
})

test_that("constant moderators and models without within centering retain units", {
  d <- plot_fixture()
  m <- plot_fit(y ~ x | g | x * g)
  prep <- prepare_wb_plot(m, d, "x", "g")
  expect_equal(prep$data$g, d$g)
  expect_identical(prep$legend.main, "g")
  expect_null(prep$modx.values)
  for (type in c("random", "contextual")) {
    m <- plot_fit(model = type)
    prep <- prepare_wb_plot(m, d, "x", "z")
    expect_equal(prep$data$x, d$x)
    expect_identical(prep$x.label, "x")
  }
})

test_that("plot weights follow fitted observations, including wrapper calls", {
  d <- plot_fixture()
  d$weight <- seq(.5, 2, length.out = nrow(d))
  d$y[1] <- NA
  m <- plot_fit(y ~ x * z | g, data = d, weights = weight^2)
  before <- m
  prep <- prepare_wb_plot(m, d[120:1, ], "x", "z")
  weights <- jtools::get_weights(prep$model, prep$data)$weights
  expect_equal(weights, rev(m@frame$.weights))
  grid <- jtools::make_new_data(prep$model, "x", pred.values = 0,
                                 at = list(z = 0), data = prep$data)
  expect_equal(grid$g, weighted.mean(prep$data$g, weights))
  expect_identical(m, before)
})

test_that("reference overrides shift prediction inputs but not deviation labels", {
  d <- plot_fixture()
  m <- plot_fit()
  at <- list(z = 0, `imean(x)` = 18, `imean(z)` = 8, `imean(x:z)` = .1)
  prep <- prepare_wb_plot(m, d, "x", "z", at = at)
  pm <- jtools::make_predictions(prep$model, "x", pred.values = c(-1, 0, 1),
                                 data = prep$data, at = at, interval = FALSE)
  expect_equal(pm$x, c(-1, 0, 1))
  expected <- prepare_wb_grid(m, data.frame(x = c(17, 18, 19), z = 8,
                          `imean(x)` = 18, `imean(z)` = 8, `imean(x:z)` = .1,
                          check.names = FALSE))
  expect_equal(unname(pm$y), unname(predict(m, expected, raw = TRUE, re.form = NA)))
  expect_match(prep$caption, "x = 18; z = 8")
  expect_error(prepare_wb_plot(m, d, "x", "z", at = list(`imean(x)` = c(1, 2))),
               "one finite numeric value")
})

test_that("extrapolation warnings concern deviations, once per variable", {
  m <- plot_fit()
  prep <- prepare_wb_plot(m, plot_fixture(), "x", "z")
  expect_warning(prepare_wb_plot_grid(prep$model, data.frame(x = 0, z = 10)),
                 "'z'.*outside the observed")
  expect_silent(prepare_wb_plot_grid(prep$model, data.frame(x = 0, z = 20)))
  prep <- prepare_wb_plot(m, plot_fixture(), "x", "z", plot.scale = "original")
  expect_warning(prepare_wb_plot_grid(prep$model, data.frame(x = 20, z = 30)),
                 "'z'.*outside the observed")
})

test_that("unsupported within displays fail instead of mislabeling transformations", {
  d <- plot_fixture()
  m <- plot_fit(y ~ log(x) * z)
  expect_error(prepare_wb_plot(m, d, "x", "z"), "untransformed numeric")
  expect_silent(prepare_wb_plot(m, d, "x", "z", plot.scale = "original"))
  prep <- prepare_wb_plot(m, d, "x", "z", plot.scale = "original")
  expect_match(prep$caption, "log(x)", fixed = TRUE)
  expect_warning(prepare_wb_plot_grid(prep$model, data.frame(x = exp(9), z = 10)),
                 "extrapolate")
  m <- plot_fit(y ~ lag(x) * z)
  expect_error(prepare_wb_plot(m, d, "x", "z"), "lags/leads")
})

test_that("GEE uses the same component display contract", {
  skip_if_not_installed("geepack")
  d <- plot_fixture()
  m <- wbgee(y ~ x * z, panel_data(d), calc.fit.stats = FALSE)
  prep <- prepare_wb_plot(m, d, "x", "z")
  expect_equal(prep$data$x, d$x - ave(d$x, d$id))
  grid <- data.frame(x = c(-1, 0, 1), z = 0)
  pm <- jtools::make_predictions(prep$model, "x", new_data = grid)
  nd <- prepare_wb_plot_grid(prep$model, grid)
  expect_equal(unname(pm$y), unname(predict(m, nd, raw = TRUE)))
})

test_that("native plots show within values, readable labels, and unmodified outcomes", {
  skip_old_interactions()
  d <- plot_fixture()
  m <- plot_fit()
  p <- interactions::interact_plot(m, x, z, interval = TRUE, plot.points = TRUE)
  expect_equal(range(p$data$x), range(m@frame$x))
  expect_equal(sort(unique(p$data$z)), c(-1, 0, 1) * sd(m@frame$z))
  expect_match(p$labels$x, "within-person deviation")
  expect_identical(p$labels$y, "Predicted y")
  expect_match(p$labels$caption, "observed mean")
  expect_identical(attr(p, "panelr")$scale, "within")
  expect_null(attr(m, "panelr_plot"))
  point_layer <- which(vapply(p$layers, function(x) inherits(x$geom, "GeomPoint"), logical(1)))
  points <- p$layers[[point_layer]]$data
  expect_equal(points$x, m@frame$x)
  expect_equal(points$y, d$y)
  expect_silent(ggplot2::ggplot_build(p))
  p <- interactions::interact_plot(m, x, z, modx.values = c(-1, 1),
                                   modx.labels = c("Below", "Above"),
                                   x.label = "Departure", y.label = "Outcome",
                                   legend.main = "Experience")
  expect_identical(p$labels$x, "Departure")
  expect_identical(p$labels$y, "Outcome")
  expect_setequal(levels(p$data$modx_group), c("Below", "Above"))
  p <- interactions::interact_plot(m, x, z, modx.values = c(1.0001, 1.0002))
  expect_length(levels(p$data$modx_group), 2)
})

test_that("three-way facets and categorical plots retain component meanings", {
  skip_old_interactions()
  m <- plot_fit(y ~ x * z | g | x:z:g)
  p <- interactions::interact_plot(m, x, g, mod2 = z,
                                   modx.values = c(-1, 1), mod2.values = c(-1, 1))
  expect_true(all(grepl("within-person deviation", levels(p$data$mod2_group))))
  expect_silent(ggplot2::ggplot_build(p))
  m <- plot_fit(y ~ x | f | x * f)
  p <- interactions::cat_plot(m, f, x)
  expect_equal(sort(unique(p$data$x)), c(-1, 0, 1) * sd(m@frame$x))
  expect_silent(ggplot2::ggplot_build(p))
  p <- interactions::interact_plot(m, x, f)
  expect_setequal(as.character(unique(p$data$f)), c("a", "b"))
})
