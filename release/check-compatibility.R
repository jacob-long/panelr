# Run against separately installed CRAN and candidate packages in fresh sessions.
# Usage: Rscript release/check-compatibility.R /path/to/release-root [current|old]
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
current <- length(args) < 2L || args[[2]] != "old"
scenarios <- list(
  released = "cran",
  jtools_only = c("jtools", "cran"),
  panelr_only = c("panelr", "cran"),
  foundations = c("panelr", "jtools", "cran"),
  interactions_old_panelr = c("interactions", "jtools", "cran"),
  all_candidates = c("interactions", "panelr", "jtools", "cran")
)

check_combination <- function() {
  library(testthat)
  library(interactions)
  versions <- vapply(c("jtools", "panelr", "interactions", "lme4"),
                     function(p) as.character(packageVersion(p)), character(1))
  print(versions)
  new_jtools <- packageVersion("jtools") >= "2.3.2"
  new_panelr <- packageVersion("panelr") >= "1.1.0"
  new_interactions <- packageVersion("interactions") >= "1.3.0"
  set.seed(918)
  d <- data.frame(id = rep(1:30, each = 6), wave = rep(1:6, 30))
  d$x <- rep(rnorm(30, 10), each = 6) + rnorm(180)
  d$z <- rep(rnorm(30, 20), each = 6) + rnorm(180)
  d$w <- sample(1:4, 180, replace = TRUE)
  d$o <- runif(180, -.2, .2)
  d$y <- 2 + .7 * d$x + .8 * d$z + .2 * d$x * d$z +
    rep(rnorm(30), each = 6) + rnorm(180, sd = .3)
  d$count <- rpois(180, exp(.2 + .1 * d$x + d$o))

  test_that("ordinary-model plotting and slopes work without loading panelr", {
    expect_false("panelr" %in% loadedNamespaces())
    m <- lm(y ~ x * z, d)
    ss <- sim_slopes(m, x, z, modx.values = c(19, 20, 21), johnson_neyman = FALSE)
    expect_equal(ss$slopes$Est., unname(coef(m)["x"] + c(19, 20, 21) * coef(m)["x:z"]))
    expect_s3_class(interact_plot(m, x, z, interval = TRUE), "ggplot")
    expect_false("panelr" %in% loadedNamespaces())
  })
  if (new_jtools) test_that("weighted quasi-family summaries retain likelihoods", {
    m <- glm(count ~ x, d, weights = w, offset = o, subset = 21:180,
             family = quasipoisson)
    full <- glm(count ~ x, d, weights = w, offset = o, subset = 21:180,
                family = poisson)
    null <- glm(count ~ 1, d, weights = w, offset = o, subset = 21:180,
                family = poisson)
    expected <- 1 - as.numeric(logLik(full)) / as.numeric(logLik(null))
    expect_equal(attr(jtools::summ(m), "rsqmc"), expected)
  })
  if (new_interactions) test_that("weighted subsetted slopes match direct contrasts", {
    m <- lm(y ~ x * z, d, weights = w, offset = o, subset = 21:180)
    ss <- sim_slopes(m, x, z, modx.values = c(19, 20, 21), johnson_neyman = FALSE)
    L <- matrix(0, 3, length(coef(m)), dimnames = list(NULL, names(coef(m))))
    L[, "x"] <- 1
    L[, "x:z"] <- c(19, 20, 21)
    expect_equal(ss$slopes$Est., unname(drop(L %*% coef(m))), tolerance = 1e-8)
    expect_equal(ss$slopes$S.E., unname(sqrt(rowSums((L %*% vcov(m)) * L))),
                 tolerance = 1e-8)
  })

  # Released panelr cannot fit with lme4 2.1-0. The old-lme4 run covers its
  # upgrade guards; this run still exercises ordinary models in that scenario.
  if (!new_panelr && packageVersion("lme4") >= "2.1.0") {
    cat("Panelr fitting skipped: released panelr has a known lme4 2.1-0 incompatibility.\n")
    return(versions)
  }
  library(panelr)
  pd <- panel_data(d, id = id, wave = wave)
  m <- suppressMessages(wbm(y ~ x * z, pd, pR2 = FALSE, pvals = FALSE))
  if (new_panelr) test_that("panelr predictions and legacy plotting agree", {
    expect_equal(as.numeric(predict(m, re.form = NA)),
                 as.numeric(model.matrix(m) %*% lme4::fixef(m)), tolerance = 1e-8)
    p <- interact_plot(m, x, z, interval = TRUE)
    expect_s3_class(p, "ggplot")
    if (!new_interactions) {
      expected <- jtools::make_predictions(m, pred = "x", new_data = p$data,
                                          message = FALSE)
      expect_equal(unname(p$data$y), unname(expected$y), tolerance = 1e-8)
      expect_equal(unname(p$data$ymin), unname(expected$ymin), tolerance = 1e-8)
    }
  })
  if (new_interactions && !new_panelr) test_that("old panelr gets actionable upgrade errors", {
    expect_error(interact_plot(m, x, z), "panelr to version 1.1.0")
    expect_error(sim_slopes(m, x, z), "panelr to version 1.1.0")
    expect_error(johnson_neyman(m, x, z), "panelr to version 1.1.0")
  })
  if (new_interactions && new_panelr) test_that("panelr slopes match plotted lines", {
    ss <- sim_slopes(m, x, z, modx.values = c(-1, 0, 1), johnson_neyman = FALSE)
    p <- interact_plot(m, x, z, modx.values = c(-1, 0, 1), interval = TRUE)
    for (i in seq_len(nrow(ss$slopes))) {
      rows <- p$data[p$data$z == ss$slopes[i, 1], ]
      expect_equal(ss$slopes$Est.[i], unname(diff(range(rows$y)) /
                     diff(range(rows$x)) * sign(tail(rows$y, 1) - rows$y[1])),
                   tolerance = 1e-8)
    }
  })
  versions
}

results <- lapply(names(scenarios), function(name) {
  libs <- file.path(root, paste0("lib-", scenarios[[name]]))
  if (!current) libs <- c(libs, file.path(root, "lib-old-lme4"))
  libs <- c(libs, file.path(root, "lib-current"))
  log <- file.path(root, "logs", paste0("compat-", name,
                                       if (current) "-current" else "-old", ".log"))
  cat("Checking", name, "\n")
  result <- tryCatch({
    callr::r(check_combination, libpath = c(libs, .libPaths()),
             stdout = log, stderr = "2>&1")
    "PASS"
  }, error = function(e) conditionMessage(e))
  data.frame(scenario = name, result = result, log = log)
})
results <- do.call(rbind, results)
write.table(results, file.path(root, paste0("compatibility-", if (current) "current" else "old", ".tsv")),
            sep = "\t", quote = TRUE, row.names = FALSE)
print(results[c("scenario", "result")], row.names = FALSE)
if (any(results$result != "PASS")) quit(status = 1)
