# Use an isolated R home whose library genuinely contains no panelr package.
# Usage: Rscript release/check-without-panelr.R ROOT
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 2L && args[[2]] == "worker") {
  stopifnot(!requireNamespace("panelr", quietly = TRUE),
            !"panelr" %in% rownames(installed.packages()))
  library(interactions)
  stopifnot(packageVersion("interactions") >= "1.3.0", packageVersion("jtools") >= "2.3.2")
  model <- lm(mpg ~ hp * wt, mtcars)
  slopes <- sim_slopes(model, hp, wt, modx.values = c(2, 3, 4), johnson_neyman = FALSE)
  stopifnot(isTRUE(all.equal(slopes$slopes$Est.,
    unname(coef(model)["hp"] + c(2, 3, 4) * coef(model)["hp:wt"]))))
  plot <- interact_plot(model, hp, wt, interval = TRUE)
  stopifnot(inherits(ggplot2::ggplot_build(plot), "ggplot_built"),
            inherits(johnson_neyman(model, hp, wt, plot = FALSE), "johnson_neyman"),
            !"panelr" %in% loadedNamespaces())
  cat("PASS: plots, simple slopes, and Johnson-Neyman work with panelr absent.\n")
  print(.libPaths())
  cat("Versions:", as.character(packageVersion("interactions")),
      as.character(packageVersion("jtools")), "\n")
  quit(status = 0)
}
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
parent <- file.path(root, "without-panelr")
dir.create(parent, recursive = TRUE, showWarnings = FALSE)
home <- tempfile("R-", tmpdir = parent)
dir.create(file.path(home, "library"), recursive = TRUE)
items <- setdiff(list.files(R.home(), all.files = TRUE, no.. = TRUE), "library")
stopifnot(all(file.symlink(file.path(R.home(), items), file.path(home, items))))
libs <- c(file.path(root, paste0("lib-", c("interactions", "jtools", "current"))), .libPaths())
packages <- do.call(rbind, lapply(libs, function(lib) installed.packages(lib.loc = lib)))
packages <- packages[!duplicated(packages[, "Package"]) & packages[, "Package"] != "panelr", ]
stopifnot(all(file.symlink(file.path(packages[, "LibPath"], packages[, "Package"]),
                           file.path(home, "library", packages[, "Package"]))))
writeLines(home, file.path(parent, "last-home.txt"))
script <- normalizePath("release/check-without-panelr.R")
log <- file.path(root, "without-panelr", "check.log")
status <- withr::with_envvar(c(R_HOME = home, R_LIBS = file.path(home, "library"),
  R_LIBS_USER = file.path(home, "library"), R_LIBS_SITE = file.path(home, "library")),
  system2(file.path(R.home("bin"), "exec", "R"),
    c("--vanilla", "--slave", "-f", shQuote(script), "--args", shQuote(root), "worker"),
    stdout = log, stderr = log))
cat(readLines(log), sep = "\n")
quit(status = status)
