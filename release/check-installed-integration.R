# Supplement --as-cran, which excludes optional Enhances libraries.
# Test the package installed by R CMD check, not a pkgload source namespace.
# Usage: Rscript release/check-installed-integration.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
check <- file.path(root, "checks", "interactions", "interactions.Rcheck")
output <- file.path(root, "installed-integration")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
libs <- c(check, file.path(root, paste0("lib-", c("panelr", "jtools", "current"))), .libPaths())
callr::r(function(check, output) {
  expected <- c(jtools = "2.3.2", panelr = "1.1.0", interactions = "1.3.0")
  actual <- vapply(names(expected), function(package)
    as.character(packageVersion(package)), character(1))
  print(actual)
  stopifnot(actual == expected,
            normalizePath(find.package("interactions")) == file.path(check, "interactions"))
  Sys.setenv(NOT_CRAN = "false")
  library(interactions)
  stopifnot(!"panelr" %in% loadedNamespaces())
  grDevices::pdf(file.path(output, "plots.pdf"))
  results <- testthat::test_dir(file.path(check, "tests", "testthat"),
    package = "interactions", load_package = "installed", reporter = "summary",
    stop_on_failure = FALSE)
  grDevices::dev.off()
  counts <- colSums(as.data.frame(results)[c("failed", "error", "warning", "skipped", "passed")])
  print(counts)
  saveRDS(results, file.path(output, "results.rds"))
  capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
  stopifnot(counts[["failed"]] == 0, counts[["error"]] == 0,
            all(c("prepare_wb_plot", "wb_slope_design") %in% getNamespaceExports("panelr")))
}, args = list(check, output), libpath = libs,
stdout = file.path(output, "tests.log"), stderr = "2>&1")
cat(readLines(file.path(output, "tests.log")), sep = "\n")
