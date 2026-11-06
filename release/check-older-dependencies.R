# Exercise ggplot2 3.4.4 and the declared reformulas floor with old lme4.
# ggplot2 3.4.0 cannot install on R 4.5; its exact floor needs an older R.
# Other dependencies remain current; this is not an all-dependencies-old check.
# Usage: Rscript release/check-older-dependencies.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
libs <- c(file.path(root, paste0("lib-", c("minimum", "old-lme4", "interactions",
                                          "panelr", "jtools", "current"))), .libPaths())
files <- list(
  jtools = c("test-refit-preservation.R", "test-release-compatibility.R",
             "test-formula-helpers.R"),
  panelr = c("test-lme4-scoping.R", "test-prediction-contract.R", "test-plot-interface.R",
             "test-simple-slopes.R", "test-two-wave-interactions.R"),
  interactions = c("test-contrast-slopes.R", "test-panelr-plots.R",
                    "test-panelr-slopes.R", "test-slopes-refits.R")
)
output <- file.path(root, "minimum")
dir.create(output, recursive = TRUE, showWarnings = FALSE)
for (package in names(files)) {
  cat("Checking", package, "with older ggplot2 and minimum reformulas\n")
  callr::r(function(root, package, files, output) {
    stopifnot(packageVersion("ggplot2") == "3.4.4",
              packageVersion("reformulas") == "0.4.2",
              packageVersion("lme4") == "1.1.38")
    Sys.setenv(NOT_CRAN = "false")
    source <- file.path(root, "sources", package)
    pkgload::load_all(source, quiet = TRUE)
    grDevices::pdf(file.path(output, paste0(package, "-plots.pdf")))
    results <- lapply(files, function(file) testthat::test_file(
      file.path(source, "tests", "testthat", file), package = package,
      reporter = "summary", stop_on_failure = FALSE))
    grDevices::dev.off()
    counts <- Reduce(`+`, lapply(results, function(x)
      colSums(as.data.frame(x)[c("failed", "error", "warning", "skipped", "passed")])))
    print(counts)
    saveRDS(results, file.path(output, paste0(package, "-results.rds")))
    capture.output(sessionInfo(), file = file.path(output, paste0(package, "-session-info.txt")))
    stopifnot(counts[["failed"]] == 0, counts[["error"]] == 0)
  }, args = list(root, package, files[[package]], output), libpath = libs,
  stdout = file.path(output, paste0(package, "-tests.log")), stderr = "2>&1")
}
