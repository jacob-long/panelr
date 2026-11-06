# Usage: Rscript release/check-candidate.R ROOT PACKAGE [numeric|visual|check]
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 2L)
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("jtools", "panelr", "interactions"))
mode <- if (length(args) >= 3L) args[[3]] else "numeric"
.libPaths(c(file.path(root, paste0("lib-", c("interactions", "panelr", "jtools", "current"))),
            .libPaths()))
source <- file.path(root, "sources", package)
if (mode == "check") {
  output <- file.path(root, "checks", package)
  dir.create(output, recursive = TRUE, showWarnings = FALSE)
  result <- withr::with_libpaths(.libPaths(), devtools::check(source, document = FALSE, vignettes = TRUE,
                            manual = TRUE, cran = TRUE,
                            env_vars = c(NOT_CRAN = "false"),
                            check_dir = output, error_on = "never"))
  saveRDS(result, file.path(root, "logs", paste0(package, "-check.rds")))
  quit(status = as.integer(length(result$errors) > 0 || length(result$warnings) > 0))
}
stopifnot(mode %in% c("numeric", "visual"))
# Snapshot cleanup must not change the candidate sources being packaged.
dir.create(file.path(root, "tests"), recursive = TRUE, showWarnings = FALSE)
test_root <- tempfile(paste0(package, "-", mode, "-"), tmpdir = file.path(root, "tests"))
dir.create(test_root)
stopifnot(file.copy(source, test_root, recursive = TRUE, overwrite = TRUE))
source <- file.path(test_root, package)
Sys.setenv(NOT_CRAN = if (mode == "visual") "true" else "false")
grDevices::pdf(file.path(test_root, "test-plots.pdf"))
pkgload::load_all(source, quiet = TRUE)
result <- testthat::test_dir(file.path(source, "tests", "testthat"),
                             reporter = "summary", package = package,
                             stop_on_failure = FALSE, stop_on_warning = FALSE)
grDevices::dev.off()
attr(result, "source.path") <- source
saveRDS(result, file.path(root, "logs", paste0(package, "-", mode, ".rds")))
counts <- as.data.frame(result)
print(colSums(counts[c("failed", "error", "warning", "skipped", "passed")]))
for (test in result) for (expectation in test$results) {
  if (inherits(expectation, "expectation_warning")) {
    cat(test$file, test$test, "\n", conditionMessage(expectation), "\n")
  }
}
quit(status = as.integer(any(counts$failed > 0 | counts$error)))
