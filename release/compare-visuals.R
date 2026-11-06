# Compare legacy plot cases under identical dependencies and random seeds.
# Usage: Rscript release/compare-visuals.R ROOT PACKAGE
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("jtools", "interactions"))
parent <- file.path(root, "visual-review", package)
dir.create(parent, recursive = TRUE, showWarnings = FALSE)
run <- tempfile("run-", tmpdir = parent)
dir.create(run)
sources <- c(baseline = file.path(root, paste0(package, "-baseline"), package),
             candidate = file.path(root, "sources", package))
files <- if (package == "jtools") c("test-effect-plot.R", "test-export-summs.R") else
  c("test_cat_plot.R", "test_interact_plot.R")
libraries <- c(file.path(root, paste0("lib-", c("jtools", "current"))), .libPaths())
for (variant in names(sources)) {
  destination <- file.path(run, variant)
  dir.create(destination)
  stopifnot(file.copy(sources[[variant]], destination, recursive = TRUE))
  path <- file.path(destination, package)
  for (file in files) {
    stopifnot(file.copy(file.path(sources[["baseline"]], "tests", "testthat", file),
                       file.path(path, "tests", "testthat", file), overwrite = TRUE))
  }
  cat("Rendering", package, variant, "\n")
  callr::r(function(path, package, files, output) {
    Sys.setenv(NOT_CRAN = "true")
    pkgload::load_all(path, quiet = TRUE)
    grDevices::pdf(file.path(output, "plots.pdf"))
    results <- lapply(files, function(file) {
      set.seed(100)
      testthat::test_file(file.path(path, "tests", "testthat", file),
                          package = package, reporter = "summary",
                          stop_on_failure = FALSE)
    })
    grDevices::dev.off()
    saveRDS(results, file.path(output, "results.rds"))
    capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
  }, args = list(path, package, files, destination), libpath = libraries,
  stdout = file.path(destination, "tests.log"), stderr = "2>&1")
}

# A failed snapshot has a fresh .new.svg; a matched snapshot uses its baseline.
snapshots <- function(variant) {
  path <- file.path(run, variant, package, "tests", "testthat", "_snaps")
  files <- list.files(path, pattern = "[.]svg$", recursive = TRUE)
  keys <- unique(sub("[.]new[.]svg$", ".svg", files))
  fresh <- sub("[.]svg$", ".new.svg", keys)
  selected <- ifelse(file.exists(file.path(path, fresh)), fresh, keys)
  setNames(file.path(path, selected), keys)
}
before <- snapshots("baseline")
after <- snapshots("candidate")
keys <- intersect(names(before), names(after))
comparison <- data.frame(snapshot = keys,
  identical = unname(tools::md5sum(before[keys])) == unname(tools::md5sum(after[keys])),
  baseline = unname(before[keys]), candidate = unname(after[keys]))
write.table(comparison, file.path(run, "comparison.tsv"), sep = "\t",
            quote = TRUE, row.names = FALSE)
cat("Identical:", sum(comparison$identical), "Changed:", sum(!comparison$identical), "\n")
print(comparison$snapshot[!comparison$identical])
cat("Evidence:", run, "\n")
