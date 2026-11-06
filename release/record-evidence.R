# Preserve human-readable results outside the disposable test libraries.
# Usage (from panelr): Rscript release/record-evidence.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
.libPaths(c(file.path(root, "lib-current"), .libPaths()))
library(testthat)
output <- "release/evidence"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
copy <- function(from, to) {
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  stopifnot(file.copy(from, to, overwrite = TRUE))
}
write_tsv <- function(data, name) {
  write.table(data, file.path(output, name), sep = "\t", quote = TRUE, row.names = FALSE)
}
checks <- lapply(c("jtools", "panelr", "interactions"), function(package) {
  result <- readRDS(file.path(root, "logs", paste0(package, "-check.rds")))
  check <- file.path(root, "checks", package, paste0(package, ".Rcheck"))
  copy(file.path(check, "00check.log"), file.path(output, paste0(package, "-check.log")))
  copy(file.path(check, "tests", "testthat.Rout"),
       file.path(output, paste0(package, "-tests.log")))
  data.frame(package, version = result$version, errors = length(result$errors),
             warnings = length(result$warnings), notes = length(result$notes))
})
write_tsv(do.call(rbind, checks), "candidate-checks.tsv")
visual <- lapply(c("jtools", "interactions"), function(package) {
  result <- readRDS(file.path(root, "logs", paste0(package, "-visual.rds")))
  counts <- colSums(as.data.frame(result)[c("failed", "error", "warning", "skipped", "passed")])
  copy(file.path(root, "logs", paste0(package, "-snapshot-refresh.tsv")),
       file.path(output, paste0(package, "-snapshot-refresh.tsv")))
  data.frame(package, as.list(counts), check.names = FALSE)
})
write_tsv(do.call(rbind, visual), "visual-tests.tsv")
normalize_errors <- function(x) gsub("0x[[:xdigit:]]+", "<address>", x)
reverse <- lapply(c("autostats", "bruceR", "dpm", "webSDM", "panelr", "interactions"),
                  function(package) {
  variants <- lapply(c("baseline", "candidate"), function(variant) {
    path <- file.path(root, "revdep", "checks", package, variant)
    copy(file.path(path, paste0(package, ".Rcheck"), "00check.log"),
         file.path(output, "reverse", paste0(package, "-", variant, ".log")))
    copy(file.path(path, "session-info.txt"),
         file.path(output, "reverse", paste0(package, "-", variant, "-session.txt")))
    readRDS(file.path(path, "result.rds"))
  })
  before <- variants[[1]]
  after <- variants[[2]]
  new_errors <- setdiff(normalize_errors(after$errors), normalize_errors(before$errors))
  data.frame(package, version = after$version,
    baseline_errors = length(before$errors), candidate_errors = length(after$errors),
    baseline_warnings = length(before$warnings), candidate_warnings = length(after$warnings),
    baseline_notes = length(before$notes), candidate_notes = length(after$notes),
    new_errors = length(new_errors))
})
write_tsv(do.call(rbind, reverse), "reverse-checks.tsv")
for (name in c("compatibility-old.tsv", "compatibility-current.tsv")) {
  copy(file.path(root, name), file.path(output, name))
}
copy(file.path(root, "without-panelr", "check.log"), file.path(output, "without-panelr.log"))
for (name in c("tests.log", "session-info.txt")) {
  copy(file.path(root, "installed-integration", name),
       file.path(output, paste0("installed-integration-", name)))
}
installed <- readRDS(file.path(root, "installed-integration", "results.rds"))
write_tsv(data.frame(as.list(colSums(as.data.frame(installed)[
  c("failed", "error", "warning", "skipped", "passed")]))), "installed-integration.tsv")
for (package in c("jtools", "panelr", "interactions")) {
  for (suffix in c("-tests.log", "-session-info.txt")) {
    name <- paste0(package, suffix)
    copy(file.path(root, "minimum", name), file.path(output, "older-dependencies", name))
  }
}
plots <- list.files(file.path(root, "visual-review", "panelr"), "[.]png$", full.names = TRUE)
for (plot in plots) copy(plot, file.path(output, "plots", basename(plot)))
cat("Recorded candidate, visual, downstream, optional-package, and older-dependency evidence.\n")
