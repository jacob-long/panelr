# Read-only URL audit of the prepared release candidates.
# Usage: Rscript release/check-urls.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
output <- "release/evidence/urls"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
for (package in c("jtools", "panelr", "interactions")) {
  cat("Checking URLs in", package, "\n")
  result <- urlchecker::url_check(file.path(root, "sources", package), progress = FALSE)
  saveRDS(result, file.path(root, "logs", paste0(package, "-urls.rds")))
  capture.output(print(result), file = file.path(output, paste0(package, ".log")))
  print(result)
}
