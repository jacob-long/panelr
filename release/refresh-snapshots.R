# Accept only snapshots reproduced by a paired baseline/candidate comparison.
# Usage: Rscript release/refresh-snapshots.R ROOT PACKAGE REVIEW_DIRECTORY
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("jtools", "interactions"))
.libPaths(c(file.path(root, "lib-current"), .libPaths()))
library(testthat)
result <- readRDS(file.path(root, "logs", paste0(package, "-visual.rds")))
stopifnot(!any(as.data.frame(result)$error))
for (test in result) for (expectation in test$results) {
  if (inherits(expectation, "expectation_failure")) {
    stopifnot(grepl("^Snapshot of .* to '.*[.]svg' has changed",
                   conditionMessage(expectation)))
  }
}
comparison <- read.delim(file.path(args[[3]], "comparison.tsv"))
for (i in which(!comparison$identical)) {
  before <- rsvg::rsvg(comparison$baseline[[i]], width = 960, height = 768)
  after <- rsvg::rsvg(comparison$candidate[[i]], width = 960, height = 768)
  stopifnot(identical(before, after))
}
snapshots <- file.path(attr(result, "source.path"), "tests", "testthat", "_snaps")
files <- list.files(snapshots, pattern = "[.]new[.]svg$", recursive = TRUE)
keys <- sub("[.]new[.]svg$", ".svg", files)
index <- match(keys, comparison$snapshot)
stopifnot(length(files) > 0L, !anyNA(index))
fresh <- file.path(snapshots, files)
stopifnot(unname(tools::md5sum(fresh)) ==
            unname(tools::md5sum(comparison$candidate[index])))
destination <- file.path(root, "sources", package, "tests", "testthat", "_snaps", keys)
stopifnot(file.exists(destination))
before_md5 <- unname(tools::md5sum(destination))
stopifnot(file.copy(fresh, destination, overwrite = TRUE))
evidence <- data.frame(snapshot = keys, before_md5 = before_md5,
                       after_md5 = unname(tools::md5sum(destination)))
write.table(evidence, file.path(root, "logs", paste0(package, "-snapshot-refresh.tsv")),
            sep = "\t", quote = TRUE, row.names = FALSE)
cat("Updated", length(files), "verified snapshots in", package, "candidate.\n")
