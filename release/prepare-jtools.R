# Recreate the narrow maintenance candidate without the dirty working tree.
# Usage (from panelr): Rscript release/prepare-jtools.R /new/output/directory
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
project <- normalizePath(".", mustWork = TRUE)
repository <- normalizePath(file.path(project, "..", "JacobTools"), mustWork = TRUE)
patch <- normalizePath(file.path(project, "release", "jtools-2.3.2.patch"), mustWork = TRUE)
output <- args[[1]]
if (dir.exists(output) && length(list.files(output, all.files = TRUE, no.. = TRUE))) {
  stop("Use an empty output directory; existing files are not overwritten.")
}
dir.create(output, recursive = TRUE, showWarnings = FALSE)
output <- normalizePath(output)
archive <- file.path(output, "jtools-base.tar")
revision <- "169f0bbd2de74ce83557d55e45ae74f461e93750"
status <- system2("git", c("-C", shQuote(repository), "archive", "--format=tar",
                           "--prefix=jtools/", shQuote(paste0("--output=", archive)), revision))
stopifnot(status == 0L)
utils::untar(archive, exdir = output)
source <- file.path(output, "jtools")
status <- system2("git", c("-C", shQuote(source), "apply", "--check", shQuote(patch)))
stopifnot(status == 0L)
status <- system2("git", c("-C", shQuote(source), "apply", shQuote(patch)))
stopifnot(status == 0L, read.dcf(file.path(source, "DESCRIPTION"))[, "Version"] == "2.3.2")
cat("Prepared jtools 2.3.2 at", source, "\n")
