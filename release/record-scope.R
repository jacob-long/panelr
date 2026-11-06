# Record proposed commit files without staging, committing, or publishing.
# Usage (from panelr): Rscript release/record-scope.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
project <- normalizePath(".", mustWork = TRUE)
output <- "release/evidence/commit-scope.tsv"
git_files <- function(repository, args) {
  result <- system2("git", c("-C", shQuote(repository), args), stdout = TRUE)
  stopifnot(is.null(attr(result, "status")))
  result
}
entries <- list()
for (package in c("jtools", "panelr", "interactions")) {
  if (package == "jtools") {
    patch <- readLines("release/jtools-2.3.2.patch")
    files <- sub(".* b/", "", grep("^diff --git ", patch, value = TRUE))
    source <- file.path(root, "sources", package)
    scope <- "maintenance patch from pinned base"
    revision <- "169f0bbd2de74ce83557d55e45ae74f461e93750"
  } else {
    source <- if (package == "panelr") project else file.path(dirname(project), package)
    files <- unique(c(git_files(source, c("diff", "--name-only", "HEAD")),
                      git_files(source, c("ls-files", "--others", "--exclude-standard"))))
    files <- setdiff(files, c("CRAN-SUBMISSION", "Rplots.pdf", output))
    scope <- "working tree"
    revision <- git_files(source, c("rev-parse", "HEAD"))
  }
  stopifnot(length(files) > 0L, all(file.exists(file.path(source, files))))
  entries[[package]] <- data.frame(package, scope, base_revision = revision,
    path = sort(files),
    sha256 = vapply(file.path(source, sort(files)), function(file)
      digest::digest(file = file, algo = "sha256"), character(1)),
    row.names = NULL)
}
write.table(do.call(rbind, entries), output, sep = "\t", quote = FALSE,
            row.names = FALSE)
print(vapply(entries, nrow, integer(1)))
cat("Scope manifest excludes its own hash. No git index changes made.\n")
