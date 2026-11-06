# Give hosted tests immutable candidate dependencies without changing release metadata.
# Usage: Rscript release/prepare-rhub-integration.R ROOT JTOOLS_REPO@SHA PANELR_REPO@SHA
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L,
          all(grepl("^r-hub2/[A-Za-z0-9-]+@[a-f0-9]{40}$", args[2:3])))
root <- normalizePath(args[[1]], mustWork = TRUE)
parent <- file.path(root, "hosted")
dir.create(parent, showWarnings = FALSE)
work <- tempfile("integration-", tmpdir = parent)
dir.create(work)
archive <- file.path(root, "checks", "interactions", "interactions_1.3.0.tar.gz")
untar(archive, exdir = work)
package <- file.path(work, "interactions")
files <- list.files(package, recursive = TRUE, all.files = TRUE, full.names = TRUE)
before <- tools::md5sum(files)
metadata <- desc::desc(file = file.path(package, "DESCRIPTION"))
original_metadata <- metadata$get(c("Imports", "Suggests", "Enhances"))
remotes <- paste0(c("jtools=github::", "panelr=github::"), args[2:3])
metadata$set(Remotes = paste(remotes, collapse = ",\n    "))
# --as-cran excludes Enhances libraries; test-only Suggests enables the integration suite.
metadata$del_dep("panelr", type = "Enhances")
metadata$set_dep("panelr", type = "Suggests", version = ">= 1.1.0")
metadata$write()
changed <- names(before)[before != tools::md5sum(files)]
stopifnot(identical(changed, file.path(package, "DESCRIPTION")))
destination <- file.path(work, "interactions_1.3.0.tar.gz")
withr::with_dir(work, utils::tar(destination, files = "interactions", compression = "gzip",
                                tar = "internal"))
audit <- list(release_archive = archive, test_archive = destination,
  release_sha256 = digest::digest(file = archive, algo = "sha256"),
  test_sha256 = digest::digest(file = destination, algo = "sha256"),
  changed_files = "DESCRIPTION", unchanged_files = length(files) - 1L,
  remotes = remotes, original_metadata = as.list(original_metadata),
  test_metadata = as.list(metadata$get(c("Imports", "Suggests", "Enhances", "Remotes"))))
jsonlite::write_json(audit, "release/evidence/hosted/interactions-test-metadata.json",
                     auto_unbox = TRUE, pretty = TRUE)
cat("Test-only archive:", destination, "\n")
cat("All", length(files) - 1L, "non-DESCRIPTION files are unchanged.\n")
