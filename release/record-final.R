# Record the checked tarballs and their code differences from hosted candidates.
# Usage (from panelr): Rscript release/record-final.R ROOT
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- normalizePath(args[[1]], mustWork = TRUE)
rows <- list()
code_changes <- list()
for (package in c("jtools", "panelr", "interactions")) {
  check <- file.path(root, "final-checks", package)
  result <- readRDS(file.path(check, "result.rds"))
  archive <- list.files(check, "[.]tar[.]gz$", full.names = TRUE)
  stopifnot(length(archive) == 1L,
            length(result$errors) == 0L, length(result$warnings) == 0L)
  paths <- utils::untar(archive, list = TRUE)
  stopifnot(!any(grepl("/(release/|CRAN-SUBMISSION$|Rplots[.]pdf$)", paths)))
  metadata <- desc::desc(file = archive)
  stopifnot(!metadata$has_fields("Remotes"))
  if (package == "interactions") {
    stopifnot(grepl("panelr", metadata$get("Enhances")),
              !grepl("panelr", metadata$get("Suggests")))
  }
  if (package == "jtools") stopifnot(!any(grepl("glmmTMB", paths)))
  rows[[package]] <- data.frame(package, version = result$version,
    errors = length(result$errors), warnings = length(result$warnings),
    notes = length(result$notes), archive,
    sha256 = digest::digest(file = archive, algo = "sha256"))

  before <- list.files(file.path(root, "checks", package), "[.]tar[.]gz$", full.names = TRUE)
  stopifnot(length(before) == 1L)
  old <- tempfile(paste0(package, "-uploaded-"))
  new <- tempfile(paste0(package, "-final-"))
  dir.create(old)
  dir.create(new)
  utils::untar(before, exdir = old)
  utils::untar(archive, exdir = new)
  files <- list.files(file.path(new, package, "R"), full.names = FALSE)
  for (file in files) {
    a <- file.path(old, package, "R", file)
    b <- file.path(new, package, "R", file)
    if (!identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))) {
      code_changes[[length(code_changes) + 1L]] <- data.frame(package,
        file = file.path("R", file), executable_change =
          !identical(parse(a, keep.source = FALSE), parse(b, keep.source = FALSE)))
    }
  }
}
output <- "release/evidence/final"
write.table(do.call(rbind, rows), file.path(output, "checks.tsv"),
            sep = "\t", quote = FALSE, row.names = FALSE)
write.table(do.call(rbind, code_changes), file.path(output, "code-changes-since-hosted.tsv"),
            sep = "\t", quote = FALSE, row.names = FALSE)
print(do.call(rbind, rows)[c("package", "errors", "warnings", "notes")], row.names = FALSE)
cat("Checked archives contain no test-only Remotes or excluded release artifacts.\n")
