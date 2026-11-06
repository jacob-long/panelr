# Check final staged candidates without overwriting the uploaded archives.
# Usage (from panelr): Rscript release/check-incoming.R ROOT PACKAGE
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("jtools", "panelr", "interactions"))
.libPaths(c(file.path(root, paste0("lib-", c("interactions", "panelr", "jtools", "current"))),
            .libPaths()))
output <- file.path(root, "final-checks", package)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
result <- withr::with_libpaths(.libPaths(), devtools::check(
  file.path(root, "sources", package), document = FALSE, vignettes = TRUE,
  manual = TRUE, cran = TRUE, check_dir = output, error_on = "never",
  env_vars = c(NOT_CRAN = "false", `_R_CHECK_CRAN_INCOMING_` = "true",
               `_R_CHECK_CRAN_INCOMING_REMOTE_` = "true")))
saveRDS(result, file.path(output, "result.rds"))
evidence <- file.path("release", "evidence", "final", package)
dir.create(evidence, recursive = TRUE, showWarnings = FALSE)
check <- file.path(output, paste0(package, ".Rcheck"))
for (name in c("00check.log", "tests/testthat.Rout")) {
  from <- file.path(check, name)
  if (file.exists(from)) {
    stopifnot(file.copy(from, file.path(evidence, basename(name)), overwrite = TRUE))
  }
}
quit(status = as.integer(length(result$errors) > 0 || length(result$warnings) > 0))
