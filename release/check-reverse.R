# Check the identical released tarball against released or candidate dependencies.
# Usage: Rscript release/check-reverse.R ROOT PACKAGE [baseline|candidate]
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("autostats", "bruceR", "dpm", "webSDM", "panelr", "interactions"))
variant <- match.arg(args[[3]], c("baseline", "candidate"))
libs <- if (variant == "candidate") c("interactions", "panelr", "jtools") else "cran"
.libPaths(c(file.path(root, paste0("lib-", c(libs, "current"))), .libPaths()))
folder <- if (package %in% c("panelr", "interactions")) "cran" else "revdep"
tarball <- list.files(file.path(root, folder), pattern = paste0("^", package, "_.*[.]tar[.]gz$"),
                      full.names = TRUE)
stopifnot(length(tarball) == 1L)
output <- file.path(root, "revdep", "checks", package, variant)
dir.create(output, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(NOT_CRAN = "false", `_R_CHECK_FORCE_SUGGESTS_` = "false",
           `_R_CHECK_CRAN_INCOMING_REMOTE_` = "false", `_R_CHECK_CRAN_INCOMING_` = "false",
           OMP_NUM_THREADS = "2")
result <- withr::with_libpaths(.libPaths(), rcmdcheck::rcmdcheck(tarball,
  args = c("--no-manual", "--no-build-vignettes"), check_dir = output, error_on = "never"))
saveRDS(result, file.path(output, "result.rds"))
capture.output(sessionInfo(), file = file.path(output, "session-info.txt"))
cat("\n", package, variant, ":", length(result$errors), "errors,",
    length(result$warnings), "warnings,", length(result$notes), "notes\n")
quit(status = as.integer(length(result$errors) > 0))
