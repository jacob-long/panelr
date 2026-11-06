# Upload an already checked archive; this does not submit anything to CRAN.
# Usage: Rscript release/submit-hosted.R ROOT PACKAGE SERVICE [TEST_ONLY_ARCHIVE]
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) %in% c(3L, 4L))
root <- normalizePath(args[[1]], mustWork = TRUE)
package <- match.arg(args[[2]], c("jtools", "panelr", "interactions"))
service <- match.arg(args[[3]], c("rhub", "win-release", "win-devel"))
archive <- list.files(file.path(root, "checks", package), "[.]tar[.]gz$", full.names = TRUE)
stopifnot(length(archive) == 1L)
release_archive <- archive
if (length(args) == 4L) {
  stopifnot(service == "rhub", package == "interactions")
  archive <- normalizePath(args[[4]], mustWork = TRUE)
  audit <- jsonlite::read_json("release/evidence/hosted/interactions-test-metadata.json")
  stopifnot(audit$test_sha256 == digest::digest(file = archive, algo = "sha256"),
            audit$release_sha256 == digest::digest(file = release_archive, algo = "sha256"))
}
check <- readRDS(file.path(root, "logs", paste0(package, "-check.rds")))
stopifnot(length(check$errors) == 0L, length(check$warnings) == 0L)
metadata <- desc::desc(file = archive)
version <- metadata$get("Version")[[1]]
stopifnot(metadata$get("Package")[[1]] == package, check$version == version)
output <- "release/evidence/hosted"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
record <- file.path(output, paste0(package, "-", version, "-", service, ".json"))
if (file.exists(record)) stop("Submission record already exists; inspect it before retrying: ", record)
if (service == "rhub") {
  previous <- lapply(list.files(output, "-rhub[.]json$", full.names = TRUE), jsonlite::read_json)
  previous <- Filter(function(x) identical(x$status, "accepted"), previous)
  if (length(previous)) {
    times <- vapply(previous, function(x) as.numeric(as.POSIXct(x$finished_utc,
      format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")), numeric(1))
    delay <- max(times) + 305 - as.numeric(Sys.time())
    if (is.finite(delay) && delay > 0) {
      cat("Waiting", ceiling(delay), "seconds for the shared-runner submission interval.\n")
      Sys.sleep(delay)
    }
  }
}
submission <- list(package = package, version = version, service = service,
  archive = archive, sha256 = digest::digest(file = archive, algo = "sha256"),
  test_only_metadata = length(args) == 4L,
  started_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), status = "started")
save_record <- function() jsonlite::write_json(submission, record, pretty = TRUE, auto_unbox = TRUE)
save_record()
tryCatch({
  if (service == "rhub") {
    platforms <- c("ubuntu-release", "ubuntu-clang", "r-release-windows", "windows")
    response <- rhub::rc_submit(archive, platforms = platforms,
                                email = "jacob.long@sc.edu", confirmation = TRUE)
    stopifnot(identical(response$result, "OK"))
    submission$platforms <- platforms
    submission$response <- response[intersect(names(response),
      c("result", "repo_url", "actions_url", "id", "name"))]
  } else {
    platform <- if (service == "win-release") "R-release" else "R-devel"
    url <- paste0("ftp://win-builder.r-project.org/", platform, "/", basename(archive))
    response <- devtools:::upload_ftp(archive, url)
    stopifnot(response$status_code >= 200, response$status_code < 300)
    submission$url <- url
    submission$response_code <- response$status_code
  }
  submission$status <- "accepted"
  submission$finished_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  save_record()
  cat(jsonlite::toJSON(submission, pretty = TRUE, auto_unbox = TRUE), "\n")
}, error = function(e) {
  submission$status <- "error-needs-inspection-before-retry"
  submission$error <- conditionMessage(e)
  save_record()
  stop(e)
})
