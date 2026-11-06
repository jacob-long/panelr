# Read hosted job status without dispatching jobs or changing repositories.
# Usage: Rscript release/refresh-hosted.R
output <- "release/evidence/hosted"
query <- function(endpoint) {
  raw <- system2("gh", c("api", shQuote(endpoint)), stdout = TRUE)
  stopifnot(is.null(attr(raw, "status")) || attr(raw, "status") == 0L)
  jsonlite::fromJSON(paste(raw, collapse = "\n"), simplifyVector = FALSE)
}
submissions <- list.files(output, "-rhub[.]json$", full.names = TRUE)
rows <- list()
for (file in submissions) {
  submission <- jsonlite::read_json(file)
  if (!identical(submission$status, "accepted")) next
  repository <- sub("https://github.com/", "", submission$response$repo_url, fixed = TRUE)
  runs <- query(paste0("repos/", repository, "/actions/runs?per_page=20"))$workflow_runs
  runs <- Filter(function(x) grepl(submission$response$id, x$display_title, fixed = TRUE), runs)
  if (!length(runs)) next
  run <- runs[[1]]
  jobs <- query(paste0("repos/", repository, "/actions/runs/", run$id, "/jobs?per_page=100"))$jobs
  result <- list(package = submission$package, checked_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    run_id = run$id, url = run$html_url, status = run$status, conclusion = run$conclusion,
    jobs = lapply(jobs, function(job) job[c("id", "name", "html_url", "status", "conclusion")]))
  status_file <- file.path(output, paste0(submission$package, "-rhub-status.json"))
  jsonlite::write_json(result, status_file, auto_unbox = TRUE, pretty = TRUE, null = "null")
  for (job in jobs) {
    if (job$name %in% c("setup", "update-repo")) next
    rows[[length(rows) + 1L]] <- data.frame(package = submission$package, platform = job$name,
      status = job$status, conclusion = if (is.null(job$conclusion)) "" else job$conclusion,
      url = job$html_url)
  }
}
if (length(rows)) {
  summary <- do.call(rbind, rows)
  write.table(summary, file.path(output, "rhub-jobs.tsv"), sep = "\t", quote = TRUE, row.names = FALSE)
  print(summary[c("package", "platform", "status", "conclusion")], row.names = FALSE)
}
