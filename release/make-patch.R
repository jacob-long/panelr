# Assemble a reviewable patch without touching either input tree.
# Usage: Rscript release/make-patch.R BEFORE AFTER OUTPUT FILE...
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) >= 4L)
patch <- character()
for (file in args[-(1:3)]) {
  before <- file.path(args[[1]], file)
  after <- file.path(args[[2]], file)
  old <- if (file.exists(before)) before else "/dev/null"
  label <- if (file.exists(before)) paste0("a/", file) else "/dev/null"
  delta <- suppressWarnings(system2("diff", c("-u", "-L", shQuote(label),
    "-L", shQuote(paste0("b/", file)), shQuote(old), shQuote(after)), stdout = TRUE))
  stopifnot(is.null(attr(delta, "status")) || attr(delta, "status") == 1L)
  if (length(delta)) patch <- c(patch, paste("diff --git", paste0("a/", file),
                                           paste0("b/", file)),
                               if (old == "/dev/null") "new file mode 100644", delta)
}
writeLines(patch, args[[3]])
