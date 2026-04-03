## Test environments
* local macOS (aarch64), R 4.5.1
* macOS-release, windows-release, ubuntu-release, ubuntu-oldrel, ubuntu-devel (via GitHub Actions)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

This is a resubmission following the 2026-03-18 archive.

The archive was caused by a test that still exercised dplyr underscore verbs
such as `mutate_()` and `filter_()`. Those APIs are now defunct in current
dplyr releases. The stale tests have been removed; there are no user-facing
code changes in this resubmission.

* NOTE: CRAN incoming checks may report the prior archive; the issue was an
  outdated test and has been corrected.
* NOTE: "McNeish" in the DESCRIPTION is a proper noun.
