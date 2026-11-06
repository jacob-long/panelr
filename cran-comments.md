## Test environments
* Local macOS (aarch64), R 4.5.1
* Linux and Windows (R-hub), R 4.6.2 and R-devel
* Windows (win-builder), R-devel

## R CMD check results
R-hub and win-builder: 0 ERRORs | 0 WARNINGs | 0 NOTEs.

The only local NOTE was skipped HTML manual validation because HTML Tidy was
out of date. A separate URL check returned HTTP 403 for some cited articles;
their DOIs were verified with Crossref.

## Reverse dependencies
`dpm` 1.3.0 passed with both `panelr` 1.0.1 and 1.1.0.
