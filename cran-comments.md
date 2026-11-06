## Reason for update

This update addresses the CRAN notice of October 24, 2026, requesting fixes
before November 14, 2026, following the release of lme4 2.1-0.

* Bind weights and offsets, including NULL defaults, in the model formula's
  environment. This fixes the `invalid type (closure)` errors in `wbm()`.
* Preserve lme4 covariance metadata when converting between model classes.
  This fixes model display and likelihood calculations with recent lme4.
* Add regression tests for defaults, weights and offsets after missing-data
  removal and lags, Gaussian and count models, and covariance metadata.
* Update the redirected WageData source URL.
* Require lmerTest 3.2-1 or later for compatibility with current lme4.
* Rework prediction and add public design/grid hooks for component-aware
  interaction plotting and contrast-based inference. These optional features
  require the coordinated interactions 1.3.0 release; panelr itself does not.
* Fix panel row binding, pdata.frame conversion, and input validation.

## Test environments

* Local macOS (aarch64), R 4.5.1, lme4 2.1-0, lmerTest 3.2-1,
  reformulas 0.4.4, ggplot2 4.0.3.
* Partial-upgrade smoke tests pass with both lme4 1.1-38 and 2.1-0.
* Targeted prediction/integration tests also pass with lme4 1.1-38,
  reformulas 0.4.2, and ggplot2 3.4.4 (324 assertions, no warnings or skips).
* R-hub Windows and Linux, R 4.6.2 and R-devel (Windows r90836,
  Linux clang r90827), all pass.
* Windows Builder, R-devel r90836 ucrt on Windows Server 2022, passes.

## R CMD check results

Final local `R CMD check --as-cran`, including incoming checks, vignettes and PDF manual:

0 ERRORs | 0 WARNINGs | 1 NOTE

Examples, the test suite (785 passing assertions), and both vignettes pass.
The NOTE is an environment limitation: the installed HTML Tidy is too old
to validate the HTML manual. CRAN incoming feasibility passes. This is a
release-preparation report, not a submission.

Windows Builder R-devel and all four R-hub Windows/Linux release/devel checks:

0 ERRORs | 0 WARNINGs | 0 NOTEs

Windows Builder also passes CRAN incoming feasibility and PDF/HTML manual
checks. Its report is archived locally from:
https://win-builder.r-project.org/BJflowhx1cY8/00check.log

Each hosted panelr test run passes 706 assertions and skips 10 optional
tests requiring the forthcoming interactions 1.3.0 integration. Those skips
are addressed by separate coordinated-candidate tests described below.
R-hub checks use `--no-manual`; Windows Builder includes the manual.

## Compatibility checks

The direct downstream package dpm 1.3.0 passes local package checks with
both released and candidate dependencies (0 errors, warnings, or notes).
Those checks run examples, tests, and vignette R code, but do not build a
PDF manual or rebuild vignettes.

The coordinated interactions 1.3.0 candidate passes its installed-package
numerical tests with panelr 1.1.0 (394 assertions, no failures or warnings).
Its full visual suite passes separately. New within-person, original-scale,
standardized, simple-slope, and Johnson-Neyman plots have been reviewed for
axis units, moderator values, reference means, and inference captions.

Separate interactions Windows and Linux R-release/R-devel checks with pinned panelr
1.1.0 and jtools 2.3.2 dependencies pass 396 assertions, including the panelr
integration tests, with no failures or warnings. These use test-only dependency
metadata; the release archives are unchanged. All four integration checks pass.
