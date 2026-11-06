#' Design contrasts for conditional within-between slopes
#'
#' Integration hook for the S3 methods in `interactions::sim_slopes()`.
#' Uses the same hypothetical-grid transformation as interaction plots. No
#' models are refitted, and no person means are recomputed from the grid.
#'
#' @param model A fitted `wbm` or `wbgee` model, optionally prepared by
#'   [prepare_wb_plot()] to use within-person display units.
#' @param pred Character name of an untransformed numeric focal predictor.
#' @param grid A data frame containing all predictor values for each requested
#'   profile, on the selected display scale. Without plotting metadata, inputs
#'   use original units and original levels.
#' @param cond.int Will conditional intercepts be reported? If so, their
#'   location at `pred = 0` on the display scale is checked for extrapolation.
#'   Intercept design rows and offsets are returned regardless.
#' @param check_support Warn about out-of-range profile values? Internal unit
#'   steps used to calculate contrasts never trigger extrapolation warnings.
#' @param linear.moderator Optional moderator name that must enter linearly,
#'   as required for analytic Johnson-Neyman regions.
#' @return A list containing slope contrasts (`L`), intercept design rows
#'   (`X0`), intercept offsets, fixed-effect coefficients, their covariance
#'   matrix, residual degrees of freedom, and the number of observations.
#' @details
#' Slopes are fixed-effect, link-scale slopes conditional on reference person
#' means and interaction-centering quantities. They are not derivatives of an
#' entire recomputed panel history, nor averages over random effects. The
#' Gaussian identity link gives response-scale slopes. Predictor units follow
#' the display scale even when the fitted model is standardized. Nonlinear
#' focal terms, predictor-dependent offsets, basis expansions, lagged/detrended
#' models, and rank-deficient fits are not supported.
#' @keywords internal
#' @export
wb_slope_design <- function(model, pred, grid, cond.int = FALSE,
                            check_support = TRUE, linear.moderator = NULL) {
  ci <- wb_call_info(model)
  spec <- wb_spec(model)
  grid <- as.data.frame(grid)
  if (!pred %in% names(grid) || !is.numeric(grid[[pred]])) {
    stop_wrap("Simple slopes require a numeric focal predictor.")
  }
  terms <- un_bt(unique(c(ci$pf$varying, ci$pf$constants)))
  original_names <- names(jtools::get_data(model))
  is_linear <- function(var) {
    involved <- vapply(terms, function(term) {
      if (term %in% original_names) return(identical(term, var))
      var %in% all.vars(parse(text = term)[[1L]])
    }, logical(1))
    any(involved) && all(terms[involved] == var)
  }
  if (!is_linear(pred)) {
    stop_wrap("Simple slopes require an untransformed numeric focal predictor.",
              " Nonlinear or transformed focal terms need local derivatives.")
  }
  if (!is.null(linear.moderator) && !is_linear(linear.moderator)) {
    stop_wrap("Analytic Johnson-Neyman regions require an untransformed numeric moderator.",
              " Use johnson_neyman = FALSE for slopes at specified transformed-moderator values.")
  }
  if (pred %in% all.vars(spec$offset_expr)) {
    stop_wrap("Simple slopes with predictor-dependent offsets are not supported.")
  }
  mixed <- inherits(model, "wbm")
  beta <- if (mixed) lme4::fixef(model) else coef(model)
  rank_deficient <- if (mixed) {
    length(attr(lme4::getME(model, "X"), "col.dropped")) > 0L
  } else model$rank < length(beta)
  if (rank_deficient || any(!is.finite(beta))) {
    stop_wrap("Simple slopes for rank-deficient fits require estimability checks",
              " and are not currently supported.")
  }
  V <- as.matrix(vcov(model))[names(beta), names(beta), drop = FALSE]
  if (any(!is.finite(V))) stop_wrap("The fitted covariance matrix must be finite.")

  # Check the requested profile, not the artificial points used for differencing.
  prepare_wb_plot_grid(model, grid, check_support = check_support)
  base <- high <- grid
  step <- pmax(1, abs(grid[[pred]]) * sqrt(.Machine$double.eps))
  high[[pred]] <- grid[[pred]] + step
  base <- prepare_wb_plot_grid(model, base, check_support = FALSE)
  high <- prepare_wb_plot_grid(model, high, check_support = FALSE)
  L <- (wb_fixed_matrix(model, high) - wb_fixed_matrix(model, base)) / step
  zero <- grid
  zero[[pred]] <- 0
  zero <- prepare_wb_plot_grid(model, zero, check_support = FALSE)
  X0 <- wb_fixed_matrix(model, zero)
  if (cond.int) {
    displayed_zero <- grid
    displayed_zero[[pred]] <- 0
    prepare_wb_plot_grid(model, displayed_zero, check_support = check_support)
  }
  intercept <- if (mixed) {
    as.numeric(predict(model, zero, raw = TRUE, re.form = NA, type = "link"))
  } else as.numeric(predict(model, zero, raw = TRUE, type = "link"))
  list(L = L, X0 = X0, offset = intercept - drop(X0 %*% beta),
       beta = beta, vcov = V, df = df.residual(model), nobs = nrow(wb_frame(model)),
       gaussian = mixed && lme4::isLMM(model), link = family(model)$link)
}
