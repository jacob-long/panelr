#' Prepare component-aware interaction plots
#'
#' Integration hook for `interactions::interact_plot()` and
#' `interactions::cat_plot()`. Plotting packages should call this before choosing
#' predictor ranges or moderator values. The fitted object is not modified.
#'
#' @param model A fitted `wbm` or `wbgee` model.
#' @param data Original fitting data, optionally a subset of its observations.
#'   Panel identifiers must be present. Extra columns are allowed.
#' @param pred,modx,mod2 Character names of the predictor and moderators.
#' @param plot.scale `"within"` (default) displays numeric within-person terms
#'   as deviations in their original units. `"original"` displays raw levels,
#'   conditional on fixed reference person means. Constant terms retain their
#'   original scale in either mode. Models without within-person centering are
#'   unchanged.
#' @param modx.values,mod2.values Moderator values on the selected display scale.
#' @param at Fixed values on the display scale. Overrides of `imean(...)`
#'   columns are always in original units.
#' @return A list containing display data, a temporary model with plotting
#'   metadata, moderator values, default labels, and a caption.
#' @keywords internal
#' @export
prepare_wb_plot <- function(model, data, pred, modx = NULL, mod2 = NULL,
                            plot.scale = c("within", "original"),
                            modx.values = NULL, mod2.values = NULL, at = NULL) {
  plot.scale <- match.arg(plot.scale)
  ci <- wb_call_info(model)
  spec <- wb_spec(model)
  pf <- ci$pf
  if (isTRUE(ci$detrend) || length(pf$matrix_terms) || any(pf$v_info$lag != 0)) {
    stop_wrap("Automatic interaction plots are not supported for detrending,",
              " lags/leads, or basis expansions. Use predict() with complete",
              " panel histories or prepared model-scale rows instead.")
  }
  within <- is_within_model(InteractionConfig(ci$interaction.style, ci$model,
                                             ci$detrend))
  varying <- un_bt(pf$v_info$term)
  original <- as.data.frame(jtools::get_data(model))
  direct <- intersect(varying, names(original))
  direct <- direct[vapply(original[direct], is.numeric, logical(1))]
  # log(x) - mean(log(x)) is not x - mean(x). Do not label these alike.
  indirect <- setdiff(varying, c(direct, unlist(lapply(spec$factors, `[[`, "columns"))))
  if (within && plot.scale == "within" && length(indirect)) {
    stop_wrap("Within-scale plots require untransformed numeric predictors.",
              " Use plot.scale = 'original' for scalar transformations.")
  }
  if (!within) direct <- character()

  data <- as.data.frame(data)
  keys <- c(ci$id, ci$wave)
  if (!all(keys %in% names(data))) {
    stop_wrap("Plot data must contain the panel identifiers: ", paste(keys, collapse = ", "),
              ". Use 'at' to specify hypothetical predictor values.")
  }
  frame <- wb_frame(model)
  key_data <- function(d) {
    out <- as.data.frame(d[keys])
    out[[ci$id]] <- as.character(out[[ci$id]])
    out
  }
  source_rows <- vctrs::vec_match(key_data(data), key_data(original))
  rows <- vctrs::vec_match(key_data(data), key_data(frame))
  if (anyNA(source_rows)) {
    stop_wrap("Plot data must contain fitting-data observations, not new panel histories.",
              " Use 'at' for hypothetical profiles or predict() for new panels.")
  }
  vars <- intersect(all.vars(spec$formula), intersect(names(data), names(original)))
  same <- vapply(vars, function(v) {
    isTRUE(all.equal(data[[v]], original[[v]][source_rows], check.attributes = FALSE))
  }, logical(1))
  if (!all(same)) {
    stop_wrap("Plot data contain changed model variables. Use 'at' to set hypothetical values.")
  }
  keep <- !is.na(rows)
  data <- data[keep, , drop = FALSE]
  rows <- rows[keep]
  if (!nrow(data)) stop_wrap("Plot data contain no estimation-sample observations.")

  # Call expressions may contain wrapper promises or transformed weights.
  # Give downstream grid builders the actual weights in the displayed row order.
  if (!is.null(frame[[".weights"]])) {
    weight_name <- utils::tail(make.unique(c(names(data), ".panelr_weights")), 1L)
    data[[weight_name]] <- frame[[".weights"]][rows]
    if (inherits(model, "wbm")) model@call$weights <- as.name(weight_name)
    else model$call$weights <- as.name(weight_name)
  }

  unscaled <- function(v) {
    x <- frame[[v]]
    s <- spec$scaling[[v]]
    if (!is.null(s)) x <- x * s[["scale"]] + s[["center"]]
    x
  }
  numeric_terms <- if (within) setdiff(varying, unlist(lapply(spec$factors, `[[`, "columns"))) else character()
  deviations <- lapply(stats::setNames(numeric_terms, numeric_terms), unscaled)
  ranges <- lapply(deviations, range, na.rm = TRUE)
  means <- stats::setNames(un_bt(pf$v_info$meanvar[match(numeric_terms, varying)]), numeric_terms)
  reference <- spec$reference
  for (v in intersect(names(at), names(reference))) reference[[v]] <- at[[v]]
  if (any(!vapply(reference, function(x) {
    is.numeric(x) && length(x) == 1L && is.finite(x)
  }, logical(1)))) {
    stop_wrap("Interaction plots require one finite numeric value per reference mean in 'at'.")
  }
  if (plot.scale == "within") {
    for (v in direct) data[[v]] <- deviations[[v]][rows]
  }

  moderator_values <- function(v, values) {
    if (is.null(v) || !v %in% direct || !is.null(values) || plot.scale != "within") {
      return(values)
    }
    x <- data[[v]]
    # Zero has a substantive meaning even when missingness shifts the sample mean.
    if (length(unique(x)) <= 2L) return(sort(unique(x)))
    unique(c(-stats::sd(x), 0, stats::sd(x)))
  }
  label <- function(v) {
    if (is.null(v)) return(NULL)
    if (v %in% direct && plot.scale == "within") {
      paste0(v, " (within-person deviation)")
    } else v
  }
  ref_text <- paste(vapply(names(means), function(v) {
    paste0(v, " = ", paste(signif(reference[[means[[v]]]], 4), collapse = ", "))
  }, character(1)), collapse = "; ")
  caption <- if (length(numeric_terms)) {
    paste0(if (plot.scale == "within") {
      "0 = the person's observed mean; deviations are in original units.\n"
    } else "Original levels, conditional on reference person means.\n",
    "Reference person means: ", ref_text, ".")
  } else ""
  if (within) {
    caption <- paste0(caption, if (nzchar(caption)) "\n" else "",
                      "Person means and interaction-centering terms held fixed.")
  }
  context <- list(scale = plot.scale, means = means, ranges = ranges,
                  reference = reference, warned = new.env(parent = emptyenv()))
  attr(model, "panelr_plot") <- context
  list(model = model, data = data, x.label = label(pred),
       legend.main = label(modx), caption = caption,
       modx.values = moderator_values(modx, modx.values),
       mod2.values = moderator_values(mod2, mod2.values))
}

prepare_wb_plot_grid <- function(model, grid, raw = FALSE, check_support = TRUE) {
  context <- attr(model, "panelr_plot", exact = TRUE)
  if (is.null(context)) {
    return(if (raw) process_raw_newdata(model, grid) else prepare_wb_grid(model, grid))
  }
  if (raw) stop_wrap("raw = TRUE cannot be combined with component-aware interaction plots.")
  grid <- as.data.frame(grid)
  for (v in names(context$reference)) {
    if (!v %in% names(grid)) grid[[v]] <- context$reference[[v]]
  }
  for (v in names(context$means)) {
    value <- if (v %in% names(grid)) grid[[v]] else {
      eval(parse(text = v), grid, environment(formula(model)))
    }
    mean_name <- context$means[[v]]
    ref <- grid[[mean_name]]
    delta <- if (context$scale == "within") value else value - ref
    support <- context$ranges[[v]]
    if (check_support &&
        any(delta < support[1] - 1e-8 | delta > support[2] + 1e-8, na.rm = TRUE) &&
        !exists(v, context$warned, inherits = FALSE)) {
      assign(v, TRUE, context$warned)
      warn_wrap("Plot values for '", v, "' imply within-person deviations outside the observed range [",
                signif(support[1], 4), ", ", signif(support[2], 4),
                "]. These predictions extrapolate.")
    }
    if (context$scale == "within") grid[[v]] <- delta + ref
  }
  prepare_wb_grid(model, grid)
}
