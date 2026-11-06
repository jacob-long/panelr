wb_call_info <- function(object) {
  if (inherits(object, "wbm")) object@call_info else object$call_info
}

wb_frame <- function(object) {
  if (inherits(object, "wbm")) object@frame else object$frame
}

wb_factor_spec <- function(formula, original, expanded) {
  original <- as.data.frame(original)
  vars <- intersect(all.vars(stats::formula(Formula::Formula(formula), lhs = 0)),
                    names(original))
  vars <- vars[!vapply(original[vars], is.numeric, logical(1))]
  out <- list()
  for (var in vars) {
    values <- unique(original[[var]])
    values <- values[!is.na(values)]
    base <- base_level(original[[var]])
    values <- values[values != base]
    columns <- paste0(var, values)
    keep <- columns %in% names(expanded)
    if (any(keep)) {
      out[[var]] <- list(values = as.character(values[keep]),
                         columns = columns[keep], base = as.character(base))
    }
  }
  out
}

wb_prediction_spec <- function(formula, prepped, data) {
  before <- as.data.frame(prepped$e$data)
  scaling <- list()
  # Record the affine transformations actually applied, including binary recoding.
  for (name in intersect(names(before), names(data))) {
    x <- before[[name]]
    y <- data[[name]]
    if (!is.numeric(x) || !is.numeric(y) || isTRUE(all.equal(x, y))) next
    good <- which(is.finite(x) & is.finite(y))
    if (length(good) < 2L) next
    lo <- good[which.min(x[good])]
    hi <- good[which.max(x[good])]
    if (x[hi] == x[lo] || y[hi] == y[lo]) next
    divisor <- (x[hi] - x[lo]) / (y[hi] - y[lo])
    scaling[[name]] <- c(center = x[lo] - y[lo] * divisor, scale = divisor)
  }
  refs <- names(before)[startsWith(names(before), "imean(")]
  reference <- lapply(before[refs], mean, na.rm = TRUE)
  for (name in names(prepped$grid_moments)) {
    moments <- prepped$grid_moments[[name]]
    reference[[name]] <- mean(moments[as.character(before[[prepped$id]])], na.rm = TRUE)
  }
  list(formula = formula,
       factors = wb_factor_spec(formula, prepped$orig_data, prepped$pf$data),
       scaling = scaling,
       reference = reference)
}

wb_grid_moments <- function(pf, data, config) {
  out <- list()
  if (!is_within_model(config) || config$detrend || config$style == "raw" ||
      length(pf$matrix_terms)) return(out)
  id <- data[[get_id(data)]]
  source <- as.data.frame(data)
  if (use_old_style_ints(config)) {
    for (i in seq_len(nrow(pf$v_info))) {
      term <- un_bt(pf$v_info$term[i])
      source[[term]] <- source[[term]] + source[[un_bt(pf$v_info$meanvar[i])]]
    }
  }
  for (term in pf$cint_labs) {
    varying <- intersect(all.vars(as.formula(paste("~", term))), un_bt(pf$varying))
    if (length(varying) < 2L) next
    name <- paste0("imean(", paste(varying, collapse = ":"), ")")
    product <- Reduce(`*`, source[varying])
    moments <- stats::ave(product, id, FUN = function(x) mean(x, na.rm = TRUE))
    keep <- !duplicated(id)
    out[[name]] <- stats::setNames(moments[keep], as.character(id[keep]))
  }
  out
}

wb_expand_new_factors <- function(data, spec) {
  for (var in names(spec)) {
    if (!var %in% names(data)) stop_wrap("newdata must contain '", var, "'.")
    info <- spec[[var]]
    x <- as.character(data[[var]])
    unknown <- setdiff(x[!is.na(x)], c(info$base, info$values))
    if (length(unknown)) {
      stop_wrap("New levels in '", var, "': ", paste(unknown, collapse = ", "))
    }
    for (i in seq_along(info$columns)) {
      data[[info$columns[i]]] <- as.numeric(x == info$values[i])
    }
  }
  data
}

wb_apply_scaling <- function(data, scaling) {
  for (name in intersect(names(scaling), names(data))) {
    info <- scaling[[name]]
    data[[name]] <- (data[[name]] - info[["center"]]) / info[["scale"]]
  }
  data
}

wb_spec <- function(object) {
  ci <- wb_call_info(object)
  if (!is.null(ci$prediction)) return(ci$prediction)
  stop_wrap("Refit this model to save its prediction preprocessing metadata,",
            " or supply prepared model-scale rows with raw = TRUE.")
}

wb_prediction_offset <- function(object, data) {
  ci <- wb_call_info(object)
  expr <- if (!is.null(ci$prediction)) ci$prediction$offset_expr else getCall(object)$offset
  if (!is.null(expr) && !".offset" %in% names(data)) {
    missing <- setdiff(all.vars(expr), names(data))
    if (length(missing)) {
      stop_wrap("Supply '.offset' or the offset variables in newdata: ",
                paste(missing, collapse = ", "))
    }
    value <- if (is.character(expr) && length(expr) == 1L && expr %in% names(data)) {
      data[[expr]]
    } else eval(expr, data, environment(formula(object)))
    if (!is.null(value)) {
      if (!length(value) %in% c(1L, nrow(data))) {
        stop_wrap("The prediction offset must have length 1 or nrow(newdata).")
      }
      data[[".offset"]] <- value
    }
  }
  data
}

process_nonraw_newdata <- function(object, newdata, re.form = NULL) {
  ci <- wb_call_info(object)
  spec <- wb_spec(object)
  pf <- ci$pf
  id <- ci$id
  wave <- ci$wave
  newdata <- as.data.frame(newdata)
  if (!id %in% names(newdata)) {
    stop_wrap("Panel predictions require the '", id,
              "' variable to calculate within-person transformations, even",
              " when random effects are excluded. Use raw = TRUE for prepared data.")
  }
  if (!wave %in% names(newdata)) {
    needs_wave <- isTRUE(ci$use.wave) || isTRUE(ci$detrend) ||
      any(ci$pf$v_info$lag != 0) || wave %in% all.vars(formula(object))
    if (needs_wave) stop_wrap("newdata must contain the '", wave, "' variable.")
    newdata[[wave]] <- seq_len(nrow(newdata))
  }
  newdata <- wb_prediction_offset(object, newdata)
  rowvar <- utils::tail(make.unique(c(names(newdata), ".panelr_row")), 1L)
  newdata[[rowvar]] <- seq_len(nrow(newdata))
  newdata <- panel_data(newdata, id = !!sym(id), wave = !!sym(wave))
  newdata <- wb_expand_new_factors(newdata, spec$factors)
  if (length(pf$matrix_terms)) {
    newdata <- expand_matrix_terms_in_data(pf$matrix_terms, newdata)$data
  }
  labs <- attr(terms(as.formula(ci$mf_form)), "term.labels")
  labs <- labs[!un_bt(labs) %in% c(ci$dv, ".weights")]
  mf <- as.formula(paste("~", paste(c(labs, bt(rowvar)), collapse = " + ")),
                   env = environment(formula(object)))
  newdata <- model_frame(mf, newdata)
  newdata <- wb_model(ci$model, pf, ci$dv, newdata, ci$detrend,
                      ci$interaction.style == "double-demean",
                      ci$interaction.style == "demean")$data
  if (isTRUE(ci$detrend)) {
    newdata <- detrend(newdata, pf, ci$dt_order, ci$balance_correction, ci$dt_random)
  }
  newdata <- wb_apply_scaling(as.data.frame(newdata), spec$scaling)
  if (isTRUE(ci$wave.factor)) {
    newdata[[wave]] <- factor(newdata[[wave]], levels = levels(wb_frame(object)[[wave]]),
                              ordered = is.ordered(wb_frame(object)[[wave]]))
  }
  newdata <- newdata[order(newdata[[rowvar]]), , drop = FALSE]
  newdata[[rowvar]] <- NULL
  rownames(newdata) <- NULL
  newdata
}

process_raw_newdata <- function(object, newdata) {
  # Prepared rows are authoritative, including interaction columns.
  as.data.frame(newdata)
}

wb_grid_data <- function(object, data = NULL) {
  if (is.null(data)) data <- jtools::get_data(object)
  data <- as.data.frame(data)
  references <- wb_spec(object)$reference
  for (name in names(references)) {
    if (!name %in% names(data)) data[[name]] <- references[[name]]
  }
  data
}

prepare_wb_grid <- function(object, grid) {
  ci <- wb_call_info(object)
  spec <- wb_spec(object)
  pf <- ci$pf
  if (isTRUE(ci$detrend) || length(pf$matrix_terms) || any(pf$v_info$lag != 0)) {
    stop_wrap("Automatic prediction grids are not supported for detrending,",
              " lags/leads, or basis expansions. Supply prepared model-scale",
              " rows with raw = TRUE, or use predict() with complete panel histories.")
  }
  grid <- wb_prediction_offset(object, as.data.frame(grid))
  grid <- wb_expand_new_factors(grid, spec$factors)
  for (term in unique(c(pf$varying, pf$constants))) {
    if (!term %in% names(grid)) {
      grid[[term]] <- eval(parse(text = term), grid, environment(formula(object)))
    }
  }
  for (name in names(spec$reference)) {
    if (!name %in% names(grid)) grid[[name]] <- spec$reference[[name]]
  }
  config <- InteractionConfig(ci$interaction.style, ci$model, ci$detrend)
  within <- is_within_model(config)
  originals <- grid
  if (within) {
    for (i in seq_len(nrow(pf$v_info))) {
      term <- un_bt(pf$v_info$term[i])
      grid[[term]] <- grid[[term]] - grid[[un_bt(pf$v_info$meanvar[i])]]
    }
  }
  for (term in c(pf$wint_labs, pf$cint_labs)) {
    name <- un_bt(gsub("*", ":", term, fixed = TRUE))
    source <- if (within && use_old_style_ints(config)) originals else grid
    parts <- all.vars(as.formula(paste("~", term)))
    varying <- intersect(parts, un_bt(pf$varying))
    constants <- setdiff(parts, varying)
    if (within && length(constants)) {
      # Constant moderators factor out of the within-person centering operation.
      if (length(varying) == 1L) {
        value <- grid[[varying]]
      } else {
        value <- Reduce(`*`, source[varying])
        if (config$style != "raw") {
          mean_name <- paste0("imean(", paste(varying, collapse = ":"), ")")
          if (!mean_name %in% names(grid)) stop_wrap("Missing reference mean: ", mean_name)
          value <- value - grid[[mean_name]]
        }
      }
      grid[[name]] <- value * Reduce(`*`, grid[constants])
      next
    }
    value <- eval(parse(text = term), source, environment(formula(object)))
    if (within && (use_old_style_ints(config) || should_demean_ints(config))) {
      mean_name <- paste0("imean(", name, ")")
      if (!mean_name %in% names(grid)) {
        stop_wrap("Missing reference interaction mean: ", mean_name)
      }
      value <- value - grid[[mean_name]]
    }
    grid[[name]] <- value
  }
  grid <- wb_apply_scaling(grid, spec$scaling)
  if (isTRUE(ci$use.wave) && !ci$wave %in% names(grid)) {
    observed <- wb_frame(object)[[ci$wave]]
    grid[[ci$wave]] <- if (is.factor(observed)) levels(observed)[1] else mean(observed)
  }
  if (isTRUE(ci$wave.factor)) {
    grid[[ci$wave]] <- factor(grid[[ci$wave]], levels = levels(wb_frame(object)[[ci$wave]]))
  }
  grid
}

wb_fixed_matrix <- function(object, newdata = NULL, na.action = na.pass) {
  if (inherits(object, "wbgee")) {
    if (is.null(newdata)) newdata <- model.frame(object)
    tt <- delete.response(terms(object))
    mf <- model.frame(tt, newdata, na.action = na.action, xlev = object$xlevels)
    X <- model.matrix(tt, mf, contrasts.arg = object$contrasts)
    return(X[, names(coef(object)), drop = FALSE])
  }
  if (is.null(newdata)) return(lme4::getME(object, "X"))
  tt <- delete.response(terms(object, fixed.only = TRUE))
  original <- model.frame(object)
  factors <- original[intersect(all.vars(tt), names(original))]
  factors <- factors[vapply(factors, is.factor, logical(1))]
  mf <- model.frame(tt, newdata, na.action = na.action, xlev = lapply(factors, levels))
  X <- model.matrix(tt, mf, contrasts.arg = attr(lme4::getME(object, "X"), "contrasts"))
  X[, names(lme4::fixef(object)), drop = FALSE]
}

predict_wb_mermod <- function(object, newdata, se.fit, re.form, type,
                              allow.new.levels, na.action, use.re.var = FALSE,
                              boot = FALSE, sims = 1000, prog.arg = "none", ...) {
  object <- wb_prediction_backend(object, newdata)
  if (use.re.var) {
    stop_wrap("use.re.var is not supported: adding random-effect variance to a",
              " fixed-effect standard error does not give a valid prediction interval.")
  }
  if (boot && se.fit) {
    fun <- function(fit) predict(fit, newdata = newdata, re.form = re.form,
                                 type = type, allow.new.levels = allow.new.levels,
                                 na.action = na.action)
    out <- lme4::bootMer(object, FUN = fun, nsim = sims, .progress = prog.arg, ...)
    return(list(fit = out$t))
  }
  eta <- predict(object, newdata = newdata, re.form = re.form, type = "link",
                  allow.new.levels = allow.new.levels, na.action = na.action, ...)
  fit <- if (type == "response") family(object)$linkinv(eta) else eta
  if (!se.fit) return(fit)
  X <- wb_fixed_matrix(object, newdata, na.action)
  se <- sqrt(pmax(0, rowSums((X %*% as.matrix(vcov(object))) * X)))
  if (type == "response") se <- se * abs(family(object)$mu.eta(eta))
  list(fit = fit, se.fit = unname(se))
}

wb_prediction_backend <- function(object, newdata = NULL) {
  object <- to_merMod(object)
  # lme4 replays formula offsets, but not the separate fitting offset argument.
  if (!is.null(newdata) && !is.null(model.offset(object@frame))) {
    if (!".offset" %in% names(newdata)) {
      stop_wrap("Prepared prediction data must include the fitted model's '.offset' column.")
    }
    object@frame[["offset(.offset)"]] <- model.offset(object@frame)
    attr(object@frame, "formula") <- stats::update.formula(formula(object), ~ . + offset(.offset))
    attr(object@frame, "terms") <- terms(stats::update.formula(
      formula(attr(object@frame, "terms")), ~ . + offset(.offset)))
  }
  object
}
