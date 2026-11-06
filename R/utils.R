#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr "%<>%"
#' @export
magrittr::`%<>%`

backtick_name <- function(x) {
  if (make.names(x) != x) {paste0("`", x, "`")} else {x}
}

make_names <- function(names, int = FALSE) {
  # Ensure valid leading character
  names <- sub('^[^A-Za-z\\.]+', '.', names)
  # See where dots were originally
  dots <- lapply(names, function(x) which(strsplit(x, NULL)[[1]] == "."))
  if (int == TRUE) {
    names <- gsub(":|\\*", "_by_", names)
  }
  # Use make.names
  names <- make.names(names, allow_ = TRUE)
  # substitute _ for .
  names <- gsub( '\\.', '_', names )
  # Now add the original periods back in
  mapply(names, dots, FUN = function(x, y) {
    x <- strsplit(x, NULL)[[1]]
    x[y] <- "."
    paste0(x, collapse = "")
  })
}

#' @import jtools


#' @importFrom lme4 isLMM
#' @importFrom methods as

# S4 coercion drops non-slot attributes, including lme4's covariance metadata.
as_merMod_class <- function(object, Class) {
  out <- as(object, Class)
  extra_attributes <- setdiff(names(attributes(object)),
                              c(methods::slotNames(object), "class"))
  for (name in extra_attributes) {
    attr(out, name) <- attr(object, name, exact = TRUE)
  }
  out
}

to_merMod <- function(x) {
  stored_call <- if (inherits(x, "wbm")) x@call_info$merMod_call else NULL
  if (isLMM(x)) {
    x <- as_merMod_class(x, "lmerMod")
  } else {
    x <- as_merMod_class(x, "glmerMod")
  }
  x@frame <- as.data.frame(x@frame)
  # Restore the standard column expected by lme4's saved terms object.
  if (is.null(x@frame[["(weights)"]]) && !is.null(x@frame[[".weights"]])) {
    x@frame[["(weights)"]] <- x@frame[[".weights"]]
  }
  if (!is.null(stored_call)) x@call <- stored_call
  return(x)
}

#' @importFrom stats getCall
#' @export

getCall.wbm <- function(x, ...) {
  
  return(x@call)
  
}

#' @title Predictions and simulations from within-between models
#' @description These methods facilitate fairly straightforward predictions
#'  and simulations from `wbm` models.
#'
#' @details
#' With no `newdata`, predictions use the fitted model's design matrix.
#' `re.form = NA` or `~0` excludes random effects; the default includes them.
#'
#' With `raw = FALSE`, supply panel histories containing the original predictor
#' variables and the entity identifier. The outcome is not required unless it
#' also occurs as a predictor (for example, a lagged outcome). Within-person
#' means and detrending are calculated from the supplied histories, using the
#' variable roles, factor coding, basis parameters, and scaling saved at fitting.
#' Predictions are returned in the input row order. Include the wave variable
#' for models with time effects, detrending, lags, or leads. Do not use a
#' hypothetical plotting grid as a panel history: its means are arbitrary.
#'
#' With `raw = TRUE`, supply prepared model-scale rows, including `imean(...)`
#' and processed interaction columns. These columns are passed through unchanged.
#' This argument is distinct from `interaction.style = "raw"` when fitting.
#' For models fitted with an offset argument, prepared rows must include
#' `.offset`; panel histories may instead contain the original offset variables.
#'
#' **Interaction plots**
#'
#' [interactions::interact_plot()] and [interactions::cat_plot()] use a separate
#' hypothetical-grid path through [jtools::make_predictions()]. With an
#' interactions version supporting `plot.scale`, the default `plot.scale = "within"`
#' displays numeric within-person predictors as deviations from each person's
#' observed mean, in original units even for standardized fits. Zero means at
#' the person's mean. Default predictor ranges use estimation-sample deviations;
#' numeric within-person moderators default to zero and plus/minus one pooled
#' within-person SD (or their observed values when there are only two).
#' Constant predictors/moderators retain their original units or categories.
#' The outcome remains on its modeled response scale, not a within-person
#' deviation. `outcome.scale = "link"` instead uses the model's link scale.
#'
#' Use `plot.scale = "original"` for raw predictor levels. Plot captions report
#' reference person means, and values implying deviations outside the observed
#' within-person range trigger warnings. These marginal range checks do not
#' establish joint support or rule out other forms of extrapolation. User-supplied
#' numeric predictor/moderator values and `at` use the selected display scale;
#' `imean(...)` overrides always use original units. Custom labels are preserved.
#' Plot `data` must be the original fitting data or a subset with panel
#' identifiers; excluded estimation rows are dropped and subsets retain fitted
#' person means. Use `at` for hypothetical values, not modified plotting data.
#'
#' Person means and interaction-product means are held at their observation-weighted averages
#' over the estimation sample, while focal predictors vary. This describes
#' conditional predictions, not population-average effects or forecasts for a
#' particular entity. Use `at = list("imean(x)" = value)` (and, when needed,
#' `"imean(x:z)"`) to set different reference means. Time-invariant moderators
#' multiply the centered within-person term; their product means are not
#' independently held fixed as those moderators change. For a within-varying factor,
#' its reference means are the means of its indicator columns.
#' Direct calls to `jtools::make_predictions()` (and older interactions versions
#' without `plot.scale`) retain original-scale inputs. `plot.scale` is a plotting
#' option, not an argument to `predict()` or an estimation option.
#'
#' Automatic plotting grids currently support scalar numeric/factor terms,
#' including two- and three-way interactions. Scalar transformations such as
#' `log(x)` require `plot.scale = "original"`; a deviation of `log(x)` is not
#' a deviation of `x`. Detrended, lagged/lead, and
#' basis-expansion models require complete panel histories for prediction or
#' explicitly prepared rows via `make_predictions(..., new_data = ..., raw = TRUE)`.
#' Partial residuals, custom plotting covariance estimates, and prediction
#' intervals are not supported. Updated interactions versions provide S3
#' methods for `sim_slopes()`, `johnson_neyman()`, and `probe_interaction()`.
#' Numeric focal predictors use exact design-matrix contrasts, not refitting or
#' recentering. Slopes share the plots' units and reference settings. Tests are
#' fixed-effect and link-scale: residual-df t approximations for Gaussian mixed
#' models, normal approximations for generalized models and GEE, with an explicit
#' `df` override. They are not Satterthwaite/Kenward-Roger tests. Nonlinear/factor
#' focal predictors, predictor-dependent offsets, rank-deficient fits, custom
#' covariance estimates, response-scale derivatives for non-identity links,
#' and FDR-adjusted Johnson-Neyman inference are not supported. These plotting
#' and slope conventions do not change the fitted estimator.
#' Models saved by older panelr versions must be refitted to use the new
#' panel-history and automatic-grid paths; prepared-row predictions remain available.
#'
#' Analytic `se.fit` values describe fixed-effect uncertainty, conditional on
#' the preprocessing/reference quantities; response-scale standard errors use
#' the delta method. Plot bands are confidence intervals, not intervals for
#' individual outcomes. `use.re.var = TRUE` is rejected because the previous
#' variance-addition calculation did not yield valid prediction intervals.
#'
#' @param raw Is `newdata` a `merMod` model frame or `panel_data`? TRUE
#'  indicates a `merMod`-style newdata, with all of the extra columns 
#'  created by `wbm`. 
#' @param se.fit Return a list containing predictions and fixed-effect standard
#'   errors? With `boot = TRUE`, returns bootstrap draws in `fit` instead.
#' @param use.re.var Unsupported; must be `FALSE`. Random-effect variance is not
#'   added to fixed-effect standard errors.
#' @importFrom stats predict na.pass
#' @inheritParams jtools::predict_merMod
#' @inheritParams lme4::simulate.merMod
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks, data = wages)
#' # With no newdata, predictions use the fitted design.
#' predict(model)
#' @export
#' @rdname predict.wbm 
predict.wbm <- function(object, newdata = NULL, se.fit = FALSE,
                        raw = FALSE, use.re.var = FALSE,
                        re.form = NULL, type = c("link", "response"), 
                        allow.new.levels = TRUE, na.action = na.pass, ...) {
  type <- match.arg(type)
  
  # Need to re-process data when raw is FALSE
  if (!is.null(newdata) && raw == FALSE) {
    newdata <- process_nonraw_newdata(object, newdata, re.form)
  }
  
  if (raw == TRUE && !is.null(newdata)) {
    newdata <- process_raw_newdata(object, newdata)
  }
  
  if (is.null(attr(attr(object@frame, "terms"), "varnames.fixed"))) {
    attr(attr(object@frame, "terms"), "varnames.fixed") <-
      c(object@call_info$varying, object@call_info$constants,
        object@call_info$meanvars)
  }
  
  predict_wb_mermod(object, newdata = newdata, se.fit = se.fit,
          re.form = re.form, type = type[1], use.re.var = use.re.var,
          allow.new.levels = allow.new.levels, na.action = na.action, ...)
  
}


#' @title Predictions and simulations from within-between GEE models
#' @description These methods facilitate fairly straightforward predictions
#'  from `wbgee` models.
#' @param raw Is `newdata` a `geeglm` model frame or `panel_data`? TRUE
#'  indicates a `geeglm`-style newdata, with all of the extra columns 
#'  created by `wbgee`. 
#' @details Panel histories and prepared rows follow the same conventions as
#'   [predict.wbm()]. Interaction plotting uses fixed estimation-sample reference
#'   means and the fitted GEE covariance matrix for confidence intervals.
#' @importFrom stats predict na.pass
#' @inheritParams stats::predict.lm
#' @examples 
#' if (requireNamespace("geepack")) {
#'   data("WageData")
#'   wages <- panel_data(WageData, id = id, wave = t)
#'   model <- wbgee(lwage ~ lag(union) + wks, data = wages)
#'   # By default, assumes you're using the processed data for newdata
#'   predict(model)
#' }
#' @export
#' @rdname predict.wbgee


predict.wbgee <- function(object, newdata = NULL, type = c("link", "response"),
                          se.fit = FALSE, raw = FALSE, ...) {

  type <- match.arg(type)

  if (!inherits(object, "wbgee")) {
    stop("Object must be of class 'wbgee'")
  }

  # Process newdata
  if (!is.null(newdata)) {
    if (raw == FALSE) {
      # Use existing function to process non-raw newdata
      newdata <- process_nonraw_newdata(object, newdata, re.form = NULL)
    } else {
      # Use existing function to process raw newdata
      newdata <- process_raw_newdata(object, newdata)
    }
  } else {
    # If newdata is NULL, use the model's original data
    newdata <- object$frame
  }

  # Use predict_gee to obtain predictions (and standard errors if se.fit = TRUE)
  pred_results <- predict_gee(model = object, newdata = newdata, se.fit = se.fit,
                              type = type, ...)

  if (se.fit) {
    return(pred_results)
  } else {
    return(pred_results$fit)
  }
}

#' @importFrom stats simulate na.pass 
#' @rdname predict.wbm 
#' @export

simulate.wbm <- function(object, nsim = 1, seed = NULL, use.u = FALSE,
                         newdata = NULL, raw = FALSE,
                         newparams = NULL, re.form = NA,
                         type = c("link", "response"),
                         allow.new.levels = FALSE, na.action = na.pass, ...) {
  
  if (!is.null(newdata)) {
    newdata <- if (raw) process_raw_newdata(object, newdata) else
      process_nonraw_newdata(object, newdata, re.form)
  }
  
  if (is.null(attr(attr(object@frame, "terms"), "varnames.fixed"))) {
    attr(attr(object@frame, "terms"), "varnames.fixed") <-
      c(object@call_info$varying, object@call_info$constants,
        object@call_info$meanvars)
  }

  object <- wb_prediction_backend(object, newdata)
  
  if (!identical(re.form, NA)) {
    ## unused terms and type arguments throw warning
    simulate(object, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, re.form = re.form,
             ## terms = NULL, type = type,
             allow.new.levels = allow.new.levels,
             na.action = na.action, ...)
  } else {
    simulate(object, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, use.u = use.u,
             ## terms = NULL, type = type,
             allow.new.levels = allow.new.levels,
             na.action = na.action, ...)
  }
  
}

#' @title Number of observations used in `wbm` models
#' @description This S3 method allows you to retrieve either the number of
#'  observations or number of entities in the data used to fit `wbm` objects.
#' @inheritParams stats::nobs
#' @param entities Should `nobs` return the number of entities in the panel
#'  or the number of rows in the `panel_data` frame? Default is TRUE, returning
#'  the number of entities.
#' @importFrom stats nobs
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks, data = wages)
#' nobs(model)
#' @export

nobs.wbm <- function(object, entities = TRUE, ...) {
  if (entities == TRUE) {
    dplyr::n_groups(panel_data(object@frame, id = !! object@call_info$id,
                               wave = !! object@call_info$wave))
  } else {
    nrow(object@frame)
  }
}

#' @export
df.residual.wbm <- function(object, ...) {
  nobs(object, entities = FALSE) - npar.wbm(object)
}

npar.wbm <- function(object) {
  n <- length(object@beta) + length(object@theta) + 
    object@devcomp[["dims"]][["useSc"]]
  if (grepl("Negative Binomial", family(object)$family)) {
    n <- n + 1
  }
  return(n)
}

#' @title Retrieve model formulas from `wbm` objects
#' @description This S3 method allows you to retrieve the formula used to 
#'  fit `wbm` objects.
#' @inheritParams stats::formula
#' @param x A `wbm` model.
#' @param raw Return the formula used in the call to `lmerMod`/`glmerMod`?
#'  Default is FALSE.
#' @importFrom stats formula
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks, data = wages)
#' # Returns the original model formula rather than the one sent to lme4
#' formula(model)
#' @export

formula.wbm <- function(x, raw = FALSE, ...) {
  if (raw == TRUE) {
    return(attr(x@frame, "formula"))
  } else {
    if (!is.null(x@call_info$prediction$formula)) return(x@call_info$prediction$formula)
    if (!is.null(x@call_info$pf$raw_formula)) return(x@call_info$pf$raw_formula)
    return(Formula::Formula(as.formula(getCall(x)$formula)))
  }
}

#' @export

formula.wbgee <- function(x, raw = FALSE, ...) {
  if (raw == TRUE) {
    return(x$formula)
  } else {
    if (!is.null(x$call_info$prediction$formula)) return(x$call_info$prediction$formula)
    if (!is.null(x$call_info$pf$raw_formula)) return(x$call_info$pf$raw_formula)
    return(Formula::Formula(as.formula(getCall(x)$formula)))
  }
}

#' @export
#' @importFrom stats terms

terms.wbm <- function(x, fixed.only = FALSE, random.only = FALSE, ...) {
    x <- to_merMod(x)
    terms(x, fixed.only = fixed.only, random.only = random.only, ...)
}

#' @importFrom jtools make_predictions
#' @importFrom stats quantile df.residual qnorm model.offset
#' @export

make_predictions.wbm <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE,  
  int.type = c("confidence", "prediction"), int.width = .95, 
  outcome.scale = "response", re.form = ~0, add.re.variance = FALSE, 
  boot = FALSE, sims = 1000, progress = "txt", set.offset = NULL, 
  new_data = NULL, return.orig.data = FALSE, partial.residuals = FALSE, 
  message = TRUE, raw = FALSE, robust = FALSE, cluster = NULL, vcov = NULL, ...) {

  if (!identical(robust, FALSE) || !is.null(cluster) || !is.null(vcov)) {
    stop_wrap("Custom/robust covariance estimates are not supported by wbm prediction grids.")
  }
  if (match.arg(int.type) != "confidence") stop_wrap("Only confidence intervals are supported.")
  if (partial.residuals) stop_wrap("Partial residuals are not supported for wbm prediction grids.")

  # Check if user provided own new_data
  if (is.null(new_data)) {
    data <- wb_grid_data(model, data)
    # Get the data ready with make_new_data()
    pm <- jtools::make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}

  prediction_data <- prepare_wb_plot_grid(model, pm, raw)

  resp <- jtools::get_response_name(model)
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  if (interval == TRUE && boot == FALSE && message == TRUE) {
    msg_wrap("Confidence intervals reflect fixed-effect uncertainty only,
              conditional on the reference means; they are not prediction intervals.")
  }
  
  # Do the predictions using built-in prediction method if robust is FALSE
  if (interval == FALSE && is.null(model.offset(model.frame(model)))) {
    predicted <- as.data.frame(predict(model, newdata = prediction_data,
                                       type = link_or_lm,
                                       re.form = re.form,
                                       allow.new.levels = FALSE,
                                       raw = TRUE))
    
    pm[[get_response_name(model)]] <- predicted[[1]]
    
  } else { # Use my custom predictions function
    
    if (interactive() & boot == TRUE & progress != "none") {
      cat("Bootstrap progress:\n")
    }
    predicted <- predict(model, newdata = prediction_data, use.re.var = add.re.variance,
                         se.fit = TRUE, allow.new.levels = TRUE, 
                         type = link_or_lm, re.form = re.form,
                         boot = boot, sims = sims, prog.arg = progress, 
                         raw = TRUE, ...)
    
    if (boot == TRUE) {
      raw_boot <- predicted$fit
      
      ## Convert the confidence percentile to a number of S.E. to multiply by
      intw <- 1 - ((1 - int.width) / 2)
      # Set the predicted values at the median
      fit <- sapply(as.data.frame(raw_boot), median)
      upper <- sapply(as.data.frame(raw_boot), quantile, probs = intw)
      lower <- sapply(as.data.frame(raw_boot), quantile, probs = 1 - intw)
      
      # Add to predicted frame
      pm[[resp]] <- fit
      pm[["ymax"]] <- upper
      pm[["ymin"]] <- lower
      
      # Drop the cases that should be missing if I had done it piecewise
      pm <- pm[complete.cases(pm),]
    } else {
      ## Convert the confidence percentile to a number of S.E. to multiply by
      intw <- 1 - ((1 - int.width)/2)
      ## Try to get the residual degrees of freedom to get the critical value
      r.df <- try({
        if (isLMM(model)) df.residual(model) else Inf
      }, silent = TRUE)
      if (is.numeric(r.df)) {
        ses <- qt(intw, r.df)
      } else {
        message(wrap_str("Could not find residual degrees of freedom for this
                       model. Using confidence intervals based on normal
                       distribution instead."))
        ses <- qnorm(intw, 0, 1)
      }
      pm[[get_response_name(model)]] <- predicted[[1]]
      pm[["ymax"]] <-
        pm[[get_response_name(model)]] + (predicted[["se.fit"]]) * ses
      pm[["ymin"]] <-
        pm[[get_response_name(model)]] - (predicted[["se.fit"]]) * ses
    }
  }
  
  
  # Back-convert the predictions to the response scale
  if (outcome.scale == "response") {
    pm[[resp]] <- family(model)$linkinv(pm[[resp]])
    if (interval == TRUE) {
      pm[["ymax"]] <- family(model)$linkinv(pm[["ymax"]])
      pm[["ymin"]] <- family(model)$linkinv(pm[["ymin"]])
    }
  }
  
  if (return.orig.data == FALSE & partial.residuals == FALSE) {
    o <- tibble::as_tibble(pm)
  } else {
    if (return.orig.data == TRUE & partial.residuals == FALSE) {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                  suppressMessages(d <- tibble::as_tibble(get_data(model))))
      # If left-hand side is transformed, make new column in original data for
      # the transformed version and evaluate it
      if (is_lhs_transformed(as.formula(formula(model)))) {
        o[[2]][get_response_name(model)] <- 
          eval(get_lhs_j(as.formula(formula(model))), o[[2]])
      }
    } else {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                  suppressMessages(
                    partialize(model, vars = pred, at = at, data = data,
                               center = center, set.offset = set.offset)
                  )
      )
    }
  }
  return(o)
}

#' @export 
partialize.wbm <- function(model, vars = NULL, data = NULL, at = NULL,
                          center = TRUE, scale = c("response", "link"),
                          set.offset = 1, ...) {
  stop_wrap("Partial residuals are not supported for within-between prediction grids.")
}

#' @export
partialize.wbgee <- partialize.wbm

#' @importFrom jtools make_predictions
#' @export
make_predictions.wbgee <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE,
  int.type = c("confidence", "prediction"), int.width = .95, 
  outcome.scale = "response", set.offset = NULL, 
  new_data = NULL, return.orig.data = FALSE, partial.residuals = FALSE,
  message = TRUE, raw = FALSE, robust = FALSE, cluster = NULL, vcov = NULL, ...) {
  if (!identical(robust, FALSE) || !is.null(cluster) || !is.null(vcov)) {
    stop_wrap("Custom covariance estimates are not supported by wbgee prediction grids.")
  }
  if (match.arg(int.type) != "confidence") stop_wrap("Only confidence intervals are supported.")
  if (partial.residuals) stop_wrap("Partial residuals are not supported for wbgee prediction grids.")
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    data <- wb_grid_data(model, data)
    # Get the data ready with make_new_data()
    pm <- jtools::make_new_data(model, pred, pred.values = pred.values, at = at, 
                                data = data, center = center,
                                set.offset = set.offset)
  } else {pm <- new_data}

  prediction_data <- prepare_wb_plot_grid(model, pm, raw)
  
  resp <- jtools::get_response_name(model)
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")

  predicted <- predict(model, newdata = prediction_data, type = link_or_lm,
                       se.fit = TRUE, raw = TRUE)
    
  pm[[get_response_name(model)]] <- predicted[[1]]
  if (interval) {
    critical <- qnorm(1 - (1 - int.width) / 2)
    pm[["ymin"]] <- predicted$fit - critical * predicted$se.fit
    pm[["ymax"]] <- predicted$fit + critical * predicted$se.fit
  }
  
  
  # Back-convert the predictions to the response scale
  if (outcome.scale == "response") {
    pm[[resp]] <- family(model)$linkinv(pm[[resp]])
    if (interval == TRUE) {
      pm[["ymax"]] <- family(model)$linkinv(pm[["ymax"]])
      pm[["ymin"]] <- family(model)$linkinv(pm[["ymin"]])
    }
  }
  
  if (return.orig.data == FALSE && partial.residuals == FALSE) {
    o <- tibble::as_tibble(pm)
  } else {
    if (return.orig.data == TRUE & partial.residuals == FALSE) {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                  suppressMessages(d <- tibble::as_tibble(get_data(model))))
      # If left-hand side is transformed, make new column in original data for
      # the transformed version and evaluate it
      if (is_lhs_transformed(as.formula(formula(model)))) {
        o[[2]][get_response_name(model)] <- 
          eval(get_lhs_j(as.formula(formula(model))), o[[2]])
      }
    } else {
      o <- list(predictions = tibble::as_tibble(pm), data = 
                  suppressMessages(
                    partialize(model, vars = pred, at = at, data = data,
                               center = center, set.offset = set.offset)
                  )
      )
    }
  }
  return(o)
}

#### jtools helpers ##########################################################
is_lhs_transformed <- function(x) {
  final <- as.character(deparse(get_lhs_j(x)))
  bare_vars <- all.vars(get_lhs_j(x))
  any(final != bare_vars)
}

get_lhs_j <- function(x) {
  if (two_sided(x) == TRUE) {
    x[[2]] 
  } else if(one_sided(x)) {
    NULL   
  } else {
    stop_wrap(x, "does not appear to be a one- or two-sided formula.")
  }
}

one_sided <- function(x, ...) {
  # Handle Formula objects
  if (inherits(x, "Formula")) {
    x <- formula(x)
  }
  
  # Basic formula check
  if (inherits(x, "formula")) {
    return(length(x) == 2)
  }
  
  operators <- c("::", ":::", "@", "$", "[", "[[", ":", "+", "-", "*", "/", "^",
                "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "%in%", "%!in%",
                "!", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", "%*%",
                "%x%", "%o%", "%>%", "%<>%", "%T>%")
  
  isTRUE(is.call(x) && 
         is.name(x[[1]]) && 
         deparse1(x[[1]]) %in% operators && 
         length(x) == 2)
}

two_sided <- function(x, ...) {
  
  # Handle Formula objects
  if (inherits(x, "Formula")) {
    # Extract just the main formula part for wbgee
    x <- formula(x)
  }
  
  # Basic formula check
  if (inherits(x, "formula")) {
    result <- length(x) > 2
    return(result)
  }
  
  # For other cases
  operators <- c("::", ":::", "@", "$", "[", "[[", ":", "+", "-", "*", "/", "^",
                "%%", "%/%", "<", "<=", ">", ">=", "==", "!=", "%in%", "%!in%",
                "!", "&", "&&", "|", "||", "~", "<-", "<<-", "=", "?", "%*%",
                "%x%", "%o%", "%>%", "%<>%", "%T>%")
  
  result <- isTRUE(is.call(x) && 
                   is.name(x[[1]]) && 
                   deparse1(x[[1]]) %in% operators && 
                   length(x) == 3)
  
  result
}

#' @export
get_formula.wblm <- function(model, ...) {
  get_formula.wbm(model, ...)
}

#' @export
get_formula.wbm <- function(model, ...) {
  reformulas::nobars(formula(formula(model), collapse = TRUE))
}

#' @export
get_formula.wbgee <- function(model, ...) {
  reformulas::nobars(formula(formula(model), collapse = TRUE))
}

need_package <- function(x) {
  stop_wrap("You must have '", x, "' installed to use this function.")
}

when <- function(., ...) {
    dots <- list(...)
    names <- names(dots)
    named <- if (is.null(names)) 
        rep(FALSE, length(dots))
    else names != ""
    if (sum(!named) == 0) 
        stop_wrap("At least one matching condition is needed.")
    is_formula <- vapply(dots, function(dot) identical(class(dot), 
        "formula"), logical(1L))
    env <- new.env(parent = parent.frame())
    env[["."]] <- .
    if (sum(named) > 0) 
        for (i in which(named)) env[[names[i]]] <- dots[[i]]
    result <- NULL
    for (i in which(!named)) {
        if (is_formula[i]) {
            action <- length(dots[[i]])
            if (action == 2 || is_true(eval(dots[[i]][[2]], env, 
                env))) {
                result <- eval(dots[[i]][[action]], env, env)
                break
            }
        } else {
            result <- dots[[i]]
        }
    }
    result
}
