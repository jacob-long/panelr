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

to_merMod <- function(x) {
  if (isLMM(x)) {
    x <- as(x, "lmerMod")
  } else {
    x <- as(x, "glmerMod")
  }
  x@frame <- as.data.frame(x@frame)
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
#' @param raw Is `newdata` a `merMod` model frame or `panel_data`? TRUE
#'  indicates a `merMod`-style newdata, with all of the extra columns 
#'  created by `wbm`. 
#' @importFrom stats predict na.pass
#' @inheritParams jtools::predict_merMod
#' @inheritParams lme4::simulate.merMod
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' model <- wbm(lwage ~ lag(union) + wks, data = wages)
#' # By default, assumes you're using the processed data for newdata
#' predict(model)
#' @export
#' @rdname predict.wbm 
predict.wbm <- function(object, newdata = NULL, se.fit = FALSE,
                        raw = FALSE, use.re.var = FALSE,
                        re.form = NULL, type = c("link", "response"), 
                        allow.new.levels = TRUE, na.action = na.pass, ...) {
  
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
  
  if (isLMM(object)) {
    object <- as(object, "lmerMod")
  } else {
    object <- as(object, "glmerMod")
  }

  jtools::predict_merMod(object, newdata = newdata, se.fit = se.fit, 
          re.form = re.form, type = type[1], use.re.var = use.re.var,
          allow.new.levels = allow.new.levels, na.action = na.action, ...)
  
}

process_nonraw_newdata <- function(object, newdata, re.form = NULL) {
  # Determine if the object is an S4 wbm object or S3 wbgee object
  if (inherits(object, "wbm")) {
    # For wbm (S4 object)
    orig_data <- object@orig_data
    call_info <- object@call_info
    formula_obj <- formula(object)
  } else if (inherits(object, "wbgee")) {
    # For wbgee (S3 object)
    orig_data <- object$orig_data
    call_info <- object$call_info
    formula_obj <- formula(object)
  } else {
    stop("Unsupported model object type.")
  }

  # Extract id and wave variables from the model object
  id <- get_id(orig_data)
  wave <- get_wave(orig_data)
  pf_allvars <- call_info$pf$allvars
  
  # Ensure newdata has the required variables
  if (!is_panel(newdata)) {
    if (inherits(object, "wbm")) {
      # Need valid ID variable if using random effects
      if (is.null(re.form) || to_char(re.form) != "~0") {
        if (id %nin% names(newdata)) {
          stop_wrap("newdata must contain the '", id, "' variable unless
                    're.form' is set to ~0.")
        }
      } else if (id %nin% names(newdata)) {
        # Otherwise ID can be anything, just need the column there for
        # valid panel_data object
        newdata[[id]] <- 1
      }
      # Need user to provide wave if it's part of the model
      if (wave %nin% names(newdata) & wave %in% pf_allvars) {
        stop_wrap("newdata must contain the '", wave, "' variable unless
                    re.form = ~0.")
      } else if (wave %nin% names(newdata)) {
        # Otherwise it can be anything
        newdata[[wave]] <- 1:nrow(newdata)
      }
    } else if (inherits(object, "wbgee")) {
      if (id %nin% names(newdata)) {
        stop_wrap("newdata must contain the '", id, "' variable.")
      }
      # Need user to provide wave if it's part of the model
      if (wave %nin% names(newdata) & wave %in% pf_allvars) {
        stop_wrap("newdata must contain the '", wave, "' variable.")
      } else if (wave %nin% names(newdata)) {
        # Otherwise it can be anything
        newdata[[wave]] <- 1:nrow(newdata)
      }
    }
    
    # Coerce newdata to panel_data
    newdata <- panel_data(newdata, id = !!sym(id), wave = !!sym(wave))
  }
  
  # Get the formula and dv from the model
  dv <- call_info$dv
  # Parse the formula using wb_formula_parser()
  pf <- wb_formula_parser(formula_obj, dv, newdata, force.constants = FALSE)
  newdata <- pf$data
  
  # Use model_frame to do variable transformations
  mf_form <- call_info$mf_form
  newdata <- model_frame(mf_form, newdata)
  
  # Process interactions and other transformations using wb_model()
  interaction.style <- call_info$interaction.style
  wb_model_result <- wb_model(
    model = call_info$model,
    pf = pf,
    dv = dv,
    data = newdata,
    detrend = call_info$detrend,
    demean.ints = interaction.style == "double-demean",
    old.ints = interaction.style == "demean"
  )
  newdata <- wb_model_result$data
  
  # Handle detrending if applicable
  if (call_info$detrend == TRUE) {
    newdata <- detrend(
      data = newdata,
      pf = pf,
      dt_order = call_info$dt_order,
      balance_correction = call_info$balance_correction,
      dt_random = call_info$dt_random
    )
  }
  
  return(newdata)
}

process_raw_newdata <- function(object, newdata) {
  # Determine if the object is an S4 wbm object or S3 wbgee object
  if (inherits(object, "wbm")) {
    # For wbm (S4 object)
    frame <- object@frame
    call_info <- object@call_info
  } else if (inherits(object, "wbgee")) {
    # For wbgee (S3 object)
    frame <- object$frame
    call_info <- object$call_info
  } else {
    stop("Unsupported model object type.")
  }

  ints <- call_info$interactions
  if (!is.null(ints)) {
    # Adjust interaction terms for processing
    ints <- gsub(":", "*", ints)
    ints <- gsub("(^.*)(?=\\*)", "`\\1`", ints, perl = TRUE)
    ints <- gsub("(?<=\\*)(.*$)", "`\\1`", ints, perl = TRUE)
    
    # Determine if double-demeaning interactions is necessary
    interaction.style <- call_info$interaction.style
    model_type <- call_info$model
    demean <- interaction.style == "double-demean"
    if (model_type %in% c("between", "contextual", "random")) {
      demean <- FALSE
    }
    
    # Process interactions
    p <- process_interactions(ints, data = newdata, demean.ints = demean)
    newdata <- p$data
  }
  return(newdata)
}

#' @title Predictions and simulations from within-between GEE models
#' @description These methods facilitate fairly straightforward predictions
#'  from `wbgee` models.
#' @param raw Is `newdata` a `geeglm` model frame or `panel_data`? TRUE
#'  indicates a `geeglm`-style newdata, with all of the extra columns 
#'  created by `wbgee`. 
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

# predict.wbgee <- function(object, newdata = NULL, se.fit = FALSE,
#                           raw = FALSE, type = c("link", "response"), ...) {
  
#   if (!is.null(newdata) & raw == FALSE) {
#     if (!is_panel(newdata)) {
#       id <- get_id(object$orig_data)
#       wave <- get_wave(object$orig_data)
#       if (id %nin% names(newdata)) {
#         # otherwise ID can be anything, just need the column there for 
#         # valid panel_data object
#         newdata[[id]] <- 1
#       }
#       # Need user to provide wave if it's part of the model
#       if ((wave %nin% names(newdata) & wave %in% object$call_info$pf$allvars)) {
#         stop_wrap("newdata must contain the ", wave, " variable.")
#       } else if (wave %nin% names(newdata)) {
#         # Otherwise it can be anything
#         newdata[[wave]] <- 1:nrow(newdata)
#       }
      
#       newdata <- panel_data(newdata, id = !! sym(id), wave = !! sym(wave))
#     }
    
#     # Get the formula originally used to pass to model_frame
#     mf_form <- object$call_info$mf_form
#     dv <- object$call_info$dv
#     pf <- wb_formula_parser(formula(object), dv, newdata, force.constants = FALSE)
#     newdata <- pf$data
#     # Use model_frame to do variable transformations
#     newdata <- model_frame(mf_form, newdata)
    
#     interaction.style <- object$call_info$interaction.style
#     newdata <- wb_model(object$call_info$model, pf, dv, newdata,
#                         object$call_info$detrend, 
#                         demean.ints = interaction.style == "double-demean",
#                         old.ints = interaction.style == "demean")$data
#     if (object$call_info$detrend == TRUE) {
#       dto <- detrend(newdata, pf, object$call_info$dt_order,
#                      object$call_info$balance_correction,
#                      object$call_info$dt_random)
#       newdata <- dto
#     }
#   }
  
#   if (raw == TRUE & !is.null(newdata)) {
#     ints <- attr(object$frame, "interactions")
#     if (!is.null(ints)) {
#       ints <- gsub(":", "*", ints)
#       ints <- gsub("(^.*)(?=\\*)", "`\\1`", ints, perl = TRUE)
#       ints <- gsub("(?<=\\*)(.*$)", "`\\1`", ints, perl = TRUE)
#       demean <- attr(object$frame, "interaction.style") == "double-demean"
#       if (object$call_info$model %in% c("between", "contextual", "random")) {
#         demean <- FALSE
#       }
#       p <- process_interactions(ints, data = newdata, demean.ints = demean)
#       newdata <- p$data
#     }
#   }
  
#   # class(object) <- c("geeglm", "gee", "glm", "lm")
#   predict_gee(object, newdata = newdata, se.fit = se.fit, type = type[1], ...)
  
# }

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
    newdata <- model.frame(object)
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
  
  if (!is.null(newdata) & raw == FALSE) {
    mf_form <- object@call_info$mf_form
    pf <- object@call_info$pf
    newdata <- model_frame(mf_form, newdata)
    dv <- object@call_info$dv
    
    if (object@call_info$detrend == TRUE) {
      dto <- detrend(newdata, pf, object@call_info$dt_order,
                     object@call_info$balance_correction,
                     object@call_info$dt_random)
      newdata <- dto
    }

    newdata <- wb_model(object@call_info$model, pf, dv, newdata,
                        object@call_info$detrend)$data
  }
  
  if (is.null(attr(attr(object@frame, "terms"), "varnames.fixed"))) {
    attr(attr(object@frame, "terms"), "varnames.fixed") <-
      c(object@call_info$varying, object@call_info$constants,
        object@call_info$meanvars)
  }

  object <- to_merMod(object)
  
  if (!is.na(re.form)) {
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
    return(x@call_info$merMod_call$formula)
  } else {
    return(Formula::Formula(as.formula(getCall(x)$formula)))
  }
}

#' @export

formula.wbgee <- function(x, raw = FALSE, ...) {
  if (raw == TRUE) {
    return(x$formula)
  } else {
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
  message = TRUE, raw = TRUE, ...) {

  # Check if user provided own new_data
  if (is.null(new_data)) {
    meanvars <- model@call_info$meanvars
    if (!is.null(meanvars)) {
      meanvarlist <- lapply(data[meanvars], mean, na.rm = TRUE)
      at <- c(at, meanvarlist)
    }
    # Get the data ready with make_new_data()
    pm <- jtools::make_new_data(model, pred, pred.values = pred.values, at = at, 
                        data = data, center = center, set.offset = set.offset)
  } else {pm <- new_data}

  resp <- jtools::get_response_name(model)
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")
  
  if (interval == TRUE && boot == FALSE && message == TRUE) {
    msg_wrap("Confidence intervals for wbm models is an experimental
              feature. The intervals reflect only the variance of the
              fixed effects, not the random effects.")
  }
  
  # Do the predictions using built-in prediction method if robust is FALSE
  if (interval == FALSE && is.null(model.offset(model.frame(model)))) {
    predicted <- as.data.frame(predict(model, newdata = pm,
                                       type = link_or_lm,
                                       re.form = re.form,
                                       allow.new.levels = FALSE,
                                       raw = raw))
    
    pm[[get_response_name(model)]] <- predicted[[1]]
    
  } else { # Use my custom predictions function
    
    if (interactive() & boot == TRUE & progress != "none") {
      cat("Bootstrap progress:\n")
    }
    predicted <- predict(model, newdata = pm, use.re.var = add.re.variance,
                         se.fit = TRUE, allow.new.levels = TRUE, 
                         type = link_or_lm, re.form = re.form,
                         boot = boot, sims = sims, prog.arg = progress, 
                         raw = raw, ...)
    
    if (boot == TRUE) {
      raw_boot <- predicted
      
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
        df.residual(model)
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
  # Get the original data if new data are not provided
  if (is.null(data)) {
    data <- model@frame
  }
  if (isLMM(model)) {
    model <- as(model, "lmerMod")
  } else {
    model <- as(model, "glmerMod")
  }
  partialize(model, vars = vars, data = data, at = at, 
             center = center, scale = scale, set.offset = set.offset, ...)
}

#' @importFrom jtools make_predictions
#' @export
make_predictions.wbgee <- function(model, pred, pred.values = NULL, at = NULL,
  data = NULL, center = TRUE, interval = TRUE,
  int.type = c("confidence", "prediction"), int.width = .95, 
  outcome.scale = "response", set.offset = NULL, 
  new_data = NULL, return.orig.data = FALSE, partial.residuals = FALSE,
  message = TRUE, raw = TRUE, ...) {
  
  # Check if user provided own new_data
  if (is.null(new_data)) {
    # Get the data ready with make_new_data()
    pm <- jtools::make_new_data(model, pred, pred.values = pred.values, at = at, 
                                data = data, center = center,
                                set.offset = set.offset)
  } else {pm <- new_data}
  
  resp <- jtools::get_response_name(model)
  link_or_lm <- ifelse(family(model)$link == "identity",
                       yes = "response", no = "link")

  predicted <- as.data.frame(predict(model, newdata = pm, type = link_or_lm,
                                     raw = raw))
    
  pm[[get_response_name(model)]] <- predicted[[1]]
  
  
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
    if (debug) {
      cat("After Formula conversion:\n")
      print(x)
    }
  }
  
  # Basic formula check
  if (inherits(x, "formula")) {
    result <- length(x) > 2
    if (debug) {
      cat("Formula length:", length(x), "\n")
      cat("Returning:", result, "\n")
    }
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
  formula(formula(model))
}

#' @export
get_formula.wbgee <- function(model, ...) {
  stats::formula(formula(model))
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
