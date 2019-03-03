#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr "%<>%"
#' @export
magrittr::`%<>%`

backtick_name <- function(x) {
  if (make.names(x) != x) {paste0("`", x, "`")} else {x}
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
#' @inheritParams lme4::predict.merMod
#' @inheritParams lme4::simulate.merMod
#' @export
#' @rdname predict.wbm 

predict.wbm <- function(object, newdata = NULL, raw = FALSE, newparams = NULL,
        re.form = NULL, terms = NULL, type = c("link", "response"),
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

  if (isLMM(object)) {
    object <- as(object, "lmerMod")
  } else {
    object <- as(object, "glmerMod")
  }
  
  predict(object, newdata = newdata, newparams = newparams,
          re.form = re.form, terms = terms, type = type,
          allow.new.levels = allow.new.levels, na.action = na.action, ...)
  
}

#' @importFrom stats simulate na.pass 
#' @rdname predict.wbm 
#' @export

simulate.wbm <- function(object, nsim = 1, seed = NULL, use.u = FALSE,
                         newdata = NULL, raw = FALSE,
                         newparams = NULL, re.form = NA, terms = NULL,
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
    simulate(object, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, re.form = re.form,
             terms = terms, type = type, allow.new.levels = allow.new.levels,
             na.action = na.action, ...)
  } else {
    simulate(object, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, use.u = use.u,
             terms = terms, type = type, allow.new.levels = allow.new.levels,
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
#' @export

nobs.wbm <- function(object, entities = TRUE, ...) {
  if (entities == TRUE) {
    dplyr::n_groups(object@frame)
  } else {
    nrow(object@frame)
  }
}

#' @title Retrieve model formulas from `wbm` objects
#' @description This S3 method allows you to retrieve the formula used to 
#'  fit `wbm` objects.
#' @inheritParams stats::formula
#' @param raw Return the formula used in the call to `lmerMod`/`glmerMod`?
#'  Default is FALSE.
#' @importFrom stats formula
#' @export

formula.wbm <- function(x, raw = FALSE, ...) {
  if (raw == TRUE) {
    return(x@call_info$merMod_call$formula)
  } else {
    return(Formula::Formula(as.formula(getCall(x)$formula)))
  }
}

#' @export
#' @importFrom stats terms

terms.wbm <- function(x, fixed.only = TRUE, random.only = FALSE, ...) {
    x <- to_merMod(x)
    terms(x, fixed.only = fixed.only, random.only = random.only, ...)
}

#' @importFrom jtools make_predictions
#' @export

make_predictions.wbm <- function(model, ...) {
  model <- to_merMod(model)
  NextMethod("make_predictions", model)
}
