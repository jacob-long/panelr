#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr "%<>%"
#' @export
magrittr::`%<>%`

#' @importFrom stats getCall
#' @export

getCall.wbm <- function(x, ...) {
  
  return(x@call)
  
}

#' @importFrom stats predict na.pass
#' @export

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

  if (isLMM(object)) {
    object <- as(object, "lmerMod")
  } else {
    object <- as(object, "glmerMod")
  }
  
  if (!is.na(re.form)) {
    simulate(object$model, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, re.form = re.form,
             terms = terms, type = type, allow.new.levels = allow.new.levels,
             na.action = na.action, ...)
  } else {
    simulate(object$model, nsim = nsim, seed = seed,
             newdata = newdata, newparams = newparams, use.u = use.u,
             terms = terms, type = type, allow.new.levels = allow.new.levels,
             na.action = na.action, ...)
  }
  
}

#' @importFrom stats nobs
#' @export

nobs.wbm <- function(object, entities = TRUE, ...) {
  if (entities == TRUE) {
    dplyr::n_groups(object@frame)
  } else {
    nrow(object@frame)
  }
}

#' @importFrom stats formula
#' @export

formula.wbm <- function(x, raw = FALSE, ...) {
  if (raw == TRUE) {
    return(x@call_info$merMod_call$formula)
  } else {
    return(getCall(x)$formula)
  }
}

#' @export
#' @importFrom stats terms

terms.wbm <- function(x, fixed.only = TRUE, random.only = FALSE, ...) {
    terms(x, fixed.only = fixed.only, random.only = random.only, ...)
}

#' @export



