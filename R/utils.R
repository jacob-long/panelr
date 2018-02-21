#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom magrittr "%<>%"
#' @export
magrittr::`%<>%`

#' @importFrom stats getCall
#' @export

getCall.wbm <- function(x, ...) {
  
  return(attr(x, "call"))
  
}

#' 

# update.wbm <- function(object, call.env = parent.frame(), ...) {
#   
#   call <- getCall(object)
#   
#   # Now get the argument names for that version of object
#   object_formals <- formals(getFromNamespace("wbm", "panelr"))
#   
#   extras <- as.list(match.call())
#   indices <- match(names(extras), names(object_formals))
#   extras <- extras[indices]
#   
#   for (i in 1:length(extras)) {
#     if (is.name(extras[[i]])) {
#       extras[[i]] <- eval(extras[[i]], envir = call.env)
#     }
#   }
#   existing <- !is.na(match(names(extras), names(call)))
#   for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
#   if (any(!existing)) {
#     call <- c(as.list(call), extras[!existing])
#     call <- as.call(call)
#   }
#   
#   env <- attr(object, "env")
#   
#   eval(call, env, parent.frame())
#   
# }

#' @importFrom stats predict na.pass
#' @export

predict.wbm <- function(object, newdata = NULL, raw = FALSE, newparams = NULL,
        re.form = NULL, terms = NULL, type = c("link", "response"),
        allow.new.levels = FALSE, na.action = na.pass, ...) {
  
  if (!is.null(newdata) & raw == FALSE) {
    mf_form <- attr(object, "mf_form")
    pf <- attr(object, "pf")
    newdata <- model_frame(mf_form, newdata)
    dv <- attr(object, "dv")
    
    if (attr(object, "detrend") == TRUE) {
      dto <- detrend(newdata, pf, attr(object, "dt_order"),
                     attr(object, "balance_correction"),
                     attr(object, "dt_random"))
      newdata <- dto
    }
    
    newdata <- wb_model(attr(object, "model"), pf, dv, newdata,
                        attr(object, "detrend"))$data
    
  }
  
  if (is.null(attr(attr(object$model@frame, "terms"), "varnames.fixed"))) {
    attr(attr(object$model@frame, "terms"), "varnames.fixed") <-
      c(attr(object, "varying"), attr(object, "constants"),
        attr(object, "meanvars"))
  }
  
  predict(object$model, newdata = newdata, newparams = newparams,
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
    mf_form <- attr(object, "mf_form")
    pf <- attr(object, "pf")
    newdata <- model_frame(mf_form, newdata)
    dv <- attr(object, "dv")
    
    if (attr(object, "detrend") == TRUE) {
      dto <- detrend(newdata, pf, attr(object, "dt_order"),
                     attr(object, "balance_correction"),
                     attr(object, "dt_random"))
      newdata <- dto
    }
    
    newdata <- wb_model(attr(object, "model"), pf, dv, newdata,
                        attr(object, "detrend"))$data
    
  }
  
  if (is.null(attr(attr(object$model@frame, "terms"), "varnames.fixed"))) {
    attr(attr(object$model@frame, "terms"), "varnames.fixed") <-
      c(attr(object, "varying"), attr(object, "constants"),
        attr(object, "meanvars"))
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

#' @title Helpers functions for interfacing with wbm objects
#' 
#' @param object A `wbm` model object
#' @param x A `wbm` model object
#' @param add.dropped for models with rank-deficient design matrix,
#'  reconstitute the full-length parameter vector by adding NA values in
#'  appropriate locations?
#' @param condVar an optional logical argument indicating if the conditional
#'  variance-covariance matrices of the random effects should be added as an
#'  attribute.
#' @param drop should components of the return value that would be data frames
#'  with a single column, usually a column called `(Intercept)`, be returned as
#'  named vectors instead?
#' @param whichel character vector of names of grouping factors for which the
#'  random effects should be returned.
#' @param postVar a (deprecated) synonym for condVar
#' @param ... some methods for these generic functions require additional
#'  arguments
#' 
#' @rdname helpers
#' @export

fixef.wbm <- function(object, add.dropped = FALSE, ...) {
  
  lme4::fixef(object$model, add.dropped = add.dropped, ...)
  
}

#' @rdname helpers
#' @importFrom lme4 ranef
#' @export

ranef.wbm <- function(object, condVar = FALSE, drop = FALSE,
                      whichel = NULL, postVar = FALSE, ...) {
  if (is.null(whichel)) {
    lme4::ranef(object$model, condVar = condVar, drop = drop,
                postVar = postVar, ...)
  } else {
    lme4::ranef(object$model, condVar = condVar, drop = drop, whichel = whichel,
                postVar = postVar, ...)
  }
  
}

#' @importFrom stats vcov sigma
#' @export

vcov.wbm <- function(object, correlation = TRUE, sigm = sigma(object$model),
                     use.hessian = NULL, ...) {
  
  vcov(object$model, correlation = correlation, sigm = sigm,
       use.hessian = use.hessian, ...)
  
}

#' @importFrom stats model.frame
#' @export

model.frame.wbm <- function(formula, ...) {
  
  formula$data
  
}

#' @importFrom stats nobs
#' @export

nobs.wbm <- function(object, entities = TRUE, ...) {
  
  if (entities == TRUE) {
    dplyr::n_groups(object$data)
  } else {
    nrow(object$data)
  }
  
}

#' @importFrom stats formula
#' @export

formula.wbm <- function(x, raw = FALSE, ...) {
  
  if (raw == TRUE) {
    return(x$fin_formula)
  } else {
    return(getCall(x)$formula)
  }
  
}

#' @importFrom stats formula
#' @export

terms.wbm <- function(x, fixed.only = TRUE, random.only = FALSE, ...) {
  
  terms(x$model, fixed.only = fixed.only, random.only = random.only, ...)
  
}

#' @importFrom stats coef
#' @export 

coef.wbm <- function(object, ...) {
  
  coef(object$model, ...)
  
}

#' @importFrom stats anova
#' @export

anova.wbm <- function(object, ..., refit = TRUE, model.names = NULL) {
  
  args <- list(object = object$model)
  nms <- c(deparse(substitute(object)))
  
  dots <- list(...)
  if (length(dots) > 0) {
    mods <- lapply(dots, function(x) {x$model})
    nms <- c(nms, sapply(substitute(...()), as.character))
    args <- append(args, unlist(mods))
  }
  
  args$refit <- refit
  if (!is.null(model.names)) {
    args$model.names <- model.names
  } else {
    args$model.names <- nms
  }
  
  
  do.call("anova", args)
  
  
}

#' @rdname helpers
#' @export

isGLMM.wbm <- function(x, ...) {
  
  if (family(x$model)$family == "gaussian") {
    return(FALSE)
  } else {
    return(TRUE)
  }
  
}

#' @rdname helpers
#' @export

isLMM.wbm <- function(x, ...) {
  
  if (family(x$model)$family == "gaussian") {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}

#' @rdname helpers
#' @export

isNLMM.wbm <- function(x, ...) {
  
  return(FALSE)
  
}


