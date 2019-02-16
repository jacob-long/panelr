
wb_prepare_data <- function(formula, data, id = NULL, wave = NULL,
           model = "w-b", detrend = FALSE, use.wave = FALSE,
           wave.factor = FALSE, min.waves = 2,
           balance_correction = FALSE, dt_random = TRUE, dt_order = 1,
           weights = NULL, offset = NULL, ...) {
    
    # Get data prepped
    if ("panel_data" %in% class(data)) {
      id <- attr(data, "id")
      wave <- attr(data, "wave")
    }
    
    if (!("data.frame" %in% class(data))) {
      stop("data argument must be a data frame.")
    }
    
    data <- panel_data(data, id = !! sym(id), wave = !! sym(wave))
    orig_data <- data
    
    weights <- eval_tidy(enquo(weights), data)
    if (!is.null(weights)) {data[".weights"] <- weights}
    offset <- eval_tidy(enquo(offset), data)
    if (!is.null(offset)) {data[".offset"] <- offset}
    
    dv <- as.character(formula[[2]])
    formula <- as.character(formula)[[3]]
    # Pass to helper function
    pf <- wb_formula_parser(formula, dv)
    
    # Temp DF to look for factors
    vdata <- model_frame(as.formula(paste("~", pf$varying_form)), data = data)
    # Check for factors in the varying part
    any_factors <- sapply(vdata[pf$varying], is.factor)
    if (any(any_factors)) {
      stop_wrap(paste(pf$varying[any_factors], collapse = " and "), 
                " is a factor variable and therefore cannot be mean-centered.
                Consider converting to numeric dummy variable(s).")
    }
    any_chars <- sapply(vdata[pf$varying], is.character)
    if (any(any_chars)) {
      stop_wrap(paste(pf$varying[any_chars], collapse = " and "), 
                " is a character variable and therefore cannot be mean-centered.
                Consider converting to numeric dummy variable(s).")
    }
    rm(vdata) # save some memory
    
    # Need to do detrending before lags, etc.
    if (detrend == TRUE) {
      data <- detrend(data, pf, dt_order, balance_correction, dt_random)
    }
    
    # models that don't use constants
    within_only <- c("within","fixed")
    if (model %in% within_only) {
      pf$allvars <- pf$allvars[pf$allvars %nin% pf$constants]
      pf$constants_form <- NULL
    }
    
    if (!is.null(pf$cross_ints_form)) {
      end_form <- as.character(
        deparse(lme4::subbars(
          as.formula(paste("~", pf$cross_ints_form))
        )[[2]])
      )
      end_form <- paste("+", end_form)
    } else {
      end_form <- NULL
    }
    
    # Create formula to pass to model_frame
    mf_form <- paste(" ~ ",
                     paste(pf$allvars, collapse = " + "),
                     " + ",
                     paste(pf$meanvars, collapse = " + "),
                     end_form, collapse = ""
    )
    
    mf_form <- as.formula(mf_form)
    mf_form <- paste(as.character(deparse(update(mf_form, ~ . - id))),
                     collapse = "")
    
    # Add weights to keep it in the DF
    if (!is.null(weights)) {
      mf_form <- paste(mf_form, "+", ".weights")
    }
    
    # Add offset to keep it in the DF
    if (!is.null(offset)) {
      mf_form <- paste(mf_form, "+", ".offset")
    }
    
    # Pass to special model_frame function that respects tibble groupings
    data <- model_frame(as.formula(mf_form), data = data)
    
    
    data <- complete_cases(data, min.waves = min.waves)
    num_distinct <- length(unique(data[[id]]))
    maxwave <- max(data[[wave]])
    minwave <- min(data[[wave]])
    
    # Modify weights to reflect missing data drops
    if (!is.null(weights)) {
      weights <- data$.weights
    }
    
    # Modify offset to reflect missing data drops
    if (!is.null(offset)) {
      offset <- data$.offset
    }
    
    e <- wb_model(model, pf, dv, data, detrend)
    
    list(e = e, num_distinct = num_distinct, maxwave = maxwave,
         minwave = minwave, weights = weights, offset = offset,
         pf = pf, id = id, wave = wave, dv = dv, orig_data = orig_data,
         mf_form = mf_form)
  
}

#' @title Prepare data for within-between modeling
#' @description This function allows users to make the changes to their data
#'  that occur in [wbm()] without having to fit the model.
#'  
#' @inheritParams wbm
#' @return A `panel_data` object with the requested specification.
#' @export

make_wb_data <- function(formula, data, id = NULL, wave = NULL,
                    model = "w-b", detrend = FALSE, use.wave = FALSE,
                    wave.factor = FALSE, min.waves = 2,
                    balance_correction = FALSE, dt_random = TRUE, dt_order = 1,
                    weights = NULL, offset = NULL, ...) {
  
  d <- wb_prepare_data(formula = formula, data, id = id, wave = id,
                       model = model, detrend = detrend, use.wave = use.wave,
                       wave.factor = wave.factor, min.waves = min.waves,
                       balance_correction = balance_correction,
                       dt_random = dt_random, dt_order = dt_order,
                       weights = weights, offset = offset)
  
  d$e$data
  
}