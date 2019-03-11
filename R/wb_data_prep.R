
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
    
    # Coerce to panel_data
    data <- panel_data(data, id = !! sym(id), wave = !! sym(wave))
    # Save this input data separately
    orig_data <- data
    
    # Get the weights argument like lm() does (can be name or object)
    weights <- eval_tidy(enquo(weights), data)
    # Append to data with special name
    if (!is.null(weights)) {data[".weights"] <- weights}
    # Get the offset argument like lm() does (can be name or object)
    offset <- eval_tidy(enquo(offset), data)
    # Append to data with special name
    if (!is.null(offset)) {data[".offset"] <- offset}
    
    # Get the left-hand side
    dv <- as.character((attr(formula, "lhs")))
    # Pass to helper function
    pf <- wb_formula_parser(formula, dv, data)
    
    # Need to do detrending before lags, etc.
    if (detrend == TRUE) {
      data <- detrend(pf$data, pf, dt_order, balance_correction, dt_random)
    } else {
      data <- pf$data
    }
    
    # models that don't use constants
    within_only <- c("within", "fixed")
    if (model %in% within_only) {
      pf$allvars <- pf$allvars[pf$allvars %nin% pf$constants]
      pf$constants_form <- NULL
      if (!is.null(pf$constants)) {
        warn_wrap("Constants are ignored with ", model, " model 
                  specifications.")
      }
    }
    
    # If there are random effects specified, substitute a plus sign to make
    # model_frame function work right
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
                     paste(pf$v_info$meanvar, collapse = " + "),
                     end_form, collapse = ""
    )
    # Escape non-syntactic variables that are in the data
    mf_form <- formula_ticks(mf_form, names(data))
    
    # Add weights to keep it in the DF
    if (!is.null(weights)) {
      mf_form <- paste(mf_form, "+", ".weights")
    }
    
    # Add offset to keep it in the DF
    if (!is.null(offset)) {
      mf_form <- paste(mf_form, "+", ".offset")
    }
    
    # Pass to special model_frame function that respects tibble groupings
    data <- model_frame(update(as.formula(mf_form),
                               as.formula(paste("~ . -", id, "-", wave))), 
                        data = data)
    
    # Now I fix any back-ticked names saved in this pf object
    pf$v_info$meanvar <- un_bt(pf$v_info$meanvar)
    pf$v_info$root <- un_bt(pf$v_info$root)
    pf$varying <- un_bt(pf$varying)
    pf$constants <- un_bt(pf$constants)
    
    # Drop missing cases, collect some metadata 
    data <- complete_cases(data, min.waves = min.waves)
    num_distinct <- length(unique(data[[id]]))
    maxwave <- max(data[[wave]])
    minwave <- min(data[[wave]])
    
    # Modify weights object to reflect missing data drops
    if (!is.null(weights)) {
      weights <- data$.weights
    }
    
    # Modify offset object to reflect missing data drops
    if (!is.null(offset)) {
      offset <- data$.offset
    }
    
    # Send to helper that will demean, etc.
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
  
  d <- wb_prepare_data(formula = Formula::Formula(formula),
                       data, id = id, wave = id,
                       model = model, detrend = detrend, use.wave = use.wave,
                       wave.factor = wave.factor, min.waves = min.waves,
                       balance_correction = balance_correction,
                       dt_random = dt_random, dt_order = dt_order,
                       weights = weights, offset = offset)
  d$e$data
  
}