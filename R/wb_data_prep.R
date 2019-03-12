
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


wb_model <- function(model, pf, dv, data, detrend) {
  
  # Create empty stab terms vector so I can pass it along even for other
  # models
  stab_terms <- c()
  # Create object to store within interactions
  within_ints <- NULL
  
  # Extract wave and id
  wave <- get_wave(data)
  id <- get_id(data)
  
  # models that require de-meaning
  within_family <- c("w-b", "within-between", "within", "stability", "fixed")
  
  # De-mean varying vars if needed
  if (model %in% within_family & detrend == FALSE) { # within models
    # Iterate through the varying variables
    for (i in seq_along(pf$v_info$term)) {
      # De-mean
      data[[pf$v_info$term[i]]] <- 
        data[[pf$v_info$term[i]]] - data[[pf$v_info$meanvar[i]]]
    }
    # Deal with within by within interactions
    if (!is.null(pf$int_labs)) {
      for (iv in pf$int_labs) {
        iv_mean <- paste0("imean(", iv, ")")
        subtract_mean <- paste(iv, "-", iv_mean)
        pretty_int <- stringr::str_replace(iv, "\\*", ":")
        data <- mutate(data,
          !! iv := !! parse_expr(iv),
          !! iv_mean := !! parse_expr(iv_mean),
          !! pretty_int := !! parse_expr(subtract_mean)
        )
        within_ints <- c(within_ints, pretty_int)
      }
      # Remove recursive back-ticking 
      names(data) <- gsub("`", "", names(data))
      within_int_form <- paste(within_ints, collapse = " + ")
      pf$varying_form <- paste(
        as.character(reformulate(pf$v_info$term))[2], " + ",
        within_int_form
      )
      # What to do with interaction mean terms?
    }
  } 
  
  # Create extra piece of formula based on model
  if (model %in% c("w-b", "within-between", "contextual")) {
    # Avoid redundant mean variables when multiple lags of the same variable
    # are included... e.g., imean(lag(x)) and imean(x). I want whichever is 
    # the most recent (or covering the most waves in the case of there being
    # leads)
    pf$v_info <- set_meanvars(pf)
    # Make formula add-on
    add_form <- paste(unique(c(pf$v_info$term, pf$v_info$meanvar)),
                      collapse = " + ")
  } else if (model %in% c("within","fixed")) { # Many know it as fixed
    # Don't need to worry about constants, etc.
    add_form <- ""
  } else if (model == "stability") {
    
    # Make formula add-on
    add_form <- paste(unique(c(pf$v_info$term, pf$v_info$meanvar)),
                      collapse = " + ")
    
    # Add the stability terms
    add_form <- paste(add_form, "+", 
                      paste(unique(pf$v_info$meanvar), "*", wave), collapse = " + ")
    stab_terms <- c(stab_terms, paste(unique(pf$v_info$meanvar), ":", wave,
                                      sep = ""))
    
    
  } else if (model %in% c("between","random")) {
    
    # It doesn't need anything special
    add_form <- ""
    
  }
  
  # Put the pieces together
  fin_formula <- paste(dv, "~", add_form, "+", pf$varying_form)
  fin_formula <- paste(c(fin_formula, pf$constants_form, pf$cross_ints_form),
                       collapse = " + ")
  
  out <- list(data = data, fin_formula = fin_formula,
              stab_terms = stab_terms, within_ints = within_ints)
  return(out)
  
}

