wb_prepare_data <- function(formula, data, id = NULL, wave = NULL,
           model = "w-b", detrend = FALSE, use.wave = FALSE,
           wave.factor = FALSE, min.waves = 2,
           balance_correction = FALSE, dt_random = TRUE, dt_order = 1,
           weights = NULL, offset = NULL, demean.ints = TRUE, old.ints = FALSE,
           ...) {
    
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
    
    # models that don't use constants
    within_only <- c("within", "fixed")
    if (model %in% within_only) {
      pf$allvars <- pf$allvars %not% pf$constants
      if (!is.null(pf$constants)) {
        warn_wrap("Constants are ignored with ", model, " model 
                  specifications.")
      }
      pf$constants <- NULL
    }

    # Create formula to pass to model_frame
    mf_form <- paste(" ~ ",
                     paste(pf$allvars, collapse = " + "),
                     " + ",
                     paste(pf$v_info$meanvar, collapse = " + "),
                     collapse = ""
    )
    # Escape non-syntactic variables that are in the data
    mf_form <- formula_ticks(mf_form, names(pf$data))
    
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
                        data = pf$data)
    
    # Now I fix any back-ticked names saved in this pf object
    pf$v_info$meanvar <- un_bt(pf$v_info$meanvar)
    pf$v_info$root <- un_bt(pf$v_info$root)
    pf$varying <- un_bt(pf$varying)
    pf$constants <- un_bt(pf$constants)
    
    # Send to helper that will demean, etc.
    e <- wb_model(model, pf, dv, data, detrend, demean.ints, old.ints)
    
    if (detrend == TRUE) {
      e$data <- detrend(e$data, pf, dt_order, balance_correction, dt_random)
    } 
    
    # Drop missing cases, collect some metadata 
    e$data <- complete_cases(e$data, min.waves = min.waves)
    num_distinct <- length(unique(e$data[[id]]))
    maxwave <- max(e$data[[wave]])
    minwave <- min(e$data[[wave]])
    
    # Modify weights object to reflect missing data drops
    if (!is.null(weights)) {
      weights <- e$data$.weights
    }
    
    # Modify offset object to reflect missing data drops
    if (!is.null(offset)) {
      offset <- e$data$.offset
    }
    
    int_terms <- c(e$within_ints, e$cross_ints)
    attr(e$data, "interactions") <- int_terms
    attr(e$data, "interaction.style") <- 
      if (demean.ints == FALSE & old.ints == FALSE) {
        "raw"
      } else if (old.ints == TRUE) "demean" else "double-demean"
    
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
#' @examples 
#' 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' make_wb_data(lwage ~ wks + union | fem, data = wages)
#' 
#' @export

make_wb_data <- function(formula, data, id = NULL, wave = NULL,
                    model = "w-b", detrend = FALSE, use.wave = FALSE,
                    wave.factor = FALSE, min.waves = 2,
                    balance.correction = FALSE, dt.random = TRUE, dt.order = 1,
                    weights = NULL, offset = NULL, 
                    interaction.style = c("double-demean", "demean", "raw"),
                    ...) {
  
  interaction.style <- match.arg(interaction.style,
                                 c("double-demean", "demean", "raw"))
  d <- wb_prepare_data(formula = Formula::Formula(formula),
                       data, id = id, wave = id,
                       model = model, detrend = detrend, use.wave = use.wave,
                       wave.factor = wave.factor, min.waves = min.waves,
                       balance_correction = balance.correction,
                       dt_random = dt.random, dt_order = dt.order,
                       weights = weights, offset = offset,
                       demean.ints = interaction.style == "double-demean",
                       old.ints = interaction.style == "demean")
  d$e$data
  
}


wb_model <- function(model, pf, dv, data, detrend, demean.ints, old.ints) {

  # Create object to store within interactions
  within_ints <- NULL
  cross_ints <- NULL
  int_means <- NULL
  
  # Extract wave and id
  wave <- get_wave(data)
  id <- get_id(data)
  
  # models that require de-meaning
  within_family <- c("w-b", "within-between", "within", "fixed")
  
  # De-mean varying vars if needed
  if (model %in% within_family) { # within models
    # Deal with within by within interactions -- old style is to make the 
    # interaction term *before* de-meaning constituent variables
    if (!is.null(c(pf$wint_labs, pf$cint_labs)) &
        (old.ints == TRUE | detrend == TRUE)) {
      ints <- process_interactions(ints = c(pf$wint_labs, pf$cint_labs),
                                   data = data, pf = pf, 
                                   within_ints = within_ints,
                                   cross_ints = cross_ints, 
                                   int_means = int_means, demean.ints = TRUE)
      data <- ints$data
      within_ints <- ints$within_ints
      cross_ints <- ints$cross_ints
    }
    
    if (detrend == FALSE) {
      # Iterate through the varying variables to de-mean them
      for (i in seq_along(pf$v_info$term)) {
        # De-mean
        data[[un_bt(pf$v_info$term[i])]] <- 
          data[[un_bt(pf$v_info$term[i])]] - data[[un_bt(pf$v_info$meanvar[i])]]
      }
    }
    
    # Deal with within interactions
    if (!is.null(c(pf$wint_labs, pf$cint_labs)) & old.ints == FALSE &
        detrend == FALSE) {
      ints <- process_interactions(ints = c(pf$wint_labs, pf$cint_labs),
                                   data = data, pf = pf, 
                                   within_ints = within_ints,
                                   cross_ints = cross_ints, 
                                   int_means = int_means,
                                   demean.ints = demean.ints)
      data <- ints$data
      within_ints <- ints$within_ints
      cross_ints <- ints$cross_ints
    }
  } else {
    # Deal with within interactions
    if (!is.null(c(pf$wint_labs, pf$cint_labs, pf$bint_labs))) {
      ints <- process_interactions(ints = c(pf$wint_labs, pf$cint_labs),
                                   data = data, pf = pf, 
                                   within_ints = within_ints,
                                   cross_ints = cross_ints, 
                                   int_means = int_means,
                                   demean.ints = FALSE)
      data <- ints$data
      within_ints <- ints$within_ints
      cross_ints <- ints$cross_ints
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
    add_form <- paste(bt(unique(pf$v_info$meanvar)), collapse = " + ")
  } else {
    add_form <- ""
  }
  
  # Put the pieces together
  the_terms <- c(bt(pf$varying), bt(pf$constants), bt(cross_ints), 
                 bt(within_ints), pf$bint_labs, 
                 if (!is.null(pf$ranefs)) bt_ranefs(pf$ranefs, data) else NULL)
  fin_formula <- paste(bt(dv), "~", add_form, "+",
                       paste(the_terms, collapse = " + "))
  
  out <- list(data = data, fin_formula = fin_formula, within_ints = within_ints,
              cross_ints = cross_ints, int_means = int_means, 
              between_ints = pf$bint_labs)
  return(out)
  
}

process_interactions <- function(ints, data, pf = NULL, within_ints = NULL,
                                 cross_ints = NULL, int_means = NULL,
                                 demean.ints = TRUE) {
  for (iv in ints) {
    iv_mean <- paste0("imean(", bt(un_bt(iv)), ")")
    iv_mean_name <- un_bt(stringr::str_replace_all(iv_mean, "\\*", ":"))
    if (demean.ints == TRUE) {
      subtract_mean <- paste(bt(un_bt(iv)), "-", bt(un_bt(iv_mean_name)))
    } else {
      subtract_mean <- bt(un_bt(iv))
    }
    pretty_int <- un_bt(stringr::str_replace_all(iv, "\\*", ":"))
    data <- mutate(data,
                   !! un_bt(iv) := !! parse_expr(iv),
                   !! un_bt(iv_mean_name) := !! parse_expr(iv_mean),
                   !! un_bt(pretty_int) := !! parse_expr(subtract_mean)
    )
    if (!is.null(pf)) {
      if (iv %in% pf$wint_labs) {
        within_ints <- c(within_ints, pretty_int)
      } else {
        cross_ints <- c(cross_ints, pretty_int)
      }
      int_means <- c(int_means, un_bt(iv_mean_name))
    }
  }
  # Remove recursive back-ticking 
  names(data) <- gsub("`", "", names(data))
  return(list(data = data, within_ints = within_ints, 
              cross_ints = cross_ints, int_means = int_means))
}
