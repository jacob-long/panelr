#' @importFrom stats as.formula terms
#' @import stringr

wb_formula_parser <- function(formula, dv, data) {
  # See how many parts the formula has 
  conds <- length(formula)[2]
  
  # Deal with non-numeric variables
  if (any(!sapply(data[all.vars(get_rhs(formula))], is.numeric))) {
    # Find the time-varying non-numeric vars 
    vars <- 
      names(sapply(data[all.vars(get_rhs(formula))], is.numeric) %just% FALSE)
    # Expand these factors into 0/1 variables in the data
    data <- expand_factors(vars, data)
    # Now create a formula that does the same
    for (var in vars) {
      if (conds < 3) {
        attr(formula, "rhs")[[1]] <- expand_formula(formula, var, data)[[2]]
      } else {
        pieces <- expand_formula(formula, var, data)
        attr(formula, "rhs")[[1]] <- pieces[[1]][[2]]
        attr(formula, "rhs")[[3]] <- pieces[[2]][[2]]
      }
    }
  }
  
  # Save varying variables
  varying <- sapply(get_term_labels(formula), function(x) {
    # If non-syntactic names are inside functions, retain backticks
    if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
  })
  # Save time-varying part of the formula
  varying_form <- formula_ticks(
    as.character(paste(deparse(get_rhs(formula)), collapse = "")),
    varying %just% names(data)
  )

  if (conds == 1) {
    # There are no constants
    constants <- NULL
    constants_form <- NULL
    
  } else if (conds > 1) {
    # Save constants
    constants <- sapply(get_term_labels(formula, which = 2), function(x) {
      # If non-syntactic names are inside functions, retain backticks
      if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
    })
    # Save constants part of the formula
    constants_form <- formula_ticks(
      as.character(paste(deparse(get_rhs(formula, which = 2)), collapse = "")),
      constants %just% names(data)
    )
  }
  
  # Retain list of all variables to go into final model
  allvars <- c(dv, varying, constants)

  if (conds >= 3) { # Deal with interactions et al. part of formula
    # Grab all the variables mentioned in this part of the formula
    int_vars <- sapply(get_term_labels(formula, which = 3), function(x) {
      # If non-syntactic names are inside functions, retain backticks
      if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
    })
    
    if (any(stringr::str_detect(int_vars, "\\|"))) {
      barred <- int_vars[stringr::str_detect(int_vars, "\\|")]
      int_vars <- int_vars[!stringr::str_detect(int_vars, "\\|")]
      split <- stringr::str_trim(unlist(stringr::str_split(barred, "\\|")))
      int_vars <- c(int_vars, split)
    }
    
    # Add them onto the allvars vector as long as they aren't redundant
    allvars <- unique(c(allvars, int_vars))
    # Save that part of the formula
    cross_ints_form <- formula_ticks(
      as.character(paste(deparse(get_rhs(formula, which = 3)), collapse = "")),
      allvars %just% names(data)
    )
  } else {
    cross_ints_form <- NULL
  }

  v_info <- tibble::tibble(term = varying, root = NA, lag = NA, meanvar = NA)
  
  # If there's a lag function call, set lag to 1 and 0 otherwise. We'll go 
  # back and look for the n argument in a second
  v_info$lag <- as.numeric(stringr::str_detect(varying, "(?<=lag\\().*(?=\\))"))

  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(v_info$lag > 0)) {
    for (i in which(v_info$lag > 0)) {
      # Convert to call to match the arguments and unambiguously get the n = 
      the_call <- 
        match.call(dplyr::lag, call = parse(text = v_info$term[i]))
      v_info$lag[i] <- if (!is.null(the_call$n)) the_call$n else 1
      v_info$root[i] <- as.character(the_call$x)
    }
  }

  # If there's a lead function call, set lag to -1 and 0 otherwise. We'll go 
  # back and look for the n argument in a second
  leads <- stringr::str_detect(varying, "(?<=lead\\().*(?=\\))")
  
  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(leads)) {
    for (i in which(leads)) {
      # Convert to call to match the arguments and unambiguously get the n = 
      the_call <- 
        match.call(dplyr::lead, call = parse(text = v_info$term[i]))
      v_info$lag[i] <- if (!is.null(the_call$n)) the_call$n * -1 else -1
      v_info$root[i] <- as.character(the_call$x)
    }
  }
  
  v_info$root[is.na(v_info$root)] <- v_info$term[is.na(v_info$root)]

  # Set all the mean var names to mean(var)
  v_info$term <- sapply(v_info$term, function(x) {
    # If non-syntactic variable name, need to escape it inside the mean function
    if (make.names(x) != x & x %in% names(data)) bt(x) else x 
  })
  v_info$meanvar <- paste0("imean(", v_info$term, ")")
  
  out <- list(conds = conds, allvars = allvars, varying = varying,
              varying_form = varying_form, constants = constants,
              constants_form = constants_form,
              cross_ints_form = cross_ints_form, v_info = v_info,
              data = data)
  return(out)

}

prepare_lme4_formula <- function(formula, pf, data, use.wave, wave, id) {
  # Append fixed wave to formula if requested
  if (use.wave == TRUE) {
    formula <- paste(formula, "+", wave)
  }
  # By default, assume no random effects have been added
  add <- FALSE
  
  # I need to escape non-syntactic variables in the model formula
  formula <- formula_ticks(formula, c(pf$varying, pf$meanvars, pf$constants))
  
  # See if the formula has 3 parts
  if (pf$conds > 2) {
    # See if there are any random effects specified
    res <- lme4::findbars(as.formula(formula))
    # If there are, let's deal with them
    if (!is.null(res)) {
      # Get info on those random effects
      refs <- lme4::mkReTrms(res, data)$cnms
      # Check if any of those random effects include the id variable
      if (any(names(refs) == id)) {
        # Check if those terms include an intercept
        inds <- which(names(refs) == id)
        if (any(unlist(refs[inds]) == "(Intercept)")) {
          # If the user specified a random intercept for id, I won't add it 
          # myself
          add <- TRUE
        }
      }
    }
  }
  # Add random intercept for id if it wasn't user-specified
  if (add == FALSE) {
    formula <- paste0(formula, " + (1 | ", id, ")")
  }
  # Lastly, I need to escape non-syntactic variables in the model formula again
  fin_formula <- formula_ticks(formula, c(pf$varying, unique(pf$v_info$meanvar),
                                          pf$constants))
  as.formula(fin_formula)
}

#' @importFrom stats resid lm coef na.exclude update

detrend <- function(data, pf, dt_order, balance_correction, dt_random) {
    
  # save id and wave 
  id <- get_id(data)
  wave <- get_wave(data)
  
  # If random slopes, nest the data
  if (dt_random == TRUE) {
    # Nest the data for efficient fitting of the lms
    data <- tidyr::nest(data)
  }
    
  # Define detrending function
  dt_model <- function(data, var, order = dt_order) {
    
    the_formula <- as.formula(paste(var, "~ poly(", wave, ",", order,
                                    ", raw = TRUE)"))
    tryCatch({
      resid(lm(formula = the_formula, data = data, na.action = na.exclude))
    }, error = function(x) {rep(NA, times = nrow(data))})
      
  }
      
  # Define between-person function
  b_model <- function(data, var, order = dt_order, bc = balance_correction) {
      
    if (bc == TRUE) {  
      
      the_formula <- as.formula(paste(var, "~ poly(", wave, ",",
                                      order, ", raw = TRUE)"))
      out <- tryCatch({
        coef(mod <- lm(formula = the_formula, data = data,
                       na.action = na.exclude))["(Intercept)"]
      }, error = function(x) {NA})
      rep(out, times = length(resid(mod)))
      
    } else {
    
      the_formula <- as.formula(paste(var, "~ 1"))
      out <- tryCatch({
        coef(mod <- lm(formula = the_formula, data = data))["(Intercept)"]
      }, error = function(x) {NA})
      rep(out, times = nrow(data))
    }
    
  }
    
  # Iterate through the varying variables
  for (v in pf$varying) {
        
    # De-trend
    # Note that b_model differs depending on balance_correction 
    the_var <- pf$non_lag_vars[v]
    mean_var <- pf$meanvars[v]
    if (dt_random == TRUE) {
      data <-  dplyr::mutate(data,
                             !!mean_var :=
                               purrr::map(data, b_model, var = the_var),
                             !!the_var :=
                               purrr::map(data, dt_model, var = the_var)
                            )
    } else {
      
      data <-  dplyr::mutate(data,
                             !!mean_var := b_model(data, var = the_var),
                             !!the_var := dt_model(data, var = the_var)
      )
      
    }
      
  }
  
  if (dt_random == TRUE) {
    # Unnest the data if it was nested
    data <- tidyr::unnest(data)
  }
  return(panel_data(data, id = !! sym(id), wave = !! sym(wave)))
  
}

formula_ticks <- function(formula, vars) {

  for (var in vars) {
    # Trying to isolate the variables from synthetic terms in the formula.
    # Can't have variable width lookbehinds so I say up to 3 open parentheses
    # that are preceded by whitespace (as opposed to the I function or 
    # something).
    regex_pattern <- paste0(
      "(?<=(~|\\s|\\*|\\+|\\:)|\\s\\(|\\s\\(\\(|\\s\\(\\(\\(\\()",
      escapeRegex(var),
      "(?=(~|\\s|\\*|\\+|\\:)|\\))")
    backtick_name <- paste("`", var, "`", sep = "")
    backtick_name <- gsub("(?<!^)`(?!$)", "", backtick_name, perl = TRUE)
    formula <- gsub(regex_pattern, backtick_name, formula, perl = TRUE)
  }

  formula <- paste0(formula, collapse = "")
  formula <- gsub("``", "`", formula, fixed = TRUE)

  return(formula)

}

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

formula_esc <- function(formula, vars) {

  for (var in vars) {

    new_var <- make.names(var)
    formula <- gsub(var, new_var, formula, fixed = TRUE)

  }

  formula <- paste0(formula, collapse = "")
  formula <- gsub("``", "`", formula, fixed = TRUE)

  return(formula)

}

bt <- function(x) {
  
  btv <- paste0("`", x, "`")
  btv <- gsub("``", "`", btv, fixed = TRUE)
  return(btv)
  
}

un_bt <- function(x) {
  gsub("`", "", x)
}

# Accessor function for Formula objects that have multiple parts
get_rhs <- function(x, which = 1, to.formula = FALSE) {
  # Coercing to formula can be useful, otherwise it's a call object
  if (to.formula == TRUE) {
    as.formula(paste("~", deparse(attr(x, "rhs")[[which]])))
  } else {
    attr(x, "rhs")[[which]]
  }
}

# Get individual terms from formula
get_term_labels <- function(x, which = 1, omit.ints = TRUE) {
  # Formula provides an unusual access method but lets me get one part at a time
  labs <- attr(terms(get_rhs(x, which = which, to.formula = TRUE)),
               "term.labels")
  # I can choose to get only first-order variables
  if (omit.ints == TRUE) {
    labs <- labs[
      which(attr(
        terms(get_rhs(x, which = which, to.formula = TRUE)),
      "order") == 1)
    ]
  }
  return(labs)
}

# Retrieve all variables involving interactions
# interaction_vars <- function(formula, variable) {
#   facs <- attr(terms(formula), "factors")
#   raw_names <- rownames(facs)
#   bare_vars <- all.vars(as.formula(paste("~", paste(raw_names, collapse = "+"))))
#   fac_vars <- raw_names[which(bare_vars == variable)]
#   ints <- sapply(fac_vars, function(x) {
#     names(which(facs[x,] > 0)) %not% x
#   })
#   vars <- list()
#   for (int in ints) {
#     vars[[which(ints == int)]] <- 
#       sapply(fac_vars, function(x) {
#         names(facs[, int] %not% 0) %not% x
#       }, USE.NAMES = FALSE)
#   }
#   vars
# }

# Generate the labels for factors that R normally does already
expand_labels <- function(data, variable) {
  paste0(variable, unique(data[[variable]] %not% base_level(data[[variable]])))
}

# Make all the labels for terms of all orders for (especially) factors
make_labels <- function(formula, variable, data) {
  term_labels <- labels(terms(formula))[which_terms(formula, variable)]
  labs <- c()
  expanded <- expand_labels(data, variable)
  for (lab in term_labels) {
    for (val in expanded) {
      labs <- c(labs, sub(variable, bt(val), lab, fixed = TRUE))
    }
  }
  labs
}

# Retrieve the base level of factors or other non-numeric variables
base_level <- function(x) {
  if (is.factor(x)) {
    return(levels(x)[1])
  } else if (!is.logical(x)) {
    return(levels(factor(x))[1])
  } else {
    return(FALSE)
  }
}

# Get the indices of terms in terms object involving a given variable
which_terms <- function(formula, variable) {
  # Get the factors matrix from the terms object
  facs <- attr(terms(formula), "factors")
  # Get the term names
  raw_names <- rownames(facs)
  # Use some jiu-jitsu to get the bare variable names for those terms
  bare_vars <- all.vars(
    as.formula(paste("~", paste(raw_names, collapse = "+"))), unique = FALSE)
  # If there's more than one term involving variable, need to handle differently
  if (length(which(bare_vars == variable)) > 1) {
    which(colSums(facs[which(bare_vars == variable),]) > 0)
  } else {
    which(facs[which(bare_vars == variable),] > 0)
  }
}

# Create a formula with an expanded factor variable (i.e., with dummies)
expand_formula <- function(formula, variable, data) {
  # get rid of constants
  if (length(attr(formula, "rhs")) > 1) {
    attr(formula, "rhs")[[2]] <- 1
  }
  if (length(attr(formula, "rhs")) < 3) {
    # Get terms that don't have anything to do with variable
    o_terms <- labels(drop.terms(terms(formula),
                                 which_terms(formula, variable)))
    # Get vector of term labels for all terms that involve variable
    labs <- make_labels(formula, variable, data)
    # Use base R's reformulate function to make a new formula using these 
    # character objects
    return(reformulate(c(o_terms, labs)))
  } else {
    out <- list()
    for (i in c(1, 3)) {
      tmp_form <- get_rhs(formula, which = i, to.formula = TRUE)
      # Get terms that don't have anything to do with variable
      o_terms <- labels(
        drop.terms(terms(tmp_form), which_terms(tmp_form, variable))
      )
      # Get vector of term labels for all terms that involve variable
      labs <- make_labels(tmp_form, variable, data)
      # Use base R's reformulate function to make a new formula using these 
      # character objects
      out <- c(out, reformulate(c(o_terms, labs)))
    }
    return(out)
  }
}

# TODO: consider more robust support of non-treatment contrasts
# This adds new columns to the data frame for the levels of factors
expand_factors <- function(variables, data) {
  # Loop through the variables
  for (var in variables) {
    # Get values of variable
    vals <- unique(data[[var]] %not% base_level(data[[var]]))
    # Loop through values
    for (val in vals) {
      # Create new column of 0/1 for whether variable equals this value
      data[[paste0(var, val)]] <- as.numeric(data[[var]] == val)
    }
  }
  return(data)
}

#### Regex helper ############################################################

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}
