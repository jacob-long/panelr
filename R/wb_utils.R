#' @importFrom stats as.formula terms
#' @import stringr

wb_formula_parser <- function(formula, dv, data) {
  # See how many parts the formula has 
  conds <- length(formula)[2]
  
  # Save varying variables
  varying <- sapply(get_term_labels(formula), function(x) {
    # If non-syntactic names are inside functions, retain backticks
    if (make.names(x) != x & x %in% data) un_bt(x) else x
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
      if (make.names(x) != x & x %in% data) un_bt(x) else x
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
      if (make.names(x) != x & x %in% data) un_bt(x) else x
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

  # Create vector of varnames for which we will get the mean
  # assuming a within-between specification
  meanvars <- varying

  # See which vars are lagged, if any
  lag_matches <-
    regexec(text = meanvars, pattern = "(?<=lag\\().*(?=\\))",
                         perl = T)

  # Indices of lagged variables
  lagvars <- which(lag_matches > 0)

  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(lag_matches > 0)) {
    for (i in lagvars) {

      the_match <- regmatches(meanvars, lag_matches)[[i]]
      # Check if there were args so that I only get the var name
      if (grepl(the_match, pattern = ",")) {
        the_match <- stringr::str_split(the_match, ",")[[1]][1]
      }
      # Now put the parent variable in the meanvars vector
      meanvars[i] <- the_match

    }
  }

  # Same deal with leads
  lead_matches <-
    regexec(text = meanvars, pattern = "(?<=lead\\().*(?=\\))",
            perl = T)

  # Indices of lagged variables
  leadvars <- which(lead_matches > 0)

  # If there are lagged vars, we need the original varname for taking
  # the mean later
  if (any(lead_matches > 0)) {
    for (i in leadvars) {

      the_match <- regmatches(meanvars, lead_matches)[[i]]
      # Check if there were args so that I only get the var name
      if (grepl(the_match, pattern = ",")) {
        the_match <- stringr::str_split(the_match, ",")[[1]][1]
      }
      # Now put the parent variable in the meanvars vector
      meanvars[i] <- the_match

    }
  }

  non_lag_vars <- meanvars
  names(non_lag_vars) <- varying
  # Set all the mean var names to mean(var)
  meanvars <- sapply(meanvars, function(x) {
    # If non-syntactic variable name, need to escape it inside the mean function
    if (make.names(x) != x & x %in% names(data)) bt(x) else x 
  })
  meanvars <- paste0("imean(", meanvars, ")")
  # Use this for matching varying vars to their parents
  names(meanvars) <- varying

  out <- list(conds = conds, allvars = allvars, varying = varying,
              varying_form = varying_form, constants = constants,
              constants_form = constants_form,
              cross_ints_form = cross_ints_form, meanvars = meanvars,
              non_lag_vars = non_lag_vars, data = data)
  return(out)

}

wb_model <- function(model, pf, dv, data, detrend) {

  # Create empty stab terms vector so I can pass it along even for other
  # models
  stab_terms <- c()
  
  # Extract wave and id
  wave <- get_wave(data)
  id <- get_id(data)

  # models that require de-meaning
  within_family <- c("w-b", "within-between", "within", "stability", "fixed")

  # De-mean varying vars if needed
  if (model %in% within_family && detrend == FALSE) { # within models

    # Iterate through the varying variables
    for (v in pf$varying) {
      # De-mean
      data[v] <- data[v] - data[pf$meanvars[v]]

    }

  }

  # Create extra piece of formula based on model
  if (model %in% c("w-b", "within-between", "contextual")) {
    # Contextual model is same as within-between, just no de-meaning

    # Make formula add-on
    for (v in pf$varying) {
      if (which(pf$varying == v) == 1) {
        add_form <- paste(v, "+", pf$meanvars[v])
      } else {
        add_form <- paste(add_form, "+", v, "+", pf$meanvars[v])
      }
    }

  } else if (model %in% c("within","fixed")) { # Many know it as fixed

    # Don't need to worry about constants, etc.
    add_form <- ""

  } else if (model == "stability") {

    # Make formula add-on
    for (v in pf$varying) {
      if (which(pf$varying == v) == 1) {
        add_form <- paste(v, "+", pf$meanvars[v])
      } else {
        add_form <- paste(add_form, "+", v, "+", pf$meanvars[v])
      }
    }

    # Add the stability terms
    for (v in pf$varying) {
      add_form <- paste(add_form, "+", pf$meanvars[v], "*", wave)
      stab_terms <- c(stab_terms, paste(pf$meanvars[v], ":", wave, sep = ""))
    }


  } else if (model %in% c("between","random")) {

    # It doesn't need anything special
    add_form <- ""

  }

  # Put the pieces together
  fin_formula <- paste(dv, "~", add_form, "+", pf$varying_form)
  if (pf$conds >= 1) {
    fin_formula <- paste(fin_formula, "+", pf$constants_form, "+",
                         pf$cross_ints_form)
  }

  out <- list(data = data, fin_formula = fin_formula,
              stab_terms = stab_terms)
  return(out)

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

    regex_pattern <- paste0("(?<=(~|\\s|\\*|\\+|\\:))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+|\\:))")
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

get_rhs <- function(x, which = 1, to.formula = FALSE) {
  if (to.formula == TRUE) {
    as.formula(paste("~", deparse(attr(x, "rhs")[[which]])))
  } else {
    attr(x, "rhs")[[which]]
  }
}

get_term_labels <- function(x, which = 1, omit.ints = TRUE) {
  labs <- attr(terms(get_rhs(x, which = which, to.formula = TRUE)),
               "term.labels")
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
  }
  vars
}
which_terms <- function(formula, variable) {
  facs <- attr(terms(formula), "factors")
  raw_names <- rownames(facs)
  bare_vars <- all.vars(
    as.formula(paste("~", paste(raw_names, collapse = "+"))), unique = FALSE)
  if (length(which(bare_vars == variable)) > 1) {
    which(colSums(facs[which(bare_vars == variable),]) > 0)
  } else {
    which(facs[which(bare_vars == variable),] > 0)
  }
}

#### Regex helper ############################################################

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}
