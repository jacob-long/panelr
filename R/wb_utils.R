#' @importFrom stats as.formula terms
#' @import stringr

wb_formula_parser <- function(formula, dv, data, force.constants = TRUE) {
  # See how many parts the formula has 
  conds <- length(formula)[2]
  
  # These are NULL unless user specifies random effects
  ranefs <- NULL
  ranef_forms <- NULL
  ranef_vars <- NULL
  grouping_vars <- NULL

  # Need to deal with custom random effects
  if (conds >= 3) { 
    # Capturing those and putting them in a list
    ranefs <- fb(get_rhs(formula, which = 3, to.formula = FALSE))
    if (!is.null(ranefs)) {
      # Convert them to strings
      ranefs <- if (is.list(ranefs)) sapply(ranefs, to_char) else to_char(ranefs)
      ranefs <- stringr::str_replace_all(ranefs, "~", "")
      # Now I need to know which are the grouping vars
      grouping_vars <- stringr::str_split(ranefs, "\\| | \\|\\|")
      grouping_vars <- sapply(grouping_vars, 
                              function(x) stringr::str_trim(x[[2]]))
    }
  }

  # Deal with non-numeric variables
  if (any(!sapply(all.vars(formula), function(x) is.numeric(data[[x]])))) {
    if (conds >= 3) {
      # Remove ranefs from formula for now
      attr(formula, "rhs")[[3]] <- reformulas::nobars(attr(formula, "rhs")[[3]])
    }
    # Find the time-varying non-numeric vars 
    vars <- 
      names(sapply(all.vars(formula),
                   function(x) is.numeric(data[[x]])) %just% FALSE)
    vars <- vars %not% grouping_vars
    # Expand these factors into 0/1 variables in the data
    data <- expand_factors(vars, data)
    # Now create a formula that does the same
    for (var in vars) {
      forms <- expand_formula(formula, var, data)
      forms <- paste(sapply(forms, to_char), collapse = "|")
      new_form <- Formula::Formula(as.formula(paste("~", forms)))
      attr(formula, "rhs") <- attr(new_form, "rhs")
    }
    # I handle factors in the random effects terms separately
    if (!is.null(ranefs)) {
      ranef_forms <- lapply(ranefs, function(x) {
        # Split into left-hand and right-hand side
        splitted <- lapply(stringr::str_split(x, "\\| |\\|\\|"), trimws)
        lhs <- splitted[[1]][[1]]
        rhs <- splitted[[1]][[2]]
        # Convert LHS to formula
        lhs_form <- Formula::Formula(as.formula(paste("~", lhs)))
        # Capture whether user asked to suppress intercept for this random effect
        intercept <- attr(terms(lhs_form), "intercept") == 1
        # Find the non-numeric vars 
        vars <- names(sapply(all.vars(lhs_form), 
                             function(x) is.numeric(data[[x]])) %just% FALSE)
        # Now just loop through and do like I did with the main part of the 
        # formula
        for (var in vars) {
          lhs_form <- expand_formula(lhs_form, var, data)
          # Need to convert back to formula so the input to expand_formula() is
          # consistent on subsequent loops (if any)
          lhs_form <- Formula::Formula(as.formula(paste("~", lhs_form)))
        } 
        lhs <- to_char(lhs_form[[2]])
        # Add the +0 back if needed
        if (!intercept) lhs <- paste0(lhs, " + 0")
        # Convert back to a string format with expanded factors, if any
        paste0("(", lhs, ifelse(stringr::str_detect(x, "\\|\\|"),
                                yes = " || ", no = " | "),  
               rhs, ")")
      })
      ranef_vars <-  c(sapply(ranef_forms, function(x) {
        splitted <- lapply(stringr::str_split(x, "\\| |\\|\\|"), trimws)
        lhs <- trimws(splitted[[1]][[1]], whitespace = "\\(")
        lhs_form <- Formula::Formula(as.formula(paste("~", lhs)))
        rhs <- trimws(splitted[[1]][[2]], whitespace = "\\)")
        form <- as.formula(paste("~", lhs, " + ", rhs))
        attr(terms(form), "term.labels")
      }))
      ranef_vars <- sapply(ranef_vars, function(x) {
        if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
      })
    }
  }
  # Save varying variables
  varying <- sapply(get_term_labels(formula), function(x) {
    # If non-syntactic names are inside functions, retain backticks
    if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
  })

  if (conds == 1) {
    # There are no constants
    constants <- NULL
  } else if (conds > 1) {
    # Save constants
    constants <- sapply(get_term_labels(formula, which = 2), function(x) {
      # If non-syntactic names are inside functions, retain backticks
      if (make.names(x) != x & x %in% names(data)) un_bt(x) else x
    })
  }
  
  # Try to check for non-varying variables in varying part of formula
  if (force.constants == TRUE) {
    for (var in varying %just% names(data)) {
      if (!(are_varying(data, !! sym(var)))) {
        varying <- varying %not% var
        constants <- c(constants, var)
        msg_wrap(var, " was included in the time-varying part of the formula 
                 but does not vary over time. It is being treated as a constant
                 instead.")
      }
    }
  }
  
  # Retain list of all variables to go into final model
  allvars <- unique(c(dv, varying, constants, ranef_vars))

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
  }

  if (!is.null(ranef_forms)) {
    new_3 <- paste(ranef_forms, collapse = " + ")
    new_3 <- 
      paste("~", to_char(get_rhs(formula, which = 3)), "+", new_3)
    attr(formula, "rhs")[[3]] <- as.formula(new_3)[[2]]
  }

  # Now I want to expand all interactions into their constituent terms in 
  # a conventional, one-part formula so I can deal with complex interactions
  # that may involve redundant variable pairings across formula parts.
  if (any_interaction(formula)) {
    formula <- expand_interactions(formula)
  }
  
  # Within by within interactions
  if (any_interaction(formula)) {
    wint_labs <- sapply(get_interactions(formula), function(x) {
      if (all(x %in% varying)) paste(bt(x), collapse = "*") else NULL
    })
  } else {wint_labs <- NULL}
  if (!is.null(wint_labs)) {
    wint_labs <- wint_labs[!sapply(wint_labs, is.null)]
  }
  
  # Between by between interactions
  if (any_interaction(formula)) {
    bint_labs <- sapply(get_interactions(formula), function(x) {
      if (all(x %nin% varying)) paste(bt(x), collapse = ":") else NULL
    })
  } else {bint_labs <- NULL}
  if (!is.null(bint_labs)) {
    bint_labs <- bint_labs[!sapply(bint_labs, is.null)]
  }
  
  # Cross-level interactions
  if (any_interaction(formula)) {
    cint_labs <- sapply(get_interactions(formula), function(x) {
      # Looking for mix of within and between vars in an interaction
      if (any(x %in% varying) & any(x %nin% varying)) {
        paste(bt(x), collapse = "*") 
      } else NULL
    })
  } else {cint_labs <- NULL}
  if (!is.null(cint_labs)) {
    cint_labs <- cint_labs[!sapply(cint_labs, is.null)]
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
              constants = constants, v_info = v_info,
              data = data, wint_labs = wint_labs, cint_labs = cint_labs,
              bint_labs = bint_labs, ranefs = ranef_forms,
              meanvars = v_info$meanvar)
  return(out)

}

prepare_lme4_formula <- function(formula, pf, data, use.wave, wave, id, ...) {
  # Append fixed wave to formula if requested
  if (use.wave == TRUE) {
    formula <- paste(formula, "+", wave)
  }
  # By default, assume no random effects have been added
  add <- FALSE
  
  # See if the formula has 3 parts
  if (pf$conds > 2) {
    # See if there are any random effects specified
    res <- reformulas::findbars(as.formula(formula))
    # If there are, let's deal with them
    if (!is.null(res)) {
      # Get info on those random effects
      refs <- reformulas::mkReTrms(res, data)$cnms
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

  as.formula(formula)
}

#' @importFrom stats resid lm coef na.exclude update

detrend <- function(data, pf, dt_order, balance_correction, dt_random) {
    
  # save id and wave 
  id <- get_id(data)
  wave <- get_wave(data)
  periods <- length(get_periods(data))
  # If random slopes, nest the data
  if (dt_random == TRUE) {
    # Nest the data for efficient fitting of the lms
    data <- tidyr::nest(data)
  }
    
  # Define detrending function
  dt_model <- function(data, var, order = dt_order) {
    
    varbt <- bt(var)
    if (is.numeric(data[[wave]])) {
      the_formula <- as.formula(paste(varbt, "~ poly(", wave, ",", order,
                                      ", raw = TRUE)"))
    } else {
      the_formula <- as.formula(paste(varbt, "~", wave))
    }
    tryCatch({
      resid(lm(formula = the_formula, data = data, na.action = na.exclude))
    }, error = function(x) {rep(NA, times = nrow(data))})
      
  }
      
  # Define between-person function
  b_model <- function(data, var, order = dt_order, bc = balance_correction) {
      
    var <- bt(var)
    if (bc == TRUE) {  
      
      varbt <- bt(var)
      if (is.numeric(data[[wave]])) {
        the_formula <- as.formula(paste(varbt, "~ poly(", wave, ",", order,
                                        ", raw = TRUE)"))
      } else {
        the_formula <- as.formula(paste(varbt, "~", wave))
      }
      out <- tryCatch({
        coef(mod <- lm(formula = the_formula, data = data,
                       na.action = na.exclude))["(Intercept)"]
      }, error = function(x) {NA})
      rep(out, times = nrow(data))
      
    } else {
    
      the_formula <- as.formula(paste(var, "~ 1"))
      out <- tryCatch({
        coef(mod <- lm(formula = the_formula, data = data))["(Intercept)"]
      }, error = function(x) {NA})
      rep(out, times = nrow(data))
    }
    
  }
  # Avoid redundant mean variables when multiple lags of the same variable
  # are included... e.g., imean(lag(x)) and imean(x). I want whichever is 
  # the most recent (or covering the most waves in the case of there being
  # leads)
  v_info <- set_meanvars(pf, return.subset = TRUE)
  if (periods - max(v_info$lag) <= 2) {
    stop_wrap("'detrend' cannot be used with only two waves of data for
              any of the variables.")
  }
  if (!is.null(pf$cint_labs) | !is.null(pf$wint_labs)) {
    ints <- c(pf$cint_labs, pf$wint_labs)
    new_tab <- tibble::tibble(
      term = un_bt(names(ints)), root = un_bt(names(ints)), 
      lag = 0, meanvar = paste0("imean(", bt(un_bt(names(ints))), ")")
    )
    v_info <- dplyr::bind_rows(v_info, new_tab)
    pf$v_info <- dplyr::bind_rows(pf$v_info, new_tab)
  }
  
  if (dt_random == TRUE) {
    # Iterate through the varying variables
    for (v in v_info$term) {
          
      # De-trend
      # Note that b_model differs depending on balance_correction 
      mean_var <- pf$v_info$meanvar[pf$v_info$term == v]
      data <- dplyr::mutate(data,
                             !! un_bt(mean_var) :=
                               purrr::map(data, b_model, var = v),
                             !! un_bt(v) :=
                               purrr::map(data, dt_model, var = v)
      )
        
    }
  } else {
    # Iterate through the varying variables
    for (v in v_info$term) {
      data <- panel_data(dplyr::mutate(unpanel(data),
                                       !! un_bt(v) := dt_model(data, var = v)
      ), !! sym(id), !! sym(wave))
      if (balance_correction == FALSE) {
        mean_var <- pf$v_info$meanvar[pf$v_info$term == v]
        v_term <- paste(bt(un_bt(v)), "-", bt(un_bt(mean_var)))
        data <- dplyr::mutate(data, 
                              !! un_bt(mean_var) :=
                                !! parse_expr(un_bt(mean_var)),
                              !! un_bt(v) := !! parse_expr(v_term))
      }
    }
    
    if (balance_correction == TRUE) {
      for (v in v_info$term) {
        # Need to nest the data
        data <- tidyr::nest(data)
        mean_var <- pf$v_info$meanvar[pf$v_info$term == v]
        v_term <- paste(bt(un_bt(v)), "-", bt(un_bt(mean_var)))
        data <- dplyr::mutate(data, 
                              !! un_bt(mean_var) := 
                                purrr::map(data, b_model, var = v)
        )
        data <- panel_data(
          tidyr::unnest(data, cols = names(data) %not%  c(id, wave),
                        names_repair = tidyr::tidyr_legacy),
          id = !! sym(id), wave = !! sym(wave)
        )
        data <- dplyr::mutate(data,  !! un_bt(v) := !! parse_expr(v_term))
      }
    } 
  }
  
  if (dt_random == TRUE | balance_correction == TRUE) {
    # Unnest the data if it was nested
    data <- tidyr::unnest(data, cols = names(data) %not% c(id, wave),
                          names_repair = tidyr::tidyr_legacy)
    data <- panel_data(data, id = !! sym(id), wave = !! sym(wave))
    # The unnesting process generates extra nuisance variables for those
    # included in the detrending (e.g., x1). I add "-" to tell select to
    # drop them
    drop_vars <- paste0(c(v_info$term, v_info$meanvar), "1")
    drop_vars <- drop_vars %just% names(data)
    if (length(drop_vars) > 0) {
      drop_vars <- paste0(
        '!contains(c(', paste0('"', drop_vars, '"', collapse = ", "), '))'
      )
      data <- select(data, !!! parse_exprs(drop_vars))
    }
  }
  
  # See if there were any multi-lagged vars
  if (any(dim(pf$v_info) != dim(v_info))) {
    for (term in pf$v_info$term %not% v_info$term) {
      other_terms <-
        which(pf$v_info$root == pf$v_info$root[pf$v_info$term == term])
      other_terms <- other_terms %not% which(pf$v_info$term == term)
      others <- pf$v_info[other_terms,]
      if (any(others$lag == 0)) {
        term_expr <- term
      } else {
        low_term <- others$term[which(others$lag == min(others$lag))]
        new_lag <- pf$v_info$lag[pf$v_info$term == term] - 
          others$lag[others$term == low_term]
        term_expr <- paste0("lag(`", low_term, "`, n = ", new_lag, ")")
      }
      data <- mutate(data, 
                     # I need to get the lag of the *new* root variable
                     !! term := !! parse_expr(term_expr)
      )
    }
  }
  return(data)
  
}

formula_ticks <- function(formula, vars) {

  for (var in vars) {
    # Trying to isolate the variables from synthetic terms in the formula.
    # Can't have variable width lookbehinds so I say up to 3 open parentheses
    # that are preceded by whitespace (as opposed to the I function or 
    # something).
    regex_pattern <- paste0(
      "(?<=(~|\\s|\\*|\\+|\\:)|\\s\\(|\\s\\(\\(|\\s\\(\\(\\(\\(|^)",
      escapeRegex(var),
      "(?=(~|$|\\s|\\*\\S*[^`]|\\+\\S*[^`]|\\:\\S*[^`])|\\)[^`])")
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
  if (!is.null(x)) {
    btv <- paste0("`", x, "`")
    btv <- gsub("``", "`", btv, fixed = TRUE)
    btv <- btv %not% c("", "`")
  } else btv <- NULL
  return(btv)
}

un_bt <- function(x) {
  gsub("`", "", x)
}

bt_ranefs <- function(ranefs, data) {
  ranef_forms <- lapply(ranefs, function(x) {
    # Split into left-hand and right-hand side
    splitted <- stringr::str_split(x, "\\| |\\|\\|")
    lhs <- trimws(splitted[[1]][[1]], whitespace = "\\(")
    rhs <- trimws(splitted[[1]][[2]], whitespace = "\\)")
    # Convert LHS to formula
    lhs_form <- Formula::Formula(as.formula(paste("~", lhs)))
    if (deparse(lhs_form) != "~1") {
      intercept <- attr(terms(lhs_form), "intercept") == 1
      term.labs <- attr(terms(lhs_form), "term.labels")
      term.labs <- sapply(term.labs, function(y) {
        if (y %in% names(data)) bt(y) else y
      })
      lhs_form <- reformulate(term.labs)[[2]]
    } else {lhs_form <- (~1)[[2]]}
    lhs <- to_char(lhs_form)
    # to_char drops ticks if there's only one term
    if (lhs != "1" && 
        length(attr(terms(reformulate(term.labs)), "term.labels")) < 2) {
      lhs <- bt(lhs)
    }
    if (!intercept) lhs <- paste0(lhs, " + 0")
    # Convert back to a string format with expanded factors, if any
    paste0("(", lhs, ifelse(stringr::str_detect(x, "\\|\\|"),
                            yes = "||", no = "|"),  
           rhs, ")")
  })
  paste(ranef_forms, collapse = " + ")
}

# Modified version of helper function inside reformulas::findbars
fb <- function(term) {
  if (is.name(term) || !is.language(term)) 
    return(NULL)
  if (term[[1]] == as.name("(")) 
    return(fb(term[[2]]))
  stopifnot(is.call(term))
  if (term[[1]] == as.name("|") | term[[1]] == as.name("||")) 
    return(term)
  if (length(term) == 2) 
    return(fb(term[[2]]))
  c(fb(term[[2]]), fb(term[[3]]))
}

# Accessor function for Formula objects that have multiple parts
get_rhs <- function(x, which = 1, to.formula = FALSE) {
  # Coercing to formula can be useful, otherwise it's a call object
  if (to.formula == TRUE) {
    the_str <- paste("~", deparse(attr(x, "rhs")[[which]]), collapse = " ")
    as.formula(gsub("\\n", "", the_str, fixed = TRUE))
  } else {
    attr(x, "rhs")[[which]]
  }
}

# automate tedious call to character conversion
to_char <- function(x) {
  paste(deparse(x), collapse = "")
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
  # Detect random effects specifications
  if (any(stringr::str_detect(labs, "\\|"))) {
    labs[stringr::str_detect(labs, "\\|")] <- 
      paste0("(", labs[stringr::str_detect(labs, "\\|")], ")")
  }
  return(labs)
}

# Generate the labels for factors that R normally does already
expand_labels <- function(data, variable) {
  paste0(variable, unique(
    data[[variable]][complete.cases(data[[variable]])] %not%
      base_level(data[[variable]]))
  )
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
  out <- list()
  for (i in 1:length(formula)[length(length(formula))]) {
    tmp_form <- get_rhs(formula, which = i, to.formula = TRUE)
    if (!all(deparse(tmp_form) == "~1") && variable %in% all.vars(tmp_form)) {
      # Get terms that don't have anything to do with variable
      num_matches <- length(which_terms(tmp_form, variable))
      if (num_matches > 0 && num_matches != length(labels(terms(tmp_form)))) {
        o_terms <- labels(
          drop.terms(terms(tmp_form), which_terms(tmp_form, variable))
        )
        # Get vector of term labels for all terms that involve variable
        labs <- make_labels(tmp_form, variable, data)
      } else if (num_matches == 0) {
        o_terms <- labels(terms(tmp_form))
        labs <- NULL
      } else {
        o_terms <- NULL
        labs <- make_labels(tmp_form, variable, data)
      }
      # Use base R's reformulate function to make a new formula using these 
      # character objects
      out <- c(out, reformulate(c(o_terms, labs)))
    } else if (all(deparse(tmp_form) == "~1")) {
      out <- c(out, reformulate("1"))
    } else {
      out <- c(out, reformulate(labels(terms(tmp_form))))
    }
  }
  return(lapply(out, function(x) x[[2]]))
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

# Taken from interactions pkg

any_interaction <- function(formula) {
  any(attr(terms(formula), "order") > 1)
}

get_interactions <- function(formula) {
  if (any_interaction(formula)) {
    ts <- terms(formula, keep.order = TRUE)
    labs <- paste("~", attr(ts, "term.labels"))
    forms <- lapply(labs, as.formula)
    forms <- forms[which(attr(ts, "order") > 1)]
    ints <- lapply(forms, function(x) {
      rownames(attr(terms(x), "factors"))
    })
    names(ints) <- attr(ts, "term.labels")[which(attr(ts, "order") > 1)]
    return(ints)
  } else {
    NULL
  }
}

expand_interactions <- function(x) {
  ranefs <- if (length(x)[2] >= 3) {
    fb(get_rhs(x, which = 3, to.formula = FALSE))
  } else NULL
  if (!is.null(ranefs)) {
    ranefs <- if (is.list(ranefs)) sapply(ranefs, to_char) else to_char(ranefs)
    ranefs <- stringr::str_replace_all(ranefs, "~", "")
  }
  if (length(attr(x, "rhs")) >= 3) {
    # Remove ranefs from formula for now
    attr(x, "rhs")[[3]] <- reformulas::nobars(attr(x, "rhs")[[3]])
  }
  
  for (i in seq_along(attr(x, "rhs"))) {
    if (to_char(get_rhs(x, i, TRUE)) %nin% c("~ 1", "~1")) {
      attr(x, "rhs")[[i]] <- 
        reformulate(unique(attr(terms(get_rhs(x, i, TRUE), keep.order = TRUE),
                                "term.labels")))[[2]]
    }
  }
  
  # I handle factors in the random effects terms separately
  if (!is.null(ranefs)) {
    ranef_forms <- lapply(ranefs, function(y) {
      splitted <- stringr::str_split(y, "\\| | \\|\\|")
      lhs <- splitted[[1]][[1]]
      rhs <- splitted[[1]][[2]]
      lhs_form <- Formula::Formula(as.formula(paste("~", lhs)))
      if (to_char(lhs_form) != "~1") {
        lhs_form <- reformulate(unique(attr(terms(lhs_form, keep.order = TRUE),
                                            "term.labels")))
        lhs <- to_char(lhs_form[[2]])
      } 
      paste0("(", lhs, ifelse(stringr::str_detect(y, "\\|\\|"),
                              yes = "||", no = "|"), rhs, ")")
    })
    ranef_forms <- paste(ranef_forms, collapse = " + ")
    new_3 <- 
      paste("~", to_char(get_rhs(x, which = 3)), "+", ranef_forms)
    attr(x, "rhs")[[3]] <- as.formula(new_3)[[2]]
  }
  x
}

set_meanvars <- function(pf, return.subset = FALSE) {
  # for CRAN getting confused by NSE
  term <- root <- NULL
  v_subset <- pf$v_info
  # If no duplicate root terms, nothing to do here
  if (any(duplicated(v_subset$root))) {
    # Get the variables with more than one instance
    multi_vars <- names(which(table(v_subset$root) > 1))
    # Loop through them
    for (var in multi_vars) {
      if (any(v_subset$term == var)) { # that means no lag
        # Drop the others
        v_subset <- dplyr::filter(v_subset, term == !! var | root != !! var)
      } else { # find the minimum lag
        min_lag <- min(dplyr::filter(v_subset, root == !!var)$lag)
        # Drop the others
        v_subset <- dplyr::filter(v_subset, root != !! var | 
                             (root == !! var & lag == !! min_lag))
      }
      # Now assign this mean variable to all instances of that root term in
      # the original d.f.
      pf$v_info$meanvar[pf$v_info$root == var] <- 
        v_subset$meanvar[v_subset$root == var]
    }
    if (return.subset == TRUE) return(v_subset) else return(pf$v_info)
  } else {return(pf$v_info)}
}

#### Regex helper ############################################################

# Taken from Hmisc
escapeRegex <- function(string) {
  gsub('([.|()\\^{}+$*?]|\\[|\\])', '\\\\\\1', string)
}

### Use this to anticipate if lmerModTest will fail
check_lmerModTest <- function(model) {
  if (!inherits(model, "lmerMod")) 
    stop("model not of class 'lmerMod': cannot coerce to class 'lmerModLmerTest")
  mc <- getCall(model)
  args <- c(as.list(mc), devFunOnly = TRUE)
  if (!"control" %in% names(as.list(mc))) 
    args$control <- lme4::lmerControl(check.rankX = "silent.drop.cols")
  Call <- as.call(c(list(quote(lme4::lmer)), args[-1]))
  ff <- environment(formula(model))
  pf <- parent.frame()
  sf <- sys.frames()[[1]]
  ff2 <- environment(model)
  devfun <- tryCatch(eval(Call, envir = pf), error = function(e) {
    tryCatch(eval(Call, envir = ff), error = function(e) {
      tryCatch(eval(Call, envir = ff2), error = function(e) {
        tryCatch(eval(Call, envir = sf), error = function(e) {
          "error"
        })
      })
    })
  })
  if ((is.character(devfun) && devfun == "error") || !is.function(devfun) || 
      names(formals(devfun)[1]) != "theta") 
    TRUE
  else FALSE
}
