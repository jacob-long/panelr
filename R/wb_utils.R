#' @importFrom stats as.formula terms
#' @import stringr

wb_formula_parser <- function(formula, dv) {

  conds <- sum(attr(gregexpr("\\|", formula)[[1]], "match.length"))

  if (conds == -1L) {conds <- 0}

  if (conds == 0) {

    constants <- NULL
    constants_form <- NULL

    varying <- stringr::str_split(formula, "\\+")
    varying <- stringr::str_split(unlist(varying), "\\*")
    varying <- stringr::str_split(unlist(varying), "\\:")
    varying <- unlist(lapply(varying, trimws))

    varying_form <- formula

    allvars <- c(dv, varying)

  } else if (conds > 0) {

    splitted <- stringr::str_split(formula, "\\|")

    varying <- stringr::str_split(splitted[[1]][1], "\\+")
    varying <- stringr::str_split(unlist(varying), "\\*")
    varying <- stringr::str_split(unlist(varying), "\\:")
    varying <- unlist(lapply(varying, trimws))

    varying_form <- splitted[[1]][1]

    constants <- stringr::str_split(splitted[[1]][2], "\\+")
    constants <- stringr::str_split(unlist(constants), "\\*")
    constants <- stringr::str_split(unlist(constants), "\\:")
    constants <- unlist(lapply(constants, trimws))

    ## Deal with number given as variable
    con_nums <- suppressWarnings(as.numeric(constants))
    constants <- constants[is.na(con_nums)]

    constants_form <- splitted[[1]][2]

    allvars <- c(dv, varying, constants)

  }

  if (conds >= 2) {

    if (length(splitted[[1]]) == 3) {
      fin_splitted <- splitted[[1]][3]
    } else {
      fin_splitted <-
        paste0(splitted[[1]][3:(conds + 1)], collapse = "|")
    }

    cross_ints_form <- fin_splitted

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

  # Set all the mean var names to mean(var)
  meanvars <- paste0("imean(", meanvars, ")")
  # Use this for matching varying vars to their parents
  names(meanvars) <- varying

  out <- list(conds = conds, allvars = allvars, varying = varying,
              varying_form = varying_form, constants = constants,
              constants_form = constants_form,
              cross_ints_form = cross_ints_form, meanvars = meanvars)
  return(out)

}

wb_model <- function(model, pf, dv, data, dynamic = FALSE) {

  # Create empty stab terms vector so I can pass it along even for other
  # models
  stab_terms <- c()

  # models that require de-meaning
  within_family <- c("w-b","within-between","within","stability","fixed")

  # De-mean varying vars if needed
  if (model %in% within_family) { # within models

    # Iterate through the varying variables
    for (v in pf$varying) {
      # De-mean
      data[v] <- data[v] - data[pf$meanvars[v]]

    }

    if (dynamic == TRUE) {
      lagdv <- paste0("lag(", dv, ")")
      dvmean <- paste0("imean(", dv, ")")
      data[lagdv] <- data[lagdv] - data[dvmean]
    }

  }

  # Create extra piece of formula based on model
  if (model %in% c("w-b","within-between","contextual")) {
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
      add_form <- paste(add_form, "+", pf$meanvars[v], "* wave")
      stab_terms <- c(stab_terms, paste(pf$meanvars[v], ":wave", sep = ""))
    }


  } else if (model %in% c("between","random")) {

    # It doesn't need anything special
    add_form <- ""

  }

  # Put the pieces together
  fin_formula <- paste(dv, "~", add_form, "+", pf$varying_form)
  if (pf$conds == 1) {
    fin_formula <- paste(fin_formula, "+", pf$constants_form)
  } else if (pf$conds >= 2) {
    fin_formula <- paste(fin_formula, "+", pf$constants_form, "+",
                         pf$cross_ints_form)
  }

  out <- list(data = data, fin_formula = fin_formula,
              stab_terms = stab_terms)
  return(out)

}

formula_ticks <- function(formula, vars) {

  for (var in vars) {

    regex_pattern <- paste0("(?<=(~|\\s|\\*|\\+))", escapeRegex(var),
                           "(?=($|~|\\s|\\*|\\+))")
    backtick_name <- paste("`", var, "`", sep = "")
    formula <- gsub(regex_pattern, backtick_name, formula, perl = T)

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


