#' @title WBFormula class for within-between model formula representation
#' @description S3 class that represents a parsed within-between formula.
#'   This provides a structured intermediate representation between the 
#'   user-specified formula and the final lme4 formula.
#' @name WBFormula
#' @keywords internal
NULL

#' @title Create a WBFormula object
#' @description Constructor for the WBFormula S3 class.
#' @param raw_formula The original Formula object
#' @param dv The dependent variable name
#' @param varying Character vector of time-varying predictor terms
#' @param constants Character vector of time-invariant predictor terms
#' @param v_info Tibble with columns: term, root, lag, meanvar
#' @param wint_labs Character vector of within x within interaction labels
#' @param cint_labs Character vector of cross-level interaction labels
#' @param bint_labs Character vector of between x between interaction labels
#' @param ranefs Character vector of random effects specifications
#' @param data The data frame (with any expanded factors)
#' @param allvars Character vector of all variables needed (passed from parser)
#' @param conds Integer number of formula conditions/parts
#' @param matrix_terms Optional list of metadata for matrix-returning terms
#'   detected in the varying part of the formula (e.g., `splines::ns()`,
#'   `splines::bs()`, `stats::poly()`). These terms are expanded into
#'   within- and between- component columns during data preparation.
#' @return A WBFormula S3 object
#' @keywords internal
WBFormula <- function(raw_formula,
                      dv,
                      varying = character(0),
                      constants = character(0),
                      v_info = NULL,
                      wint_labs = NULL,
                      cint_labs = NULL,
                      bint_labs = NULL,
                      ranefs = NULL,
                      data = NULL,
                      allvars = NULL,
                      conds = NULL,
                      matrix_terms = NULL) {
  
  # Ensure v_info has the right structure
  if (is.null(v_info) && length(varying) > 0) {
    v_info <- tibble::tibble(
      term = varying,
      root = varying,
      lag = 0L,
      meanvar = paste0("imean(", varying, ")")
    )
  }
  
  # Calculate allvars only if not provided (for backward compatibility)
  if (is.null(allvars)) {
    ranef_vars <- if (!is.null(ranefs)) {
      extract_ranef_vars(ranefs, data)
    } else {
      character(0)
    }
    allvars <- unique(c(dv, varying, constants, ranef_vars))
  }
  
  # Count formula conditions (parts separated by |) if not provided
  if (is.null(conds)) {
    conds <- if (inherits(raw_formula, "Formula")) {
      length(raw_formula)[2]
    } else {
      1L
    }
  }
  
  structure(
    list(
      raw_formula = raw_formula,
      dv = dv,
      varying = varying,
      constants = constants,
      v_info = v_info,
      wint_labs = wint_labs,
      cint_labs = cint_labs,
      bint_labs = bint_labs,
      ranefs = ranefs,
      data = data,
      # Computed fields for backward compatibility
      allvars = allvars,
      conds = conds,
      meanvars = if (!is.null(v_info)) v_info$meanvar else character(0),
      # Matrix/basis terms (splines, poly, etc.)
      matrix_terms = matrix_terms
    ),
    class = "WBFormula"
  )
}

#' @title Extract variables from random effects terms
#' @description Helper to extract variable names from random effects specifications
#' @param ranefs Character vector of random effects terms
#' @param data Data frame to check for variable existence
#' @return Character vector of variable names
#' @keywords internal
extract_ranef_vars <- function(ranefs, data) {
  if (is.null(ranefs)) return(character(0))
  
  vars <- c()
  for (ranef in ranefs) {
    splitted <- stringr::str_split(ranef, "\\| |\\|\\|")
    if (length(splitted[[1]]) >= 1) {
      lhs <- trimws(splitted[[1]][[1]], whitespace = "\\(")
      if (lhs != "1") {
        form <- tryCatch(
          as.formula(paste("~", lhs)),
          error = function(e) NULL
        )
        if (!is.null(form)) {
          vars <- c(vars, all.vars(form))
        }
      }
    }
    if (length(splitted[[1]]) >= 2) {
      rhs <- trimws(splitted[[1]][[2]], whitespace = "\\)")
      vars <- c(vars, rhs)
    }
  }
  unique(vars)
}

#' @title Check if WBFormula has interactions
#' @param x A WBFormula object
#' @return Logical indicating if any interactions are present
#' @keywords internal
has_interactions <- function(x) {
  UseMethod("has_interactions")
}

#' @export
has_interactions.WBFormula <- function(x) {
  !is.null(x$wint_labs) || !is.null(x$cint_labs) || !is.null(x$bint_labs)
}

#' @title Get all interaction labels from WBFormula
#' @param x A WBFormula object
#' @param type One of "all", "within", "cross", or "between"
#' @return Character vector of interaction labels
#' @keywords internal
get_interactions.WBFormula <- function(x, type = c("all", "within", "cross", "between")) {
  type <- match.arg(type)
  switch(type,
    "all" = c(x$wint_labs, x$cint_labs, x$bint_labs),
    "within" = x$wint_labs,
    "cross" = x$cint_labs,
    "between" = x$bint_labs
  )
}

#' @title Check if a variable is time-varying in WBFormula
#' @param x A WBFormula object
#' @param var Variable name to check
#' @return Logical
#' @keywords internal
is_varying_term <- function(x, var) {
  UseMethod("is_varying_term")
}

#' @export
is_varying_term.WBFormula <- function(x, var) {
  un_bt(var) %in% un_bt(x$varying)
}

#' @title Get mean variable name for a term
#' @param x A WBFormula object
#' @param term The term to look up
#' @return The mean variable name, or NULL if not found
#' @keywords internal
get_meanvar <- function(x, term) {
  UseMethod("get_meanvar")
}

#' @export
get_meanvar.WBFormula <- function(x, term) {
  if (is.null(x$v_info)) return(NULL)
  idx <- which(un_bt(x$v_info$term) == un_bt(term))
  if (length(idx) == 0) return(NULL)
  x$v_info$meanvar[idx[1]]
}

#' @title Print method for WBFormula
#' @param x A WBFormula object
#' @param ... Additional arguments (ignored)
#' @export
print.WBFormula <- function(x, ...) {
  cat("WBFormula object\n")
  cat("  DV:", x$dv, "\n")
  cat("  Varying:", length(x$varying), "terms\n")
  if (length(x$varying) > 0) {
    cat("    ", paste(utils::head(x$varying, 5), collapse = ", "))
    if (length(x$varying) > 5) cat(" ...")
    cat("\n")
  }
  cat("  Constants:", length(x$constants), "terms\n")
  if (length(x$constants) > 0) {
    cat("    ", paste(utils::head(x$constants, 5), collapse = ", "))
    if (length(x$constants) > 5) cat(" ...")
    cat("\n")
  }
  cat("  Interactions:\n")
  cat("    Within x Within:", length(x$wint_labs), "\n")
  cat("    Cross-level:", length(x$cint_labs), "\n")
  cat("    Between x Between:", length(x$bint_labs), "\n")
  if (!is.null(x$ranefs)) {
    cat("  Random effects:", length(x$ranefs), "terms\n")
  }
  invisible(x)
}

#' @title Convert WBFormula to list (for backward compatibility)
#' @param x A WBFormula object
#' @return A list with the same structure as wb_formula_parser() output
#' @keywords internal
as_parser_list <- function(x) {
  UseMethod("as_parser_list")
}

#' @export
as_parser_list.WBFormula <- function(x) {
  list(
    conds = x$conds,
    allvars = x$allvars,
    varying = x$varying,
    constants = x$constants,
    v_info = x$v_info,
    data = x$data,
    wint_labs = x$wint_labs,
    cint_labs = x$cint_labs,
    bint_labs = x$bint_labs,
    ranefs = x$ranefs,
    meanvars = x$meanvars
  )
}

#' @title Create WBFormula from parser output (for migration)
#' @param pf List output from wb_formula_parser()
#' @param formula The original Formula object
#' @param dv The dependent variable name
#' @return A WBFormula object
#' @keywords internal
WBFormula_from_parser <- function(pf, formula, dv) {
  WBFormula(
    raw_formula = formula,
    dv = dv,
    varying = pf$varying,
    constants = pf$constants,
    v_info = pf$v_info,
    wint_labs = pf$wint_labs,
    cint_labs = pf$cint_labs,
    bint_labs = pf$bint_labs,
    ranefs = pf$ranefs,
    data = pf$data
  )
}
