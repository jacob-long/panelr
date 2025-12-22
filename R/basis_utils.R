#' @title Utilities for handling basis expansion functions in formulas
#' @description Helper functions for detecting and processing matrix-returning
#'   transformations like ns(), bs(), and poly() in within-between formulas.
#' @name basis_utils
#' @keywords internal
NULL

#' Registry of known basis functions and their reproducible attributes
#' @keywords internal
basis_function_registry <- list(

  ns = list(
    package = "splines",
    attrs = c("knots", "Boundary.knots", "intercept"),
    # Arguments that should be preserved when reconstructing the call
    preserve_args = c("df", "knots", "intercept", "Boundary.knots")
  ),
  bs = list(
    package = "splines",
    attrs = c("knots", "Boundary.knots", "intercept", "degree"),
    preserve_args = c("df", "knots", "degree", "intercept", "Boundary.knots")
  ),
  poly = list(
    package = "stats",
    # Note: 'degree' attribute is 1:n vector, but argument expects scalar
    # So we don't extract degree as an attr - preserve from original call only
    attrs = c("coefs"),
    preserve_args = c("degree", "coefs", "raw", "simple")
  )
)

#' Check if a term returns a matrix when evaluated
#' 
#' @param term Character string representing a formula term
#' @param data Data frame to evaluate against (should be ungrouped)
#' @return Logical indicating if the term returns a matrix
#' @keywords internal
is_matrix_term <- function(term, data) {
  # Ensure data is ungrouped for evaluation
  data <- dplyr::ungroup(data)
  
  tryCatch({
    result <- eval(parse(text = term), envir = data)
    is.matrix(result) || (is.array(result) && length(dim(result)) == 2) ||
      (!is.null(ncol(result)) && ncol(result) > 1)
  }, error = function(e) {
    FALSE
  })
}

#' Extract the function name from a formula term
#' 
#' @param term Character string representing a formula term like "ns(age, df=3)"
#' @return The function name (e.g., "ns") or NULL if not a function call
#' @keywords internal
extract_fn_name <- function(term) {
  # Parse the term and get the call
  parsed <- tryCatch(parse(text = term)[[1]], error = function(e) NULL)
  if (is.null(parsed) || !is.call(parsed)) {
    return(NULL)
  }
  as.character(parsed[[1]])
}

#' Check if a function is a known basis function
#' 
#' @param fn_name Character string of function name
#' @return Logical
#' @keywords internal
is_known_basis_fn <- function(fn_name) {
  fn_name %in% names(basis_function_registry)
}

#' Extract the primary variable from a basis function call
#' 
#' @param term Character string like "ns(age, df=3)" or "poly(x, degree=2)"
#' @return Character string of the variable name (e.g., "age", "x")
#' @keywords internal
extract_basis_variable <- function(term) {
  parsed <- tryCatch(parse(text = term)[[1]], error = function(e) NULL)
  if (is.null(parsed) || !is.call(parsed)) {
    return(NULL)
  }
  # The first argument is the variable (x argument)
  first_arg <- parsed[[2]]
  deparse(first_arg)
}

#' Evaluate a basis function on pooled data and extract attributes
#' 
#' @param term Character string like "ns(age, df=3)"
#' @param data Ungrouped data frame
#' @return List with components:
#'   - result: The evaluated matrix
#'   - attrs: Named list of reproducible attributes
#'   - ncol: Number of columns
#'   - fn_name: The function name
#'   - var_name: The primary variable name
#' @keywords internal
evaluate_basis_term <- function(term, data) {
  # Ensure ungrouped

  data <- dplyr::ungroup(data)
  
  fn_name <- extract_fn_name(term)
  var_name <- extract_basis_variable(term)
  
  # Evaluate on pooled data
  result <- eval(parse(text = term), envir = data)
  
  # Extract known attributes if this is a registered basis function
  attrs <- list()
  if (is_known_basis_fn(fn_name)) {
    attr_names <- basis_function_registry[[fn_name]]$attrs
    for (attr_name in attr_names) {
      attr_val <- attr(result, attr_name)
      if (!is.null(attr_val)) {
        attrs[[attr_name]] <- attr_val
      }
    }
  } else {
    # For unknown functions, try to extract all attributes
    all_attrs <- attributes(result)
    # Exclude standard attributes
    exclude <- c("dim", "dimnames", "class")
    attrs <- all_attrs[setdiff(names(all_attrs), exclude)]
  }
  
  list(
    result = result,
    attrs = attrs,
    ncol = ncol(result),
    fn_name = fn_name,
    var_name = var_name
  )
}

#' Reconstruct a basis function call with a modified variable
#'
#' @param term Original term like "ns(age, df=3)"
#' @param new_var New variable expression like "age - imean(age)"
#' @param attrs Optional list of attributes to add as arguments
#' @return Character string of the new call
#' @keywords internal
reconstruct_basis_call <- function(term, new_var, attrs = NULL) {
  parsed <- parse(text = term)[[1]]
  fn_name <- as.character(parsed[[1]])
  
  # Replace the first argument (the variable)
  parsed[[2]] <- parse(text = new_var)[[1]]
  
  # Add attributes as explicit arguments if provided
  if (!is.null(attrs) && length(attrs) > 0) {
    for (attr_name in names(attrs)) {
      attr_val <- attrs[[attr_name]]
      # Convert to R expression
      attr_expr <- NULL
      
      if (is.matrix(attr_val)) {
        # For poly coefs which is a matrix, skip it - poly uses its own method
        next
      } else if (is.numeric(attr_val) && length(attr_val) > 1) {
        # Vector: use c()
        attr_expr <- paste0("c(", paste(format(attr_val, digits = 15), collapse = ", "), ")")
      } else if (is.numeric(attr_val) && length(attr_val) == 1) {
        attr_expr <- format(attr_val, digits = 15)
      } else if (is.logical(attr_val) && length(attr_val) == 1) {
        attr_expr <- if (attr_val) "TRUE" else "FALSE"
      } else {
        # Skip complex attributes (NULL, functions, etc.)
        next
      }
      
      # Safely parse the expression
      if (!is.null(attr_expr) && nchar(attr_expr) > 0) {
        parsed_expr <- tryCatch(
          parse(text = attr_expr),
          error = function(e) NULL
        )
        if (!is.null(parsed_expr) && length(parsed_expr) > 0) {
          parsed[[attr_name]] <- parsed_expr[[1]]
        }
      }
    }
  }
  
  deparse(parsed, width.cutoff = 500)
}

#' Generate column names for expanded basis matrix
#' 
#' @param fn_name Function name (e.g., "ns")
#' @param var_name Variable name (e.g., "age")
#' @param ncol Number of columns
#' @param suffix Suffix for within ("w") or between ("b") 
#' @return Character vector of column names
#' @keywords internal
generate_basis_colnames <- function(fn_name, var_name, ncol, suffix = NULL) {
  base_name <- paste0(fn_name, "_", var_name)
  if (!is.null(suffix)) {
    paste0(base_name, ".", suffix, seq_len(ncol))
  } else {
    paste0(base_name, ".", seq_len(ncol))
  }
}

#' Expand a basis matrix into individual columns in a data frame
#' 
#' @param data Data frame to add columns to
#' @param mat Matrix to expand
#' @param colnames Character vector of column names
#' @return Data frame with added columns
#' @keywords internal
expand_basis_columns <- function(data, mat, colnames) {
  if (length(colnames) != ncol(mat)) {
    stop("Column names length must match matrix ncol")
  }
  for (i in seq_len(ncol(mat))) {
    data[[colnames[i]]] <- mat[, i]
  }
  data
}

#' Process a matrix term for within-between decomposition
#' 
#' This is the main entry point for handling basis functions in formulas.
#' It evaluates the term on pooled data, extracts attributes, and creates
#' both within and between versions of the term.
#' 
#' @param term Character string like "ns(age, df=3)"
#' @param data panel_data frame
#' @return List with:
#'   - data: Updated data frame with expanded columns
#'   - within_cols: Character vector of within column names
#'   - between_cols: Character vector of between column names
#'   - var_name: Original variable name
#'   - fn_name: Function name
#' @keywords internal
process_matrix_term <- function(term, data) {
  # Step 1: Evaluate on ungrouped (pooled) data
  basis_info <- evaluate_basis_term(term, data)
  
  fn_name <- basis_info$fn_name
  var_name <- basis_info$var_name
  ncol <- basis_info$ncol
  attrs <- basis_info$attrs
  
  # Step 2: Generate column names
  within_cols <- generate_basis_colnames(fn_name, var_name, ncol, "w")
  between_cols <- generate_basis_colnames(fn_name, var_name, ncol, "b")
  
  # Step 3: Construct the within and between calls
  # Within: fn(var - imean(var), ...)
  within_var <- paste0(var_name, " - imean(", var_name, ")")
  within_call <- reconstruct_basis_call(term, within_var, attrs)
  
  # Between: fn(imean(var), ...)
  between_var <- paste0("imean(", var_name, ")")
  between_call <- reconstruct_basis_call(term, between_var, attrs)
  
  # Step 4: Evaluate the new calls and expand into columns
  # For within: need to evaluate per-row (the imean will be computed by model_frame)
  # For between: similar
  
  # Actually, we can't evaluate these yet because imean() needs grouped context
  # We'll store the calls and let model_frame handle them
  
  list(
    term = term,
    fn_name = fn_name,
    var_name = var_name,
    ncol = ncol,
    attrs = attrs,
    within_call = within_call,
    between_call = between_call,
    within_cols = within_cols,
    between_cols = between_cols
  )
}

#' Check if any terms in a formula are matrix-returning
#'
#' @param terms Character vector of formula terms
#' @param data Data frame to evaluate against
#' @return Logical vector
#' @keywords internal
detect_matrix_terms <- function(terms, data) {
  sapply(terms, is_matrix_term, data = dplyr::ungroup(data))
}

#' Expand matrix terms into data columns
#'
#' This function takes the processed matrix_terms from wb_formula_parser
#' and creates the actual columns in the data frame for within and between
#' components.
#'
#' @param matrix_terms List of processed matrix term info
#' @param data panel_data frame
#' @return List with:
#'   - data: Updated data frame with expanded columns
#'   - within_cols: All within column names
#'   - between_cols: All between column names
#' @keywords internal
expand_matrix_terms_in_data <- function(matrix_terms, data) {
  # Get id and wave for panel_data
  id_var <- get_id(data)
  wave_var <- get_wave(data)
  
  # Ensure we work with ungrouped data for pooled evaluation
  data_ungrouped <- dplyr::ungroup(data)
  
  all_within_cols <- c()
  all_between_cols <- c()
  
  for (term_name in names(matrix_terms)) {
    term_info <- matrix_terms[[term_name]]
    
    # Step 1: Compute individual means for the base variable first
    # We need this BEFORE computing the within spline
    var_name <- term_info$var_name
    mean_col_name <- paste0(".imean_", gsub("[^a-zA-Z0-9]", "_", var_name))
    
    # Compute imean in grouped context (by id)
    data <- dplyr::group_by(data, !!rlang::sym(id_var))
    data <- dplyr::mutate(
      data,
      !!mean_col_name := mean(!!rlang::sym(var_name), na.rm = TRUE)
    )
    
    # Step 2: Compute deviation from mean
    dev_col_name <- paste0(".dev_", gsub("[^a-zA-Z0-9]", "_", var_name))
    data <- dplyr::mutate(
      dplyr::ungroup(data),
      !!dev_col_name := !!rlang::sym(var_name) - !!rlang::sym(mean_col_name)
    )
    # Re-group
    data <- dplyr::group_by(data, !!rlang::sym(id_var))
    
    # Step 3: Evaluate within spline on deviations
    # Build the call with the deviation variable
    within_call_text <- reconstruct_basis_call(
      term_info$term,
      dev_col_name,
      term_info$attrs
    )
    
    # Evaluate on ungrouped data (knots are fixed from attrs)
    data_eval <- dplyr::ungroup(data)
    within_mat <- tryCatch(
      eval(parse(text = within_call_text), envir = data_eval),
      error = function(e) {
        stop("Error evaluating within spline: ", e$message)
      }
    )
    
    # Expand within matrix into columns
    within_cols <- term_info$within_cols
    for (i in seq_len(ncol(within_mat))) {
      data_eval[[within_cols[i]]] <- within_mat[, i]
    }

    # Step 3b: Double-demean within basis columns
    # Option B semantics:
    #   - compute basis on deviations (x_it - xbar_i)
    #   - then demean each basis column within-person so the within spline
    #     has zero mean within each id
    data_eval <- dplyr::group_by(data_eval, !!rlang::sym(id_var))
    data_eval <- dplyr::mutate(
      data_eval,
      dplyr::across(
        dplyr::all_of(within_cols),
        ~ .x - mean(.x, na.rm = TRUE)
      )
    )
    data_eval <- dplyr::ungroup(data_eval)
    
    # Step 4: Evaluate between spline on individual means
    between_call_text <- reconstruct_basis_call(
      term_info$term,
      mean_col_name,
      term_info$attrs
    )
    
    between_mat <- tryCatch(
      eval(parse(text = between_call_text), envir = data_eval),
      error = function(e) {
        stop("Error evaluating between spline: ", e$message)
      }
    )
    
    # Expand between matrix into columns
    between_cols <- term_info$between_cols
    for (i in seq_len(ncol(between_mat))) {
      data_eval[[between_cols[i]]] <- between_mat[, i]
    }
    
    # Update data (preserving panel_data structure)
    # Need to reconstruct panel_data from data_eval
    data <- panel_data(data_eval, id = !!rlang::sym(id_var), wave = !!rlang::sym(wave_var))
    
    all_within_cols <- c(all_within_cols, within_cols)
    all_between_cols <- c(all_between_cols, between_cols)
    
    # Clean up temporary columns
    data[[mean_col_name]] <- NULL
    data[[dev_col_name]] <- NULL
  }
  
  list(
    data = data,
    within_cols = all_within_cols,
    between_cols = all_between_cols
  )
}

#' Update parsed formula object for matrix terms
#'
#' Updates the v_info and other fields in pf to reflect the expanded
#' matrix term columns.
#'
#' @param pf WBFormula object
#' @param expanded Result from expand_matrix_terms_in_data
#' @return Updated WBFormula object
#' @keywords internal
update_pf_for_matrix_terms <- function(pf, expanded) {
  # The varying field should already be updated in wb_formula_parser
  # to include within_cols instead of the original term
  
  # Update data
  pf$data <- expanded$data

  # Option B semantics: within spline columns are already within-demeaned
  # in expand_matrix_terms_in_data(), so they should NOT be added to v_info
  # (which would cause additional demeaning in wb_model()).
  #
  # Between spline columns are time-invariant (functions of xbar_i), so they
  # are treated as constants.
  if (!is.null(pf$matrix_terms) && length(pf$matrix_terms) > 0) {
    for (term_name in names(pf$matrix_terms)) {
      term_info <- pf$matrix_terms[[term_name]]
      pf$constants <- c(pf$constants, term_info$between_cols)
    }
    pf$constants <- unique(pf$constants)
  }
  
  # Update allvars
  pf$allvars <- unique(c(
    pf$dv,
    pf$varying,
    pf$constants,
    expanded$within_cols,
    expanded$between_cols
  ))
  
  # Update meanvars
  pf$meanvars <- if (!is.null(pf$v_info)) pf$v_info$meanvar else character(0)
  
  pf
}
