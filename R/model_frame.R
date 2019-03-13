#' @title Make model frames for panel_data objects
#' @description This is similar to model.frame, but is designed specifically
#'   for [panel_data()] data frames. It's a workhorse in [wbm()]
#'   but may be useful in scripting use as well.
#' @param formula A formula. Note that to get an individual-level mean with
#'   incomplete data (e.g., panel attrition), you should use `imean()` rather
#'   than `mean()`.
#' @param data A [panel_data()] frame.
#' @return A [panel_data()] frame with only the columns needed to fit
#'   a model as described by the formula.
#' @import dplyr
#' @rdname model_frame
#' @export

# TODO: consider making into S3 method
model_frame <- function(formula, data) {
  
  # model.frame does this, not sure why it would be needed...
  formula <- as.formula(formula)
  
  # we need terms from the formula
  if (!inherits(formula, "terms")) {
    formula <- terms(formula, data = data)
  }
  
  # Get all grouping variables so they aren't dropped
  the_groups <- group_vars(data)
  # Get all the original variables (sans functions) so they aren't dropped
  ovars <- all.vars(formula)
  
  wave <- get_wave(data)
  id <- get_id(data)
  
  # These are the names with transformations
  vars <- rownames(attr(terms(formula), "factors"))
  
  # Only do the mutations for vars with transformations
  inds <- which(as.character(vars) %nin% names(data) &
                  as.character(vars) %nin% ovars)
  
  existing_vars <- which(as.character(vars) %in% names(data) &
                           as.character(vars) %nin% ovars)
  
  # Keeping only needed vars to save memory
  mf <- data[c(ovars, as.character(vars)[existing_vars], the_groups, wave)]
  
  # Add new columns for transformed variables
  for (var in vars[inds]) {
    mf <- get_var(data = mf, var = var)
  }
  
  # Only keep the model frame vars plus groups and wave if panel_data
  mf <- mf[c(as.character(vars))]
  
  # Remove recursive back-ticking 
  names(mf) <- gsub("`", "", names(mf))
  
  return(mf)
  
}

#' @import dplyr
#' @importFrom rlang :=

get_var <- function(data, var) {
  
  var_expr <- parse_expr(var) 
  
  # It barks about preserving grouping variables (and labelled vectors)
  mutate(data, !!var := !!var_expr)
  
}

imean <- function(x, ..., na.rm = TRUE) {

  base::mean(x, ..., na.rm = na.rm)

}
