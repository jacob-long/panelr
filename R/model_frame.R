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

# model_frame <- function(formula, data) {
# 
#   # model.frame does this, not sure why it would be needed...
#   formula <- as.formula(formula)
# 
#   # we need terms from the formula
#   if (!inherits(formula, "terms")) {
#     formula <- terms(formula, data = data)
#   }
# 
#   vars <- attr(formula, "variables")
#   predvars <- attr(formula, "predvars")
#   if (is.null(predvars)) {
#     predvars <- vars
#   }
# 
#   vars <- as.list(vars)[-1] # probably more elegant way to do this
#   cols <- lapply(vars, FUN = get_var, data = data)
# 
#   the_groups <- group_vars(data)
#   if (!is.null(the_groups)) {
#     index <- length(the_groups) + 1
#   
#     # This is the output model frame, starting with the first column plus
#     # grouping vars
#     mf <- cols[[1]]
#   
#     # Need to combine cols without duplicating group column
#     for (i in seq(from = 2, to = length(vars))) {
#   
#       mf %<>% bind_cols(cols[[i]][index])
#   
#     }
#   }
# 
#   # Add wave variable back if it was there before
#   if ("wave" %in% names(data)) {
#     mf$wave <- data$wave
#   }
# 
#   return(mf)
#   # variables <- eval(predvars, data, env) # replace me!
# 
#   # resp <- attr(formula, "response")
# 
# }

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
  
  if ("wave" %in% names(data)) {wave <- "wave"} else {wave <- NULL}
  
  # These are the names with transformations
  vars <- attr(formula, "variables")
  vars <- as.list(vars)[-1] # probably more elegant way to do this
  
  # Only do the mutations for vars with transformations
  inds <- which(as.character(vars) %nin% names(data) &
                  as.character(vars) %nin% ovars)
  
  existing_vars <- which(as.character(vars) %in% names(data) &
                           as.character(vars) %nin% ovars)
  
  # Keeping only needed vars to save memory
  mf <- data[c(ovars, as.character(vars)[existing_vars], the_groups, wave)]
  
  # Add new columns for transformed variabels
  for (var in vars[inds]) {
    mf <- get_var(data = mf, var = var)
  }
  
  # Only keep the model frame vars plus groups and wave if panel_data
  mf <- mf[c(as.character(vars), the_groups, wave)]
  
  return(mf)
  # variables <- eval(predvars, data, env) # replace me!
  
  # resp <- attr(formula, "response")
  
}

#' @import dplyr
#' @importFrom rlang :=

# get_var <- function(data, var) {
# 
#   varname <- quo_name(var)
# 
#   # It barks about preserving grouping variables (and labelled vectors)
#   suppressWarnings(suppressMessages(transmute(data, !!varname := !!var)))
# 
# }

get_var <- function(data, var) {
  
  varname <- quo_name(var)
  
  # It barks about preserving grouping variables (and labelled vectors)
  mutate(data, !!varname := !!var)
  
}

imean <- function(x, ..., na.rm = TRUE) {

  base::mean(x, ..., na.rm = na.rm)

}
