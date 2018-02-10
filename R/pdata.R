##### panel_data ############################################################

#' @title Create panel data frames
#' @description Format your data for use with \pkg{panelr}.
#' @param data A data frame.
#' @param id The name of the column (unquoted) that identifies
#'   participants/entities. A new column will be created called `id`,
#'   overwriting any column that already has that name.
#' @param wave The name of the column (unquoted) that identifies
#'   waves or periods. A new column will be created called `wave`,
#'   overwriting any column that already has that name.
#' @param ... Attributes for adding onto this method. See
#'  [tibble::new_tibble()] for a run-through of the logic. 
#' @return A `panel_data` object.
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' @rdname panel_data
#' @import dplyr
#' @export

panel_data <- function(data, id = "id", wave = "wave", ...) {

  id <- as.character(substitute(id))
  wave <- as.character(substitute(wave))

  # Append case ID column if not already named ID
  if (id != "id") {
    data$id <- data[[id]]
  }

  # Let's make sure ID var doesn't get confused for numeric
  if (!is.factor(data$id)) {data$id <- factor(data$id)}

  # Group by case ID
  if ("id" %nin% group_vars(data)) {data <- group_by(data, id, add = TRUE)}
  # Warn about multi-grouped DFs
  if (length(group_vars(data)) > 1) {
    message(paste("Detected additional grouping variables. Be aware this may",
                  "\ncause unexpected behavior or incorrect results."))
  } 

  # Append wave column if wave isn't already called wave
  if (wave != "wave") {
    data[["wave"]] <- data[[wave]]
  }

  # Make sure wave variable is in format I can understand
  if (is.factor(data$wave)) {
    data$wave <- as.numeric(data$wave)
    message("Factor wave variable was converted to numeric.")
  } else if (!is.numeric(data$wave)) {
    stop("The wave variable must be numeric.")
  }

  # Ordering by wave and then group ensures lag functions work right
  data <- arrange(data, wave, .by_group = TRUE)
  
  # Inherit from df, tibble, and grouped_df (last one is critical)
  data <- tibble::new_tibble(data, ..., 
                             idvar = id,
                             wavevar = wave,
                             subclass = c("panel_data", "grouped_df"))

  return(data)

}

#' @title Filter out entities with too few observations
#' @description This function allows you to define a minimum number of
#'   waves/periods and exclude all individuals with fewer observations than
#'   that.
#' @param data A [panel_data()] frame.
#' @param formula A formula, like the one you'll be using to specify your model.
#' @param vars As an alternative to formula, a vector of variable names.
#' @param min.waves What is the minimum number of observations to be kept?
#'   Default is `"all"`, but it can be any number.
#' @return A `panel_data` frame.
#' @rdname complete_data
#' @export
#' @importFrom stats complete.cases as.formula

complete_data <- function(data, formula = NULL, vars = NULL,
                          min.waves = "all") {
  # OG data frame for reconstruct()
  old <- data
  
  if (!is.null(formula)) {
    d <- data[c("id", "wave", all.vars(formula))]
  } else if (!is.null(vars)) {
    d <- data[c("id", "wave", vars)]
  } else {
    d <- data
  }

  # Keep only complete cases
  d <- d[complete.cases(d),]

  # Using the table to count up how many obs. of each person
  t <- table(d["id"])


  if (min.waves == "all") {
    min.waves <- max(t) # Whoever has the most observations has all the waves
  }

  # Keep only people who were observed minimum number of times
  keeps <- which(t >= min.waves)
  keeps <- names(t)[keeps]

  data <- data[data[["id"]] %in% keeps,]
  
  data <- reconstruct(data, old)

  return(data)

}

#' @importFrom tibble deframe
#' @import dplyr 
#' @import rlang

is_varying <- function(data, variable) {
  
  variable <- enquo(variable)
  
  # It appends a message every...single...time
  suppressMessages({
    out <- data %>%
      # For each group, does the variable vary?
      transmute(n_distinct(!! variable, na.rm = TRUE) %in% c(0L,1L)) %>%
      # Changing to a vector
      deframe() %>%
      # Asking if all groups had zero changes within the groups
      all(na.rm = TRUE)
  })
  
  # Because the above operation basically produces the answer to is_constant
  # I now need to return the opposite of out
  return(!out)
  
}

#' @title Check if variables are constant or variable over time.
#' @description This function is designed for use with [panel_data()] objects.
#' @param data A data frame, typically of [panel_data()] class.
#' @param ... Variable names. If none are given, all variables are checked.
#' @return A named logical vector. If TRUE, the variable is varying.
#' @examples 
#' 
#' wages <- panel_data(WageData, id = id, wave = t)
#' wages %>% are_varying(occ, ind, fem, blk)
#'
#' @rdname are_varying
#' @import rlang
#' @importFrom purrr map_lgl
#' @export 

are_varying <- function(data, ...) {
  
  class(data) <- class(data)[class(data) %nin% "panel_data"]
  dots <- quos(...)
  if (length(dots) == 0) {
    reserved_names <- c("id","wave", attr(data, "idvar"), attr(data, "wavevar"))
    dnames <- names(data)[names(data) %nin% reserved_names]
    dots <- syms(as.list(dnames))
  } else {
    dnames <- as.character(exprs(...))
  }
  out <- map_lgl(dots, function(x, d) { is_varying(!! x, data = d) }, d = data)
  names(out) <- dnames
  out
}

##### reshaping ##############################################################

#' @title Convert long panel data to wide format
#' @description This function takes [panel_data()] objects as input as converts
#'   them to wide format for use in SEM and other situations when such a format
#'   is needed.
#' @param data The `panel_data` frame.
#' @param separator When the variables are labeled with the wave number,
#'   what should separate the variable name and wave number? By default,
#'   it is "_". In other words, a variable named `var` will be 
#'   `var_1`, `var_2`, and so on in the wide data frame.
#' @param ignore.attributes If the `data` was created by [long_panel()],
#'   it stores information about which variables vary over time and which
#'   are constants. Sometimes, though, this information is not accurate (
#'   it is only based on the wide data's variable names) and you may want to
#'   force this function to check again based on the actual values of the
#'   variables.
#' @param varying If you want to skip the checks for whether variables are 
#'   varying and specify yourself, as is done with [stats::reshape()], you
#'   can supply them as a vector here. 
#' @return A data.frame with 1 row per respondent.
#'
#' @details 
#' 
#'  This is a wrapper for [stats::reshape()], which is renowned for being 
#'  pretty confusing to use. This function automatically detects which of the
#'  variables vary over time and which don't, not appending wave information
#'  to constants.
#' 
#' @examples 
#' 
#' wages <- panel_data(WageData, id = id, wave = t)
#' wide_wages <- widen_panel(wages)
#' 
#' @seealso 
#'  \code{\link[stats]{reshape}}
#' @rdname widen_panel
#' @export 
#' @importFrom stats reshape
#' @importFrom rlang syms

widen_panel <- function(data, separator = "_", ignore.attributes = FALSE,
                        varying = NULL) {
  
  # Get the var names that we never transform
  reserved_names <- c("id","wave", attr(data, "idvar"), attr(data, "wavevar"))
  
  if (ignore.attributes == TRUE) {
    attr(data, "reshaped") <- FALSE
    attr(data, "varying") <- NULL
    attr(data, "constants") <- NULL
  }
  
  # Get the names of all non-focal variables
  allvars <- names(data)[names(data) %nin% reserved_names]
  
  # Expedite process if the data have been reshaped before
  if (!is.null(attr(data, "reshaped")) && attr(data, "reshaped")) {
    allvars <- allvars[allvars %nin% c(attr(data, "varying"),
                                       attr(data, "constants"))]
  }
  
  # If varying vars specified, use those instead
  if (is.null(varying)) {
  
    # They will be included as arguments to are_varying
    args <- syms(as.list(allvars))
    # As will the data
    args$data <- data
    # Now we get a named vector of TRUE/FALSE values
    allvars <- do.call(are_varying, args)
    
    # If true, we want the variable name
    varying <- c(names(allvars[allvars]), attr(data, "varying"))
    
  }
  
  # Drop redundant wave variable
  if (!is.null(attr(data, "wavevar")) && attr(data, "wavevar") != "wave") {
    data <- data[names(data) %nin% attr(data, "wavevar")]
  }
  
  # Reshape doesn't play nice with tibbles
  data <- as.data.frame(data)
  
  if (ignore.attributes == FALSE) {
    data <- stats::reshape(data = data, v.names = varying, timevar = "wave",
                           idvar = "id", direction = "wide", sep = separator)
  } else { # This usually involves treating some "varying" vars as constants
    suppressWarnings({
    data <- stats::reshape(data = data, v.names = varying, timevar = "wave",
                           idvar = "id", direction = "wide", sep = separator)
    })
  }
  
  return(data)

}


#' @title Convert wide panels to long format
#' @description This function takes wide format panels as input and 
#'   converts them to long format. 
#' @param data The wide data frame.
#' @param prefix What character(s) go before the period indicator? If none,
#'   set this argument to NULL.
#' @param suffix What character(s) go after the period indicator? If none,
#'   set this argument to NULL.
#' @param begin What is the label for the first period? Could be `1`, `"A"`,
#'   or anything that can be sequenced.
#' @param end What is the label for the final period? Could be `2`, `"B"`,
#'   or anything that can be sequenced and lies further along the sequence
#'   than the `begin` argument.
#' @param id The name of the ID variable as a string. If there is no ID 
#'   variable, set to NULL and one will automatically be created. 
#' @param periods If you period indicator does not lie in a sequence or is 
#'   not understood by the function, then you can supply them as a vector
#'   instead. For instance, you could give `c("one","three","five")` if
#'   your variables are labeled `var_one`, `var_three`, and `var_five`.
#' @param label_location Where does the period label go on the variable?
#'   If the variables are labeled like `var_1`, `var_2`, etc., then it is
#'   `"end"`. If the labels are more like `A_var`, `B_var`, and so on, then
#'   it is `"beginning"`.
#' @param as_panel_data Should the return object be a [panel_data()] object?
#'   Default is TRUE.
#' @return Either a `data.frame` or `panel_data` frame.
#' @details 
#' 
#'   There is no easy way to convert panel data from wide to long format because
#'   the both formats are basically non-standard for other applications. 
#'   This function can handle the common case in which the wide data frame
#'   has a regular labeling system for each period. The key thing is 
#'   providing enough information for the function to understand the pattern.
#'   
#'   In the end, this function calls [stats::reshape()] but should be easier
#'   to use and able to handle more situations, such as when the label occurs
#'   at the beginning of the variable name. Also, just as important, this 
#'   function has built-in utilities to handle unbalanced data --- when 
#'   variables occur more than once but every single period, which breaks
#'   [stats::reshape()]. 
#'   
#' 
#' @seealso [widen_panel()]
#' @examples 
#' 
#' ## We need a wide data frame, so we will make one from the long-format 
#' ## data included in the package.
#' 
#' # Convert WageData to panel_data object
#' wages <- panel_data(WageData, id = id, wave = t)
#' # Convert wages to wide format
#' wide_wages <- widen_panel(wages)
#' 
#' # Note: wide_wages has variables in the following format:
#' # var1_1, var1_2, var1_3, var2_1, var2_2, var2_3, etc.
#' long_wages <- long_panel(wide_wages, prefix = "_", begin = 1, end = 7,
#'                          id = "id", label_location = "end")
#' # Note that in this case, the prefix and label_location arguments are
#' # the defaults but are included just for clarity.
#' 
#' 
#' @rdname long_panel
#' @importFrom stringr str_extract str_detect
#' @export 

long_panel <- function(data, prefix = "_", suffix = NULL, begin, end,
                       id = NULL, periods = NULL,
                       label_location = c("end","beginning"),
                       as_panel_data = TRUE) {
  
  if (is.numeric(begin) & is.null(periods)) { # Handle numeric period labels
    if (!is.numeric(end)) {stop("begin and end must be the same type.")}
    
    periods <- seq(from = begin, to = end)
    
  } else if (is.character(begin) & is.null(periods)) { # Handle letter labels
    if (!is.character(end)) {stop("begin and end must be the same type.")}
    
    if (is.finite(as.numeric(begin))) { # in case it was "1" or some such
      periods <- seq(from = as.numeric(begin), to = as.numeric(end))
    }
    
    if (begin %in% letters) { # is it a lowercase letter?
      alpha_start <- which(letters == begin)
      alpha_end <- which(letters == end)
      periods <- letters[alpha_start:alpha_end]
    } else if (begin %in% LETTERS) { # or an uppercase letter?
      alpha_start <- which(LETTERS == begin)
      alpha_end <- which(LETTERS == end)
      periods <- LETTERS[alpha_start:alpha_end]
    } else {stop("begin is a non-letter character.")}
    
  }
  
  # Make sure there is an ID column
  if (is.null(id)) {
    data$id <- 1:nrow(data)
    id <- "id"
  }
  # Now is time to find the varying variables
  wvars <- names(data)[names(data) %nin% id]
  
  # Escaping the prefix and suffix
  if (!is.null(prefix)) {
    pre_reg <- escapeRegex(paste0(prefix))
  } else {pre_reg <- NULL} 
  if (!is.null(suffix)) {
    post_reg <- escapeRegex(paste0(suffix))
  } else {post_reg <- NULL}
  
  # Programmatically building vector of regex patterns for each period
  patterns <- c()
  if (label_location[1] == "beginning") {
    for (i in periods) {
      pattern <- paste0("(?<=", pre_reg, escapeRegex(i), post_reg, ").*")
      patterns <- c(patterns, pattern)
    }
    sep <- suffix
  } else if (label_location[1] == "end") {
    for (i in periods) {
      pattern <- paste0(".*(?=", pre_reg, escapeRegex(i), post_reg, ")")
      patterns <- c(patterns, pattern)
    }
    sep <- prefix
  } else {stop("label_location must be 'beginning' or 'end'.")}
  
  # Using regex patterns to build up a list of variable names for 
  # reshape's "varying" argument
  varying_by_period <- as.list(rep(NA, times = length(periods)))
  names(varying_by_period) <- periods
  stubs_by_period <- as.list(rep(NA, times = length(periods)))
  names(stubs_by_period) <- periods
  for (p in patterns) {
    stubs <- str_extract(wvars, p) 
    matches <- str_detect(wvars, p)
    stubs_by_period[[periods[which(patterns == p)]]] <- stubs[matches]
    varying_by_period[[periods[which(patterns == p)]]] <-  wvars[matches]
  }
  
  # Count up how many instances of each stub there are
  stub_tab <- table(unlist(stubs_by_period))
  if (any(stub_tab != length(periods))) {
    
    which_miss <- names(stub_tab)[which(stub_tab != length(periods))]
    
    for (var in which_miss) { # Iterate through stubs with missing periods
      
      for (period in periods) { # Iterate through periods
        
        if (var %nin% stubs_by_period[[period]]) { # If stub missing in period
          # Build variable name
          if (label_location[1] == "beginning") {
            vname <- paste0(prefix, period, suffix, var)
          } else {
            vname <- paste0(var, prefix, period, suffix)
          }
          # Create column in data with empty values
          data[vname] <- rep(NA, times = nrow(data))
          # Add to var list (has to be done this way to preserve time order)
          varying_by_period[[period]] <- c(varying_by_period[[period]], vname)
          
        }
        
      }
      
    }
    
  }
  
  # Call reshape
  out <- reshape(as.data.frame(data), timevar = "wave", idvar = id,
                 times = periods,
                 sep = sep, direction = "long",
                 varying = unlist(varying_by_period))
  if (as_panel_data == TRUE) { # Return panel_data object if requested
    out$id <- out[[id]]
    out <- panel_data(out, id = "id", wave = "wave", reshaped = TRUE,
                      varying = names(stub_tab), 
                      constants = names(out)[names(out) %nin% names(stub_tab)])
  }
  return(out)
  
}

##### internal panel_data tools #############################################

complete_cases <- function(data, min.waves = "all") {
  
  # Keep only complete cases
  data <- data[complete.cases(data),]
  
  # Using the table to count up how many obs. of each person
  t <- table(data["id"])
  
  
  if (min.waves == "all") {
    min.waves <- max(t) # Whoever has the most observations has all the waves
  }
  
  # Keep only people who were observed minimum number of times
  keeps <- which(t >= min.waves)
  keeps <- names(t)[keeps]
  
  data <- data[data[["id"]] %in% keeps,]
  
  return(data)
  
}