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
  wave <- get_wave(data)
  id <- get_id(data)
  reserved_names <- c(id, wave)
  
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
    
    # Set the constants such that reshape treats them that way
    data <- set_constants(data, names(allvars)[!allvars])
    
  }
  
  # Reshape doesn't play nice with tibbles
  data <- as.data.frame(data)
  
  # Remove reshape's saved attributes
  attributes(data)$reshapeLong <- NULL
  
  if (ignore.attributes == FALSE & is.null(varying)) {
    data <- stats::reshape(data = data, v.names = varying, timevar = wave,
                           idvar = id, direction = "wide", sep = separator)
  } else { # This usually involves treating some "varying" vars as constants
    suppressWarnings({
      data <- stats::reshape(data = data, v.names = varying, timevar = wave,
                             idvar = id, direction = "wide", sep = separator)
    })
  }
  
  # Remove reshape's saved attributes
  attributes(data)$reshapeWide <- NULL
  
  return(tibble::as_tibble(data))
  
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
#'   variable, then this will be the name of the newly-created ID variable. 
#' @param wave This will be the name of the newly-created wave variable. 
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
#' @param match The regex that will match the part of the variable names other
#'   than the wave indicator. By default it will match any character any 
#'   amount of times. Sometimes you might know that the variable names should
#'   start with a digit, for instance, and you might use `"\\d.*"` instead.
#' @param use.regex Should the `begin` and `end` arguments be treated as 
#'   regular expressions? Default is FALSE.
#' @param check.varying Should the function check to make sure that every 
#'   variable in the wide data with a wave indicator is actually time-varying?
#'   Default is TRUE, meaning that a constant like "race_W1" only measured in 
#'   wave 1 will be defined in each wave in the long data. With very large
#'   datasets, however, sometimes setting this to FALSE can save memory.
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
#' \dontrun{
#' long_wages <- long_panel(wide_wages, prefix = "_", begin = 1, end = 7,
#'                          id = "id", label_location = "end")
#' }
#' # Note that in this case, the prefix and label_location arguments are
#' # the defaults but are included just for clarity.
#' 
#' 
#' @rdname long_panel
#' @importFrom stringr str_extract str_detect
#' @export 

long_panel <- function(data, prefix = NULL, suffix = NULL, begin = NULL,
                       end = NULL, id = "id", wave = "wave", periods = NULL,
                       label_location = c("end", "beginning"),
                       as_panel_data = TRUE, match = ".*", 
                       use.regex = FALSE, check.varying = TRUE) {
  
  if (is.numeric(begin) & is.null(periods)) { # Handle numeric period labels
    if (!is.numeric(end)) {stop("begin and end must be the same type.")}
    
    periods <- seq(from = begin, to = end)
    
  } else if (is.character(begin) & is.null(periods)) { # Handle letter labels
    if (!is.character(end)) {stop("begin and end must be the same type.")}
    
    if (suppressWarnings(is.finite(as.numeric(begin)))) { # in case it's e.g. "1"
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
  if (id %nin% names(data)) {
    data[id] <- 1:nrow(data)
  }
  # Now is time to find the varying variables
  wvars <- names(data)[names(data) %nin% id]
  
  # Escaping the prefix and suffix
  if (!is.null(prefix)) {
    pre_reg <- if (use.regex == FALSE) escapeRegex(paste0(prefix)) else prefix
  } else {pre_reg <- NULL} 
  if (!is.null(suffix)) {
    post_reg <- if (use.regex == FALSE) escapeRegex(paste0(suffix)) else suffix
  } else {post_reg <- NULL}
  
  label_location <- label_location[1]
  
  if (label_location == "end" & (is.null(prefix) || nchar(prefix) == 0) |
      label_location == "beginning" & (is.null(suffix) || nchar(suffix) == 0)) {
    no_sep <- TRUE
    if (label_location == "end") {
      sep <- prefix <- "__"
    } else {
      sep <- suffix <- "__"
    }
  } else {no_sep <- FALSE}
  
  period_strings <- as.character(periods)
  period_regex <- paste0(escapeRegex(period_strings), collapse = "|")
  sanitize_match <- function(pattern) {
    gsub("(?<!\\\\)\\((?!\\?)", "(?:", pattern, perl = TRUE)
  }
  to_empty <- function(x) {
    if (length(x) == 0 || is.null(x)) "" else x
  }
  match_pattern <- sanitize_match(match)
  pre_pattern <- to_empty(pre_reg)
  post_pattern <- to_empty(post_reg)
  
  if (label_location == "beginning") {
    pattern <- paste0("^", pre_pattern, "(", period_regex, ")", post_pattern,
                      "(", match_pattern, ")$")
    sep <- suffix
    sep <- prefix <- paste0("__", sep, prefix)
    suffix <- NULL
    parsed <- stringr::str_match(wvars, pattern)
    wave_col <- 2
    stub_col <- 3
  } else if (label_location == "end") {
    pattern <- paste0("^(", match_pattern, ")", pre_pattern, "(", period_regex, ")",
                      post_pattern, "$")
    sep <- paste0("__", prefix)
    parsed <- stringr::str_match(wvars, pattern)
    stub_col <- 2
    wave_col <- 3
  } else {stop("label_location must be 'beginning' or 'end'.")}
  
  matched_idx <- which(!is.na(parsed[, 1]))
  parsed_df <- data.frame(idx = matched_idx,
                          stub = parsed[matched_idx, stub_col],
                          period = parsed[matched_idx, wave_col],
                          stringsAsFactors = FALSE)
  parsed_df$new_name <- paste0(parsed_df$stub, sep, parsed_df$period)
  
  new_wvars <- wvars
  if (nrow(parsed_df) > 0) {
    new_wvars[parsed_df$idx] <- parsed_df$new_name
  }
  names(data)[names(data) %nin% id] <- new_wvars
  
  init_list <- stats::setNames(rep(list(character()), length(period_strings)),
                        period_strings)
  stubs_by_period <- init_list
  varying_by_period <- init_list
  if (nrow(parsed_df) > 0) {
    parsed_df <- parsed_df[order(parsed_df$idx), ]
    for (period in period_strings) {
      rows <- parsed_df$period == period
      stubs_by_period[[period]] <- parsed_df$stub[rows]
      varying_by_period[[period]] <- parsed_df$new_name[rows]
    }
  }
  
  # Count up how many instances of each stub there are
  stub_tab <- table(unlist(stubs_by_period))
  if (any(stub_tab != length(periods))) {
    
    which_miss <- names(stub_tab)[which(stub_tab != length(periods))]
    
    for (var in which_miss) { # Iterate through stubs with missing periods
      for (period in periods) { # Iterate through periods
        if (var %nin% stubs_by_period[[as.character(period)]]) { # If stub missing in period
          # Build variable name (all suffixes are deleted by now)
          vname <- paste0(var, sep, period)
          # Create column in data with empty values
          data[vname] <- rep(NA, times = nrow(data))
          # Add to var list (has to be done this way to preserve time order)
          varying_by_period[[as.character(period)]] <- 
            c(varying_by_period[[as.character(period)]], vname)
        }
      }
    }
  }

  # Remove reshape's saved attributes
  attributes(data)$reshapeLong <- NULL
  # Call reshape
  out <- reshape(as.data.frame(data), timevar = wave,
                 idvar = id, times = periods, sep = sep, direction = "long",
                 varying = unlist(varying_by_period))
                 # v.names = unique(unname(unlist(stubs_by_period))))
  # Remove reshape's saved attributes
  attributes(out)$reshapeWide <- NULL
  attributes(out)$reshapeLong <- NULL
  # If the periods are character, convert to an ordered factor
  if (is.character(periods)) {
    out[[wave]] <- ordered(out[[wave]], levels = periods)
  }
  v.names <- unique(unname(unlist(stubs_by_period))) 
  
  out <- as_tibble(out)
  needs_panel <- as_panel_data || check.varying
  if (!needs_panel) {
    return(out)
  }
  
  tmp_pd <- panel_data(out, id = !!sym(id), wave = !!sym(wave))
  if (check.varying) {
    if (length(v.names) == 0) {
      varying <- logical(0)
      constants <- logical(0)
    } else {
      tmp_raw <- unpanel(tmp_pd)
      id_col <- get_id(tmp_pd)
      groups <- split(seq_len(nrow(tmp_raw)), tmp_raw[[id_col]])
      var_names <- un_bt(v.names)
      vary_flags <- vapply(seq_along(var_names), function(idx) {
        values <- tmp_raw[[var_names[idx]]]
        for (g in groups) {
          gvals <- values[g]
          if (n_distinct(gvals, na.rm = TRUE) > 1) {
            return(TRUE)
          }
        }
        FALSE
      }, logical(1))
      names(vary_flags) <- v.names
      if (any(vary_flags == FALSE)) {
        const_vars <- un_bt(names(vary_flags)[!vary_flags])
        for (var in const_vars) {
          tmp_pd <- mutate(tmp_pd, !! sym(var) := uniq_nomiss(!! sym(var)))
        }
      }
      constants <- vary_flags[!vary_flags]
      varying <- vary_flags[vary_flags]
    }
  } else {
    varying <- unlist(stubs_by_period)
    constants <- names(out) %not% varying
  }
  if (as_panel_data) {
    attr(tmp_pd, "reshaped") <- TRUE
    attr(tmp_pd, "varying") <- un_bt(names(varying))
    attr(tmp_pd, "constants") <- un_bt(names(constants))
    return(tmp_pd)
  }
  return(unpanel(tmp_pd))
  
}
