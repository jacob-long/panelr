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

panel_data <- function(data, id = id, wave = wave, ...) {

  id <- as_name(enexpr(id))
  wave <- as_name(enexpr(wave))
  
  if (id %nin% names(data)) {
    stop(id, "was not found in the data.")
  } else if (wave %nin% names(data)) {
    stop(wave, "was not found in the data.")
  }

  # Let's make sure ID var doesn't get confused for numeric
  if (!is.factor(data[[id]])) {data[[id]] <- factor(data[[id]])}

  # Group by case ID
  if (id %nin% group_vars(data)) {data <- group_by(data, !!sym(id), add = TRUE)}
  # Warn about multi-grouped DFs
  if (length(group_vars(data)) > 1) {
    msg_wrap("Detected additional grouping variables. Be aware this may
             cause unexpected behavior or incorrect results.")
  } 

  # Make sure wave variable is in format I can understand
  if (is.factor(data[[wave]]) & !is.ordered(data[[wave]])) {
    data[[wave]] <- factor(data[[wave]], ordered = TRUE)
    msg_wrap("Unordered factor wave variable was converted to ordered.
             You may wish to check that the order is correct.")
    periods <- levels(data[[wave]])
  } else if (!is.ordered(data[[wave]]) & !is.numeric(data[[wave]])) {
    stop("The wave variable must be numeric or an ordered factor.")
  } else {
    periods <- sort(unique(data[[wave]]))
  }
  
  # if (!is.ordered(wave) && 0 %in% data[[wave]]) {
  #   message("There cannot be a wave 0. Adding 1 to each wave.\n")
  #   data[[wave]] <- data[[wave]] + 1
  #   periods <- periods + 1
  # }

  # Ordering by wave and then group ensures lag functions work right
  data <- arrange(data, !!sym(wave), .by_group = TRUE)
  
  # Order the columns to put id and wave first (must do this *before*
  # creating the panel_data object or else there are endless loops in
  # reconstruct functions)
  data <- data[c(id, wave, names(data) %not% c(id, wave))]
  
  # Inherit from df, tibble, and grouped_df (last one is critical)
  data <- tibble::new_tibble(data, ..., 
                             id = id,
                             wave = wave,
                             subclass = c("panel_data", "grouped_df"),
                             nrow = nrow(data),
                             periods = periods)

  return(data)

}

#' @title Check if object is panel_data
#' @description This is a convenience function that checks whether an object
#'  is a `panel_data` object.
#' @param x Any object.
#' @examples 
#'  data("WageData")
#'  is_panel(WageData) # FALSE
#'  wages <- panel_data(WageData, id = id, wave = t)
#'  is_panel(wages) # TRUE
#' @export 

is_panel <- function(x) {
  "panel_data" %in% class(x)
}

#' @title Filter out entities with too few observations
#' @description This function allows you to define a minimum number of
#'   waves/periods and exclude all individuals with fewer observations than
#'   that.
#' @param data A [panel_data()] frame.
#' @param ... Optionally, unquoted variable names/expressions separated by
#'  commas to be passed to [dplyr::select()]. Otherwise, all columns are 
#'  included if `formula` and `vars` are also NULL.
#' @param formula A formula, like the one you'll be using to specify your model.
#' @param vars As an alternative to formula, a vector of variable names.
#' @param min.waves What is the minimum number of observations to be kept?
#'   Default is `"all"`, but it can be any number.
#' @return A `panel_data` frame.
#' @details 
#' 
#' If `...` (that is, unquoted variable name(s)) are included, then `formula`
#' and `vars` are ignored. Likewise, `formula` takes precedence over `vars`.
#' These are just different methods for selecting variables and you can choose
#' whichever you prefer/are comfortable with. `...` corresponds with the
#' "tidyverse" way, `formula` is useful for programming or working with 
#' model formulas, and `vars` is a "standard" evaluation method for when you
#' are working with strings.
#' 
#' @examples
#' 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' complete_data(wages, wks, lwage, min.waves = 3)
#' 
#' @rdname complete_data
#' @export
#' @importFrom stats complete.cases as.formula

complete_data <- function(data, ..., formula = NULL, vars = NULL, 
                          min.waves = "all") {
  # OG data frame for reconstruct()
  old <- data
  
  id <- get_id(data)
  wave <- get_wave(data)
  
  # Handling case of no selected vars --- I want to assume a selection
  # of none means selection of all rather than default select behavior
  # (which is to return nothing)
  cols <- enexprs(...)
  if (length(cols) == 0 & is.null(formula) & is.null(vars)) {
    
    cols <- names(data)
    cols <- sapply(cols, backtick_name) # Avoid parsing non-syntactic names
    cols <- lapply(cols, parse_expr)
    # cols <- c(sym(id), sym(wave), cols)
    d <- select(data, !!! cols)
    
  } else if (length(cols) > 0) {
    
    # cols <- lapply(cols, parse_expr)
    # cols <- c(sym(id), sym(wave), cols)
    d <- select(data, !!! cols)
    
  } else {
    
    if (!is.null(formula)) {
      d <- data[all.vars(formula)]
    } else if (!is.null(vars)) {
      d <- data[vars]
    } else {
      d <- data
    }
    
  }

  # Keep only complete cases
  d <- d[complete.cases(d),]

  # Using the table to count up how many obs. of each person
  t <- table(d[[id]])

  if (min.waves == "all") {
    min.waves <- max(t) # Whoever has the most observations has all the waves
  }

  # Keep only people who were observed minimum number of times
  keeps <- which(t >= min.waves)
  keeps <- names(t)[keeps]

  data <- data[data[[id]] %in% keeps,]
  data <- reconstruct(data, old)
  return(data)

}

#' @importFrom tibble deframe
#' @import dplyr 
#' @import rlang

is_varying <- function(data, variable) {
  
  variable <- enquo(variable)
  
  out <- data %>%
    # For each group, does the variable vary?
    mutate(variable := n_distinct(!! variable, na.rm = TRUE) %in% c(0L,1L)) %>%
    unpanel() %>%
    select(variable) %>%
    # Changing to a vector
    deframe() %>%
    # Asking if all groups had zero changes within the groups
    all(na.rm = TRUE)
  
  # Because the above operation basically produces the answer to is_constant
  # I now need to return the opposite of out
  return(!out)
  
}

#' @title Check if variables are constant or variable over time.
#' @description This function is designed for use with [panel_data()] objects.
#' @param data A data frame, typically of [panel_data()] class.
#' @param ... Variable names. If none are given, all variables are checked.
#' @param type Check for variance over time or across individuals? Default
#'  is `"time"`. `"individual"` considers variables like age to be non-varying
#'  because everyone ages at the same speed.
#' @return A named logical vector. If TRUE, the variable is varying.
#' @examples 
#' 
#' wages <- panel_data(WageData, id = id, wave = t)
#' wages %>% are_varying(occ, ind, fem, blk)
#'
#' @rdname are_varying
#' @import rlang
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @export 

are_varying <- function(data, ..., type = "time") {
  
  # class(data) <- class(data)[class(data) %nin% "panel_data"]
  dots <- quos(...)
  if (length(dots) == 0) {
    dnames <- names(data) %not% c(get_id(data), get_wave(data))
    dots <- syms(as.list(dnames))
  } else {
    data <- dplyr::select(data, ...)
    dots <- as.character(enexprs(...))
    is_wave <- if (get_wave(data) %in% dots) NULL else get_wave(data)
    dots <- syms(
      as.list(names(data) %not% c(get_id(data), is_wave))
    )
  }
  # Get time variation
  if ("time" %in% type) {
    outt <- map_lgl(dots, function(x, d) {
      is_varying(!! x, data = select(d, !! x))
    }, d = data)
  } 
  # Get individual variation
  if ("individual" %in% type) {
    outi <- map_lgl(dots, function(x, d) {
      is_varying_individual(!! x, data = select(d, !! x))
      }, d = data)
    # If both, rbind them into a d.f.
    if (exists("outt")) {
      out <- as.data.frame(rbind(outt, outi))
      rownames(out) <- c("time", "individual")
    } else {out <- outi}
  }
  # If not both, make time the out object
  if (!exists("out", inherits = FALSE)) {
    out <- outt
  }
  
  names(out) <- as.character(unlist(dots))
  out
}

#' @importFrom tibble deframe
#' @import dplyr 
#' @import rlang
#' @importFrom stats var
is_varying_individual <- function(data, variable) {
  
  variable <- enquo(variable)
  
  # Need to deal with non-numeric data
  if (!is.numeric(data[[as_name(variable)]])) {
    # If ordered, pretend it's numeric
    if (is.ordered(data[[as_name(variable)]])) {
      data[[as_name(variable)]] <- as.numeric(data[[as_name(variable)]])
    } else {
      # Otherwise just check if it varies at all
      return(is_varying(data, !! variable))
    }
  }

  out <- data %>%
    # make new variable with the within-subject variance
    mutate(variable = var(!! variable, na.rm = TRUE)) %>%
    # ungroup
    unpanel() %>%
    # select only our new value
    select(variable) %>%
    # change to a vector
    deframe() %>%
    # see how many distinct values there are
    n_distinct(na.rm = TRUE) %nin% c(0L, 1L)
  
}

## Using these to get around limitations with constants that are measured
## after Wave 1 in labeled wide data. e.g., the wide data has var_W2, but only
## measured in W2. 

set_constants <- function(data, vars) {
  constants <- lapply(syms(vars), set_constant, data = data)
  data[vars] <- constants
  return(data)
}

set_constant <- function(data, var) {
  var <- enquo(var)
  var_name <- quo_name(var)
  transmute(data, !! var_name := uniq_nomiss(!! var)) %>%
    unpanel() %>%
    select(!! var_name) %>%
    deframe()
}

## This is my way of grabbing the lone non-NA value from the group
## ...unless, of course, they are all NA in which case I need to give back NA

uniq_nomiss <- function(x) {
  un <- unique(x)
  if (!all(is.na(un))) {
    un <- un[!is.na(un)]
  }
  return(un)
}

#' @export
#' @importFrom tibble trunc_mat
#'
print.panel_data <- function(x, ...) {
  
  # Original trunc_mat
  print_tbl <- tibble::trunc_mat(x, ...)
  
  # Overwrite "A tibble" with "Panel data"
  names(print_tbl$summary)[[1]] <- "Panel data"
  
  # Panel metadata
  periods <- get_periods(x)
  if (length(periods) > 3) {
    periods <- paste0(paste(periods[1:3], collapse = ", "), ", ... (", 
                      n_distinct(periods), " waves)")
  } else {
    periods <- paste0(paste(periods, collapse = ", "), " (", 
                      n_distinct(periods), " waves)")
  }
  panel_meta <- c("entities" =
                    paste0(get_id(x), " [", n_distinct(x[[get_id(x)]]), "]"),
                    "wave variable" = paste0(get_wave(x), " [", periods, "]")
                 )
  # Add panel metadata
  print_tbl$summary <- append(print_tbl$summary, panel_meta, after = 1)
  # Drop groups 
  print_tbl$summary <- 
    print_tbl$summary[names(print_tbl$summary) %nin% "Groups"]
  
  # Print
  # cat("# ", names(summary), ": ", summary, "\n", sep = "")
  print(print_tbl)
}

#' @title Convert panel_data to regular data frame
#' @description This convenience function removes the special features of 
#'  `panel_data`.
#' @param panel A `panel_data` object.
#' @return An ungrouped `tibble`.
#' @examples
#' data("WageData") 
#' wages <- panel_data(WageData, id = id, wave = t)
#' wages_non_panel <- unpanel(wages)
#' @export

unpanel <- function(panel) {
  ungroup(panel)
}

#' @title Retrieve panel_data metadata
#' @description `get_id()`, `get_wave()`, and `get_periods()` are extractor
#'  functions that can be used to retrieve the names of the id and wave
#'  variables or time periods of a `panel_data` frame.
#' @param data A `panel_data` frame
#' @return A `panel_data` frame
#' @examples 
#' 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' get_wave(wages)
#' get_id(wages)
#' get_periods(wages)
#' 
#' @rdname get_wave
#' @export

get_wave <- function(data) {
  attr(data, "wave")
}

#' @export
#' @rdname get_wave

get_id <- function(data) {
  attr(data, "id")
}

#' @export
#' @rdname get_wave
get_periods <- function(data) {
  attr(data, "periods")
}

##### internal panel_data tools #############################################

complete_cases <- function(data, min.waves = "all") {
  
  id <- get_id(data)
  wave <- get_wave(data)
  
  # Keep only complete cases
  data <- data[complete.cases(data),]
  
  # Using the table to count up how many obs. of each person
  t <- table(data[[id]])
  
  
  if (min.waves == "all") {
    min.waves <- max(t) # Whoever has the most observations has all the waves
  }
  
  # Keep only people who were observed minimum number of times
  keeps <- which(t >= min.waves)
  keeps <- names(t)[keeps]
  
  data <- data[data[[id]] %in% keeps,]
  
  return(data)
  
}
