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
                             subclass = c("panel_data", "grouped_df"))

  return(data)

}

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

  return(data)

}

#' @title Convert long panel data to wide format
#' @description This function takes [panel_data()] objects as input as converts
#'   them to wide format for use in SEM and other situations when such a format
#'   is needed.
#' @param data The `panel_data` frame.
#' @param separator When the variables are labeled with the wave number,
#'   what should separate the variable name and wave number? By default,
#'   it is "_". In other words, a variable named `var` will be 
#'   `var_1`, `var_2`, and so on in the wide data frame.
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

widen_panel <- function(data, separator = "_") {
  
  # Automatically detect constants
  constants <- c() # Empty vector; I know, inelegant
  for (v in names(data)[names(data) %nin% c("id","wave")]) { 
    # Pass to is_varying function
    c <- is_varying(data, v)
    # Add variable to constants list if TRUE
    if (c) {constants <- c(constants, v)}
  }
  
  # If not a constant, then varying
  varying <- names(data)[names(data) %nin% c("id", "wave", constants)]
  
  # Reshape doesn't play nice with tibbles
  data <- as.data.frame(data)
  
  data <- stats::reshape(data = data, v.names = varying, timevar = "wave",
                         idvar = "id", direction = "wide", sep = separator)
  
  return(data)

}


#' @importFrom tibble deframe
#' @import dplyr 
is_varying <- function(data, variable) {
  
  # It appends a message every...single...time
  suppressMessages({
  data %>%
    # For each group, does the variable vary?
    transmute(length(unique(!!rlang::sym(variable))) == 1L) %>%
    # Changing to a vector
    deframe() %>%
    # Asking if all groups had zero changes within the groups
    all(na.rm = TRUE)
  })
  
}
