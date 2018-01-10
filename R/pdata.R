#' @title Create panel data frames
#' @description Format your data for use with \pkg{panelr}.
#' @param data A data frame.
#' @param id The name of the column (unquoted) that identifies
#'   participants/entities. A new column will be created called `id`,
#'   overwriting any column that already has that name.
#' @param wave The name of the column (unquoted) that identifies
#'   waves or periods. A new column will be created called `wave`,
#'   overwriting any column that already has that name.
#' @return A `panel_data` object.
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' @rdname panel_data
#' @import dplyr
#' @export

panel_data <- function(data, id = NULL, wave = NULL) {

  id <- as.character(substitute(id))
  wave <- as.character(substitute(wave))

  # Append case ID column if not already named ID
  if (id != "id") {
    data$id <- data[[id]]
  }

  # Let's make sure ID var doesn't get confused for numeric
  data$id <- factor(data$id)

  # Group by case ID
  data <- group_by(data, id)

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

  # Inherit from df, tibble
  data <-
    structure(data, class = c("panel_data", "grouped_df", "tbl_df",
                              "data.frame"))

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



