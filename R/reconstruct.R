### Thx tibbletime for thinking of this                    ###
### This file is a series of S3 methods for panel_data     ###
### Implement generic reconstruct() until sloop is on CRAN ###

#' Reconstruct an S3 class from a template
#'
#' This is an implementation of `sloop::reconstruct()` that users can
#' ignore. Once `sloop` is on CRAN, this function will be removed and that
#' version will be used. It currently must be exported for use in `tidyquant`.
#'
#' @param new Freshly created object
#' @param old Existing object to use as template
#'
#' @export
reconstruct <- function(new, old) {
  UseMethod("reconstruct", old)
}

#' @export
reconstruct.panel_data <- function(new, old) {
  
  if (is.data.frame(new) == FALSE) {
    # warning("The panel_data object is no longer a data frame.")
    return(new)
  }
  
  if ("id" %nin% names(new)) {
    # warning("The panel_data object no longer has the id variable. ",
    #         "Returning as a ", class(new)[1], " object instead.")
    return(new)
  }
  if ("wave" %nin% names(new)) {
    # warning("The panel_data object no longer has the wave variable. ",
    #         "Returning as a ", class(new)[1], " object instead.")
    return(new)
  }
  
  if ("panel_data" %nin% class(new) | "id" %nin% dplyr::group_vars(new)) {
    return(panel_data(new))
  } else {
    return(new)
  }
  
}

##### dplyr ##################################################################

#' @export
#' @importFrom dplyr mutate
mutate.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr transmute
transmute.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise
summarise.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr filter
filter.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

# Required to export filter, otherwise:
# Warning: declared S3 method 'filter.panel_data' not found
# because of stats::filter

#' @export
#'
dplyr::filter

#' @export
#' @importFrom dplyr arrange
arrange.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr distinct
distinct.panel_data <- function(.data, ..., .keep_all = FALSE) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr full_join
#'
full_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr inner_join
#'
inner_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr left_join
#'
left_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr right_join
#'
right_join.panel_data <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr anti_join
#'
anti_join.panel_data <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr semi_join
#'
semi_join.panel_data <- function(x, y, by = NULL, copy = FALSE, ...) {
  reconstruct(NextMethod(), x)
}

#' @export
#' @importFrom dplyr select
#'
select.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr slice
#'
slice.panel_data <- function(.data, ...) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr group_by
group_by.panel_data <- function(.data, ..., add = FALSE) {
  reconstruct(NextMethod(), .data)
}

### Backwards compat support for deprecated standard eval dplyr

# Only a few of them need it. arrange_.tbl_df() directly calls arrange_impl()
# causing a problem.

#' @export
#' @importFrom dplyr arrange_
#'
arrange_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr mutate_
#'
mutate_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarise_
#'
summarise_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr summarize_
#'
summarize_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}

#' @export
#' @importFrom dplyr slice_
#'
slice_.panel_data <- function(.data, ..., .dots = list()) {
  reconstruct(NextMethod(), .data)
}


##### tidyr #################################################################

nest.panel_data <- function(data, ..., .key = "data") {
  reconstruct(NextMethod(), data)
}

unnest.panel_data <- function(data, ..., .drop = NA, .id = NULL, .sep = NULL) {
  # This is called after nesting but excluding the index in the nest
  reconstruct(NextMethod(), data)
}

# gather() and spread() seem to be needed as well

gather.panel_data <- function(data, key = "key", value = "value", ...,
                              na.rm = FALSE, convert = FALSE,
                              factor_key = FALSE)  {
  reconstruct(NextMethod(), data)
}

spread.panel_data <- function(data, key, value, fill = NA, convert = FALSE,
                              drop = TRUE, sep = NULL)  {
  reconstruct(NextMethod(), data)
}