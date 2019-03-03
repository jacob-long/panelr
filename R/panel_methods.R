#' @rdname panel_data
#' @export

as_pdata.frame <- function(data) {
  
  if (!requireNamespace("plm")) {
    stop_wrap("You must have the plm package to convert to pdata.frame.")
  }
  
  plm::pdata.frame(unpanel(data), index = c(get_id(data), get_wave(data)))
  
}

#' @rdname panel_data
#' @export
as_panel_data <- function(data, ...) {
  UseMethod("as_panel_data")
}

#' @rdname panel_data
#' @export
as_panel_data.default <- function(data, id = id, wave = wave, ...) {
  panel_data(data, id = !! id, wave = !! wave, ...)
}

#' @rdname panel_data
#' @export
as_panel_data.pdata.frame <- function(data, ...) {
  if (!requireNamespace("plm")) {
    stop_wrap("You must have the plm package to convert from pdata.frame.")
  }
  indices <- plm::index(data)
  id <- names(indices)[1]
  wave <- names(indices)[2]
  if (id %nin% names(data)) {
    x[id] <- indices[id]
  }
  if (wave %nin% names(data)) {
    x[wave] <- indices[wave]
  }
  panel_data(x, id = !! id, wave = !! wave, ...)
}

#' @rdname panel_data
#' @export
as_panel <- as_panel_data

#' as_panel_data.tsibble <- function(x, ...) {
#'   
#' } 
