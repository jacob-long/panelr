#' @export

as_pdata.frame <- function(x) {
  
  if (!requireNamespace("plm")) {
    stop_wrap("You must have the plm package to convert to pdata.frame.")
  }
  
  pdata.frame(unpanel(x), index = c(get_id(x), get_wave(x)))
  
}

#' @rdname panel_data
#' @export
as_panel_data <- function(x, ...) {
  UseMethod("as_panel_data")
}

#' @rdname panel_data
#' @export
as_panel_data.default <- function(x, id = id, wave = wave, ...) {
  panel_data(x, id = !! id, wave = !! wave, ...)
}

#' @rdname panel_data
#' @export
as_panel_data.pdata.frame <- function(x, ...) {
  indices <- plm::index(x)
  id <- names(indices)[1]
  wave <- names(indices)[2]
  if (id %nin% names(x)) {
    x[id] <- indices[id]
  }
  if (wave %nin% names(x)) {
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
