#' @importFrom stats cor
do_heise <- function(data, measure) {
  
  # measure <- enexpr(measure)
  id <- get_id(data)
  
  l <- dplyr::select(data, !! measure)
  w <- widen_panel(l, ignore.attributes = TRUE)
  w <- w[names(w) %nin% id]

  r13 <- cor(w[,1], w[,3], use = "pairwise.complete.obs")
  r12 <- cor(w[,1], w[,2], use = "pairwise.complete.obs")
  r23 <- cor(w[,2], w[,3], use = "pairwise.complete.obs")

  rel <- (r12 * r23) / r13
  stab13 <- r13^2 / (r12 * r23)
  stab12 <- r13/r23
  stab23 <- r13/r12
  
  list(var = quo_name(measure), rel = rel, stab13 = stab13, stab12 = stab12, 
       stab23 = stab23)
  
}

#' @title Estimate Heise stability and reliability coefficients
#' @description This function uses three waves of data to estimate stability
#' and reliability coefficients as described in Heise (1969).
#' @param data A `panel_data` frame.
#' @param ... unquoted variable names that are passed to [dplyr::select()]
#' @param waves Which 3 waves should be used? If NULL (the default), the 
#'  first, middle, and last waves are used.
#' @return A `tibble` with reliability (`rel`), waves 1-3 stability (`stab13`),
#'  waves 1-2 stability (`stab12`), and waves 2-3 stability (`stab23`) and
#'  the variable these values refer to (`var`).
#' @examples 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' heise(wages, wks, lwage) # will use waves 1, 4, and 7 by default
#' @references 
#' Heise, D. R. (1969). Separating reliability and stability in test-retest
#'  correlation. *American Sociological Review*, *34*, 93–101. 
#'  https://doi.org/10.2307/2092790
#'  
#' @export
#' @importFrom stats median
heise <- function(data, ..., waves = NULL) {
  wave <- get_wave(data)
  # Drop other variables
  data <- dplyr::select(data, ...)
  # Automatically use the first, middle, and last waves 
  # unless others are requested
  if (is.null(waves)) {
    waves <- get_periods(data)
    if (length(waves) > 3) {
      which_middle <- as.integer(median(1:length(waves)))
      waves <- c(waves[1], waves[which_middle], waves[length(waves)])
      msg_wrap("Using waves ", waves[1], ", ", waves[2], ", and ", waves[3])
    } else if (length(waves) < 3) {
      stop_wrap("Your panel needs at least 3 waves to estimate 
                reliability/stability.")
    }
  }
  # Drop the other waves
  data <- dplyr::filter(data, !! sym(wave) %in% waves) 
  # Make the data complete
  data <- complete_data(data)
  # Get a list of non-id/wave variables
  measures <- syms(names(data) %not% c(wave, get_id(data)))
  # Return data frame of data
  purrr::map_dfr(measures, do_heise, data = data)
}
