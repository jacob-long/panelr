#' @title Summarize panel data frames
#' @description `summary` method for `panel_data` objects.
#' @param object A `panel_data` frame.
#' @param ... Optionally, unquoted variable names/expressions separated by
#'  commas to be passed to [dplyr::select()]. Otherwise, all columns are 
#'  included.
#' @param by.wave (if `skimr` is installed) Separate descriptives by wave?
#'  Default is TRUE.
#' @param by.id (if `skimr` is installed) Separate descriptives by entity?
#'  Default is FALSE. Be careful if you have a large number of entities as
#'  the output will be massive.
#' @examples 
#' 
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' summary(wages, lwage, exp, wks)
#' 
#' @importFrom purrr when
#' @importFrom rlang UQS UQ
#' @export

summary.panel_data <- function(object, ..., by.wave = TRUE, by.id = FALSE) {
  
  # Handling case of no selected vars --- I want default summary behavior
  # rather than default select behavior (which is to return nothing)
  vars <- as.character(enexprs(...))
  if (length(vars) == 0) {
    vars <- names(object)
    vars <- sapply(vars, backtick_name) # Avoid parsing non-syntactic names
  } 
  
  vars <- lapply(vars, parse_expr)
  
  if (!requireNamespace("skimr")) {
    msg_wrap("Get better summaries of panel_data frames by installing the 
             skimr package. Falling back to default summary.data.frame...")
    return(summary.data.frame(suppressMessages({
      panel_data %>% select(!!! vars)
    })))
  }
  
  id <- get_id(object)
  wave <- get_wave(object)
  
  # Avoiding message from adding wave/id vars
  suppressMessages({object %>% select(UQS(vars))}) %>%
    # Behavior conditional on by.id arg
    when(by.id == FALSE ~ unpanel(.) %>% ungroup(.) %>% select(., - !! sym(id)), 
         by.id == TRUE ~ unpanel(.) %>% group_by(., !! sym(id))) %>% 
    # Behavior conditional on by.wave arg
    when(by.wave == TRUE ~ group_by(., !! sym(wave)),
         by.wave == FALSE ~ select(., - !! sym(wave))) %>%
    # Call skim
    skimr::skim() -> out
  
  class(out) <- c("summary.panel_data", class(out))
  out
  
}

#' @export
print.summary.panel_data <- function(x, ...) {
  class(x) <- class(x) %not% "summary.panel_data"
  print(x, include_summary = FALSE)
}

#' @rawNamespace 
#' if (getRversion() >= "3.6.0") {
#'   S3method(knitr::knit_print, summary.panel_data)
#' } else {
#'   export(knit_print.summary.panel_data)
#' }
knit_print.summary.panel_data <- function(x, ...) {
  class(x) <- class(x) %not% "summary.panel_data"
  knitr::knit_print(x, options = list(skimr_include_summary = FALSE))
}
 
## WIP describe within and between variance
#' @importFrom stats weighted.mean
describe <- function(.data, ...) {
  out <- lapply(enexprs(...), function(x) {
    btw <- summarize(.data,
                     mean = mean(!! x, na.rm = TRUE),
                     count = n())
    wts <- btw$count / mean(btw$count, na.rm = TRUE)
    the_mean <- weighted.mean(btw$mean, weights = wts, na.rm = TRUE)
    btw_var <- sum((wts * (btw$mean - the_mean)^2) /
                     (sum(wts) - 1), na.rm = TRUE)
    within <- mutate(.data, 
                     mean = mean(!! x, na.rm = TRUE),
                     within_var = (!! x - mean) ^ 2)
    within_var <- sum(within$within_var, na.rm = TRUE) / 
                    (table(is.na(within$within_var))[1] - 1)
    c("mean" = the_mean, "between" = sqrt(btw_var), 
      "within" = sqrt(unname(within_var)))
  })
  names(out) <- sapply(enexprs(...), as_name)
  out
}

#' @export
#' @importFrom dplyr select
#'
# Used to be a simple reconstruct but now I want to be more opinionated and
# force the key variables to ride along.
select.panel_data <- function(.data, ...) {
  # Get args
  dots <- enexprs(...)
  # Get name of wave variable
  wave <- get_wave(.data)
  # Get name of id variable
  id <- get_id(.data)
  # Add them in (it's okay if they're already there)
  dots <- c(sym(id), sym(wave), dots)
  # Go ahead and select
  NextMethod(generic = "select", .data, !!! dots)
}

#' @export
#' @importFrom dplyr transmute
transmute.panel_data <- function(.data, ...) {
  # Get args
  dots <- enexprs(...)
  # Get name of wave variable
  wave <- get_wave(.data)
  # Add it in there if it's not already included (id is automatically added)
  if (wave %nin% names(dots)) {
    onames <- names(dots)
    dots <- c(sym(wave), dots)
    names(dots) <- c(wave, onames)
  }
  reconstruct(NextMethod(generic = "transmute", .data, !!! dots), .data)
}

#' @export
#' @importFrom dplyr arrange
arrange.panel_data <- function(.data, ..., .by_group = TRUE) {
  # Get args
  dots <- enexprs(...)
  # Get name of wave variable
  wave <- get_wave(.data)
  # Basically saying you get a warning if you do anything but arrange by time
  if (!all(unlist(as.character(dots)) == wave)) {
    warn_wrap("Arranging panel_data frames by something other than the wave
              variable may cause incorrect results when using time-based 
              functions like lag() and lead().")
  } else if (.by_group == FALSE) {
    warn_wrap("Arranging panel_data frames with '.by_group = FALSE' may cause
              incorrect results when using time-based  functions like lag() and
              lead().")
  }
  reconstruct(NextMethod(generic = "arrange", .data, !!! dots,
                         .by_group = .by_group), .data)
}

#' @export
`[.panel_data` <- function(x, i, j, drop = FALSE) {
  # have to differentiate between x[i] and x[i,]
  if (!missing(i) & missing(j) & "" %nin% as.character(sys.call())) {
    if (is.numeric(i)) {
      id <- which(names(x) == get_id(x))
      wave <- which(names(x) == get_wave(x))
      if (wave %nin% i) i <- c(wave, i)
      if (id %nin% i) i <- c(id, i)
    } else if (is.character(i)) {
      if (get_wave(x) %nin% i) i <- c(get_wave(x), i)
      if (get_id(x) %nin% i) i <- c(get_id(x), i)
    } else if (is.logical(i)) {
      id <- which(names(x) == get_id(x))
      wave <- which(names(x) == get_wave(x))
      i[c(id, wave)] <- TRUE
    }
  }
  # more straightforward is j is defined
  if (!missing(j)) {
    if (is.numeric(j)) {
      id <- which(names(x) == get_id(x))
      wave <- which(names(x) == get_wave(x))
      if (wave %nin% j) j <- c(wave, j)
      if (id %nin% j) j <- c(id, j)
    } else if (is.character(j)) {
      if (get_wave(x) %nin% j) j <- c(get_wave(x), j)
      if (get_id(x) %nin% j) j <- c(get_id(x), j)
    } else if (is.logical(j)) {
      id <- which(names(x) == get_id(x))
      wave <- which(names(x) == get_wave(x))
      j[c(id, wave)] <- TRUE
    }
  }
  reconstruct(NextMethod(), x)
}

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
  panel_data(data, id = !! sym(id), wave = !! sym(wave), ...)
}

#' @rdname panel_data
#' @export
as_panel <- as_panel_data

#' as_panel_data.tsibble <- function(x, ...) {
#'   
#' } 
