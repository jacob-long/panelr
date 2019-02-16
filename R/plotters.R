#' @title Plot trends in longitudinal variables
#' @description `line_plot` allows for flexible visualization of repeated 
#'  measures variables from `panel_data` frames.
#' @param data Either a `panel_data` frame or another data frame.
#' @param var The unquoted name of the variable of interest.
#' @param id If `data` is not a `panel_data` object, then the id variable.
#' @param wave If `data` is not a `panel_data` object, then the wave variable.
#' @param overlay Should the lines be plotted in the same panel or each in 
#'  their own facet/panel? Default is TRUE, meaning they are plotted in the 
#'  same panel.
#' @param show.points Plot a point at each wave? Default is TRUE.
#' @param subset.ids Plot only a subset of the entities' lines? Default is NULL,
#'  meaning plot all ids. If TRUE, a random subset (the number defined by 
#'  `n.random.subset`) are plotted. You may also supply a vector of ids to
#'  choose them yourself.
#' @param n.random.subset How many entities to randomly sample when `subset.ids`
#'  is TRUE.
#' @param add.mean Add a line representing the mean trend? Default is FALSE.
#'  Cannot be combined with `overlay`.
#' @param mean.function The mean function to supply to `geom_smooth` when
#'  `add.mean` is TRUE. Default is `"lm"`, but another option of interest is
#'  `"loess"`.
#' @param line.size The thickness of the plotted lines. Default: 0.5
#' @return The `ggplot` object.
#' @rdname line_plot
#' @export 
#' @importFrom ggplot2 facet_wrap ggplot geom_path geom_point geom_smooth aes
line_plot <- function(data, var, id = NULL, wave = NULL, overlay = TRUE,
                      show.points = TRUE,  subset.ids = NULL,
                      n.random.subset = 8, add.mean = FALSE,
                      mean.function = "lm", line.size = 0.5) {
  
  if (is_panel(data)) {
    id <- get_id(data)
    wave <- get_wave(data)
  } else {
    if (is.null(id) | is.null(wave)) {
      stop_wrap("id and/or wave have not been defined. Either use a panel_data
                frame or set the id and wave arguments.")
    }
  }
  
  odata <- data
  if (!is.null(subset.ids)) {
    if (length(subset.ids) == 1 && subset.ids %in% c(TRUE, "random")) {
      subset.ids <- sample(unique(data[[id]]), size = n.random.subset)
    }
    data <- data[data[[id]] %in% subset.ids, ]
  } else {
    subset.ids <- FALSE
  }
  
  p <- ggplot(data = data,
              aes(x = !! sym(wave), y = !! enquo(var), group = !! sym(id) ))
  
  if (show.points == TRUE) {
    p <- p + geom_point()
  }
  
  p <- p + geom_path(size = line.size)
  
  if (overlay == FALSE) {
    p <- p + facet_wrap(as.formula(paste0("~", id)))
  }
  
  if (add.mean == TRUE & overlay == TRUE) {
    p <- p + geom_smooth(data = odata, aes(x = !!sym(wave), y = !! enquo(var)),
                         inherit.aes = FALSE, method = mean.function,
                         colour = "blue", se = FALSE)
  } else if (add.mean == TRUE) {
    warning("Cannot add mean when `overlay` is FALSE.")
  }
  
  p + jtools::theme_nice(legend.pos = "right")
  
}