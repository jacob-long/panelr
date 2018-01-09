#' @export

widen_panel <- function(data, id = NULL, wave = NULL, varying) {

  if (class(data)[1] == "panel_data" & is.null(id) & is.null(wave)) {
    id <- attr(data, "id")
    wave <- attr(data, "wave")
  }

  for (var in varying) {
    for (i in unique(data[,wave])) {

      varname <- paste(var, "_", i, sep = "")
      data[,varname] <- NA

      for (u in unique(data[,id])) {

        value <- data[,var][data[,wave] == i & data[,id] == u]
        if (length(value) > 0) {
          data[,varname][data[,id] == u] <- value
        } else {
          data[,varname][data[,id] == u] <- NA
        }

      }

    }
  }

  data <- data[unique(data[,id]),]
  data <- data[!(colnames(data) %in% c(wave,varying))]
  return(data)

}
