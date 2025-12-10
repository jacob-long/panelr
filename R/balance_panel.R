#' Check if panel data has gaps
#'
#' This function checks whether a [panel_data()] object has implicit gaps
#' (missing rows for some entity-wave combinations).
#'
#' @param data A `panel_data` frame.
#' @return A logical value. `TRUE` if there are gaps, `FALSE` otherwise.
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#' has_gaps(wages)  # FALSE (complete data)
#'
#' # Create data with gaps
#' wages_gaps <- wages[wages$t != 3 | wages$id != wages$id[1], ]
#' has_gaps(wages_gaps)  # TRUE
#'
#' @seealso [scan_gaps()], [balance_panel()]
#' @export
has_gaps <- function(data) {
  if (!is_panel(data)) {
    stop_wrap("has_gaps() requires a panel_data object.")
  }
  
  id <- get_id(data)
  wave <- get_wave(data)
  periods <- get_periods(data)
  
  # Count expected vs actual observations
  n_entities <- dplyr::n_distinct(data[[id]])
  n_periods <- length(periods)
  expected_rows <- n_entities * n_periods
  
  nrow(data) < expected_rows
}

#' Scan for gaps in panel data
#'
#' This function identifies which entity-wave combinations are missing
#' in a [panel_data()] object.
#'
#' @param data A `panel_data` frame.
#' @return A tibble with columns for the id variable and wave variable,
#'   showing which combinations are missing. If there are no gaps,
#'   returns a tibble with zero rows.
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' # Create data with gaps
#' wages_gaps <- wages[!(wages$t == 3 & wages$id == wages$id[1]), ]
#' scan_gaps(wages_gaps)
#'
#' @seealso [has_gaps()], [balance_panel()]
#' @export
scan_gaps <- function(data) {
  if (!is_panel(data)) {
    stop_wrap("scan_gaps() requires a panel_data object.")
  }
  
  id <- get_id(data)
  wave <- get_wave(data)
  periods <- get_periods(data)
  
  # Get unique entities
  entities <- unique(data[[id]])
  
  # Create full grid of all possible combinations
  full_grid <- tidyr::expand_grid(
    !!rlang::sym(id) := entities,
    !!rlang::sym(wave) := periods
  )
  
  # Find which combinations are missing
  existing <- dplyr::select(unpanel(data), !!rlang::sym(id), !!rlang::sym(wave))
  
  # Anti-join to find gaps
  gaps <- dplyr::anti_join(full_grid, existing, by = c(id, wave))
  
  # Sort by id then wave
  gaps <- dplyr::arrange(gaps, !!rlang::sym(id), !!rlang::sym(wave))
  
  tibble::as_tibble(gaps)
}

#' Balance panel data by filling gaps
#'
#' This function makes implicit missing values explicit by adding rows
#' with NA values for entity-wave combinations that are not present
#' in the data.
#'
#' @param data A `panel_data` frame.
#' @param ... Optional fill values specified as `column = value`. Any columns
#'   not specified will be filled with `NA`.
#' @return A `panel_data` frame with all entity-wave combinations present.
#' @details
#' 
#' Panel data often has implicit gaps where certain entities are not

#' observed in certain waves. This function makes these gaps explicit
#' by adding rows filled with NA values (or custom values if specified).
#'
#' This is the inverse operation of removing incomplete cases. It can be
#' useful for:
#' - Visualizing the pattern of missing data
#' - Using functions that require complete (balanced) panels
#' - Explicit handling of missing waves in models
#'
#' @examples
#' data("WageData")
#' wages <- panel_data(WageData, id = id, wave = t)
#'
#' # Create data with gaps
#' wages_gaps <- wages[!(wages$t == 3 & wages$id == wages$id[1]), ]
#' nrow(wages_gaps)  # Missing one row
#'
#' # Balance the panel (add NA row)
#' wages_balanced <- balance_panel(wages_gaps)
#' nrow(wages_balanced)  # Back to full size
#'
#' # Balance with custom fill values
#' wages_balanced <- balance_panel(wages_gaps, wks = 0, union = 0)
#'
#' @seealso [has_gaps()], [scan_gaps()], [complete_data()]
#' @export
balance_panel <- function(data, ...) {
  if (!is_panel(data)) {
    stop_wrap("balance_panel() requires a panel_data object.")
  }
  
  # If no gaps, return as-is
  if (!has_gaps(data)) {
    return(data)
  }
  
  id <- get_id(data)
  wave <- get_wave(data)
  periods <- get_periods(data)
  
  # Get fill values from ...
  fill_values <- rlang::list2(...)
  
  # Get the gaps
  gaps <- scan_gaps(data)
  
  if (nrow(gaps) == 0) {
    return(data)
  }
  
  # Create template for new rows with all columns from data
  template <- tibble::as_tibble(data[0, ])
  
  # For each gap, create a row
  new_rows <- lapply(seq_len(nrow(gaps)), function(i) {
    row <- template[1, ]  # Start with template structure
    
    # Fill in the id and wave
    row[[id]] <- gaps[[id]][i]
    row[[wave]] <- gaps[[wave]][i]
    
    # Fill all other columns with NA (they already are from template)
    # Then apply any user-specified fill values
    for (col in names(fill_values)) {
      if (col %in% names(row)) {
        row[[col]] <- fill_values[[col]]
      }
    }
    
    row
  })
  
  # Bind all new rows
  new_rows_df <- dplyr::bind_rows(new_rows)
  
  # Ensure column types match
  for (col in names(data)) {
    if (col %in% names(new_rows_df)) {
      # Coerce to match original column type
      new_rows_df[[col]] <- vctrs::vec_cast(
        new_rows_df[[col]], 
        data[[col]]
      )
    }
  }
  
  # Combine with original data
  combined <- dplyr::bind_rows(unpanel(data), new_rows_df)
  
  # Reconstruct panel_data, re-sorting by id and wave
  result <- panel_data(combined, id = !!rlang::sym(id), wave = !!rlang::sym(wave))
  
  # Preserve attributes from original
  attr(result, "reshaped") <- attr(data, "reshaped")
  attr(result, "varying") <- attr(data, "varying")
  attr(result, "constants") <- attr(data, "constants")
  
  result
}