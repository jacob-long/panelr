#' Internal vctrs methods
#'
#' These methods are the extensions that allow panel_data objects to
#' work with vctrs and modern dplyr/tidyr operations.
#'
#' @importFrom vctrs vec_restore vec_proxy vec_ptype2 vec_cast
#' @importFrom vctrs new_data_frame df_ptype2 tib_cast df_cast
#' @keywords internal
#' @name panel_data-vctrs
NULL

# --- is_panel_sorted: O(n) check if panel data is properly sorted ---

#' Check if panel data is properly sorted
#'
#' Internal function that checks if a data frame is sorted by id (grouped),
#' then by wave within each id. This is O(n) - just one pass through the data,
#' much cheaper than O(n log n) sorting.
#'
#' @param x A data frame
#' @param id Name of the id column (string)
#' @param wave Name of the wave column (string)
#' @return TRUE if properly sorted, FALSE otherwise
#' @keywords internal
is_panel_sorted <- function(x, id, wave) {
  if (nrow(x) <= 1) return(TRUE)
  
  id_col <- x[[id]]
  wave_col <- x[[wave]]
  
  # Handle NA values - if any NAs in key columns, can't validate, assume unsorted
  if (anyNA(id_col) || anyNA(wave_col)) {
    return(FALSE)
  }
  
  # For efficiency, work with integer codes for factors
  if (is.factor(id_col)) {
    id_codes <- as.integer(id_col)
  } else {
    id_codes <- as.integer(factor(id_col))
  }
  
  if (is.factor(wave_col)) {
    wave_codes <- as.integer(wave_col)
  } else {
    wave_codes <- as.numeric(wave_col)
  }
  
  # Check: for each row i, either:
  # - id[i+1] > id[i] (new group, any wave is fine), or
  # - id[i+1] == id[i] AND wave[i+1] >= wave[i] (same group, wave non-decreasing)
  n <- length(id_codes)
  for (i in seq_len(n - 1)) {
    if (id_codes[i + 1] < id_codes[i]) {
      # IDs went backwards
      return(FALSE)
    } else if (id_codes[i + 1] == id_codes[i]) {
      # Same ID - wave should be non-decreasing
      if (wave_codes[i + 1] < wave_codes[i]) {
        return(FALSE)
      }
    }
    # else: id_codes[i+1] > id_codes[i], new group, always OK
  }
  
  TRUE
}

# --- build_panel_data: Lightweight internal constructor ---
# Unlike panel_data(), this does NOT validate, sort, or set up grouping by default.
# It simply attaches attributes and class. Use for fast reconstruction.
# Set validate_order = TRUE to check (and fix) sorting, which is O(n) to check
# and O(n log n) only if actually unsorted.

#' Lightweight panel_data constructor
#'
#' Internal helper function for fast reconstruction of panel_data objects.
#' Unlike [panel_data()], this does NOT validate, sort, or set up grouping
#' by default. It simply attaches attributes and class. Use for fast
#' reconstruction after operations that preserve the panel structure.
#'
#' Set `validate_order = TRUE` to check if data is sorted and fix if needed.
#' The check is O(n); sorting only happens if data is actually unsorted.
#'
#' @param x A data frame to convert
#' @param id Name of the id column (string)
#' @param wave Name of the wave column (string)
#' @param periods Vector of time periods (optional)
#' @param reshaped Logical indicating if data was reshaped (optional)
#' @param varying Character vector of varying variable names (optional)
#' @param constants Character vector of constant variable names (optional)
#' @param validate_order If TRUE, check if data is sorted and re-sort if not.
#'   Default FALSE for speed. Set TRUE when row order might have changed.
#' @return A panel_data object
#' @keywords internal
build_panel_data <- function(x, id, wave, periods = NULL,
                              reshaped = NULL, varying = NULL,
                              constants = NULL, validate_order = FALSE) {
  # Only set periods if not provided
  if (is.null(periods) && wave %in% names(x)) {
    periods <- sort(unique(x[[wave]]))
  }
  
  # Ensure grouping is maintained
  if (id %in% names(x) && !inherits(x, "grouped_df")) {
    x <- dplyr::group_by(x, !!rlang::sym(id))
  }
  
  # Validate and fix order if requested (O(n) check, O(n log n) sort only if needed)
  if (validate_order && id %in% names(x) && wave %in% names(x)) {
    if (!is_panel_sorted(x, id, wave)) {
      x <- dplyr::arrange(x, !!rlang::sym(wave), .by_group = TRUE)
    }
  }

  # Build the tibble with panel_data class and attributes
  tibble::new_tibble(
    x,
    id = id,
    wave = wave,
    periods = periods,
    reshaped = reshaped,
    varying = varying,
    constants = constants,
    class = c("panel_data", "grouped_df"),
    nrow = nrow(x)
  )
}

# --- vec_restore: Restore panel_data after subsetting ---

#' @rdname panel_data-vctrs
#' @export
#' @method vec_restore panel_data
vec_restore.panel_data <- function(x, to, ...) {
  id <- get_id(to)
  wave <- get_wave(to)
  
  # If key columns are missing, fall back to tibble
  if (id %nin% names(x)) {
    return(tibble::as_tibble(x))
  }
  if (wave %nin% names(x)) {
    return(tibble::as_tibble(x))
  }
  
  # Restore panel_data with attributes from 'to'
  # validate_order = TRUE ensures data stays sorted (O(n) check, sort only if needed)
  build_panel_data(
    x,
    id = id,
    wave = wave,
    periods = get_periods(to),
    reshaped = attr(to, "reshaped"),
    varying = attr(to, "varying"),
    constants = attr(to, "constants"),
    validate_order = TRUE
  )
}

# --- vec_proxy: Return plain data frame for vctrs operations ---

#' @rdname panel_data-vctrs
#' @export
#' @method vec_proxy panel_data
vec_proxy.panel_data <- function(x, ...) {
  vctrs::new_data_frame(x)
}

# --- vec_ptype2: Define coercion hierarchy ---

#' @rdname panel_data-vctrs
#' @export
#' @method vec_ptype2 panel_data
vec_ptype2.panel_data <- function(x, y, ...) {
  UseMethod("vec_ptype2.panel_data", y)
}

#' @rdname panel_data-vctrs
#' @export
vec_ptype2.panel_data.panel_data <- function(x, y, ...) {
  panel_ptype2(x, y, ...)
}

#' @rdname panel_data-vctrs
#' @export
vec_ptype2.panel_data.data.frame <- function(x, y, ...) {
  panel_ptype2(x, y, ...)
}

#' @rdname panel_data-vctrs
#' @export
vec_ptype2.data.frame.panel_data <- function(x, y, ...) {
  panel_ptype2(y, x, ...)
}

#' @rdname panel_data-vctrs
#' @export
vec_ptype2.panel_data.tbl_df <- function(x, y, ...) {
  panel_ptype2(x, y, ...)
}

#' @rdname panel_data-vctrs
#' @export
vec_ptype2.tbl_df.panel_data <- function(x, y, ...) {
  panel_ptype2(y, x, ...)
}

# Internal helper for ptype2
panel_ptype2 <- function(x, y, ...) {
  id <- get_id(x)
  wave <- get_wave(x)
  periods <- get_periods(x)
  
  # Get common prototype from underlying data frames
  out <- vctrs::df_ptype2(x, y, ...)
  
  build_panel_data(
    out,
    id = id,
    wave = wave,
    periods = periods
  )
}

# --- vec_cast: Type conversion ---

#' @rdname panel_data-vctrs
#' @export
#' @method vec_cast panel_data
vec_cast.panel_data <- function(x, to, ...) {
  UseMethod("vec_cast.panel_data")
}

#' @rdname panel_data-vctrs
#' @export
vec_cast.panel_data.panel_data <- function(x, to, ...) {
  id <- get_id(to)
  wave <- get_wave(to)
  
  # Cast underlying data
  tbl <- vctrs::tib_cast(x, to, ...)
  
  build_panel_data(
    tbl,
    id = id,
    wave = wave,
    periods = get_periods(to),
    reshaped = attr(to, "reshaped"),
    varying = attr(to, "varying"),
    constants = attr(to, "constants")
  )
}

#' @rdname panel_data-vctrs
#' @export
vec_cast.panel_data.data.frame <- function(x, to, ...) {
  id <- get_id(to)
  wave <- get_wave(to)
  
  # Verify required columns exist
  if (id %nin% names(x)) {
    stop("Cannot cast to panel_data: missing id column '", id, "'")
  }
  if (wave %nin% names(x)) {
    stop("Cannot cast to panel_data: missing wave column '", wave, "'")
  }
  
  # Cast underlying data
  tbl <- vctrs::tib_cast(x, to, ...)
  
  build_panel_data(
    tbl,
    id = id,
    wave = wave,
    periods = get_periods(to)
  )
}

#' @rdname panel_data-vctrs
#' @export
vec_cast.panel_data.tbl_df <- vec_cast.panel_data.data.frame

#' @rdname panel_data-vctrs
#' @export
vec_cast.data.frame.panel_data <- function(x, to, ...) {
  vctrs::df_cast(x, to, ...)
}

#' @rdname panel_data-vctrs
#' @export
vec_cast.tbl_df.panel_data <- function(x, to, ...) {
  vctrs::tib_cast(x, to, ...)
}