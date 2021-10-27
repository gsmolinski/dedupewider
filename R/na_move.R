#' Move \code{NA} across columns or rows
#'
#' For chosen columns, move \code{NA} to right or left (i.e. across columns)
#' or to top or bottom (i.e. across rows).
#'
#' @param data A data.frame without column named "....idx".
#' @param cols A character vector of columns' names in \code{data} across which function will be performed. If
#' \code{NULL}, first column in \code{data} will be used. By default all columns will be used.
#' @param direction A character vector of length 1 indicating where to move \code{NA}. Can be one of \code{"top", "right",
#' "bottom", "left"}. If \code{NULL} and also by default, \code{"right"} direction will be used.
#'
#' @return A data.frame with only these attributes preserved, which are returned by \code{\link[base]{attributes}}
#' function used on object passed to \code{data} parameter.
#' @note Internally, function is mainly based on \code{\link[=data.table]{data.table}} functions and thus enabling parallel computation
#' is possible. To do this, just call \code{\link[data.table]{setDTthreads}} before calling \code{na_move} function.
#' @export
#' @import data.table
#' @examples
#' data <- data.frame(col1 = c(1, 2, 3),
#'                    col2 = c(NA, NA, 4),
#'                    col3 = c(5, NA, NA),
#'                    col4 = c(6, 7, 8))
#' data
#' na_move(data, c("col2", "col3", "col4"), direction = "right")
na_move <- function(data, cols = names(data), direction = "right") {
  direction <- match.arg(direction, c("right", "bottom", "left", "top"))
  cols <- match.arg(cols, names(data), several.ok = TRUE)
  check_prerequisites_na_move(data)

  attributes_data <- attributes(data)
  cols_all <- names(data) # before setDT, because names makes reference
  data <- copy(data)

  if (!is.data.table(data)) {
    setDT(data)
  }

  if (direction %in% c("right", "left")) {
    check_cols_lgl <- cols_all %in% cols

    if (!all(check_cols_lgl)) { # no need to work on entire dataframe
      data_rest <- data[, .SD, .SDcols = cols_all[!check_cols_lgl]]
      data <- data[, .SD, .SDcols = cols_all[check_cols_lgl]]
    }

    data <- melt_data(data)
    data <- dcast_data(data, direction)

    add_missing_cols(data, cols) # modify data in place, so no assignment

    if (direction == "left") {
      reverse_direction(data, cols, direction) # modify data in place, so no assignment
    }

    if (exists("data_rest")) {
      data <- cbind.data.frame(data, data_rest)
    }

    # restore original cols order, not just after cbind but also when cols
    # were in different places in dataframe
    setcolorder(data, cols_all)

  } else { # "top" or "bottom"
    data[, (cols) := lapply(.SD, top_bottom_move, direction), .SDcols = cols]
  }

  attributes(data) <- NULL
  attributes(data) <- attributes_data

  data
}

check_prerequisites_na_move <- function(data) {
  if (any(names(data) == "....idx")) {
    stop("Argument passed to data cannot contain column named '....idx'.")
  } else if (!is.data.frame(data)) {
    stop(paste0("data must be of class data.frame, but is ", paste0(class(data), collapse = ", "), "."))
  }
}

melt_data <- function(data) {
  ....idx <- NULL
  data[, ....idx := .I]
  indexes <- data[, .(....idx)] # just in case if the row contained only NA so will be removed and we want it back
  data <- suppressWarnings(melt.data.table(data, id.vars = "....idx", na.rm = TRUE)) # ....idx was needed for melting
  if (indexes[, .N] > uniqueN(data, by = "....idx")) { # in case row was removed because contained only NA
    data <- data[indexes, on = "....idx"] # we add this row, rest of columns have NA for this added row(s)
    data[is.na(variable), variable := "col1"] # rest will be filled by NA later, so we need just col1
  }
  data
}

dcast_data <- function(data, direction) {
  ....idx <- variable <- NULL
  # easier to work with just "....idx" column name
  setnames(data, 1L, "....idx")
  data[, variable := NULL] # we won't use it for dcast

  data <- dcast.data.table(data, ....idx ~ rowid(....idx, prefix = "col"), value.var = "value")

  data[, ....idx := NULL]
  data
}

add_missing_cols <- function(data, cols) {
  cols_data <- names(data)
  cols_included <- 1L:length(cols_data)
  setnames(data, cols_data, cols[cols_included])
  cols_missing <- cols[-cols_included]
  if (length(cols_missing) > 0L) {
    data[, (cols_missing) := NA] # add columns contained only NA at the beginning
  }
}

reverse_direction <- function(data, cols, direction) {
    setcolorder(data, rev(cols))
    setnames(data, names(data), cols)
}

top_bottom_move <- function(vec, direction) {
  if (direction == "top") {
    c(vec[is.na(vec)], vec[!is.na(vec)])
  } else { # "bottom"
    c(vec[!is.na(vec)], vec[is.na(vec)])
  }
}
