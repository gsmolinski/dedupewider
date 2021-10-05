#' Title
#'
#' @param data
#' @param cols
#' @param direction
#'
#' @return
#' @export
#'
#' @examples
na_move <- function(data, cols = names(data), direction = "right") {
  direction <- match.arg(direction, c("right", "bottom", "left", "top"))
  cols <- match.arg(cols, names(dane), several.ok = TRUE)

  attributes_data <- attributes(data)
  data <- copy(dane)

  if (!is.data.table(data)) {
    setDT(data)
  }

  if (direction %in% c("right", "left")) {
    cols_all <- names(data)
    check_cols_lgl <- cols_all %in% cols

    if (!all(check_cols_lgl)) { # no need to work on entire dataframe
      data_rest <- data[, .SD, .SDcols = cols_all[!check_cols_lgl]]
      data <- data[, .SD, .SDcols = cols_all[check_cols_lgl]]
    }

    data <- melt_data(data)
    data <- dcast_data(data, direction)

    add_missing_cols(data, cols) # modify data in place, so no assignment

    reverse_direction(data, cols, direction) # modify data in place, so no assignment

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

melt_data <- function(data) {
  data[, ....idx := .I]
  data <- suppressWarnings(melt.data.table(data, id.vars = "....idx", na.rm = TRUE)) # ....idx was needed for melting
  data
}

dcast_data <- function(data, direction) {
  # easier to work with just "....idx" column name
  setnames(data, 1L, "....idx")
  data[, variable := NULL] # we won't use it for dcast

  data <- dcast.data.table(data, ....idx ~ rowid(....idx, prefix = "col"), value.var = "value") # ....idxs as rows

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
  if (direction == "left") {
    setcolorder(data, rev(cols))
    setnames(data, names(data), cols)
  }
}

top_bottom_move <- function(vec, direction) {
  if (direction == "top") {
    c(vec[is.na(vec)], vec[!is.na(vec)])
  } else { # "bottom"
    c(vec[!is.na(vec)], vec[is.na(vec)])
  }
}
