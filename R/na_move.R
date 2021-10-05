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
  } else { # "top" or "bottom"

  }
}
