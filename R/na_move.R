na_move <- function(data, cols = names(data), direction = "right") {
  direction <- match.arg(direction, c("right", "bottom", "left", "top"))
  cols <- match.arg(cols, names(dane), several.ok = TRUE)

  attributes_data <- attributes(data)
  data <- copy(dane)

  if (!is.data.table(data)) {
    setDT(data)
  }


}
