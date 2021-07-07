#' Dedupe across multiple columns
#'
#' Collapse many rows connected by duplicated data (which can exist in different
#' rows and columns) into one, based on data in chosen columns, optionally putting
#' non-consistent data into newly created additional columns.
#'
#' @param x A data.frame without column named '....idx' and any column which ends by four dots and number (e.g. 'column....2').
#' @param cols_dedupe A character vector of length min. 2 of columns' names in \code{x} used to dedupe. Deduplicated data from these columns will be saved into new columns, number of which is control by \code{max_new_cols}.
#' @param cols_expand A character vector of columns' names in \code{x} or \code{NULL} (means: none except those used to dedupe) indicating columns with data to keep in case of non-consistent data, i.e. unique data from these columns will be saved into new columns, number of which is control by \code{max_new_cols}.
#' @param max_new_cols A numeric vector length 1 or \code{NULL} (means: without limit) indicating how many new columns can be created to store data from columns used to dedupe. Cannot be lower than 1.
#' @param enable_drop A logical vector length 1: should given column be dropped if (after deduplication) contains only missing data (\code{NA})? Applicable only to columns used to dedupe.
#' @details Columns passed to \code{cols_dedupe} must be atomic.
#'
#' Row names will always be removed. If you want to preserve row names, simply put in into separate column. Note that if this column won't be passed to \code{cols_expand} argument, only the one row name for duplicated rows will be preserved (row name closest to the top of the table).
#'
#' Although \code{\link[base]{duplicated}} or \code{\link[base]{unique}} treats missing data (\code{NA}) as duplicated data, this function do not do this (see second example below).
#'
#' Type of columns passed to \code{cols_dedupe} will be coerced to the most general type.
#' @return If duplicated data found - data.frame with changed columns' names and optionally additional columns (in some cases less columns, depends on \code{enable_drop} argument). Otherwise data.frame without changes (except row names removed).
#' @export
#' @import data.table
#' @note Internally, function is mainly based on \code{\link[=data.table]{data.table}} functions and thus enabling parallel computation
#' is possible. To do this, just call \code{\link[data.table]{setDTthreads}} before calling \code{dedupe_wide} function.
#' @examples
#' x <- data.frame(tel_1 = c(111, 222, 444, 555),
#'                 tel_2 = c(222, 666, 666, 555),
#'                 name = paste0("name", 1:4))
#'# rows 1, 2, 3 share the same phone numbers
#'
#'dedupe_wide(x,
#'            cols_dedupe = c("tel_1", "tel_2"),
#'            cols_expand = "name")
#' # first three collapsed into one, for name4 keeped only one phone number (555)
#' # 'name1', 'name2', 'name3' keeped in new columns
#'
#' y <- data.frame(tel_1 = c(777, 888, NA, NA),
#'                 tel_2 = c(888, 777, NA, NA),
#'                 name = paste0("name", 5:8))
#' # rows 3 and 4 has only missing data
#'
#' dedupe_wide(y,
#'            cols_dedupe = c("tel_1", "tel_2"),
#'            cols_expand = "name")
#' # first two rows collapsed into one, nothing change for the rest of rows
dedupe_wide <- function(x, cols_dedupe, cols_expand = NULL, max_new_cols = NULL, enable_drop = TRUE) {
  ....idx <- filter_col <- V1 <- main_index <- rest_indexes <- value <- NULL
  check_prerequisites(x, cols_dedupe, cols_expand, max_new_cols, enable_drop)
  class_x <- attr(x, "class")
  pointer_x <- attr(x, ".internal.selfref")
  x <- copy(x)
  if (!is.data.table(x)) {
    setDT(x)
  }

  if (x[, .N] > 0L) {

    if (!all(x[, list(atomic = unlist(lapply(.SD, is.atomic), use.names = FALSE)), .SDcols = cols_dedupe][["atomic"]])) {
      stop("All columns passed to cols_dedupe must be atomic.")
    }

    x[, ....idx := .I]
    cols_original <- names(x)
    setkey(x, "....idx")

    indexes <- find_duplicated_indexes(x, cols_dedupe)

    if (!is.null(indexes)) {
      indexes_more_than_one_occurrence <- indexes[filter_col > 1L][, filter_col := NULL]
      indexes_all_uniq <- indexes[, list(indexes = unique(sort(unlist(V1, use.names = FALSE))))][["indexes"]]
      x_2 <- x[!....idx %in% indexes_all_uniq]
      x <- x[....idx %in% indexes_all_uniq] # not necessary to work on all indexes

      # even if x_2 is empty we need this to rbindlist later, because some columns from x can be missed after melt (e.g. when column contained only NA)
      new_names_cols_dedupe <- paste0(cols_dedupe[1L], "....", seq_len(length(cols_dedupe)))
      names(x_2)[names(x_2) %in% cols_dedupe] <- new_names_cols_dedupe
      if (!is.null(cols_expand)) {
        names(x_2)[names(x_2) %in% cols_expand] <- sapply(names(x_2)[names(x_2) %in% cols_expand], function(x) paste0(x, "....", seq_len(length(x))))
      }

      if (indexes_more_than_one_occurrence[, .N] > 0L) {
        indexes_more_than_one_occurrence_vector <- indexes_more_than_one_occurrence[, list(indexes = unique(sort(unlist(V1, use.names = FALSE), decreasing = TRUE)))][["indexes"]] # exclude indexes where main and rest are the same, decreasing order - important!

        indexes_more_than_one_occurrence[, V1 := lapply(V1, function(x) x[-which.min(x)])] # min number will be key
        indexes_more_than_one_occurrence <- indexes_more_than_one_occurrence[, list(rest_indexes = unlist(V1, use.names = FALSE)), by = main_index]
        setorder(indexes_more_than_one_occurrence, -rest_indexes, -main_index) # for cascading we need specific order, so on the end the lowest number in each group will be a key
        indexes_more_than_one_occurrence <- unique(indexes_more_than_one_occurrence)

        indexes_more_than_one_occurrence <- cascade_indexes(indexes_more_than_one_occurrence_vector, indexes_more_than_one_occurrence) # simulate process of deduplication on indexes
        dedupe_indexes(indexes_more_than_one_occurrence, x) # dedupe indexes in main data
      }

      expanded_columns <- expand_columns(x, cols_dedupe, cols_expand, max_new_cols)
      setkey(expanded_columns, "....idx")
      x <- unique(x, by = "....idx")

      x <- x[, .SD, .SDcols = names(x)[!names(x) %in% c(cols_dedupe, cols_expand)]]
      x <- expanded_columns[x, on = "....idx", nomatch = 0L]

      if (enable_drop) {
        cols_to_drop <- x_2[, lapply(.SD, function(x) all(is.na(x))), .SDcols = new_names_cols_dedupe[new_names_cols_dedupe != paste0(cols_dedupe[1L], "....1")]]
        cols_to_drop <- melt.data.table(cols_to_drop, measure.vars = names(cols_to_drop))
        cols_to_drop <- cols_to_drop[value == TRUE]
        x_2 <- x_2[, .SD, .SDcols = names(x_2)[!names(x_2) %in% cols_to_drop$variable]]
      }
      x <- rbindlist(list(x, x_2), fill = TRUE) # even if x_2 is empty, because in x some columns from cols_expand can be missed (e.g. if contained all NA)
      if (x_2[, .N] > 0L) {
        x <- x[order(....idx)] # to preserve natural order of rows
      }

      sorted_cols <- sort_columns(names(x), cols_original, cols_dedupe, cols_expand)
      setcolorder(x, names(x)[sorted_cols])
    }

    x[, ....idx := NULL]
    attr(x, "class") <- class_x
    if (is.null(pointer_x)) {
      attr(x, ".internal.selfref") <- NULL
    }
    x
  } else {
    attr(x, "class") <- class_x
    if (is.null(pointer_x)) {
      attr(x, ".internal.selfref") <- NULL
    }
    x
  }
}

check_prerequisites <- function(x, cols_dedupe, cols_expand, max_new_cols, enable_drop) {
  if (!is.data.frame(x)) {
    stop(paste0("x must be of class data.frame, but is ", paste0(class(x), collapse = ", "), "."))
  } else if (anyNA(cols_dedupe) || is.null(cols_dedupe)) {
    stop(paste0("Argument passed to cols_dedupe cannot be NULL or contains NA."))
  } else if (length(cols_dedupe) < 2) {
    stop("Vector passed as argument to cols_dedupe must be of length 2 or more.")
  } else if (anyNA(cols_expand)) {
    stop("Argument passed to cols_expand cannot contains NA.")
  } else if (!all(cols_dedupe %in% names(x))) {
    stop(paste0("All columns passed as argument to cols_dedupe must exists as names of columns in x, now '"),
         paste0(cols_dedupe[!cols_dedupe %in% names(x)], collapse = ", "), "' are not present in x.")
  } else if (!all(cols_expand %in% names(x))) {
    stop(paste0("All columns passed as argument to cols_expand must exists as names of columns in x, now '"),
         paste0(cols_expand[!cols_expand %in% names(x)], collapse = ", "), "' are not present in x.")
  } else if (any("....idx" == names(x))) {
    stop("In x cannot be a column named '....idx'.") # we will use this name later
  } else if (any(grepl("[.]{4}\\d+$", names(x)))) {
    stop("In x cannot be column ends with 4 dots and number.") # using for new colums after expand or dedupe
  } else if (!is.null(max_new_cols) && (!is.numeric(max_new_cols) || is.na(max_new_cols) || length(max_new_cols) > 1)) {
    stop("Argument passed to max_new_cols must be of length 1, of type numeric and cannot be NA.")
  } else if (!is.null(max_new_cols) && (max_new_cols < 1)) {
    stop("Argument passed to max_new_cols cannot be lower than 1.")
  } else if (!is.logical(enable_drop) || is.na(enable_drop) || length(enable_drop) > 1) {
    stop("Argument passed to enable_drop must be of length 1, of type logical and cannot be NA.")
  } else if (any(cols_dedupe %in% cols_expand)) {
    stop("Names passed to cols_dedupe cannot be passed to cols_expand.")
  }
}

find_duplicated_indexes <- function(x, cols_dedupe) {
  variable <- value <- V1 <- ....idx <- filter_col <- NULL
  x_tmp <- suppressWarnings(melt.data.table(x, id.vars = "....idx", measure.vars = cols_dedupe, na.rm = TRUE))
  if (x_tmp[, .N] > 0L) { # in case we had data.frame with all NA in all cols_dedupe columns
    x_tmp[, variable := NULL]
    setkey(x_tmp, "value") # to speedup
    x_tmp[, V1 := list(list(....idx)), by = value][, ....idx := NULL]
    x_tmp <- unique(x_tmp, by = "value")
    x_tmp[, `:=`(filter_col = lengths(V1),
                 value = NULL)]
    x_tmp <- x_tmp[filter_col > 1L]
    if (x_tmp[, .N] == 0L) { # means no duplicated data found
      NULL
    } else {
      x_tmp[, V1 := lapply(V1, unique)][, `:=`(main_index = unlist(lapply(V1, min), use.names = FALSE),
                                                    filter_col = lengths(V1))]
      x_tmp
    }
  } else {
    NULL
  }
}

cascade_indexes <- function(indexes_more_than_one_occurrence_vector, indexes_more_than_one_occurrence) {
  rest_indexes <- main_index <- filter <- NULL
  for (i in indexes_more_than_one_occurrence_vector) {
    which <- which(indexes_more_than_one_occurrence[["rest_indexes"]] == i)
    value <- shift(indexes_more_than_one_occurrence[which][["main_index"]], fill = i)
    set(indexes_more_than_one_occurrence, which, "rest_indexes", value)
    setorder(indexes_more_than_one_occurrence, -rest_indexes, -main_index)
  }
  indexes_more_than_one_occurrence[, filter := fifelse(main_index == rest_indexes, FALSE, TRUE)]
  indexes_more_than_one_occurrence <- indexes_more_than_one_occurrence[filter == TRUE][, filter := NULL]
  indexes_more_than_one_occurrence <- unique(indexes_more_than_one_occurrence, by = "rest_indexes")
  indexes_more_than_one_occurrence
}

dedupe_indexes <- function(indexes_more_than_one_occurrence, x) {
  for (i in seq_len(indexes_more_than_one_occurrence[, .N])) {
    which <- which(x[["....idx"]] == indexes_more_than_one_occurrence[["rest_indexes"]][[i]])
    value <- indexes_more_than_one_occurrence[["main_index"]][[i]]
    set(x, which, "....idx", value)
  }
}

expand_columns <- function(x, cols_dedupe, cols_expand, max_new_cols) {
  variable <- variable_pasted <- value <- ....idx <- number <- NULL
  x_tmp <- x[, .SD, .SDcols = names(x)[names(x) %in% c("....idx", cols_dedupe, cols_expand)]]
  x_tmp <- suppressWarnings(melt.data.table(x_tmp, measure.vars = cols_dedupe, na.rm = TRUE)) # because we want to treat cols_dedupe as one column, not in separate
  x_tmp[, variable := NULL]
  names(x_tmp)[names(x_tmp) == "value"] <- cols_dedupe[1L] # to get a name of cols_dedupe first column as a base to make names to cols_dedupe after expand
  setkey(x_tmp, "....idx")
  classes <- x_tmp[, list(type = lapply(.SD, typeof))]
  classes[, variable := names(x_tmp)]
  x_tmp <- suppressWarnings(melt.data.table(x_tmp, id.vars = "....idx", na.rm = TRUE))
  x_tmp <- x_tmp[, list(value = unique(value)), by = list(....idx, variable)]
  x_tmp[, number := seq_len(.N), by = list(....idx, variable)]
  if (!is.null(max_new_cols) && max_new_cols < max(x_tmp$number)) {
    x_tmp <- x_tmp[number <= max_new_cols]
  }
  x_tmp[, `:=`(variable_pasted = paste0(variable, "....", number),
               number = NULL)]
  cols_names_for_classes <- unique(x_tmp[, list(variable_pasted, variable)], by = "variable_pasted")
  classes <- classes[cols_names_for_classes, on = "variable"]
  setorder(classes, variable_pasted)
  classes <- rbindlist(list(data.frame(type = "integer", variable = "....idx", variable_pasted = "....idx"), classes))
  x_tmp[, variable := NULL]
  x_tmp <- dcast.data.table(x_tmp, ....idx ~ variable_pasted)
  for (col in seq_len(classes[, .N])) {
    set(x_tmp, j = col, value = methods::as(x_tmp[[col]], classes[["type"]][[col]]))
  } # we want to preserve original types of columns
  x_tmp
}

sort_columns <- function(cols_names, cols_original, cols_dedupe, cols_expand) {
  cols <- idx_original <- number <- NULL
  cols_all <- data.table(cols = cols_names,
                              idx_all = seq_len(length(cols_names)))
  cols_original_tbl <- data.table(cols = cols_original,
                                     idx_original = seq_len(length(cols_original)))
  suppressWarnings(cols_all[, `:=`(number = as.integer(sub("^.+[.]", "", cols)),
                          cols = sub("[.]{4}\\d+$", "", cols))])

  cols_joined <- cols_original_tbl[cols_all, on = "cols"]
  setorder(cols_joined, idx_original, number)
  sorted_cols <- cols_joined[["idx_all"]]
  sorted_cols
} # all of this to preserve natural order of columns
