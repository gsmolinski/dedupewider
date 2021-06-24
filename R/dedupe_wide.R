check_prerequisites <- function(x, cols_dedupe, cols_expand, max_new_cols, enable_drop) {
  if (!is.data.frame(x)) {
    stop(paste0("x must be of class dataframe, but is ", paste0(class(x), collapse = ", "), "."))
  } else if (is.na(cols_dedupe) || is.null(cols_dedupe)) {
    stop(paste0("Argument passed to cols_dedupe cannot be NULL or NA."))
  } else if (!all(cols_dedupe %in% names(x))) {
    stop(paste0("All columns passed as argument to cols_dedupe must exists as names of columns in x, now '"),
         paste0(cols_dedupe[!cols_dedupe %in% names(x)], collapse = ", "), "' are not present in x.")
  } else if (!(is.na(cols_expand) || is.null(cols_expand))) {
    if (!all(cols_expand %in% names(x))) {
      stop(paste0("All columns passed as argument to cols_expand must exists as names of columns in x, now '"),
           paste0(cols_expand[!cols_expand %in% names(x)], collapse = ", "), "' are not present in x.")
    }
  } else if (any("....idx" == names(x))) {
    stop("In x cannot be a column named '....idx'.") # we will use this name later
  } else if (any(grepl("[.]{4}\\d+$", names(x)))) {
    stop("In x cannot be column ends with 4 dots and number.") # using for new colums after expand or dedupe
  } else if (!is.null(max_new_cols) && (!is.numeric(max_new_cols) || is.na(max_new_cols))) {
    stop("Argument passed to max_new_cols must be numeric and cannot be NA.")
  } else if (!is.null(max_new_cols) && (max_new_cols < length(cols_dedupe))) {
    stop("Argument passed to max_new_cols cannot be lower than length of argument passed to cols_dedupe.")
  } else if (!is.logical(enable_drop) || is.na(enable_drop)) {
    stop("Argument passed to enable_drop must be logical and cannot be NA.")
  }
}

find_duplicated_indexes <- function(x, cols_dedupe) {
  x_tmp <- suppressWarnings(data.table::melt.data.table(x, id.vars = "....idx", measure.vars = cols_dedupe, na.rm = TRUE))
  if (x_tmp[, .N] > 0L) { # in case we had data.frame with all NA in all cols_dedupe columns
    x_tmp[, variable := NULL]
    data.table::setkey(x_tmp, "value") # to speedup
    x_tmp[, V1 := list(list(....idx)), by = "value"][, ....idx := NULL]
    x_tmp <- unique(x_tmp, by = "value")
    x_tmp[, `:=`(filter_col = unlist(lapply(V1, length), use.names = FALSE),
                 value = NULL)]
    x_tmp <- x_tmp[filter_col > 1L]
    if (x_tmp[, .N] == 0L) { # means no duplicated data found
      NULL
    } else {
      x_tmp[, V1 := lapply(V1, unique)][, `:=`(main_index = unlist(lapply(V1, min), use.names = FALSE),
                                                    filter_col = unlist(lapply(V1, length), use.names = FALSE))]
      x_tmp
    }
  } else {
    NULL
  }
}

cascade_indexes <- function(indexes_more_than_one_occurence_vector, indexes_more_than_one_occurence) {
  for (i in indexes_more_than_one_occurence_vector) {
    which <- which(indexes_more_than_one_occurence[["rest_indexes"]] == i)
    value <- data.table::shift(indexes_more_than_one_occurence[which][["main_index"]], fill = i)
    data.table::set(indexes_more_than_one_occurence, which, "rest_indexes", value)
    data.table::setorder(indexes_more_than_one_occurence, -rest_indexes, -main_index)
  }
  indexes_more_than_one_occurence[, filter := data.table::fifelse(main_index == rest_indexes, FALSE, TRUE)]
  indexes_more_than_one_occurence <- indexes_more_than_one_occurence[filter == TRUE][, filter := NULL]
  indexes_more_than_one_occurence <- unique(indexes_more_than_one_occurence, by = "rest_indexes")
  indexes_more_than_one_occurence
}

dedupe_indexes <- function(indexes_more_than_one_occurence, x) {
  for (i in seq_len(indexes_more_than_one_occurence[, .N])) {
    which <- which(x[["....idx"]] == indexes_more_than_one_occurence[["rest_indexes"]][[i]])
    value <- indexes_more_than_one_occurence[["main_index"]][[i]]
    data.table::set(x, which, "....idx", value)
  }
}

expand_columns <- function(x, cols_dedupe, cols_expand, max_new_cols) {
  x_tmp <- x[, .SD, .SDcols = names(x)[names(x) %in% c("....idx", cols_dedupe, cols_expand)]]
  x_tmp <- suppressWarnings(data.table::melt.data.table(x_tmp, measure.vars = cols_dedupe, na.rm = TRUE)) # because we want to treat cols_dedupe as one column, not in separate
  x_tmp[, variable := NULL]
  names(x_tmp)[names(x_tmp) == "value"] <- cols_dedupe[1L] # to get a name of cols_dedupe first column as a base to make names to cols_dedupe after expand
  data.table::setkey(x_tmp, "....idx")
  x_tmp <- suppressWarnings(data.table::melt.data.table(x_tmp, id.vars = "....idx", na.rm = TRUE))
  x_tmp <- x_tmp[, list(value = unique(value)), by = list(....idx, variable)]
  x_tmp[, number := seq_len(.N), by = list(....idx, variable)]
  if (!is.null(max_new_cols) && max_new_cols < max(x_tmp$number)) {
    x_tmp <- x_tmp[number <= max_new_cols]
  }
  x_tmp[, `:=`(variable = paste0(variable, "....", number),
               number = NULL)]
  x_tmp <- data.table::dcast.data.table(x_tmp, ....idx ~ variable)
  x_tmp
}

sort_columns <- function(cols_names, cols_original, cols_dedupe, cols_expand) {
  cols_all <- data.table::data.table(cols = cols_names,
                              idx_all = seq_len(length(cols_names)))
  cols_original_tbl <- data.table::data.table(cols = cols_original,
                                     idx_original = seq_len(length(cols_original)))
  suppressWarnings(cols_all[, `:=`(number = as.integer(sub("^.+[.]", "", cols)),
                          cols = sub("[.]{4}\\d+$", "", cols))])
  
  cols_joined <- cols_original_tbl[cols_all, on = "cols"]
  data.table::setorder(cols_joined, idx_original, number)
  sorted_cols <- cols_joined[["idx_all"]]
  sorted_cols
} # all of this to preserve natural order of columns

dedupe_wide <- function(x, cols_dedupe, cols_expand = NULL, max_new_cols = NULL, enable_drop = FALSE) {
  
  check_prerequisites(x, cols_dedupe, cols_expand, max_new_cols, enable_drop)
  class_x <- attr(x, "class")
  x <- data.table::copy(x)
  if (!data.table::is.data.table(x)) {
    data.table::setDT(x)
  }
  
  if (x[, .N] > 0L) {
    
    x[, ....idx := .I]
    cols_original <- names(x)
    data.table::setkey(x, "....idx")
    
    indexes <- find_duplicated_indexes(x, cols_dedupe)
    
    if (!is.null(indexes)) {
      indexes_more_than_one_occurence <- indexes[filter_col > 1L][, filter_col := NULL]
      indexes_all_uniq <- indexes[, list(indexes = unique(sort(unlist(V1, use.names = FALSE))))][["indexes"]]
      x_2 <- x[!....idx %in% indexes_all_uniq]
      x <- x[....idx %in% indexes_all_uniq] # not necessary to work on all indexes
      
      x_2_not_empty <- x_2[, .N] > 0L
      if (x_2_not_empty) {
        new_names_cols_dedupe <- paste0(cols_dedupe[1L], "....", seq_len(length(cols_dedupe)))
        names(x_2)[names(x_2) %in% cols_dedupe] <- new_names_cols_dedupe
        if (!(is.null(cols_expand) || is.na(cols_expand))) {
          names(x_2)[names(x_2) %in% cols_expand] <- sapply(names(x_2)[names(x_2) %in% cols_expand], function(x) paste0(x, "....", seq_len(length(x))))
        }
      }
      
      if (indexes_more_than_one_occurence[, .N] > 0L) {
        indexes_more_than_one_occurence_vector <- indexes_more_than_one_occurence[, list(indexes = unique(sort(unlist(V1, use.names = FALSE), decreasing = TRUE)))][["indexes"]] # exclude indexes where main and rest are the same, decreasing order - important!
        
        indexes_more_than_one_occurence[, V1 := lapply(V1, function(x) x[-which.min(x)])] # min number will be key
        indexes_more_than_one_occurence <- indexes_more_than_one_occurence[, list(rest_indexes = unlist(V1, use.names = FALSE)), by = main_index]
        data.table::setorder(indexes_more_than_one_occurence, -rest_indexes, -main_index) # for cascading we need specific order, so on the end the lowest number in each group will be a key
        indexes_more_than_one_occurence <- unique(indexes_more_than_one_occurence)
        
        indexes_more_than_one_occurence <- cascade_indexes(indexes_more_than_one_occurence_vector, indexes_more_than_one_occurence) # simulate process of deduplication on indexes
        dedupe_indexes(indexes_more_than_one_occurence, x) # dedupe indexes in main data
      }
      
      expanded_columns <- expand_columns(x, cols_dedupe, cols_expand, max_new_cols)
      data.table::setkey(expanded_columns, "....idx")
      x <- unique(x, by = "....idx")
      
      x <- x[, .SD, .SDcols = names(x)[!names(x) %in% c(cols_dedupe, cols_expand)]]
      x <- expanded_columns[x, on = "....idx", nomatch = 0L]
      
      if (x_2_not_empty) {
        if (enable_drop) {
          cols_to_drop <- x_2[, lapply(.SD, function(x) all(is.na(x))), .SDcols = new_names_cols_dedupe[new_names_cols_dedupe != paste0(cols_dedupe[1L], "....1")]]
          cols_to_drop <- data.table::melt.data.table(cols_to_drop, measure.vars = names(cols_to_drop))
          cols_to_drop <- cols_to_drop[value == TRUE]
          x_2 <- x_2[, .SD, .SDcols = names(x_2)[!names(x_2) %in% cols_to_drop$variable]]
        }
        x <- data.table::rbindlist(list(x, x_2), fill = TRUE)
        x <- x[order(....idx)] # to preserve natural order of rows
      }
      
      sorted_cols <- sort_columns(names(x), cols_original, cols_dedupe, cols_expand)
      data.table::setcolorder(x, names(x)[sorted_cols])
    }
    
    x[, ....idx := NULL]
    attr(x, "class") <- class_x
    x
  } else {
    attr(x, "class") <- class_x
    x
  }
}
