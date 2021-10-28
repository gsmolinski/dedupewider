df_simple <- data.frame(col1 = c(1, NA, 2, NA),
                 col2 = c(NA, NA, NA, NA),
                 col3 = c(1, 2, NA, NA),
                 col4 = c(NA, NA, 1, 2))

test_that("moving NA to right works", {
  df_correct <- data.frame(col1 = c(1, 2, 2, 2),
                           col2 = c(1, NA, 1, NA),
                           col3 = c(NA, NA, NA, NA),
                           col4 = c(NA, NA, NA, NA))
  df_moved <- na_move(df_simple)
  expect_equal(df_moved, df_correct)
})

test_that("moving NA to left works", {
  df_correct <- data.frame(col1 = c(NA, NA, NA, NA),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(1, NA, 1, NA),
                           col4 = c(1, 2, 2, 2))
  df_moved <- na_move(df_simple, direction = "left")
  expect_equal(df_moved, df_correct)
})

test_that("moving NA to top works", {
  df_correct <- data.frame(col1 = c(NA, NA, 1, 2),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(NA, NA, 1, 2),
                           col4 = c(NA, NA, 1, 2))
  df_moved <- na_move(df_simple, direction = "top")
  expect_equal(df_moved, df_correct)
})

test_that("moving NA to bottom works", {
  df_correct <- data.frame(col1 = c(1, 2, NA, NA),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(1, 2, NA, NA),
                           col4 = c(1, 2, NA, NA))
  df_moved <- na_move(df_simple, direction = "bottom")
  expect_equal(df_moved, df_correct)
})

test_that("cols order is preserved when moving NA to right", {
  df_moved <- na_move(df_simple, cols = c("col2", "col3"))
  expect_equal(names(df_moved), names(df_simple))
  df_moved <- na_move(df_simple, cols = c("col1", "col4")) # this is also a case when given row contains only NA
  expect_equal(names(df_moved), names(df_simple))
})

test_that("cols order is preserved when moving NA to left", {
  df_moved <- na_move(df_simple, cols = c("col2", "col3"), direction = "left")
  expect_equal(names(df_moved), names(df_simple))
  df_moved <- na_move(df_simple, cols = c("col1", "col4"), direction = "left") # this is also a case when given row contains only NA
  expect_equal(names(df_moved), names(df_simple))
})

test_that("even if row contains only NA, function works", {
  df_simple <- df_simple[, c(1, 4)] # row 2 has only NA
  df_moved <- na_move(df_simple)
  expect_equal(nrow(df_moved), nrow(df_simple))
})

test_that("errors if prerequisites not met", {
  names(df_simple)[[1]] <- "....idx"
  expect_error(na_move(df_simple), "Argument passed to data cannot contain column named '....idx'")
})

df_list <- data.table::data.table(col1 = list(c(1, 1, NA), NA, c(2, 2), NA),
                      col2 = c(2, NA, NA, NA),
                      col3 = c(1, 2, NA, NA),
                      col4 = c(NA, NA, 1, 2))

test_that("moving NA to right works if some column is of type list", {
  df_moved <- na_move(df_list)
  expect_equal(sum(is.na(df_moved$col4)), 4) # we want to ignore types, because it is not easy to check this and also not necessary probably
})

test_that("moving NA to left works if some column is of type list", {
  df_moved <- na_move(df_list, direction = "left")
  expect_equal(sum(is.na(df_moved$col1)), 4)
})

test_that("moving NA to top works if some column is of type list", {
  df_moved <- na_move(df_list, direction = "top")
  expect_equal(sum(is.na(as.logical(unlist(df_moved[1, ])))), 4)
})

test_that("moving NA to bottom works if some column is of type list", {
  df_moved <- na_move(df_list, direction = "bottom")
  expect_equal(sum(is.na(as.logical(unlist(df_moved[4, ])))), 4)
})
