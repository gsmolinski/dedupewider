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
  expect_equal(df_correct, df_moved)
})

test_that("moving NA to left works", {
  df_correct <- data.frame(col1 = c(NA, NA, NA, NA),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(1, NA, 1, NA),
                           col4 = c(1, 2, 2, 2))
  df_moved <- na_move(df_simple, direction = "left")
  expect_equal(df_correct, df_moved)
})

test_that("moving NA to top works", {
  df_correct <- data.frame(col1 = c(NA, NA, 1, 2),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(NA, NA, 1, 2),
                           col4 = c(NA, NA, 1, 2))
  df_moved <- na_move(df_simple, direction = "top")
  expect_equal(df_correct, df_moved)
})

test_that("moving NA to bottom works", {
  df_correct <- data.frame(col1 = c(1, 2, NA, NA),
                           col2 = c(NA, NA, NA, NA),
                           col3 = c(1, 2, NA, NA),
                           col4 = c(1, 2, NA, NA))
  df_moved <- na_move(df_simple, direction = "bottom")
  expect_equal(df_correct, df_moved)
})


