x <- data.frame(tel_1 = c(111, 222, 444, 555),
                tel_2 = c(222, 666, 666, 555),
                tel_3 = c(NA, NA, NA, 555),
                tel_4 = c(NA, NA, NA, 555),
                tel_5 = c(NA, NA, NA, 555),
                name = paste0("name", 1:4),
                nace = c("01.19", "01.64", "55.90", "09.10"))
cols_dedupe <- paste0("tel_", 1:5)


test_that("errors", {
  expect_error(dedupe_wide(x, c(cols_dedupe, NA), cols_expand, max_new_cols, enable_drop),
               "Argument passed to cols_dedupe cannot be NULL or contains NA.")
  expect_error(dedupe_wide(x, cols_dedupe, c(cols_expand, NA), max_new_cols, enable_drop),
               "Argument passed to cols_expand cannot contains NA.")
  enable_drop <- c(1, 2, 3)
  expect_error(dedupe_wide(x, cols_dedupe, cols_expand, max_new_cols, enable_drop),
               "Argument passed to enable_drop must be of length 1, of type logical and cannot be NA.")
  enable_drop <- TRUE
  max_new_cols <- NA
  expect_error(dedupe_wide(x, cols_dedupe, cols_expand, max_new_cols, enable_drop),
               "Argument passed to max_new_cols must be of length 1, of type numeric and cannot be NA.")
})

test_that("no changes in df", {
  x <- x[, c(1, 6, 7)]
  x$tel_2 <- 1:4
  cols_dedupe <- c("tel_1", "tel_2")
  expect_equal(dedupe_wide(x, cols_dedupe), x)
  x[, c(1, 4)] <- NA
  expect_equal(dedupe_wide(x, cols_dedupe), x)
  x <- x[FALSE, ]
  expect_equal(dedupe_wide(x, cols_dedupe), x)
})
