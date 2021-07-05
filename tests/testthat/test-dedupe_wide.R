x <- data.frame(tel_1 = c(111, 222, 444, 555),
                tel_2 = c(222, 666, 666, 555),
                tel_3 = c(NA, NA, NA, 555),
                tel_4 = c(NA, NA, NA, 555),
                tel_5 = c(NA, NA, NA, 555),
                name = paste0("name", 1:4),
                nace = c("01.19", "01.64", "55.90", "09.10"))
cols_dedupe <- paste0("tel_", 1:5)


test_that("errors", {
  expect_error(dedupe_wide(x, c(cols_dedupe, NA)),
               "Argument passed to cols_dedupe cannot be NULL or contains NA.")
  expect_error(dedupe_wide(x, cols_dedupe, cols_expand = NA),
               "Argument passed to cols_expand cannot contains NA.")
  enable_drop <- c(1, 2, 3)
  expect_error(dedupe_wide(x, cols_dedupe, enable_drop = enable_drop),
               "Argument passed to enable_drop must be of length 1, of type logical and cannot be NA.")
  max_new_cols <- NA
  expect_error(dedupe_wide(x, cols_dedupe, max_new_cols = max_new_cols),
               "Argument passed to max_new_cols must be of length 1, of type numeric and cannot be NA.")
})

test_that("no changes in df when empty or NA or no duplicates", {
  x <- x[, c(1, 6, 7)]
  x$tel_2 <- 1:4
  cols_dedupe <- c("tel_1", "tel_2")
  expect_equal(dedupe_wide(x, cols_dedupe), x)
  x[, c(1, 4)] <- NA
  expect_equal(dedupe_wide(x, cols_dedupe), x)
  x <- x[FALSE, ]
  expect_equal(dedupe_wide(x, cols_dedupe), x)
})

test_that("rows with NA are not duplicated", {
  x <- data.frame(tel_1 = c(777, 888, NA, NA),
                  tel_2 = c(888, 777, NA, NA),
                  name = paste0("name", 5:8))
  cols_dedupe <- paste0("tel_", 1:2)
  expect_equal(nrow(dedupe_wide(x, cols_dedupe)), 3)
})

test_that("deduplication", {
  x <- data.frame(tel_1 = c(111, 222, 333, 444, 333, 333,
                            666, 777, 333, 333, 333, 333),
                  tel_2 = c(777, 666, NA, 666, NA, NA, NA, NA, NA, 777,
                            888, 777),
                  tel_3 = c(999, NA, NA, 333, NA, NA, NA, NA, NA, 101, 102,
                            777),
                  tel_4 = c(102, NA, NA, NA, NA, NA, NA, NA, NA, 103, NA, 103),
                  name = paste0("name", 1:12))
  cols_dedupe <- paste0("tel_", 1:4)
  expect_equal(nrow(dedupe_wide(x, cols_dedupe)), 1)
})

test_that("columns order", {
  x <- data.frame(tel_1 = c(111, 222, 444, 555),
                  nace = c("01.19", "01.64", "55.90", "09.10"),
                  name = paste0("name", 1:4),
                  tel_2 = c(222, 666, 666, 555),
                  tel_3 = c(NA, NA, NA, 555),
                  tel_4 = c(NA, NA, NA, 555),
                  tel_5 = c(NA, NA, NA, 555))
  expect_equal(names(dedupe_wide(x, paste0("tel_", 1:5), "name")), c("tel_1....1", "tel_1....2", "tel_1....3",
                                                                     "tel_1....4", "nace", "name....1",
                                                                     "name....2", "name....3"))
})
