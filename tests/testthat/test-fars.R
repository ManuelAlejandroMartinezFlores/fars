context("fars")

test_that("fars_map_state works", {
  setwd(system.file("extdata", package = "fars"))

  expect_null(fars_map_state(13, 2015))
  expect_error(fars_map_state(15, 2015), "nothing to draw: all regions out of bounds")
  expect_error(fars_map_state(63, 2013), "invalid STATE number: 63")
})

test_that("single year data is summarized successfully", {
  setwd(system.file("extdata", package = "fars"))

  fars_summary_2013 <- tibble::tribble(
    ~MONTH, ~`2013`,
    1,   2230,
    2,   1952,
    3,   2356,
    4,   2300,
    5,   2532,
    6,   2692
  )
  fars_summary_2013 <- purrr::map_df(fars_summary_2013, as.integer)

  expect_equal(dim(fars_summarize_years(2014)), c(12, 2))
  expect_equal(head(fars_summarize_years(2013)), fars_summary_2013)
})


test_that("mulitple years data is summarized successfully", {
  setwd(system.file("extdata", package = "fars"))

  fars_summary_2013_to_15 <- tibble::tribble(
    ~MONTH, ~`2013`, ~`2014`, ~`2015`,
    7,   2660,   2696,   2998,
    8,   2899,   2800,   3016,
    9,   2741,   2618,   2865,
    10,   2768,   2831,   3019,
    11,   2615,   2714,   2724,
    12,   2457,   2604,   2781
  )
  fars_summary_2013_to_15 <- purrr::map_df(fars_summary_2013_to_15, as.integer)

  expect_equal(dim(fars_summarize_years(c(2013, 2014, 2015))), c(12, 4))
  expect_equal(tail(fars_summarize_years(c(2013, 2014, 2015))),
               fars_summary_2013_to_15)
})

test_that("fars_read works", {
  path_to_2013_data <- system.file("extdata", "accident_2013.csv.bz2",
                                   package = "fars")

  expect_error(fars:::fars_read("no_file_there"), "file 'no_file_there' does not exist")
  expect_is(fars:::fars_read(path_to_2013_data), "tbl_df")
  expect_equal(dim(fars:::fars_read(path_to_2013_data)), c(30202, 50))
})


test_that("make_filename works", {
  expect_equal(fars:::make_filename(2013), "accident_2013.csv.bz2")
})


test_that("fars_read_years works", {
  setwd(system.file("extdata", package = "fars"))

  fars_2014_preprocessed <- tibble::tribble(
    ~MONTH,  ~year,
    11,  2014,
    12,  2014,
    12,  2014,
    12,  2014,
    12,  2014,
    12,  2014
  )
  fars_2014_preprocessed$MONTH <- as.integer(fars_2014_preprocessed$MONTH)

  expect_warning(fars:::fars_read_years(3000), "invalid year: 3000")
  expect_is(fars:::fars_read_years(2014), "list")
  expect_equal(dim(fars:::fars_read_years(2014)[[1]]), c(30056, 2))
  expect_equal(tail(fars:::fars_read_years(2014)[[1]]), fars_2014_preprocessed)

  expect_is(fars:::fars_read_years(c(2013, 2014)), "list")
  expect_equal(length(fars:::fars_read_years(c(2013, 2014))), 2)
  expect_equal(tail(fars:::fars_read_years(c(2013, 2014))[[2]]), fars_2014_preprocessed)
})
