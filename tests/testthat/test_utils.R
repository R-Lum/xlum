# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".convert2character", {
  testthat::skip_on_cran()

  ## .convert2character
    ## test list
    l <- list(a = c(12, 12, 13), b = NA)
    expect_type(.convert2character(l), "list")

    ## test character
    a <- c(12, 12, 13, NA)
    expect_type(.convert2character(a), "character")

  ## .convert2character with base64 encoding
  expect_type(.convert2character(l, encode_base64 = TRUE), "list")
  expect_type(.convert2character(a, encode_base64 = TRUE), "character")
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".convert2numeric", {
  testthat::skip_on_cran()

  ## .convert2numeric (standard)
  l <- list(a = c("12 12 13"), b = "0")
  out <- expect_type(xlum:::.convert2numeric(l), "list")

    ## regression test
    expect_type(out[[1]], "double")
    expect_type(out[[2]], "double")

  ## .convert2numeric (base64 encoding)
  l <- list(a = base64enc::base64encode(charToRaw(c("12 12 13"))), b = "0")
  out <- expect_type(xlum:::.convert2numeric(l, check_base64 = TRUE), "list")

    ## regression tests
  expect_type(out[[1]], "double")
  expect_type(out[[2]], "double")
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".get_element_index", {
  testthat::skip_on_cran()

  ## .get_element_index()
  l <- list(a = list(aa = 1), b = list(bb = 2), c = list(cc = 2))
  out <- expect_type(xlum:::.get_element_index(l, match = "cc"), "list")

    ## regression test
    expect_equal(sum(out[[1]]), 4)
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".toISODate", {
  testthat::skip_on_cran()

  expect_error(.toISODate("2021-01-0112:12:12"),
               "\\[\\.toISODate\\(\\)\\] Unknown date format")
  expect_error(.toISODate("2021-01-0112:12:12", format = "error"),
               "\\[\\.toISODate\\(\\)\\] Unknown date format")

  expect_equal(.toISODate("20221111150310", format = "lexsyg"),
               "2022-11-11T15:03:10Z")
  expect_equal(.toISODate("20102012:12:12", format = "Risoe"),
               "2020-10-20T12:12:12Z")
  expect_equal(.toISODate("06/12/2021 12:12:12", format = "Daybreak"),
               "2021-12-06T12:12:12Z")
  expect_equal(.toISODate("2021-01-0112:12:12", format = "psl"),
               "2021-01-01T12:12:12Z")

  ## vectorised input
  expect_equal(.toISODate(c("20221111150310", "20230101000000"), format = "lexsyg"),
               c("2022-11-11T15:03:10Z", "2023-01-01T00:00:00Z"))

  ## unparsable dates return NA
  expect_true(is.na(.toISODate("error", format = "lexsyg")))
  expect_true(is.na(.toISODate(NA_character_, format = "lexsyg")))
})

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".regmatches", {
  testthat::skip_on_cran()

  text <- c("Text to (extract )")
  t <- expect_type(.regmatches(text, "(?<=\\()[a-z]+"), "character")
  expect_equal(t, "extract")

  ## check for NA
  expect_true(is.na(.regmatches(text, "error")))
})
