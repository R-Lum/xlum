# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".convert2character", {
  testthat::skip_on_cran()
  local_edition(3)

  ## .convert2character
    ## test list
    l <- list(a = c(12, 12, 13), b = NA)
    expect_type(.convert2character(l), "list")

    ## test character
    a <- c(12, 12, 13, NA)
    expect_type(.convert2character(a), "character")
})
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".convert2numeric", {
  testthat::skip_on_cran()
  local_edition(3)

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
  local_edition(3)

  ## .get_element_index()
  l <- list(a = list(aa = 1), b = list(bb = 2), c = list(cc = 2))
  out <- expect_type(xlum:::.get_element_index(l, match = "cc"), "list")

    ## regression test
    expect_equal(sum(out[[1]]), 4)

})
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".toISODate", {
  testthat::skip_on_cran()

  t <- expect_type(.toISODate("20221111150310", format = "lexsyg"), "character")
  expect_true(!is.na(t))
  t <- expect_type(.toISODate("20102012:12:12", format = "Risoe"), "character")
  expect_true(!is.na(t))
  t <- expect_type(.toISODate("06/12/2021 12:12:12", format = "Daybreak"), "character")
  expect_true(!is.na(t))
  t <- expect_type(.toISODate("2021-01-0112:12:12", format = "psl"), "character")
  expect_true(!is.na(t))
  expect_error(.toISODate("2021-01-0112:12:12", format = "error"),
               regexp = "\\[\\.toISODate\\(\\)\\] unknown date format")

})
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".regmatches", {
  testthat::skip_on_cran()
  local_edition(3)

  text <- c("Text to (extract )")
  t <- expect_type(.regmatches(text, "(?<=\\()[a-z]+"), "character")
  expect_equal(t, "extract")

  ## check for NA
  expect_true(is.na(.regmatches(text, "error")))

})
