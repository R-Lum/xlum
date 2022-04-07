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

  ## .convert2numeric
  l <- list(a = c("12 12 13"), b = "0")
  out <- expect_type(xlum:::.convert2numeric(l), "list")

    ## regression test
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

  expect_type(.toISODate("20221111150310"), "character")
  expect_type(.toISODate("201020", format = "YYMMDD"), "character")

})
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
test_that(".regmatches", {
  testthat::skip_on_cran()
  local_edition(3)

  text <- c("Text to (extract )")
  t <- expect_type(.regmatches(text, "(?<=\\()[a-z]+"), "character")
  expect_equal(t, "extract")

})
