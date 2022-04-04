test_that("utils", {
  testthat::skip_on_cran()
  local_edition(3)

  ## .convert2numeric
  l <- list(a = c("12 12 13"), b = "0")
  out <- expect_type(xlum:::.convert2numeric(l), "list")

    ## regression test
    expect_type(out[[1]], "double")
    expect_type(out[[2]], "double")

  ## ,get_element_index()
  l <- list(a = list(aa = 1), b = list(bb = 2), c = list(cc = 2))
  out <- expect_type(xlum:::.get_element_index(l, match = "cc"), "list")

    ## regression test
    expect_equal(sum(out[[1]]), 4)

})
