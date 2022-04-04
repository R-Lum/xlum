test_that("validate_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## validate against prototype
  file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
  expect_true(validate_xlum(file))

})
