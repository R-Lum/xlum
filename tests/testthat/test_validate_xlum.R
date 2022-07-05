test_that("validate_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## validate against prototype
  file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
  expect_true(validate_xlum(file))

  ## use different version
  expect_true(validate_xlum(file, xsd_version = 1))

  ## trigger error
  expect_error(expect_true(validate_xlum(file, xsd_version = 9.999)),
               regexp = "\\[validate\\_xlum\\(\\)\\] XSD version unknown")

  ## check broken prototype
  file_invalid <- system.file("extdata/xlum_invalid.xlum", package = "xlum")
  expect_false(suppressWarnings(validate_xlum(file_invalid)))

})
