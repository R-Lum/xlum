test_that("read_xlum()", {
  testthat::skip_on_cran()

  ## validate against prototype
  file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")

  ## test options for output
  expect_s3_class(read_xlum(file, output = "xlum_list"), "xlum_list")
  expect_type(read_xlum(file, output = "list"), "list")
  expect_s3_class(read_xlum(file, output = "xml_document"), "xml_document")

  ## verify argument
  xlum <- read_xlum(file, verify = FALSE)
  expect_s3_class(xlum, "xlum_list")
  expect_equal(read_xlum(file, verify = NA),
               xlum)
})
