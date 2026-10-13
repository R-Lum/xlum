test_that("convert_binx2_xlum()", {
  testthat::skip_on_cran()

  # BINX
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")

  SW({
  testthat::expect_s3_class(convert_binx2xlum(file), "xml_document")
  testthat::expect_s3_class(convert_bin2xlum(file), "xml_document")

  # arguments
  testthat::expect_s3_class(convert_binx2xlum(file, position = 1:2), "xml_document")
  })
})
