test_that("convert_binx2_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ##TODO: activate after 'Luminescence' is shipped with the example BINX
  ## BINX
  #file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  #testthat::expect_s3_class(convert_binx2xlum(file), "xml_document")

  ## BIN
  #testthat::expect_s3_class(convert_bin2xlum(file), "xml_document")

  ## arguments
  #testthat::expect_s3_class(convert_binx2xlum(file, position = 1), "xml_document")

})
