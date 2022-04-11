test_that("convert_binx2_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## BINX
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  testthat::expect_s3_class(convert_binx2xlum(file), "xml_document")

  ## BIN
  testthat::expect_s3_class(convert_bin2xlum(file), "xml_document")

})
