test_that("convert_xsyg2xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## XSYG
  file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
  testthat::expect_s3_class(convert_xsyg2xlum(file), "xml_document")


})
