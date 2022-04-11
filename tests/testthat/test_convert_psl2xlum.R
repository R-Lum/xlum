test_that("convert_psl2xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## PSL
  file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
  testthat::expect_s3_class(convert_psl2xlum(file), "xml_document")


})
