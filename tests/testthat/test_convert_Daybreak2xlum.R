test_that("convert_Daybreak2xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## Daybreak
  file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")

  SW({
  testthat::expect_s3_class(convert_Daybreak2xlum(file), "xml_document")
  })
})
