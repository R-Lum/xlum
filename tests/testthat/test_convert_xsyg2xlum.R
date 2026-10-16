test_that("convert_xsyg2xlum()", {
  testthat::skip_on_cran()

  ## XSYG
  file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
  testthat::expect_s3_class(convert_xsyg2xlum(file), "xml_document")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
  expect_snapshot_xlum(convert_xsyg2xlum(file))
})
