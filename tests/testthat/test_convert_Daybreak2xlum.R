test_that("convert_Daybreak2xlum()", {
  testthat::skip_on_cran()

  ## Daybreak
  file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  testthat::expect_s3_class(convert_Daybreak2xlum(file), "xml_document")
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## Started=6/4/2015 3:58:46 PM
  file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  expect_snapshot_xlum(convert_Daybreak2xlum(file))

  ## Started=2023-10-19 08:00:30
  file <- test_path("_data/daybreak-1.txt")
  expect_snapshot_xlum(convert_Daybreak2xlum(file))

  ## IRRAD_DATE=2005-2-22
  file <- test_path("_data/daybreak-2.dat")
  expect_snapshot_xlum(convert_Daybreak2xlum(file))
})
