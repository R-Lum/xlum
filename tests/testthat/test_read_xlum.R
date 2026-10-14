file <- system.file("extdata/xlum_example.xlum", package = "xlum")
proto <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
base64 <- system.file("extdata/xlum_examplebase64.xlum", package = "xlum")

test_that("input validation", {
  testthat::skip_on_cran()

  file_invalid <- system.file("extdata/xlum_invalid.xlum", package = "xlum")

  expect_warning(expect_s3_class(read_xlum(file_invalid), "xlum_list"),
                 "The attribute 'startDate' is not allowed")
  expect_error(expect_warning(read_xlum("not_existing.xlum"),
                              "No such file or directory"),
               "does not exist in current working directory")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## validate against prototype
  expect_s3_class(read_xlum(proto, output = "xlum_list"),
                  "xlum_list")

  ## test base64 decoding
  expect_identical(read_xlum(file),
                   read_xlum(base64))

  ## test options for output
  expect_s3_class(read_xlum(file, output = "unknown"),
                  "xml_document")
  expect_s3_class(read_xlum(file, output = NA),
                  "xml_document")

  ## verify argument
  xlum <- read_xlum(file, verify = FALSE)
  expect_s3_class(xlum, "xlum_list")
  expect_equal(read_xlum(file, verify = NA),
               xlum)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_output(read_xlum(file))
  expect_snapshot_output(read_xlum(file, output = "list"))
  expect_snapshot_xlum(read_xlum(file, output = "xml_document"))
})
