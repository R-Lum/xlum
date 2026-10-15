file <- system.file("extdata/xlum_example.xlum", package = "xlum")
proto <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
base64 <- system.file("extdata/xlum_examplebase64.xlum", package = "xlum")
invalid <- system.file("extdata/xlum_invalid.xlum", package = "xlum")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(expect_true(validate_xlum(proto, xsd_version = 9.999)),
               "[validate_xlum()] XSD version unknown, known versions: 1",
               fixed = TRUE)
  expect_error(validate_xlum(proto, xsd_version = NA),
               "XSD version unknown")
  expect_error(validate_xlum("not_existing.xlum"),
               "does not exist")

  ## a file that is not XML at all
  not_xml <- tempfile()
  expect_error(validate_xlum(not_xml),
               "does not exist")
  writeLines("not xml at all", not_xml)
  expect_error(validate_xlum(not_xml),
               "[validate_xlum()] Start tag expected, '<' not found",
               fixed = TRUE)
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_true(validate_xlum(proto))
  expect_true(validate_xlum(base64))
  expect_true(validate_xlum(file, xsd_version = 1))
  expect_true(validate_xlum(file, xsd_version = "1"))
  expect_true(validate_xlum(read_xlum(file, output = "xml_document")))

  expect_false(suppressWarnings(validate_xlum(invalid)))
  expect_false(suppressWarnings(validate_xlum(read_xlum(invalid,
                                                        output = "xml_document"))))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_output(validate_xlum(file))
  SW({
  expect_snapshot_output(validate_xlum(invalid))
  })
})
