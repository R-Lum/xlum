test_that("write_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

  ## import file
  file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
  xlum_data_xml_document <- read_xlum(file, output = "xml_document")
  xlum_data_list <- read_xlum(file, output = "list")
  xlum_data_xlum_list <- read_xlum(file, output = "xlum_list")

  ## set outfile
  out_file <- tempfile()

  ## crash function
  expect_error(write_xlum("error", file = out_file),
               regexp = "\\[write\\_xlum\\(\\)\\] object of class 'character' not supported as input!")

  ## ensure successful writing
  expect_silent(write_xlum(xlum_data_xml_document, out_file))
  expect_silent(write_xlum(xlum_data_list, out_file))
  expect_silent(write_xlum(xlum_data_xlum_list, out_file))

})
