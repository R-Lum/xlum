file <- system.file("extdata/xlum_example.xlum", package = "xlum")
xlum <- read_xlum(file)
xlum_data_xml_document <- read_xlum(file, output = "xml_document")
xlum_data_list <- read_xlum(file, output = "list")
out_file <- tempfile(fileext = ".xlum")
out_file_b64 <- tempfile(fileext = ".xlum")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_xlum("error", file = out_file),
               "[write_xlum()] Object of class 'character' not supported as input",
               fixed = TRUE)
  expect_error(write_xlum(xlum, file.path(tempdir(), "sub_dir", "test.xlum")),
               "does not exist")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_silent(expect_null(write_xlum(xlum, out_file)))
  expect_silent(expect_null(write_xlum(xlum, out_file, encode_base64 = TRUE)))
  expect_silent(write_xlum(xlum_data_xml_document, out_file))
  expect_silent(write_xlum(xlum_data_list, out_file))

  ## encode_base64 is ignored if the input is not an xlum_list
  write_xlum(xlum_data_list, out_file, encode_base64 = FALSE)
  write_xlum(xlum_data_list, out_file_b64, encode_base64 = TRUE)
  expect_identical(readLines(out_file),
                   readLines(out_file_b64))

  ## an existing .xlum extension is left untouched
  path <- tempfile(fileext = ".xlum")
  write_xlum(xlum, path)
  expect_true(file.exists(path))
  expect_false(file.exists(paste0(path, ".xlum")))

  ## the .xlum extension is appended when missing
  path <- tempfile()
  write_xlum(xlum, path)
  expect_false(file.exists(path))
  expect_true(file.exists(paste0(path, ".xlum")))

  path <- tempfile(fileext = ".txt")
  write_xlum(xlum, path)
  expect_false(file.exists(path))
  expect_true(file.exists(paste0(path, ".xlum")))
})

test_that("round-trips between write_xlum() and read_xlum()", {
  testthat::skip_on_cran()

  write_xlum(xlum, out_file)
  expect_identical(read_xlum(out_file),
                   xlum)

  write_xlum(xlum_data_list, out_file)
  expect_identical(read_xlum(out_file, output = "list"),
                   xlum_data_list)

  write_xlum(xlum_data_xml_document, out_file)
  expect_identical(as.character(read_xlum(out_file, output = "xml_document")),
                   as.character(xlum_data_xml_document))

  ## encode_base64 argument
  write_xlum(xlum, out_file_b64, encode_base64 = TRUE)
  expect_identical(read_xlum(out_file_b64),
                   xlum)
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  write_xlum(xlum, out_file)
  expect_snapshot(cat(readLines(out_file), sep = "\n"))

  write_xlum(xlum, out_file, encode_base64 = TRUE)
  expect_snapshot(cat(readLines(out_file), sep = "\n"))
})
