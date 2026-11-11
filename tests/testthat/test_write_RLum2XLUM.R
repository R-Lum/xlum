data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
analysis <- IRSAR.RF.Data
list.analysis <- list(analysis, analysis)
out <- tempfile(fileext = ".xlum")

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(write_RLum2XLUM("any", out),
               "unable to find an inherited method for function 'write_RLum2XLUM'")
  expect_error(write_RLum2XLUM(iris, out),
               "Class 'data.frame' not supported as input")
  expect_error(write_RLum2XLUM(analysis, iris),
               "'file' must be of class 'character'")
  expect_error(write_RLum2XLUM(list(), out),
               "'object' must not be empty")
  expect_error(write_RLum2XLUM(list.analysis, out, split_samples = 1:2),
               "'split_samples' must be a list of indices or NULL")
  expect_error(write_RLum2XLUM(list.analysis, out, split_samples = iris),
               "'split_samples' must be a list of indices or NULL")
  expect_error(write_RLum2XLUM(list.analysis, out,
                               split_samples = list(integer(0))),
               "'split_samples' must contain only non-empty")
  expect_error(write_RLum2XLUM(list.analysis, out,
                               split_samples = list(NA)),
               "'split_samples' must contain only non-empty")
  expect_error(write_RLum2XLUM(list.analysis, out,
                               split_samples = list(TRUE)),
               "'split_samples' must contain only non-empty")
  expect_error(write_RLum2XLUM(list.analysis, out,
                               split_samples = list("a")),
               "'split_samples' must contain only non-empty")
  expect_error(write_RLum2XLUM(list.analysis, out,
                               split_samples = list(1, 1000)),
               "'split_samples' contains out of range indices")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  expect_xlum_samples <- function(file, n) {
    expect_true(file.exists(file))
    expect_length(xml2::xml_find_all(xml2::read_xml(file), ".//sample"), n)
  }

  expect_silent(expect_null(write_RLum2XLUM(analysis, out)))
  expect_xlum_samples(out, 1)

  expect_silent(expect_null(write_RLum2XLUM(list.analysis, out)))
  expect_xlum_samples(out, 1)

  expect_silent(expect_null(write_RLum2XLUM(list.analysis, out,
                                            split_samples = list())))
  expect_xlum_samples(out, 1)

  expect_silent(expect_null(write_RLum2XLUM(list.analysis, out,
                                            split_samples = list(1, 2))))
  expect_xlum_samples(out, 2)
})
