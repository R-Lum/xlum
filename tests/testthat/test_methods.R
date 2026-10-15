file <- system.file("extdata/xlum_example.xlum", package = "xlum")
xlum <- read_xlum(file)

test_that("check functionality", {
  testthat::skip_on_cran()

  xlum_data <- as.list(xlum)
  expect_type(xlum_data, "list")
  expect_false(inherits(xlum_data, "xlum_list"))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  expect_snapshot_output(as.list(xlum))
  expect_snapshot_output(as.list(xlum, encode_base64 = TRUE))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 9
  xlum0 <- xlum
  attributes(xlum0$xlum$sample$sequence[[1]]$curve) <- NULL
  attributes(xlum0$xlum$sample$sequence[[2]]$curve) <- NULL
  expect_silent(res <- as.list(xlum0))
  expect_equal(attr(res$xlum$sample$sequence[[1]][[1]], "tValues"), "")
  expect_equal(attr(res$xlum$sample$sequence[[1]][[1]], "xValues"), "")
  expect_equal(attr(res$xlum$sample$sequence[[1]][[1]], "yValues"), "")
})
