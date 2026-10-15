data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
data(ExampleData.RLum.Data.Image, envir = environment(), package = "Luminescence")
data(ExampleData.XSYG, envir = environment(), package = "Luminescence")
RLum.Data.Curve <- IRSAR.RF.Data@records

test_that("input validation", {
  testthat::skip_on_cran()

  expect_error(convert_rlum2xlum("error"),
               "[convert_rlum2xlum()] Class 'character' not supported as input",
               fixed = TRUE)
  expect_error(convert_rlum2xlum(list(IRSAR.RF.Data, iris, "error")),
               "Class 'data.frame' not supported as input")
  expect_error(convert_rlum2xlum(list(IRSAR.RF.Data, IRSAR.RF.Data@records[[1]])),
               "All input objects must be of the same class, either")
})

test_that("check functionality", {
  testthat::skip_on_cran()

  ## writing to file
  out_file <- tempfile(fileext = ".xlum")
  expect_silent(convert_rlum2xlum(RLum.Data.Curve[[1]], out_file = out_file))
  expect_true(file.exists(out_file))

  out_file_noext <- tempfile()
  expect_silent(convert_rlum2xlum(RLum.Data.Curve[[1]], out_file = out_file_noext))
  expect_true(file.exists(paste0(out_file_noext, ".xlum")))

  ## use BIN/BINX data
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  SW({
  BINfile <- Luminescence::read_BIN2R(file, fastForward = TRUE, verbose = FALSE)
  })
  xlum <- convert_rlum2xlum(BINfile)
  expect_s3_class(xlum, "xml_document")
  expect_true(validate_xlum(xlum))

  ## Daybreak
  file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  temp <- Luminescence::read_Daybreak2R(file, verbose = FALSE)
  xlum <- convert_rlum2xlum(temp)
  expect_s3_class(xlum, "xml_document")
  expect_true(validate_xlum(xlum))

  ## PSL
  file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
  psl <- Luminescence::read_PSL2R(file, drop_bg = FALSE, as_decay_curve = TRUE,
                                  smooth = TRUE, merge = FALSE, verbose = FALSE)
  xlum <- convert_rlum2xlum(psl)
  expect_s3_class(xlum, "xml_document")
  expect_true(validate_xlum(xlum))

  ## RF
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- Luminescence::read_RF2R(file, verbose = FALSE)
  xlum <- convert_rlum2xlum(temp)
  expect_s3_class(xlum, "xml_document")
  expect_true(validate_xlum(xlum))
})

test_that("snapshot tests", {
  testthat::skip_on_cran()

  ## RLum.Data.Analysis
  expect_snapshot_xlum(convert_rlum2xlum(IRSAR.RF.Data))

  ## RLum.Data.Curve
  expect_snapshot_xlum(convert_rlum2xlum(RLum.Data.Curve[[1]]))
  expect_snapshot_xlum(convert_rlum2xlum(RLum.Data.Curve))

  ## RLum.Data.Image
  image_1_frame <- ExampleData.RLum.Data.Image
  image_1_frame@data <- image_1_frame@data[1:100, 1:10, , drop = FALSE]
  expect_snapshot_xlum(convert_rlum2xlum(image_1_frame))

  image_2_frames <- image_1_frame
  dim(image_2_frames@data) <- c(50, 10, 2)
  expect_snapshot_xlum(convert_rlum2xlum(image_2_frames))

  ## RLum.Data.Spectrum
  spectrum <- TL.Spectrum
  spectrum@data <- spectrum@data[1:100, 1:10]
  expect_snapshot_xlum(convert_rlum2xlum(spectrum))
})

test_that("regression tests", {
  testthat::skip_on_cran()

  ## issue 5
  image <- as(array(1:200, dim = c(10, 10, 2)), "RLum.Data.Image")
  expect_snapshot_xlum(convert_rlum2xlum(image))

  ## issue 11
  xlum <- convert_rlum2xlum(IRSAR.RF.Data)
  expect_true(validate_xlum(xlum))
})
