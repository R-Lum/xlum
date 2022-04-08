test_that("read_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

   ## break function
   expect_error(
     object = convert_rlum2xlum("error"),
     regexp = "\\[convert\\_rlum2xlum\\(\\)\\] class 'character' not supported as input")

   ## use example data from Luminescence
   data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
   data(ExampleData.RLum.Data.Image, envir = environment(), package = "Luminescence")
   data(ExampleData.XSYG, envir = environment(), package = "Luminescence")

   RLum.Data.Curve <- IRSAR.RF.Data@records
   RLum.Data.Analysis <- IRSAR.RF.Data
   RLum.Data.Image <- ExampleData.RLum.Data.Image
   RLum.Data.Spectrum <- TL.Spectrum

   ## simple conversions
    ## RLum.Data.Curve
    expect_s3_class(convert_rlum2xlum(RLum.Data.Curve[[1]]), "xml_document")
    expect_silent(convert_rlum2xlum(RLum.Data.Curve[[1]], file = tempfile()))
    expect_s3_class(convert_rlum2xlum(RLum.Data.Curve), "xml_document")

    ## RLum.Data.Analysis
    expect_s3_class(convert_rlum2xlum(RLum.Data.Analysis), "xml_document")

    ## RLum.Data.Image
    expect_s3_class(convert_rlum2xlum(RLum.Data.Image), "xml_document")

    ## RLum.Data.Spectrum
    expect_s3_class(convert_rlum2xlum(RLum.Data.Spectrum), "xml_document")

  ## use BIN/BINX data
  file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
  BINfile <- Luminescence::read_BIN2R(file, fastForward = TRUE, verbose = FALSE)
  expect_s3_class(convert_rlum2xlum(BINfile), "xml_document")

  ## Daybreak
  file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
  temp <- Luminescence::read_Daybreak2R(file)
  expect_s3_class(convert_rlum2xlum(temp), "xml_document")

  ## PSL
  file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
  psl <- Luminescence::read_PSL2R(file, drop_bg = FALSE, as_decay_curve = TRUE, smooth = TRUE, merge = FALSE)
  expect_s3_class(convert_rlum2xlum(psl), "xml_document")

  ## RF
  file <- system.file("extdata", "RF_file.rf", package = "Luminescence")
  temp <- Luminescence::read_RF2R(file)
  expect_s3_class(convert_rlum2xlum(temp), "xml_document")

})
