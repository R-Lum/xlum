test_that("read_xlum()", {
  testthat::skip_on_cran()
  local_edition(3)

   ## use example data from Luminescence
   data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
   data(ExampleData.RLum.Data.Image, envir = environment(), package = "Luminescence")
   data(ExampleData.XSYG, envir = environment(), package = "Luminescence")

   RLum.Data.Curve <- IRSAR.RF.Data@records
   RLum.Data.Analysis <- IRSAR.RF.Data
   RLum.Data.Image <- ExampleData.RLum.Data.Image
   RLum.Data.Spectrum <- TL.Spectrum

   ## break function
   expect_error(
     object = convert_rlum2xlum("error"),
     regexp = "\\[convert\\_rlum2xlum\\(\\)\\] class 'character' not supported as input")



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
})
