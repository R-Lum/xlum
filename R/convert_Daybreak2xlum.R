#'@title Convert Daybreak TXT and DAT files to XLUM
#'
#'@description Wrapped function to quickly convert Daybreak files
#'to `xlum` (files)
#'
#'@param file [character] (**required**): file
#'
#'@param out_file [character] (*optional*): output file, if set a file
#'output is created
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@section Function version: 0.1.0
#'
#'@return Depending on the setting. If `out_file == NULL` an [xml2::xml_document-class] object is returned,
#' if file is set the function attempts to write an `*.xlum` file`.
#'
#'
#'@seealso [convert_rlum2xlum], [Luminescence::read_Daybreak2R]
#'
#'@examples
#'file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
#'convert_Daybreak2xlum(file)
#'
#'@md
#'@export
convert_Daybreak2xlum <- function(
    file,
    out_file = NULL
){
  rlum <-
    Luminescence::read_Daybreak2R(
      file = file,
      verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}

