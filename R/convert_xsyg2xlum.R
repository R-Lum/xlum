#'@title Convert Freiberg Instruments XSYG files to XLUM
#'
#'@decription Wrapped function to quickly convert Freiberg Instruments XSYG-files
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
#'@seealso [convert_rlum2xlum], [Luminescence::read_XSYG2R]
#'
#'@examples
#'file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
#'convert_xsyg2xlum(file)
#'
#'@md
#'@export
convert_xsyg2xlum <- function(
    file,
    out_file = NULL
){
  rlum <-
    Luminescence::read_XSYG2R(
      file = file,
      fastForward = TRUE,
      verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}

