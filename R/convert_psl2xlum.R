#'@title Convert SUERC PSL-files to XLUM
#'
#'@decription Wrapped function to quickly convert PSL files
#'to `xlum` (files) from the portable luminescence system produced by the SUERC
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
#'@seealso [convert_rlum2xlum], [Luminescence::read_PSL2R]
#'
#'@examples
#'file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
#'convert_psl2xlum(file)
#'
#'@md
#'@export
convert_psl2xlum <- function(
    file,
    out_file = NULL
){
  rlum <-
    Luminescence::read_PSL2R(
      file = file)
  convert_rlum2xlum(rlum, out_file)
}

