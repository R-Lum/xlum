#'@title Convert Risø BIN/BINX to XLUM
#'
#'@decription Wrapped function to quickly convert BIN/BINX files
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
#'@seealso [convert_rlum2xlum], [Luminescence::read_BIN2R]
#'
#'@examples
#'
#'file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#'convert_binx2xlum(file)
#'
#'@md
#'@export
convert_binx2xlum <- function(
    file,
    out_file = NULL
){
  rlum <-
    Luminescence::read_BIN2R(
      file = file,
      fastForward = TRUE,
      verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}

#'
#'@rdname convert_binx2xlum
convert_bin2xlum <- function(file, out_file = NULL) convert_binx2xlum(file, out_file)
