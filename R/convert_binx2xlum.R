#'@title Convert Risø BIN/BINX to XLUM
#'
#'@description Wrapped function to quickly convert BIN/BINX files
#'to `xlum` (files)
#'
#'@param file [character] (**required**): file
#'
#'@param out_file [character] (*optional*): output file, if set a file
#'output is created
#'
#'@param ... support for a limited number of additional arguments to be forwarded to [Luminescence::read_BIN2R]: `position`, `n.records`
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
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
#'\dontrun{
#'file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#'convert_binx2xlum(file)
#'}
#'
#'@md
#'@export
convert_binx2xlum <- function(
    file,
    out_file = NULL,
    ...
){
  ## additional argument handling
  import_args <- modifyList(
    x = list(
      position = NULL,
      n.records = NULL
    ),
    val = list(...),
    keep.null = TRUE)

  ## import
  rlum <-
    as.list(Luminescence::read_BIN2R(
      file = file,
      position = import_args$position,
      n.records = import_args$n.records,
      fastForward = TRUE,
      verbose = FALSE))

  convert_rlum2xlum(rlum, out_file)
}

#'
#'@rdname convert_binx2xlum
#'@export
convert_bin2xlum <- function(file, out_file = NULL) convert_binx2xlum(file, out_file)
