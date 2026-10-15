#' @title Write XLUM files
#'
#' @description
#' Create XLUM files using [xml2::write_xml].
#'
#' @param x `xlum_list`, [list], [xml2::xml_document-class] (**required**):
#' input object.
#'
#' @param file [character] (**required**):
#' path to the output file, with the `.xlum` extension added automatically if
#' needed.
#'
#' @param encode_base64 [logical] (*with default*):
#' enable/disable base64 encoding for values in the `<curve/>` node. Please
#' note that this parameter works only if the input is of class `xlum_list`.
#'
#' @param ... further arguments to be passed to [xml2::write_xml].
#'
#' @return
#' The document is written to `file`; `NULL` is returned invisibly.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @seealso [xml2::write_xml], [read_xlum]
#'
#' @keywords IO
#'
#' @examples
#' file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
#' xlum_data <- read_xlum(file)
#' write_xlum(xlum_data, tempfile())
#'
#' @export
write_xlum <- function(
  x,
  file,
  encode_base64 = FALSE,
  ...
){

# Check input -------------------------------------------------------------
  if (!inherits(x, c("xlum_list", "list", "xml_document")))
    stop("[write_xlum()] Object of class '", class(x)[1], "' not supported as input",
         call. = FALSE)

# Sanitize file -----------------------------------------------------------
  ## check for ending
  if (!endsWith(file, ".xlum"))
    file <- normalizePath(paste0(file, ".xlum"), mustWork = FALSE)

# Export depending on the input -------------------------------------------
  if(inherits(x, "xlum_list"))
    x <- as.list(x, encode_base64 = encode_base64)

# Return ------------------------------------------------------------------
  return(xml2::write_xml(xml2::as_xml_document(x), file, ...))
}
