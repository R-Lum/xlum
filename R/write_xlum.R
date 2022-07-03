#'@title Write XLUM-files
#'
#'@description Create XLUM-files using [xml2::read_xml]
#'
#'@param x `xlum_list`, [list], [xml2::xml_document-class] (**required**) : input object
#'
#'@param file [character] (**required**): valid file path and file name with ending
#'`.xlum` (will be add automatically if needed)
#'
#'@param ... further arguments to be passed to [xml2::write_xml]
#'
#'@return Creates an XML-file with ending `*.xlum`
#'
#'@seealso [xml2::write_xml], [read_xlum]
#'
#'@section Function version: 0.1.0
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (united Kingdom)
#'
#'@keywords IO
#'
#'@examples
#'file <- system.file("extdata/xlum_prototype.xlum", package="xlum")
#'out_file <- tempfile()
#'
#'xlum_data <- read_xlum(file)
#'write_xlum(xlum_data, out_file)
#'
#'@md
#'@export
write_xlum <- function(
  x,
  file,
  ...
){

# Check input -------------------------------------------------------------
  if(!(class(x)[1] %in% c("xlum_list", "list", "xml_document")))
    stop(paste0("[write_xlum()] object of class '", class(x)[1], "' not supported as input!"),
         call. = FALSE)

# Sanitize file -----------------------------------------------------------
  ## check for ending
  if(!grepl("^(.*)\\.xlum$", file, perl = TRUE))
    file <- normalizePath(paste0(file, ".xlum"), mustWork = FALSE)

# Export depending on the input -------------------------------------------
  if(inherits(x, "xlum_list"))
    x <- as.list(x)

# Return ------------------------------------------------------------------
  return(xml2::write_xml(xml2::as_xml_document(x), file, ...))
}
