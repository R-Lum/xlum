#'@title Import XLUM files
#'
#'@description Imports XLUM-files using [xml2::read_xml]. Comes with a few
#'convenience features, such as the transformation of string lists to numeric
#'vectors where meaningful.
#'
#'@details
#'
#'**Supported output options**\cr
#'
#'`"xlum_list"` (default): imports XML-data and coerces them to list. Numeric values
#'for level `<curve>` are transposed into [numeric] vectors for better handling
#'
#'`"list"`: imports XML-data and coerces it to a [list], no further treatment of data
#'
#'`"xml_document"`: unprocessed output from [xml2::read_xml]
#'
#'@param file [character] (**required**): path to file
#'
#'@param verify [logical] (*with default*): enable/disable format validation
#'
#'@param output [character] (*with default*): output object of the import,
#'supported are `"xml_document"`, `"list"`, `"xlum_list"` (the default)
#'
#'@return The output depends on the setting selected in `output`. It will
#'be either an [xml2::xml_document-class], [list], or a [list] of class `xlum_list`.
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@section Function version: 0.1.0
#'
#'@seealso [xml2::read_xml], [validate_xlum], [write_xlum]
#'
#'@keywords IO
#'
#'@examples
#'file <- system.file("extdata/xlum_example.xlum", package="xlum")
#'read_xlum(file)
#'
#'@md
#'@export
read_xlum <- function(
 file,
 verify = TRUE,
 output = "xlum_list"
){

# Import ------------------------------------------------------------------
  xlum <- xml2::read_xml(normalizePath(file))

# Validate xlum schema -------------------------------  ---------------------
  if (verify[1])
    validate_xlum(xlum)

# Sanitize input ----------------------------------------------------------
  if (any(output[1] %in% c("list", "xlum_list"))) {

    if (output[1] == "xlum_list") {
    ## we have to find all curve objects to create numeric vectors
    curve_nodes <- xml2::xml_find_all(xlum, "//curve", xml2::xml_ns(xlum))

      ## extract the numeric values for nodes
      ## the measured values
      curve_values <- .convert2numeric(xml2::xml_text(curve_nodes, trim = TRUE), check_base64 = TRUE)
      tValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "tValues"))
      xValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "xValues"))
      yValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "yValues"))
    }

    ## convert input to standard R list
    xlum <- xml2::as_list(xlum)

    if (output[1] == "xlum_list") {
      ## replace all long string values with numeric values
      ## we hard-code this here; it suffices
      j <- 1
      for (i in .get_element_index(xlum)) {
        xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]][[1]] <- curve_values[[j]]
        attr(xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "tValues") <- tValues[[j]]
        attr(xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "xValues") <- xValues[[j]]
        attr(xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "yValues") <- yValues[[j]]
        j <- j + 1

      }
    }

  }

# Return ------------------------------------------------------------------
  if (output[1] == "xlum_list") attr(xlum, "class") <- "xlum_list"
  return(xlum)

}
