#' @title Import XLUM files
#'
#' @description
#' Imports XLUM files using [xml2::read_xml]. Comes with a few convenience
#' features, such as the transformation of string lists to numeric vectors
#' where meaningful.
#'
#' @details
#' **Supported output options**
#'
#' - `"xlum_list"` (default): imports XML data and coerces it to a list.
#' Numeric values for level `<curve/>` are converted into [numeric] vectors
#' for better handling.
#'
#' - `"list"`: imports XML data and coerces it to a [list], no further
#' treatment of data.
#'
#' - `"xml_document"`: unprocessed output from [xml2::read_xml].
#'
#' @param file [character] (**required**):
#' path to file.
#'
#' @param verify [logical] (*with default*):
#' enable/disable format validation.
#'
#' @param output [character] (*with default*):
#' output object of the import, one of `"xlum_list"` (default), `"list"`, or
#' `"xml_document"` (see Details).
#'
#' @return
#' The output depends on the setting selected in `output`. It will
#' be either an [xml2::xml_document-class], a plain [list], or a [list] of
#' class `xlum_list`.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @seealso [xml2::read_xml], [validate_xlum], [write_xlum]
#'
#' @keywords IO
#'
#' @examples
#' file <- system.file("extdata/xlum_example.xlum", package = "xlum")
#' read_xlum(file)
#'
#' @export
read_xlum <- function(
 file,
 verify = TRUE,
 output = "xlum_list"
){

# Import ------------------------------------------------------------------
  xlum <- xml2::read_xml(normalizePath(file))

# Validate xlum schema -------------------------------  ---------------------
  if (isTRUE(verify[1]))
    validate_xlum(xlum)

# Sanitize input ----------------------------------------------------------
  output <- output[1]
  as_xlum_list <- identical(output, "xlum_list")

  if (output %in% c("list", "xlum_list")) {
    ## extract the values from the XML document before it is converted to a list
    if (as_xlum_list) {
      ## we have to find all curve objects to create numeric vectors
      curve_nodes <- xml2::xml_find_all(xlum, "//curve", xml2::xml_ns(xlum))

      ## extract the numeric values and the attributes
      curve_values <- .convert2numeric(xml2::xml_text(curve_nodes, trim = TRUE), check_base64 = TRUE)
      tValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "tValues"))
      xValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "xValues"))
      yValues <- .convert2numeric(xml2::xml_attr(curve_nodes, attr = "yValues"))
    }

    ## convert input to standard R list
    xlum <- xml2::as_list(xlum)

    if (as_xlum_list) {
      ## replace all long string values with numeric values
      ## we hard-code this here; it suffices
      curve_index <- .get_element_index(xlum)
      for (j in seq_along(curve_index)) {
        i <- curve_index[[j]]
        node <- xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]]
        node[[1]] <- curve_values[[j]]
        attr(node, "tValues") <- tValues[[j]]
        attr(node, "xValues") <- xValues[[j]]
        attr(node, "yValues") <- yValues[[j]]
        xlum[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]] <- node
      }
      attr(xlum, "class") <- "xlum_list"
    }
  }

# Return ------------------------------------------------------------------
  return(xlum)
}
