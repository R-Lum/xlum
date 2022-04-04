#'@title Write xlum files
#'
#'@description Create xlum files using [xml2::read_xml]
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
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
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
  if(!grepl("(.)+\\.xlum", file, perl = TRUE))
    file <- normalizePath(paste0(file, ".xlum"), mustWork = FALSE)


# Export depending on the input -------------------------------------------
  if(inherits(x, "xlum_list")) {
    ## convert values back to character otherwise it does not work
    ## properly
    curve_index <- .get_element_index(x)
    curve_values <- tValues <- xValues <- yValues <- list()

    ## extract values
    j <- 1
    for (i in curve_index) {
      curve_values[[j]] <- x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]][[1]]
      tValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "tValues")
      xValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "xValues")
      yValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "yValues")
      j <- j + 1
    }

    ## convert values
    #print(curve_values)
    curve_values <- .convert2character(curve_values)
    tValues <- .convert2character(tValues)
    xValues <- .convert2character(xValues)
    yValues <- .convert2character(yValues)

    ## write back
    j <- 1
    for (i in curve_index) {
      x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]][[1]] <- curve_values[[j]]
      attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "tValues") <- tValues[[j]]
      attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "xValues") <- xValues[[j]]
      attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "yValues") <- yValues[[j]]
      j <- j + 1

    }

    ## replace class attribute
    attr(x, "class") <- "list"

  }

# Return ------------------------------------------------------------------
  return(xml2::write_xml(xml2::as_xml_document(x), file, ...))
}
