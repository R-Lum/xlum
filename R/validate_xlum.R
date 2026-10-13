#' @title Validate XLUM format against an XSD reference schema
#'
#' @description
#' A convenience wrapper around [xml2::xml_validate] to check an XLUM file
#' against the reference format description shipped with [xlum-package].
#'
#' @param file [character] or [xml2::xml_document-class] (**required**):
#' path to an XLUM file, or an in-memory [xml2::read_xml] document to test.
#'
#' @param xsd_version [numeric] (*optional*):
#' format version to be used for validation, e.g., 1. If `NULL` (default), the
#' most recent version will be used.
#'
#' @return
#' A [logical] value, with the validation errors in its attributes.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @seealso [xml2::xml_validate], [read_xlum], [write_xlum]
#'
#' @examples
#' file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
#' validate_xlum(file)
#'
#' @export
validate_xlum <- function(
    file,
    xsd_version = NULL
){
  ## fall back
  if (!inherits(file, "xml_document"))
    file <- xml2::read_xml(file)

  ## load validation schema
  ## load all schema files
  f_schema <- sort(list.files(
    path = system.file("extdata/", package = "xlum"),
    full.names = TRUE,
    pattern = "xlum\\_schema"),
    decreasing = TRUE)

    ## use different schema
    if(!is.null(xsd_version)) {
      if(!any(xsd_version[1] %in% .regmatches(x = basename(f_schema), match = "(?<=v).+(?=.xsd)")))
        stop(
          paste("[validate_xlum()] XSD version unknown. Known versions:",
                paste(
                  .regmatches(x = basename(f_schema), match = "(?<=v).+(?=.xsd)"),
                collapse = ", ")
                ),
          call. = FALSE)

      tmp <- f_schema[grepl(pattern = xsd_version[1], x = f_schema, fixed = TRUE)]

    }

  ## validate schema
  schema_check <- xml2::xml_validate(
    x = file,
    schema = xml2::read_xml(f_schema[1]))

  ## return results
  if (!all(schema_check))
    warning(paste0(paste(attr(schema_check, "errors"), collapse = "\n")),
            call. = FALSE)

  return(schema_check)
}
