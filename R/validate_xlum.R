#'@title Validate xlum format against an XSD reference schema
#'
#'@description A a convenience wrapper around [xml2::xml_validate] against
#'the reference format description shipped with [xlum-package]
#'
#'@param file [xml2::xml_document-class] (**required**): object to test
#'
#'@param xsd_version [numeric] (*optional*): specify format version for the
#'validation, e.g., `1`, `1.1`. The default is `NULL`, which will use the most recent version
#'
#'@return Results of the validation [logical] with attributes
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University
#'
#'@examples
#'file <- system.file("extdata/xlum_prototype.xlum", package = "xlum")
#'validate_xlum(file)
#'
#'@md
#'@export
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
