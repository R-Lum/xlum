#'@title Validate xlum format against an XSD reference schema
#'
#'@description A a convenience wrapper around [xml2::xml_validate] against
#'the reference format description shipped with [xlum-package]
#'
#'@param file [xml2::xml_document-class] (**required**): object to test
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
validate_xlum <- function(file){
  ## fall back
  if (!inherits(file, "xml_document"))
    file <- xml2::read_xml(file)

  ## validate schema
  schema_check <- xml2::xml_validate(
    x = file,
    schema = xml2::read_xml(system.file("extdata/xlum_schema.xsd", package = "xlum")))

  ## return results
  if (!all(schema_check))
    warning(paste0(paste(attr(schema_check, "errors"), collapse = "\n")),
            call. = FALSE)

  return(schema_check)
}
