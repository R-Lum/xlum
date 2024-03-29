#'@title Convert RLum-class objects to the XLUM format
#'
#'@description Converts [Luminescence::RLum-class] objects to xlum format
#'
#'@details
#'
#'The function tries to make a best possible conversion of the [Luminescence::RLum-class]
#'to the `xlum` format. [Luminescence::RLum.Results-class] objects are not supported.
#'
#'Because of the nature the [Luminescence::RLum-class] structure, the nodes `<xlum/>`, `<sample/>`
#'are set automatically because the [Luminescence::RLum-class] does not distinguish between
#'different samples.
#'
#'@param rlum [Luminescence::RLum.Data-class] and [Luminescence::RLum.Analysis-class] or a [list] of it (**required**): input
#'for the conversion
#'
#'@param file [character] (*optional*): file name for the export, if `NULL` (the default),
#'an [xml2::xml_document-class] object is returned
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@section Function version: 0.1.0
#'
#'@return Depending on the setting. If `file == NULL` an [xml2::xml_document-class] object is returned,
#' if file is set the function attempts to write an `*.xlum` file`.
#'
#'@keywords datagen
#'
#'@seealso [Luminescence::RLum.Data-class], [write_xlum]
#'
#'@examples
#'data(ExampleData.RLum.Analysis,
#' envir = environment(),
#' package = "Luminescence")
#'convert_rlum2xlum(IRSAR.RF.Data)
#'
#'@md
#'@export
convert_rlum2xlum <- function(
  rlum,
  file = NULL
){

# Sanitize input ---------------------------------------------------------
## always convert to a list
if(!inherits(rlum, "list")) {
  tmp_class <- class(rlum)[1]
  rlum <- list(rlum)
}

## now check whether input is allowed
if(!all(vapply(rlum, class, "character") %in%
   c("RLum.Analysis", "RLum.Data.Curve", "RLum.Data.Image", "RLum.Data.Spectrum")))
  stop(paste0("[convert_rlum2xlum()] class '", tmp_class,"' not supported as input"), call. = FALSE)

## we can assume that we have correct objects, so always coerce to RLum.Analysis
if(all(grepl("RLum\\.Data", vapply(rlum, class, "character"))))
  rlum <- list(Luminescence::set_RLum("RLum.Analysis", records = rlum, originator = rlum[[1]]@originator))

# Set prototype ------------------------------------------------------------
  ## get prototype attributes
  xlum_proto <- xml2::read_xml(system.file("extdata/xlum_prototype.xlum", package="xlum"))
  prototype_attrs <- list(
    xlum = xml2::xml_attrs(xml2::xml_root(xlum_proto)),
    sample = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//sample", xml2::xml_ns(xlum_proto))),
    sequence = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//sequence", xml2::xml_ns(xlum_proto))),
    record = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//record", xml2::xml_ns(xlum_proto))),
    curve = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//curve", xml2::xml_ns(xlum_proto))))


# Helper functions --------------------------------------------------------
  .create_node_values <- function(x){
    switch(class(x)[1],
      "RLum.Data.Curve" = node_text <- .convert2character(x@data[,2]),
      "RLum.Data.Image" = node_text <- .convert2character(as.numeric(x@data)),
      "RLum.Data.Spectrum" = node_text <- .convert2character(as.numeric(x@data)))

  }

  .create_node_attrs <- function(x, attrs){
      ## obtain tValues (this should be consistent)
       switch(class(x)[1],
         "RLum.Data.Curve" = attrs[["tValues"]] <- .convert2character(x@data[,1]),
         "RLum.Data.Image" = attrs[["tValues"]]  <- .convert2character(seq_len(dim(x@data)[3])),
         "RLum.Data.Spectrum" = attrs[["tValues"]]<- .convert2character(rownames(x@data)))

       ## tackle x and y values
        if(inherits(x, "RLum.Data.Image")) {
          attrs[["xValues"]] <- .convert2character(seq_len(dim(x@data)[2]))
          attrs[["yValues"]] <- .convert2character(seq_len(dim(x@data)[1]))

        }

       if(inherits(x, "RLum.Data.Spectrum"))
          attrs[["xValues"]] <- .convert2character(seq_len(dim(x@data)[2]))

      ## common attributes
      ## curve type
      attrs[["curveType"]] <- x@curveType

      ##XSYG and default
      if(is.na(x@originator) || x@originator == "read_XSYG2R") {
        ## create parameter translation
         lookup <- c(
          detector = "component",
          startDate = "startDate",
          duration = "duration",
          offset = "offset",
          parentID = "parentID",
          state = "state")

        ## replace names
        attrs_curve <- x@info
        names(attrs_curve) <- lookup[names(attrs_curve)]

        ## replace known components
        attrs <- modifyList(as.list(attrs), attrs_curve)

        ## further manual modifications
        if(!is.null(x@info[["curveDescripter"]])) {
          labels <- strsplit(x@info[["curveDescripter"]], ";", fixed = TRUE)[[1]]
          attrs[["tLabel"]] <- .regmatches(labels[1], ".+(?=\\[)")
          attrs[["tUnit"]] <- .regmatches(labels[1], "(?<=\\[).+(?=\\])")
          attrs[["vLabel"]] <- .regmatches(labels[2], ".+(?=\\[)")
          attrs[["vUnit"]] <- .regmatches(labels[2], "(?<=\\[).+(?=\\])")
        }

        ## date
        attrs[["startDate"]] <- .toISODate(attrs[["startDate"]], format = "lexsyg")

      } else if (any(grepl("BIN", x@originator))) {
        ## create parameter translation
        lookup <- c(
          DETECTOR_ID = "component",
          COMMENT = "comment",
          state = "state")

        ## replace names
        attrs_curve <- x@info
        names(attrs_curve) <- lookup[names(attrs_curve)]

        ## replace known components
        attrs <- modifyList(as.list(attrs), attrs_curve)

        ## date
        attrs[["startDate"]] <- .toISODate(
          paste(c(x@info[c("DATE", "TIME")]), collapse = ""), "Risoe")

      } else if(x@originator[1] == "read_Daybreak2R") {
        ## date
        attrs[["startDate"]] <- .toISODate(x@info[["Started"]], format = "Daybreak")

      } else if(x@originator[1] == "read_PSL2R") {
        str <-  paste(
          c(as.character(x@info[["settings"]][["Date"]]),
            x@info[["settings"]][["Time"]]),
          collapse = "")
        attrs[["startDate"]] <- .toISODate(str, format = "psl")

      }

      return(unlist(attrs))
  }

# Generate xlum document --------------------------------------------------
## personal reminder:
## - xml2 works with pointers some typical R style programming attempts do not work!
## - attributes are copied from the prototype
  ## create new xml document
  out <- xml2::xml_new_document()

  ## add <xlum/>
  node_xlum <- xml2::xml_add_child(out, "xlum")
  xml2::xml_attrs(node_xlum) <- prototype_attrs[["xlum"]]

  ## add <sample/>
  node_sample <- xml2::xml_add_child(node_xlum, "sample")
  xml2::xml_attrs(node_sample) <- prototype_attrs[["sample"]]

    ##set a few attributes if applicable
    if(!is.null(unlist(rlum[[1]]@records[[1]]@info[c("SAMPLE")])[1]))
      xml2::xml_attr(node_sample, "name") <- unlist(rlum[[1]]@records[[1]]@info[c("SAMPLE")])[1]

  ## add <sequence/>
  for (s in seq_along(rlum)) {
    node_sequence <- xml2::xml_add_child(node_sample, "sequence")
    xml2::xml_attrs(node_sequence) <- prototype_attrs[["sequence"]]

      ## set sequence attributes
        ## the name and position should be similar for all curves
        xml2::xml_attr(node_sequence, "name") <- unlist(
          rlum[[s]]@records[[1]]@info[c("name", "FNAME")])[1]
        xml2::xml_attr(node_sequence, "position") <- unlist(
          rlum[[s]]@records[[1]]@info[c("position", "POSITION")])[1]

    ## get curve ID; including unique IDs and recordType
    id_curves <- vapply(rlum[[s]]@records, function(x) {
      parentID <- if(is.null(x@info[["parentID"]])) x@.uid else x@info[["parentID"]]
      c(strsplit(x@recordType," ", TRUE)[[1]][1], parentID)
     }, character(2))

    ## get unique records
    id_records <- unique(id_curves[2,])

    ## add <record/>
    for(r in seq_along(id_records)) {
      node_record <- xml2::xml_add_child(node_sequence, "record")
      xml2::xml_attrs(node_record) <- prototype_attrs[["record"]]

        ##modify attributes where possible and needed
        xml2::xml_attr(node_record, "recordType") <-
          id_curves[1, id_curves[2,] == id_records[r]][1]
        xml2::xml_attr(node_record, "sequenceStepNumber") <- r

      ## add <curve/>
      for(c in which(id_curves[2,] == id_records[r])) {
        node_curve <- xml2::xml_add_child(node_record, "curve")
        xml2::xml_attrs(node_curve) <- prototype_attrs[["curve"]]

        ## extract node values
        xml2::xml_text(node_curve) <- .create_node_values(rlum[[s]]@records[[c]])

        ## assign attributes
        xml2::xml_attrs(node_curve) <- .create_node_attrs(
          x = rlum[[s]]@records[[c]],
          attrs = xml2::xml_attrs(node_curve))

      } ## end loop curve

    } ## end loop record
  } ## end loop sequence


# Return ------------------------------------------------------------------
  if (!is.null(file)) {
    write_xlum(out, file)

  } else {
    return(out)
  }

}
