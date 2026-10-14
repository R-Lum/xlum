#' @title Convert RLum-class objects to the XLUM format
#'
#' @description
#' Converts [Luminescence::RLum-class] objects to XLUM format.
#'
#' @details
#' The function tries to make the best possible conversion of an
#' [Luminescence::RLum-class] object to the XLUM format.
#' Supported are [Luminescence::RLum.Analysis-class] objects as well as the
#' [Luminescence::RLum.Data-class] objects they may contain.
#' [Luminescence::RLum.Results-class] objects are not supported.
#'
#' The input may be a single object or a [list] of objects. All objects in a
#' list must be of the same class, either [Luminescence::RLum.Analysis-class]
#' or [Luminescence::RLum.Data-class]. A list of data
#' objects is combined into a single sequence, whereas a list of
#' [Luminescence::RLum.Analysis-class] objects produces one `<sequence/>` per
#' element.
#'
#' Each analysis record is mapped to a `<record/>` node holding one or more
#' `<curve/>` nodes. The measured values are stored as the text content of the
#' `<curve/>` node, while `tValues`, `xValues`, and `yValues` are kept as node
#' attributes. Additional record attributes, such as `curveType` and
#' `startDate`, are transferred where available.
#'
#' As [Luminescence::RLum-class] objects do not distinguish between different
#' samples, the `<xlum/>` and `<sample/>` nodes are created automatically, so
#' all converted objects end up in a single sample.
#'
#' @param rlum [Luminescence::RLum-class] or a [list] of it (**required**):
#' input object.
#'
#' @inheritParams convert_2xlum
#'
#' @return
#' When `file` is `NULL` (default), an [xml2::xml_document-class] object
#' is returned. Otherwise the document is written to `file` (with the `.xlum`
#' extension added automatically if needed) and `NULL` is returned invisibly.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @seealso [convert_2xlum], [write_xlum], [read_xlum],
#' [Luminescence::RLum.Analysis-class]
#'
#' @keywords datagen
#'
#' @examples
#' ## load example data
#' data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
#'
#' ## convert to an in-memory XLUM document
#' xlum <- convert_rlum2xlum(IRSAR.RF.Data)
#'
#' ## convert and write to a file
#' convert_rlum2xlum(IRSAR.RF.Data, tempfile(fileext = ".xlum"))
#'
#' @export
convert_rlum2xlum <- function(
  rlum,
  out_file = NULL
){

# Sanitize input ---------------------------------------------------------
## always convert to a list
if(!inherits(rlum, "list")) {
  rlum <- list(rlum)
}

  ## check whether the input is valid
  class.valid <- c("RLum.Analysis", "RLum.Data.Curve", "RLum.Data.Image",
                   "RLum.Data.Spectrum")
  class.found <- vapply(rlum, class, character(1))
  valid <- class.found %in% class.valid
  if (!all(valid)) {
    stop("[convert_rlum2xlum()] Class '", class.found[which(!valid)[1]],
         "' not supported as input", call. = FALSE)
  }

  ## don't support lists of mixed RLum.Analysis/RLum.Data objects
  class.found <- gsub("(RLum\\.Data).*", "\\1", class.found)
  if (length(unique(class.found)) > 1) {
    stop("[convert_rlum2xlum()] All input objects must be of the same class, ",
         "either 'RLum.Analysis' or 'RLum.Data'", call. = FALSE)
  }

  ## we can assume that we have correct objects, so always coerce to RLum.Analysis
  if (all(vapply(rlum, inherits, "RLum.Data", FUN.VALUE = logical(1)))) {
    rlum <- list(Luminescence::set_RLum("RLum.Analysis",
                                        records = rlum,
                                        originator = rlum[[1]]@originator))
  }

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
    vals <- switch(class(x)[1],
                   "RLum.Data.Curve" = x@data[, 2],
                   "RLum.Data.Image" = as.numeric(x@data),
                   "RLum.Data.Spectrum" = as.numeric(x@data)
                   )
    .convert2character(vals)
  }

  .create_node_attrs <- function(x, attrs){
      ## obtain tValues (this should be consistent)
    vals <- switch(class(x)[1],
                   "RLum.Data.Curve" = x@data[, 1],
                   "RLum.Data.Image" = seq_len(dim(x@data)[3]),
                   "RLum.Data.Spectrum" = rownames(x@data)
                   )
    attrs[["tValues"]] <- .convert2character(vals)

       ## tackle x and y values
    if (inherits(x, "RLum.Data.Image")) {
          attrs[["xValues"]] <- .convert2character(seq_len(dim(x@data)[2]))
          attrs[["yValues"]] <- .convert2character(seq_len(dim(x@data)[1]))
        }

    else if (inherits(x, "RLum.Data.Spectrum"))
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

  ## set a few attributes if applicable
  sample_name <- unlist(rlum[[1]]@records[[1]]@info[c("SAMPLE")])[1]
  if (!is.null(sample_name))
    xml2::xml_attr(node_sample, "name") <- sample_name

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
      parentID <- x@info[["parentID"]] %||% x@.uid
      if (is.na(parentID))
        parentID <- "none"
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
  if (is.null(out_file))
    return(out)

  ## silently return NULL
  write_xlum(out, out_file)
}
