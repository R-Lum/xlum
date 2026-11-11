#' @title Write an `RLum` object into XLUM format
#'
#' @description
#' The function writes an [Luminescence::RLum-class] object into XLUM format.
#' The function is a simple wrapper around [convert_rlum2xlum] and [write_xlum].
#'
#' @param object [Luminescence::RLum-class] or [list] (**required**):
#' S4 object of class `RLum` or a list of such objects.
#'
#' @param file [character] (**required**):
#' path to the output file.
#'
#' @param encode_base64 [logical] (*with default*):
#' enable/disable base64 encoding for values in the `<curve/>` node (`FALSE`
#' by default).
#'
#' @param ... currently ignored.
#'
#' @return
#' The document is written to `file`, with the `.xlum` extension added
#' automatically if needed; `NULL` is returned invisibly.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @seealso [convert_rlum2xlum], [write_xlum]
#'
#' @keywords IO
#'
#' @examples
#' data(ExampleData.RLum.Analysis, envir = environment(), package = "Luminescence")
#' out_file <- tempfile()
#' write_RLum2XLUM(IRSAR.RF.Data, out_file)
#'
#' @rdname write_RLum2XLUM
#' @export
setMethod("write_RLum2XLUM", "RLum",
function(
  object,
  file,
  encode_base64 = FALSE,
  ...
) {
  ## delegate to the list method
  write_RLum2XLUM(list(object), file, encode_base64 = encode_base64)
})

#' @rdname write_RLum2XLUM
#'
#' @param split_samples [list] (**optional**):
#' list of index vectors defining how the `object` list should be split into
#' different `<sample>` nodes. If `NULL` (default), only one `<sample>` node
#' is generated.
#'
#' @export
setMethod("write_RLum2XLUM", "list",
function(
  object,
  file,
  encode_base64 = FALSE,
  split_samples = NULL,
  ...
) {
  ## input validation
  if (length(object) == 0)
    stop("[write_RLum2XLUM()] 'object' must not be empty", call. = FALSE)

  if (is.null(split_samples) || length(split_samples) == 0) {
    split_samples <- list(seq_along(object))
  } else if (!inherits(split_samples, "list")) {
    stop("[write_RLum2XLUM()] 'split_samples' must be a list of indices or NULL",
         call. = FALSE)
  } else {
    if (!all(vapply(split_samples,
                    function(x) is.numeric(x) && length(x) > 0, logical(1))))
      stop("[write_RLum2XLUM()] 'split_samples' must contain only non-empty ",
           "numeric index vectors", call. = FALSE)
    vals <- unlist(split_samples)
    if (anyNA(vals) || any(vals < 1) || any(vals > length(object)))
      stop("[write_RLum2XLUM()] 'split_samples' contains out of range indices",
           call. = FALSE)
  }

  ## generate an xlum object for the first sample
  xlum <- convert_rlum2xlum(object[split_samples[[1]]])

  ## append a <sample> node for any additional sample to be added
  if (length(split_samples) > 1) {
    root <- xml2::xml_root(xlum)
    for (idx in split_samples[-1]) {
      sample <- xml2::xml_root(convert_rlum2xlum(object[idx]))
      xml2::xml_add_child(root, xml2::xml_child(sample), "sample")
    }

    ## coerce to xlum_list so that write_xlum() honours encode_base64
    xlum <- structure(xml2::as_list(root), class = "xlum_list")
  }

  write_xlum(xlum, file, encode_base64)
})
