#' @title Convert input files to XLUM
#'
#' @description
#' These functions convert several types of input files to XLUM format. They
#' are implemented as shallow wrappers around an appropriate `Luminescence`
#' read function and [convert_rlum2xlum].
#'
#' @param file [character] (**required**):
#' input file.
#'
#' @param out_file [character] (*optional*):
#' path to an output file where the XLUM file should be written to. If `NULL`
#' (default), no file is written, but an [xml2::xml_document-class] object is
#' returned.
#'
#' @param ... support for a limited number of additional arguments (currently
#' only `position` and `n.records`) to be forwarded to [Luminescence::read_BIN2R].
#'
#' @return
#' When `out_file` is `NULL` (default), an [xml2::xml_document-class] object
#' is returned, otherwise the function attempts to write an `.xlum` file with
#' the path provided.
#'
#' @section Function version: 0.1.0
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @seealso [convert_rlum2xlum], [Luminescence::read_BIN2R],
#' [Luminescence::read_Daybreak2R], [Luminescence::read_PSL2R],
#' [Luminescence::read_XSYG2R]
#'
#' @name convert_2xlum
NULL

#' @describeIn convert_2xlum
#' Convert Risø BIN/BINX files to XLUM format. This is a shallow wrapper
#' around [Luminescence::read_BIN2R] and [convert_rlum2xlum].
#'
#' @examples
#' ## load BINX example data
#' file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
#' convert_binx2xlum(file)
#'
#' @export
convert_binx2xlum <- function(
  file,
  out_file = NULL,
  ...
) {
  ## additional argument handling
  import_args <- modifyList(
    x = list(
      position = NULL,
      n.records = NULL
    ),
    val = list(...),
    keep.null = TRUE)

  ## import
  rlum <- as.list(Luminescence::read_BIN2R(
    file = file,
    position = import_args$position,
    n.records = import_args$n.records,
    fastForward = TRUE,
    verbose = FALSE)
    )
  convert_rlum2xlum(rlum, out_file)
}

#' @rdname convert_2xlum
#' @export
convert_bin2xlum <- function(file, out_file = NULL, ...) {
  convert_binx2xlum(file, out_file, ...)
}

#' @describeIn convert_2xlum
#' Convert Freiberg Instruments XSYG files to `xlum` format. This is a shallow
#' wrapper around [Luminescence::read_XSYG2R] and [convert_rlum2xlum].
#'
#' @examples
#' ## load XSYG example data
#' file <- system.file("extdata/XSYG_file.xsyg", package = "Luminescence")
#' convert_xsyg2xlum(file)
#'
#' @export
convert_xsyg2xlum <- function(
  file,
  out_file = NULL
) {
  rlum <- Luminescence::read_XSYG2R(
    file = file,
    fastForward = TRUE,
    verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}

#' @describeIn convert_2xlum
#' Convert Daybreak TXT and DAT files to XLUM format. This is a shallow
#' wrapper around [Luminescence::read_Daybreak2R] and [convert_rlum2xlum].
#'
#' @examples
#' ## load Daybreak example data
#' file <- system.file("extdata/Daybreak_TestFile.txt", package = "Luminescence")
#' convert_Daybreak2xlum(file)
#'
#' @export
convert_Daybreak2xlum <- function(
  file,
  out_file = NULL
) {
  rlum <- Luminescence::read_Daybreak2R(
    file = file,
    verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}

#' @describeIn convert_2xlum
#' Convert SUERC PSL files to XLUM format. This is a shallow wrapper around
#' [Luminescence::read_PSL2R] and [convert_rlum2xlum].
#'
#' @examples
#' ## load PSL example data
#' file <- system.file("extdata", "DorNie_0016.psl", package = "Luminescence")
#' convert_psl2xlum(file)
#'
#' @export
convert_psl2xlum <- function(
  file,
  out_file = NULL
) {
  rlum <- Luminescence::read_PSL2R(
    file = file)
  convert_rlum2xlum(rlum, out_file)
}
