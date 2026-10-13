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
#' (default), no file is written.
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
