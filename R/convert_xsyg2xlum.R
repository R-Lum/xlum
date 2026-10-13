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
){
  rlum <-
    Luminescence::read_XSYG2R(
      file = file,
      fastForward = TRUE,
      verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}
