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
){
  rlum <-
    Luminescence::read_PSL2R(
      file = file)
  convert_rlum2xlum(rlum, out_file)
}
