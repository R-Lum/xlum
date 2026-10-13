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
){
  rlum <-
    Luminescence::read_Daybreak2R(
      file = file,
      verbose = FALSE)
  convert_rlum2xlum(rlum, out_file)
}
