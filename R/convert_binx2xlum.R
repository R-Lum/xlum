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
){
  ## additional argument handling
  import_args <- modifyList(
    x = list(
      position = NULL,
      n.records = NULL
    ),
    val = list(...),
    keep.null = TRUE)

  ## import
  rlum <-
    as.list(Luminescence::read_BIN2R(
      file = file,
      position = import_args$position,
      n.records = import_args$n.records,
      fastForward = TRUE,
      verbose = FALSE))

  convert_rlum2xlum(rlum, out_file)
}

#' @rdname convert_2xlum
#' @export
convert_bin2xlum <- function(file, out_file = NULL, ...) {
  convert_binx2xlum(file, out_file, ...)
}
