SW <- function(expr) {
  capture.output(suppressMessages(suppressWarnings(expr)))
}

expect_snapshot_xlum <- function(xlum, ...) {
  expect_snapshot(cat(as.character(xlum)), ...)
}
