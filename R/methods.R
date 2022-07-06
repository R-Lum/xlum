################################################################################
##                      METHODS FOR S3 GENERICS                               ##
################################################################################

#'@md
#'@export
as.list.xlum_list <- function(x, ...){
  ## convert values back to character otherwise it does not work
  ## properly
  curve_index <- .get_element_index(x)
  curve_values <- tValues <- xValues <- yValues <- list()

  ## extract values
  ## yes, it looks ugly and it is, and should be refactored
  ## the idea is to access the lowest and deepest element of type curve
  j <- 1
  for (i in curve_index) {
    curve_values[[j]] <- x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]][[1]]
    tValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "tValues")
    xValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "xValues")
    yValues[[j]] <- attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "yValues")
    j <- j + 1
  }

  ## convert values
  #print(curve_values)
  curve_values <- .convert2character(curve_values)
  tValues <- .convert2character(tValues)
  xValues <- .convert2character(xValues)
  yValues <- .convert2character(yValues)

  ## write back
  j <- 1
  for (i in curve_index) {
    x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]][[1]] <- curve_values[[j]]
    attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "tValues") <- tValues[[j]]
    attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "xValues") <- xValues[[j]]
    attr(x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]], "yValues") <- yValues[[j]]
    j <- j + 1

  }

  ## replace class attribute
  attr(x, "class") <- "list"

  return(x)

}
