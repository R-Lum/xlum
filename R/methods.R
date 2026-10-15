################################################################################
##                      METHODS FOR S3 GENERICS                               ##
################################################################################

#' @export
as.list.xlum_list <- function(x, encode_base64 = FALSE, ...){
  ## convert values back to character otherwise it does not work
  ## properly
  ## the idea is to access the lowest and deepest element of type curve
  for (i in .get_element_index(x)) {
    node <- x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]]

    node[[1]] <- .convert2character(node[[1]], encode_base64 = encode_base64)
    attr(node, "tValues") <- .convert2character(attr(node, "tValues"))
    attr(node, "xValues") <- .convert2character(attr(node, "xValues"))
    attr(node, "yValues") <- .convert2character(attr(node, "yValues"))

    x[[i[1]]][[i[2]]][[i[3]]][[i[4]]][[i[5]]] <- node
  }

  ## replace class attribute
  attr(x, "class") <- "list"

  return(x)
}
