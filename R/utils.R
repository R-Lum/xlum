#'@title Fast Conversion of White Space Separated Strings to Numeric
#'
#'@description Converts strings with white space separated numbers to a numeric vector. `"0"`
#'values are transformed to `NA`.
#'
#'@param l [character] [list] (**required**): list of character vectors (see example)
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@return [list] of [numeric] vectors
#'
#'@examples
#'
#'l = list(a <- c("12 12 13"), b = "0")
#'.convert2numeric(l)
#'
#'@md
#'@noRd
.convert2numeric <- function(l){
  ## handle NA values
  if (any(l == "0")) l[which(l == "0")] <- NA_character_

  ##extract values
  lapply(
    unlist(
      lapply(l, strsplit, split = " ", fixed = TRUE),
      recursive = FALSE),
    as.numeric)
}

#'@title Fast Conversion of Numeric Values into White Space Separated Strings
#'
#'@description Converts numeric vector to a character with numbers separated by white space. `NA`
#'values are transformed to `"0"`.
#'
#'@param l [numeric] [list] (**required**): list of numeric vectors (see example)
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@return [list] of [numeric] vectors
#'
#'@examples
#'
#'l = list(a <- c(12, 12, 13), b = NA)
#'.convert2character(l)
#'
#'@md
#'@noRd
.convert2character <- function(l){
  ## handle NA values
  if (any(is.na(l))) l[which(is.na(l))] <- "0"

  ##extract values
  lapply(l, function(x) paste(as.character(x), collapse = " "))
}

#'@title Get Names-based Element Index from Nested list
#'
#'@description Obtain numeric index vectors for a specific named list element from nested list
#'
#'@param l [list] (**required**): input list
#'
#'@param match [character] (**required**): named list element in question
#'
#'@author Sebastian Kreutzer, Geography & Earth Sciences, Aberystwyth University (United Kingdom)
#'
#'@return [list] of index vectors. The length of vector depends index level where `match` was found
#'
#'@examples
#'
#'l <- list(a = list(aa = 1), b = list(bb = 2), c = list(cc = 2))
#'.get_element_index(l, match = "cc")
#'
#'@md
#'@noRd
.get_element_index <- function(l, match = "curve"){
  ## set crawler function
  .crawler <- function(l, match, i = 0){
    lapply(seq_along(l), function(x) {
      ## if name match was found, record results in variable id
      if (hasName(l[x], match))
        id <<- c(id, list(c(i[-1],x)))

      ## if the element is still a list, move to the next
      ## list element
      if (class(l[x])[1] == "list")
        .crawler(l[[x]], match, i = c(i,x))
      else
        NULL

    })
  }

  ## initialise id
  id <- list()

  ## run crawler
  .crawler(l, match)

  return(id)
}
