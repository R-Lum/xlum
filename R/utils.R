# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# NON-EXPORTED INTERNAL PACKAGE UTILTY FUNCTIONS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#'@title Fast Conversion of Numeric Values into White Space Separated Strings
#'
#'@description Converts numeric vector to a character with numbers separated by white space. `NA`
#'values are transformed to `"0"`.
#'
#'@param l [numeric] [list] (**required**): list of numeric vectors (see example)
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@return [list] of [numeric] vectors
#'
#'@examples
#'
#'l  <- list(a = c(12, 12, 13), b = NA)
#'.convert2character(l)
#'
#'@md
#'@noRd
.convert2character <- function(l, decode_base64 = FALSE){
  ## handle NA values
  if (anyNA(l)) l[which(is.na(l))] <- "0"

  ##list
  if(class(l)[1] == "list")
    return(lapply(l, function(x) paste(as.character(x), collapse = " ")))

  ##non-list
  return(paste(as.character(l), collapse = " "))

}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#'@title Fast Conversion of White Space Separated Strings to Numeric
#'
#'@description Converts strings with white space separated numbers to a numeric vector. `"0"`
#'values are transformed to `NA`.
#'
#'@param l [character] [list] (**required**): list of character vectors (see example)
#'
#'@param check_base64 [logical] (*with default*): check for base 64 encoding and decode if needed
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@return [list] of [numeric] vectors
#'
#'@examples
#'
#'l <- list(a = c("12 12 13"), b = "0")
#'.convert2numeric(l)
#'
#'@md
#'@noRd
.convert2numeric <- function(l, check_base64 = FALSE){
  ## handle NA values
  if (any(l == "0")) l[which(l == "0")] <- NA_character_

  ## check for base64 encoding and convert if needed
  if(check_base64) {
    l <- lapply(l, function(x){
      ## if it contains white space it is not base64
      if(any(grepl(" ", x, fixed = TRUE)) || is.na(x)) return(x)

      ## decode to raw convert to char
      rawToChar(x = base64enc::base64decode(x), multiple = FALSE)

    })
  }

  ##extract values
  lapply(
    unlist(
      lapply(l, strsplit, split = " ", fixed = TRUE),
      recursive = FALSE),
    as.numeric)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#'@title Get Names-based Element Index from Nested list
#'
#'@description Obtain numeric index vectors for a specific named list element from nested list
#'
#'@param l [list] (**required**): input list
#'
#'@param match [character] (**required**): named list element in question
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
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
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#'@title Regular expression match wrapper to extract found values conveniently
#'
#'@description Combination of [regmatches], [regexpr] and [trimws]. The regular
#'expressions use `perl = TRUE` by default
#'
#'@param x [character] vector (**required**): input
#'
#'@param match [character] (**required**): regular expression
#'
#'@param ignore.case [logical] (*with default*): enable/disable case sensitivity
#'
#'@param invert [logical] (*with default*): enable/disable inversion
#'
#'@return Matched expression
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@examples
#'t <- c("Text to (extract )")
#'.regmatches(t, "(?<=\\()[a-z]+")
#'
#'@md
#'@noRd
.regmatches <- function(x, match, ignore.case = FALSE, invert = FALSE){
  t <- trimws(
    regmatches(
      x = x,
      m = regexpr(
        pattern = match[1],
        text = x,
        ignore.case = ignore.case[1],
        perl = TRUE),
      invert = invert[1]))

  if(length(t) == 0)
    return(NA_character_)
  else
    return(t)
}
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#'@title Character strings to ISO 8601-1:2019 date format
#'
#'@description Fast conversion of characters of certain pattern to ISO conform
#'dates
#'
#'@param x [character] (**required**): input character
#'
#'@param format [character] (*with default*): fixed formats
#'
#'@author Sebastian Kreutzer, Institute of Geography, Heidelberg University, Heidelberg (Germany)
#'
#'@return Returns a correctly formatted character
#'
#'@examples
#'.toISODate("20221005110501", "lexsyg")
#'
#'@md
#'@noRd
.toISODate <- function(x, format = "default"){
  switch(format,
    "lexsyg"  = return(format(strptime(x, "%Y%m%d%H%M%S", tz = "UTC"), "%Y-%m-%dT%XZ")),
    "Risoe"  = return(format(strptime(x, "%y%m%d%H:%M:%S", tz = "UTC"), "%Y-%m-%dT%XZ")),
    "Daybreak" = return(format(strptime(x, "%d/%m/%Y %H:%M:%S", tz = "UTC"), "%Y-%m-%dT%XZ")),
    "psl"  =  return(format(strptime(x, "%Y-%m-%d%H:%M:%S", tz = "UTC"), "%Y-%m-%dT%XZ")),
    stop("[.toISODate()] unknown date format", call. = FALSE)

  )

}
