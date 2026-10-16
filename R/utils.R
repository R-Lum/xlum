# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# NON-EXPORTED INTERNAL PACKAGE UTILTY FUNCTIONS
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#' @title Fast conversion of numeric values into whitespace-separated strings
#'
#' @description
#' Converts numeric vector to a character with numbers separated by white
#' space. `NA` values are transformed to `"0"`.
#'
#' @param l [numeric] [list] (**required**):
#' list of numeric vectors (see example).
#'
#' @param encode_base64 [logical] (*with default*):
#' enable/disable base64 encoding.
#'
#' @return
#' A [list] of [numeric] vectors.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @examples
#' l  <- list(a = c(12, 12, 13), b = NA)
#' .convert2character(l)
#'
#' @noRd
.convert2character <- function(l, encode_base64 = FALSE){
  ## handle NA values
  if (anyNA(l)) l[which(is.na(l))] <- "0"

  ##list and non-list
  if(inherits(l, "list")) {
    out <- lapply(l, function(x) paste(x, collapse = " "))
  } else {
    out <- paste(l, collapse = " ")
  }

  ## base64 encoding
  if (isTRUE(encode_base64)) {
   out <- switch(class(out)[1],
      "list" = lapply(out, function(x) base64enc::base64encode(charToRaw(x))),
      "character" = base64enc::base64encode(charToRaw(out))
    )
  }

  return(out)
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#' @title Fast conversion of whitespace-separated strings to numeric
#'
#' @description
#' Converts strings with white space separated numbers to a numeric vector. `"0"`
#' values are transformed to `NA`.
#'
#' @param l [character] [list] (**required**):
#' list of character vectors (see example).
#'
#' @param check_base64 [logical] (*with default*):
#' check for base 64 encoding and decode if needed.
#'
#' @return
#' A [list] of [numeric] vectors.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @examples
#' l <- list(a = c("12 12 13"), b = "0")
#' .convert2numeric(l)
#'
#' @noRd
.convert2numeric <- function(l, check_base64 = FALSE){
  ## handle NA values
  if (any(l == "0")) l[which(l == "0")] <- NA_character_

  ## check for base64 encoding and convert if needed
  if (isTRUE(check_base64)) {
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
#' @title Get names-based element index from nested list
#'
#' @description
#' Obtain numeric index vectors for a specific named list element from nested
#' list.
#'
#' @param l [list] (**required**): input list
#'
#' @param match [character] (**required**):
#' named list element in question.
#'
#' @return
#' A [list] of index vectors. The length of vector depends index level where
#' `match` was found.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @examples
#' l <- list(a = list(aa = 1), b = list(bb = 2), c = list(cc = 2))
#' .get_element_index(l, match = "cc")
#'
#' @noRd
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
#' @title Regular expression match wrapper to extract found values conveniently
#'
#' @description
#' Combination of [regmatches], [regexpr] and [trimws]. The regular expressions
#' use `perl = TRUE` by default.
#'
#' @param x [character] vector (**required**):
#' input.
#'
#' @param match [character] (**required**):
#' regular expression.
#'
#' @param ignore.case [logical] (*with default*):
#' enable/disable case sensitivity.
#'
#' @param invert [logical] (*with default*):
#' enable/disable inversion.
#'
#' @return
#' The matched expressions.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @examples
#' t <- c("Text to (extract )")
#' .regmatches(t, "(?<=\\()[a-z]+")
#'
#' @noRd
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
#' @title Character strings to ISO 8601-1:2019 date format
#'
#' @description
#' Fast conversion of characters of certain pattern to ISO-conform dates.
#'
#' @param x [character] (**required**):
#' input string.
#'
#' @param format [character] (*with default*):
#' date format.
#'
#' @return
#' A correctly formatted character.
#'
#' @author
#' Sebastian Kreutzer, F2.1 Geophysical Parametrisation/Regionalisation, LIAG - Institute for Applied Geophysics (Germany)\cr
#'
#' @examples
#' .toISODate("20221005110501", "lexsyg")
#'
#' @noRd
.toISODate <- function(x, format = "default"){
  fmt <- switch(format,
                "lexsyg"   = "%Y%m%d%H%M%S",
                "Risoe"    = "%y%m%d%H:%M:%S",
                "Daybreak" = "%d/%m/%Y %H:%M:%S",
                "Daybreak2"= "%Y-%m-%d %H:%M:%S",
                "psl"      = "%Y-%m-%d%H:%M:%S",
                stop("[.toISODate()] Unknown date format", call. = FALSE)
                )
  format(strptime(x, fmt, tz = "UTC"), "%Y-%m-%dT%XZ")
}

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#' @title Read and cache the XLUM prototype attributes
#'
#' @description
#' Reads the XLUM prototype shipped with the package and returns the
#' attributes of its `xlum`, `sample`, `sequence`, `record`, and `curve`
#' nodes. The result is computed only once and cached for subsequent calls.
#'
#' @return
#' A named [list] of character vectors.
#'
#' @author
#' Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
#'
#' @noRd
.get_prototype_attrs <- function(){
  if (is.null(.xlum.env$prototype_attrs)) {
    xlum_proto <- xml2::read_xml(system.file("extdata/xlum_prototype.xlum",
                                             package = "xlum"))
    xlum_proto_ns <- xml2::xml_ns(xlum_proto)

    .xlum.env$prototype_attrs <- list(
        xlum = xml2::xml_attrs(xml2::xml_root(xlum_proto)),
        sample = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//sample",
                                                 xlum_proto_ns)),
        sequence = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//sequence",
                                                   xlum_proto_ns)),
        record = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//record",
                                                 xlum_proto_ns)),
        curve = xml2::xml_attrs(xml2::xml_child(xlum_proto, ".//curve",
                                                xlum_proto_ns)))
  }

  return(.xlum.env$prototype_attrs)
}
