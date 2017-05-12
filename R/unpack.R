#' @useDynLib msgpackr _wtf
wtf <- function() {
  .Call(`_wtf`)
}

#' Unpack a raw byte object in msgpack format into an R data object.
#'
#' @param dat [base::raw()] byte data.
#' @param use_envs if FALSE, msgpack dicts are unpacked into a vector
#'   or list with a name attribute. If true, msgpack dicts are
#'   unpacked into environment objects.
#' @param simplify If `FALSE`, msgpack arrays will be unpacked
#'   into lists. If `TRUE`, will produce atomic vectors when possible.
#' @param transform A function will be called for each data element.
#' @param dict What type of object to produce given a msgpack dict. If given an env, will encode as an env with that as parent. If given a vector, will 
#' @return an R data object.
#'
#' The msgpack format does not have typed arrays, so all msgpack
#' arrays are effectively lists from the R perspective. However, if an
#' array containing compatibly typed elements is read, `unpack` will
#' return a logical, integer, real or string vector as
#' appropriate. Integer values may be converted to real, but boolean
#' values will not be cast to numeric, not any types to string. If
#' conversion from a large integer to real loses precision, a warning
#' is produced. If option `simplify` is FALSE, msgpack arrays will
#' always be turned into R lists.
#'
#' Because msgpack does not distinguish NA and NULL, nil values in a
#' msgpack will be decoded as NA when producing a vector, and NULL
#' when returning a list.
#'
#' @useDynLib msgpackr _unpackb
unpackb <- function(x
                    # use_envs = FALSE,
                    # simplify = TRUE,
                    # transform = NULL,
                    # dict = emptyenv() or list() or c(),
                    # null = NULL
                    # na = NULL
                    # empty = c()
                   ) {
  .Call(`_unpackb`, x)
}
