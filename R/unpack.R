#' Unpack a raw byte object in msgpack format into an R data object.
#'
#' @param dat [base::raw()] byte data, suck as read from a [make.socket()] or 
#' @param use_env if FALSE, msgpack dicts are unpacked into a vector
#'   or list with a name attribute. If `TRUE`, msgpack dicts are
#'   unpacked into environment objects. Note that this overrides the
#'   option use_df.
#' @param array Controls how to unpack msgpack arrays. `c()`, the
#'   default, will unpack arrays into the simplest type of
#'   vector. `list()` here will always unpack arrays into lists.
#' @param transform A function will be called for each data element.
#' @param dict Controls how to unpack a msgpack dict. Give en
#'   environment, such as [emptyenv()], and environments will be
#'   constructed using the given environment as parent. Otherwise
#'   dicts will be unpacked into named vectors. Note that unpacking
#'   into environments precludes `use_df`.
#' @param use_df When `TRUE`, msgpack dicts, whose elements are all
#'   arrays having the same length, are converted to [data.frame()]s.
#' @param nil If NA, will represent msgpack nil values in msgpack
#'   arrays as NA. If `NULL`, will unpack nils as `NULL`, upcasting
#'   primitives to lists in the process. If `c()`, will omit null
#'   values from arrays.
#' @return an R data object.
#'
#' The msgpack format does not have typed arrays, so all msgpack
#' arrays are effectively lists from the R perspective. However, if an
#' array containing compatibly typed elements is read, `unpack` will
#' return a logical, integer, real or string vector as appropriate.
#' The coercion used is more conservative than R's coercion: Integer
#' values may be converted to real, but boolean values will not be
#' cast to numeric, nor any types to string. If conversion from a
#' large integer to real loses precision, a warning is printed.
#'
#' Msgpack also does not distinguish between `NA` and `NULL`. The
#' argument `nil` controls how msgpack nils are unpacked.
#'
#' Strings are read as UTF-8. If a msgpack string does not appear to
#' be valid UTF-8, a warning is printed and a raw object is produced
#' instead.
#'
#' R does not implement vector names or environment keys as anything
#' other than string. If a non-string appears as key in a msgpack
#' dict, it will be converted to string with `dput()`.
#'
#' @useDynLib msgpackr _unpackb
unpackb <- function(x,
                    use_df = TRUE,
                    dict = c()
                    # simplify = TRUE,
                    # transform = NULL,
                    # dict = emptyenv() or list() or c(),
                    # nil = NA
                    # empty = c()
                   ) {
  .Call(`_unpackb`, x, dict, use_df, environment())
}
