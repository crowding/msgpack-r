#' Unpack a raw byte object in msgpack format into an R data object.
#'
#' @param dat [base::raw()] byte data.
#' @param use_env if FALSE, msgpack dicts are unpacked into a vector
#'   or list with a name attribute. If `TRUE`, msgpack dicts are
#'   unpacked into environment objects. Note that this overrides the
#'   option use_df.
#' @param simplify If `FALSE`, all msgpack arrays will be unpacked
#'   into lists. If `TRUE`, will produce atomic vectors when possible.
#' @param transform A function will be called for each data element.
#' @param dict What type of object to produce given a msgpack dict. If
#'   given an environment (e.g. `emptyenv()`, will produce an
#'   environment with the given parent. If given a vector, will try to
#'   produce a named list.
#' @param use_df If TRUE, msgpack dictionaries whose elements are all
#'   arrays having the same length are converted to [data.frame()]s.
#' @param nil If NA, will represent nil values in msgpack arrays as NA. If
#'   `NULL`, will unpack nil values in arrays as `NULL`. If `c()`,
#'   will omit null values from arrays.
#' @return an R data object.
#'
#' The msgpack format does not have typed arrays, so all msgpack
#' arrays are effectively lists from the R perspective. However, if an
#' array containing compatibly typed elements is read, `unpack` will
#' return a logical, integer, real or string vector as
#' appropriate. Integer values may be converted to real, but boolean
#' values will not be cast to numeric, not any types to string. If
#' conversion from a large integer to real loses precision, a warning
#' is printed.
#'
#' Strings are treated as UTF-8. If a msgpack string does not appear
#' to be a valid UTF-8 sequence, a warning is printed and a raw object
#' is produced instead.
#'
#' Dictionary entries with keys that are other than strings are not
#' yet supported. If use
#'
#' If `use_env=TRUE` and duplicate names are given in a dictionary, a
#' warning is produced and thew duplicate entries are dropped.
#'
#' @useDynLib msgpackr _unpackb
unpackb <- function(x
                    # use_df = TRUE,
                    # use_env = FALSE,
                    # simplify = TRUE,
                    # transform = NULL,
                    # dict = emptyenv() or list() or c(),
                    # nil = NA
                    # empty = c()
                   ) {
  .Call(`_unpackb`, x)
}
