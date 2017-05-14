#' Encode R data structures in bytes using msgpack.
#'
#' @param x An R object, which can be null, a vector, list, environment, raw,
#'   and any combinations thereof.
#' @param compatible If TRUE, emitted bytes conform to version 1.0 of
#'   msgpack encoding. This means that msgpack strings are used for
#'   raw objects.
#' @param as_is If TRUE, R vectors of length 1 having no names
#'   attribute are encoded as msgpack arrays of length 1. Otherwise
#'   singleton vectors are simplified to msgpack scalars.
#' @param max_size The largest buffer that will be allocated.
#' @param warn Whether to emit warnings.
#' @param use_dict if TRUE, vectors having a "names" attribute are
#'   encoded as dicts. If false, the names are discarded.
#' @return An object of class "raw".
#'
#' Strings are always re-encoded to UTF-8. Integral floating-point values
#' may be emitted as integers to save space.
#'
#' A hook for pre-processing R objects before packing is supported, by
#' giving the object a [class] attribute and implementing a method
#' "prepack" for that class. For example, [prepack.data.frame(x)]
#' simply adds the [AsIs] class to `x`.  Objects of class `AsIs,` and
#' their contents, are encoded without using scalars (same as when option
#' `as_is` is TRUE) and are not further pre-processed.
#'
#' Dictionary objects are output with the keys in binary sorted order,
#' but named vectors are output in the order which they appear.
#'
#' Object attributes other than `name` and `class` are ignored.
#'
#' @useDynLib msgpackr _packb
packb <- function(x
                  , compatible = FALSE
                  , as_is = FALSE
                  , max_size = Inf
                  , use_dict = TRUE
                  ) {
  .Call(`_packb`, x, compatible, as_is, environment(), max_size, use_dict)
}

#' @export
prepack <- function(x) UseMethod("prepack")

#' @export
prepack.default <- function(x) unclass(x)

prepack.data.frame <- function(x) I(unclass(x))
