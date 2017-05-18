#' Encode R data structures in bytes using msgpack.
#'
#' @param x An R object, which can be null, a vector, list,
#'   environment, raw, and any combinations thereof.
#'
#' Strings are always re-encoded to UTF-8. Integral floating-point values
#' may be emitted as integers to save space.
#'
#' A hook for pre-processing R objects before packing is supported, by
#' giving the object an S3 [class] and implementing a method
#' "prepack". For instance, [prepack.data.frame(x)] simply adds the
#' [AsIs] class to `x`.  Objects having class `AsIs,` and their
#' contents, are encoded without using scalars (same as when option
#' `as_is` is TRUE) and are not further pre-processed.
#'
#' Dictionary objects are output with the keys in binary sorted order,
#' but named vectors are output in the order which they appear.
#'
#' Object attributes other than `name` and `class` are ignored.
#'
#' @param ... Options controlling packing, as described on this page.)
#' @useDynLib msgpackr _pack_msg
#' @export
packMsg <- function(x, ...)  {
  .Call(`_pack_msg`, x, packOpts(...))
}

#' @param compatible If TRUE, emitted bytes conform to version 1.0 of
#'   msgpack encoding. This means that msgpack strings are used for
#'   raw objects.
#' @param as_is If TRUE, R vectors of length 1 having no names
#'   attribute are encoded as msgpack arrays of length 1. Otherwise
#'   singleton vectors are simplified to msgpack scalars.
#' @param dict if TRUE, vectors having a "names" attribute are
#'   encoded as dicts. If false, the names are discarded.
#' @param max_size The largest buffer that will be allocated.
#' @param buf_size How much memory, in bytes, to allocate for packing
#'   to be done in vector. There is little reason to change this for
#'   [unpack()]. For [write_msg()] it controls how much data is passed
#'   for each call to [writeBytes()].
#' @param warn Whether to emit warnings.
#' @return An object of class "raw".
#' @rdname packMsg
#' @useDynLib msgpackr _pack_opts
packOpts = function(compatible = FALSE,
                    as_is = FALSE,
                    use_dict = TRUE,
                    max_size = NA,
                    buf_size = 512,
                     package) {
  .Call(`_pack_opts`,
        compatible,
        as_is,
        use_dict,
        max_size,
        buf_size,
        parent.env(environment()))
}

#' @export
prepack <- function(x) UseMethod("prepack")

#' @export
prepack.default <- function(x) unclass(x)

prepack.data.frame <- function(x) I(unclass(x))
