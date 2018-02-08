#' Convert R objects to msgpack format.
#'
#' @param x An R object, which can be null, a vector, list,
#'   environment, raw, or any combinations thereof.
#' @param ... Options passed to [packOpts()]
#' @return An object of class "raw".
#' @details
#' Strings are re-encoded to UTF-8 if necessary. Real numbers
#' taking integral values may be emitted as integers to save space.
#'
#' Normally an R vector of length 1 will be unboxed, e.g. packMsg(1)
#' will make a msgpack integer, but packMsg(c(1,2)) will make a
#' msgpack list. To prevent this and produce a list of length 1 in the
#' first case, specify `as_is = TRUE`. Objects of class `AsIs` or
#' `data.frame` will always be encoded as-is.
#'
#' A hook for pre-processing R objects before packing is supported, by
#' giving the object an S3 [class] and implementing a method
#' `prepack`. For instance, `prepack.data.frame(x)` simply adds the
#' `"AsIs"` class to `x`.
#'
#' Environment objects are written out with the keys in sorted order,
#' but named vectors are written in the order which the entries
#' appear.
#'
#' Object attributes other than `names` and `class` are ignored.
#'
#' @examples
#' packMsg( list(compact=TRUE, schema=0) )
#' @export
packMsg <- function(x, ...)  {
  .Call("_pack_msg", x, packOpts(...))
}

#' @param xs a list of objects to pack.
#' @rdname packMsg
#' @examples
#' x <- packMsgs(list("one", "two", "three"))
#' unpackMsgs(x, 2)
#' @export
packMsgs <- function(xs, ...) {
 opts <- packOpts(...)
 unlist(lapply(xs, function(xx) .Call("_pack_msg", xx, opts)))
}

#' [packOpts()] interprets the `...` argument of packMsg and
#' packMsgs. it is not exported.
#'
#' @param compatible If TRUE, emitted bytes conform to version 1.0 of
#'   msgpack encoding. This means that msgpack strings are used for
#'   raw objects.
#' @param as_is If TRUE, singletons (R primitive vectors of length 1
#'   having no names attribute) are encoded as msgpack arrays of
#'   length 1. Otherwise singletons are simplified to msgpack scalars.
#' @param use_dict If TRUE, vectors having a "names" attribute are
#'   encoded as dicts. If false, they are encoded as arrays and the
#'   names are discarded.
#' @param max_size The largest buffer that will be allocated.
#' @param buf_size The initial amount of memory, in bytes, to allocate
#'   for packing each message. This will be dynamically grown if a
#'   larger message is passed, so there is little reason to change
#'   this.
#' @rdname packMsg
packOpts <- function(compatible = FALSE,
                    as_is = FALSE,
                    use_dict = TRUE,
                    max_size = NA,
                    buf_size = 512) {
  .Call("_pack_opts",
        compatible,
        as_is,
        use_dict,
        max_size,
        buf_size,
        parent.env(environment()))
}

#' @rdname packMsg
#' @export
prepack <- function(x) UseMethod("prepack")

#' @rdname packMsg
#' @export
prepack.default <- function(x) unclass(x)

#' @rdname packMsg
#' @export
prepack.data.frame <- function(x) I(unclass(x))
