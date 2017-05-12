#' Encode R data structures in bytes using msgpack.
#'
#' @param x An R object, which can be a vectors, lists, environments,
#'   and combinations thereof.
#' @param warn Whether to emit warnings.
#' @param compatible If TRUE, emitted bytes conform to version 1.0 of
#'   msgpack encoding. This means that msgpack strings are used for
#'   raw objects.
#' @param as.is If TRUE, vectors of length 1 having no names attribute
#'   are encoded as msgpack arrays of length 1. Otherwise singleton
#'   vectors are simplified to msgpack scalars.
#' @param use.dicts if TRUE, vectors having a "names" attribute
#' @param x An R object.
#' @return An object of class "raw".
#'
#' Strings are always emitted in UTF-8.
#'
#' @useDynLib msgpackr _packb
packb <- function(x
                  , compatible = FALSE
                  #, as_is = FALSE
                  #, warn = TRUE
                  #, null = null
                  #, na = null
                  ) {
  .Call(`_packb`, x, compatible)
}
