#' Encode R data structures in bytes using msgpack.
#' @param datum An R object.
#' @param warn Whether to emit warnings.
#' @param compatible If TRUE, emitted bytes conform to version 1.0 of
#'   msgpack encoding.
#' @param always_arrays If TRUE, vectors of length 1 having no names
#'   attribute are encoded as msgpack arrays of length 1. Otherwise singleton
#'   vectors are encoded as msgpack scalars.
#' @param x An R object.
#' @return An object of class "raw".
#' @useDynLib msgpackr _packb
packb <- function(datum, warn = TRUE, compatible = FALSE, all_arrays = TRUE) {
  .Call(`_packb`, datum, warn, compatible, all_arrays)
}
