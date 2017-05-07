#' Encode R data structures in bytes using msgpack.
#' @param x An R object.
#' @return An object of class "raw".
#' @useDynLib msgpackr _packb
packb <- function(datum, warn = TRUE, compatible = FALSE, use_arrays = "auto") {
  .Call(`_packb`, datum, warn, compatible, use_arrays)
}
