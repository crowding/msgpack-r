#' Unpack a raw byte object in msgpack format into an R data object.
#'
#' @param dat Raw byte data.
#' @param warn Whether to generate warnings (e.g. when an object is
#'   passed that is unknown)
#' @param use_envs if FALSE, msgpack dicts are unpacked into a vector
#'   or list with aname attribute. If true, msgpack dicts are unpacked
#'   into environment objects.
#' @param simplify If `FALSE`, arrays are always unpacked into as
#'   lists. If TRUE, will attempt to produce simpler vectors.
#' @param nil How to represent nil values.
#' @return an R data object.
#' @useDynLib msgpackr _unpackb
unpackb <- function(dat,
                    warn = TRUE,
                    use_envs = TRUE,
                    simplify = TRUE,
                    nil = NA) {
  .Call(`_unpackb`, dat, warn, use_envs, simplify, nil)
}
