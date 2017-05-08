#' Unpack a raw byte object in msgpack format into an R data object.
#'
#' @param dat A raw byte object.
#' @param warn Whether to generate warnings.
#' @param dicts_as_environments Whether to represent msgpack dicts as
#'   R environments.
#' @param simplify If `FALSE`, arrays are always represented as lists.
#' @return an R data object.
#' @useDynLib msgpackr _unpackb
unpackb <- function(dat, warn=TRUE, use_envs=FALSE, simplify=TRUE) {
  .Call(`_unpackb`, dat, warn, use_envs, simplify)
}
