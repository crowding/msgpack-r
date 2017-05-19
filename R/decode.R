#' Unpack a raw byte object in msgpack format into an R data object.
#' @return an R data object.
#'
#' The msgpack format does not have typed arrays, so all msgpack
#' arrays are effectively lists from the R perspective. However, if an
#' array containing compatibly typed elements is read, `unpack` will
#' return a logical, integer, real or string vector as
#' appropriate. This behavior is disabled with `simplify=FALSE`.  The
#' coercion used is more conservative than R's coercion: Integer
#' values may be converted to real, but boolean values will not be
#' cast to numeric, nor any types to string. If conversion from a
#' large integer to real loses precision, a warning is printed.
#'
#' Msgpack also does not distinguish between `NA` and `NULL`. All nils
#' will be decoded as NA.
#'
#' Strings are read as UTF-8. If a msgpack string does not appear to
#' be valid UTF-8, a warning is printed and a raw object is produced
#' instead.
#'
#' R does not implement vector names or environment keys as anything
#' other than string. If a non-string appears as key in a msgpack
#' dict, it will be converted to string with `dput()`.
#'
#' @useDynLib msgpackr _unpack_msg
#' @export
unpackMsg <- function(x, ...) {
  .Call(`_unpack_msg`, x, unpackOpts(...))
}

#' Extract a number of msgpack messages from a raw object.
#'
#' @param x A raw object.
#' @param n How many messages to read. An "NA" here means to read as
#'   much as possible.
#' @param ... Unpacking parameters (see [unpackOpts()])
#' @return A list A with three elements: `A$messages` is a list of the
#'   messages read. a$remaining is a [raw()] vector of remaingin data
#'   that is not read. "status" is a character indicating why parsing
#'   stopped.
#'
#' Some status values are `"ok"` meaning that the requested number of
#' messages was read; `"end of input"` meaning a smaller number of
#' messages was read; and `"buffer underflow"` meaning that input
#' ended with a partially completed message. Other status values
#' indicate errors encountered in parsing.
#' @export
#' @useDynLib msgpackr _unpack_msg_partial
unpackMsgs <- function(x, n=NA, ...) {
  nmsgs <- 0
  offset <- 0
  messages <- list(NULL)
  status <- "ok"

  opts = unpackOpts(...)
  tryCatch(
    while ((is.na(n) || nmsgs < n) && status == "ok") {
      result <- .Call(`_unpack_msg_partial`, x, offset, opts)
      nmsgs <- nmsgs + 1
      messages[[nmsgs]] <- result[[1]] #message
      offset <- offset + result[[2]] #bytes read
      status <- result[[3]] #return status
      if (nmsgs == length(messages))
        length(messages) <- 2 * length(messages)
    }, error = function(e) {
      status <<- e$message
    })
  length(messages) <- nmsgs
  list(messages = messages,
       remaining = if (offset < length(x))
                     x[(offset+1):length(x)]
                   else raw(0),
        status = status)
}

#' @useDynLib msgpackr _unpack_opts
#' @param dat [raw()] byte data, such as read using [readBin()].
#' @param simplify Controls how to unpack msgpack arrays. `TRUE`, the
#'   default, will unpack arrays into the simplest available type of
#'   vector. `FALSE` here will always unpack arrays into lists.
#' @param parent If an environment is given, (such as [emptyenv()]),
#'   msgpack dicts will be unpacked into environment objects, with the
#'   given value as parent. Otherwise dicts will be unpacked into
#'   named vectors. Note that unpacking into environments precludes
#'   `use_df`.
#' @param df When `TRUE`, msgpack dicts, whose elements are all arrays
#'   having the same length, are converted to [data.frame()]s.
#'
#' unpackOpts interprets the options that
unpackOpts <- function(parent = NULL,
                       df = TRUE,
                       simplify = TRUE) {
  .Call(`_unpack_opts`,
        parent,
        df,
        simplify,
        parent.env(environment()));
}

# use dput to come up with a name when a non-string is used as key.
repr <- function(x) {
  con <- textConnection(NULL, "w")
  dput(x, con, c("keepNA"))
  textConnectionValue(con)
}
