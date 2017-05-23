#' Unpack a raw byte object in msgpack format into an R data object.
#' @return [unpackMsg(x)] returns one decoded message (which might be
#'   shorter than the input raw), or throws an error.
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
#' Strings are assumed to be UTF-8 encoded. If a msgpack string does
#' not appear to be valid UTF-8, a warning is printed and a raw object
#' is produced instead.
#'
#' Msgpack allows any type to be the key of a dict, but R only
#' supports strings in vector names or environment keys. If a
#' non-string appears as key in a msgpack dict, it will be converted
#' to string with [deparse()].
#'
#' Extension types will be decoded as raw objects with a class like
#' `"ext120"` and a warning.
#'
#' @useDynLib msgpackr _unpack_msg
#' @export
unpackMsg <- function(x, ...) {
  .Call(`_unpack_msg`, x, unpackOpts(...))
}

#' Extract a number of msgpack messages from a raw object.
#'
#' @param x [raw()] byte data, such as read using [readBin()].
#' @param n How many messages to read. An "NA" here means to read as
#'   much as possible.
#' @param ... Unpacking parameters (see [unpackOpts()])
#' @return [unpackMsgs(r, n)] returns a list `X` with three elements:
#'   `X$msgs` is a list of the messages unpacked. `X$remaining` is a
#'   [raw()] vector of data that was not unpacked. `x$status` is a
#'   character value indicating why parsing stopped.
#'
#' Some status values you many want to check for are `"ok"` meaning
#' that the requested number of messages was read; `"end of input"`
#' meaning a smaller number of messages was read; and
#' `"buffer underflow"` meaning that we were in the middle of a
#' message when the end of input was reached. Other status values
#' indicate errors encountered in parsing.
#' @export
#' @useDynLib msgpackr _unpack_msg_partial
unpackMsgs <- function(x, n=NA, ...) {
  nmsgs <- 0
  offset <- 0
  messages <- list(NULL)
  status <- "ok"

  opts = unpackOpts(...)
  if (is.na(n))
    n <- .Machine$integer.max
  tryCatch(
    while (nmsgs < n && status == "ok") {
      result <- .Call(`_unpack_msg_partial`, x, offset, opts)
      nmsgs <- nmsgs + 1
      messages[[nmsgs]] <- result[[1]] #message
      offset <- offset + result[[2]] #bytes read
      status <- result[[3]] #return status
      if (nmsgs == length(messages)) {
        length(messages) <- 2 * length(messages)
      }
    }, error = function(e) {
      status <<- e$message
    })
  length(messages) <- nmsgs
  list(msgs = messages,
       remaining = if (offset < length(x))
                     x[(offset+1):length(x)]
                   else raw(0),
        status = status)
}

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
#' [unpackOpts()] interprets the options that are common to
#' [unpackMsgs()], [unpackMsg()], and [msgConnection()].
#' @rdname unpackMsg
#' @useDynLib msgpackr _unpack_opts
unpackOpts <- function(parent = NULL,
                       df = TRUE,
                       simplify = TRUE,
                       max_size = NA,
                       max_depth = NA) {
  .Call(`_unpack_opts`,
        parent,
        df,
        simplify,
        parent.env(environment()),
        max_size,
        max_depth);
}

# Come up with a name when a non-string is used as key.
repr <- function(x) {
  paste0(deparse(x, control = c("keepNA")), collapse="")
}
