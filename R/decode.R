#' Unpack raw objects in msgpack format into R data structures.
#'
#' @param x A [raw()] object, perhaps read from a file or socket.
#' @param ... Options passed to [unpackOpts].
#' @return `unpackMsg(x)` returns one decoded message (which might be
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
#' supports strings. If a non-string appears as key in a msgpack dict,
#' it will be converted to string with [deparse()].
#'
#' Extension types will be decoded as raw objects with a class like
#' `"ext120"` and a warning.
#'
#' @useDynLib msgpack, .registration = TRUE
#' @examples
#' msg <- as.raw(c(0x82, 0xa7, 0x63, 0x6f, 0x6d, 0x70, 0x61, 0x63, 0x74, 0xc3,
#'                 0xa6, 0x73, 0x63, 0x68, 0x65, 0x6d, 0x61, 0x00))
#' unpackMsg(msg)
#' @export
unpackMsg <- function(x, ...) {
  .Call("_unpack_msg", x, unpackOpts(...))
}

dbg <- alist
#dbg <- cat

#' Extract a number of msgpack messages from a raw object.
#'
#' @param n How many messages to read. An "NA" here means to read as
#'   much as possible.
#' @param reader For implementing connections; a function that takes
#'   no arguments and returns a raw containing more data.
#' @return `unpackMsgs(r, n)` returns a list `X` with four elements:
#'   * `X$msgs` is a list of the messages unpacked.
#'   * `X$remaining` is data remaining to be parsed.
#'   * `X$status` is a status message, typically "ok", "end of input",
#'     or "buffer underflow".
#'   * `x$bytes_read` the number of bytes consumed.
#'
#' @examples
#' x <- packMsgs(list("one", "two", "three"))
#' unpackMsgs(x, 2)
#' @rdname unpackMsg
#' @export
unpackMsgs <- function(x, n = NA, reader = NULL, ...) {
  if (is.na(n)) {
    n <- .Machine$integer.max
  }

  bread <- 0                            # bytes read
  saveMessage <- lister()               # messages read so far
  saveData <- catenator(x)              # bytes pending
  offset <- 0                           # position within buffer

  status <- "ok"

  readMore <- function(current, desired) {
    dbg("readMore:", "current =", current, "desired =", desired, "\n")
    start <- saveData(action="start")
    saveData(reader(desired))
    new_start <- saveData(action="start")
    current <- new_start - start + current
    result <- c(saveData(action="contents"), current)
    # cat("after read: "); print(saveData(action="contents"))
    result
  }

  opts = unpackOpts(...,
                    underflow_handler = if (is.null(reader)) NULL else readMore)

  # cat("buffer: "); print(saveData(action="contents"))
  tryCatch(
    while(saveMessage(action="length") < n) {
      last_start <- saveData(action="start")
      result <- .Call("_unpack_msg_partial",
                      saveData(action="buf"),
                      offset,
                      saveData(action="end"),
                      opts)
      ## result is a pairlist( message, status, new_offset )
      # cat("unpack result: "); print(result)
      status <- result[[1]]
      if (status == "ok") {
        # got a good message,
        saveMessage(result[[2]])
        message_length <- ((result[[3]] - offset) +
                           (saveData(action="start") - last_start))
        bread <- bread + message_length
        saveData(message_length, action="drop")
        offset <- result[[3]]
        # dbg("after 1 msg, buffer: "); print(saveData(action="contents"))
      } else {
        stop(status)
      }
    },
    error = function(e) {
      dbg("Stopping with exception: ", e$message, "in", deparse(e$call), "\n")
      status <<- e$message
    }
  )
  # dbg("after failure, buffer: "); print(saveData(action="contents"))

  list(msgs = saveMessage(action="read"),
       remaining = saveData(action="read"),
       status = status,
       bytes_read = bread)
}

#' [unpackOpts()] interprets the options that are common to
#' [unpackMsgs()], [unpackMsg()], and [msgConnection()]. It is not
#' exported.
#'
#' @param parent When an environment is given, (such as [emptyenv()]),
#'   unpack msgpack dicts into environment objects, with the given
#'   value as parent. This option overrides `use_df=TRUE`. Otherwise,
#'   unpack dicts into named vectors / lists.
#' @param df When `TRUE`, convert msgpack dicts, whose elements are
#'   all arrays having the same length, into [data.frame()]s.
#' @param simplify If `TRUE`, simplify msgpack lists into primitive
#'   vectors.
#' @param max_size The maximum length of message to decode.
#' @param max_depth The maximum degree of nesting to support.
#' @param underflow_handler Used internally.
#' @rdname unpackMsg
unpackOpts <- function(parent = NULL,
                       df = TRUE,
                       simplify = TRUE,
                       max_size = NA,
                       max_depth = NA,
                       underflow_handler = NULL) {
  .Call("_unpack_opts",
        parent,
        df,
        simplify,
        parent.env(environment()),
        max_size,
        max_depth,
        underflow_handler);
}

# Come up with a name when a non-string is used as key.
repr <- function(x) {
  paste0(deparse(x, control = c("keepNA")), collapse="")
}
