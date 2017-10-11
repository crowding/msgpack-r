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
#' @useDynLib msgpack _unpack_msg
#' @examples
#' msg <- as.raw(c(0x82, 0xa7, 0x63, 0x6f, 0x6d, 0x70, 0x61, 0x63, 0x74, 0xc3,
#'                 0xa6, 0x73, 0x63, 0x68, 0x65, 0x6d, 0x61, 0x00))
#' unpackMsg(msg)
#' @export
unpackMsg <- function(x, ...) {
  .Call(`_unpack_msg`, unpackOpts(..., buf=x))
}

#' Extract a number of msgpack messages from a raw object.
#'
#' @param n How many messages to read. An "NA" here means to read as
#'   much as possible.
#' @return `unpackMsgs(r, n)` returns a list `X` with three elements:
#'   * `X$msgs` is a list of the messages unpacked.
#'   * `X$remaining` is data remaining to be parsed.
#'   * `X$status` is a status message, typically "ok", "end of input",
#'     or "buffer underflow".
#'   * `x$bytes_read` the number of bytes consumed.
#'
#' @examples
#' x <- packMsgs(list("one", "two", "three"))
#' unpackMsgs(x, 2)
#' @useDynLib msgpack _unpack_msg_partial
#' @rdname unpackMsg
#' @export
unpackMsgs <- function(x, n=NA, reader = NULL, ...) {
  if (is.na(n)) {
    n <- .Machine$integer.max
  }

  offset <- 0                           # offset in current buffer "x"
  bread <- 0                            # bytes read
  saveMessage <- lister()               # messages read so far
  saveData <- catenator(x)              # bytes read so far
                                        # note invariant: saveData duplicates working buffwer
  status <- "ok"

  readMore <- function(buf,   # a raw
                       x) {   # the number of bytes that have been consumed.
    # To return: the unread bytes plus some new bytes.
    #
    # Observe the contortions that R's busted: operator makes us
    # go through.
    out <- if (x == 0) {
      c(buf, saveData(reader()))
    } else if (x == length(buf)) {
      saveData(reader())
    } else {
      # If : were not busted we could just write this:
      c(buf[(x+1):length(buf)], saveData(reader()))
    }
    # Note 'seq' would be busted here too because seq(from=1, to=0,
    # by=1) throws an error instead of a zero-length vector
    return(out)
  }

  opts = unpackOpts(..., buf = x,
                    underflow_handler = if (is.null(reader)) NULL else readMore)
  tryCatch(
    while(saveMessage(action="length") < n) {
      result <- .Call(`_unpack_msg_partial`, offset, opts)
      status <- result[[3]]
      if (status == "ok") {
        # got a good message,
        # result <- list( message, new_offset, status, new_buffer)
        # before we started, the catenator and working buffer overlapped at end.
        saveMessage(result[[1]])
        bread <- ( bread
                 + (result[[2]] - offset)
                 + (saveData(action="length") - length(result[[4]])))
        offset <- result[[2]]
        x <- result[[4]]
        saveData(result[[4]], action = "reset") # catenator and working buffer again overlap at end.
      } else {
        # we also get exceptions from C code, so go to the exception handler
        stop(status)
      }
    },
    error = function(e) {
      status <<- e$message
      x <<- saveData(action = "read")
      saveData(action="reset", x)
      # offset should be good from start of last read
    }
  )

  list(msgs = saveMessage(action="read"),
       remaining = if (offset < length(x)) x[(offset+1):length(x)] else raw(0),
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
#' @rdname unpackMsg
#' @useDynLib msgpack _unpack_opts
unpackOpts <- function(parent = NULL,
                       df = TRUE,
                       simplify = TRUE,
                       max_size = NA,
                       max_depth = NA,
                       underflow_handler = NULL,
                       buf = raw(0)) {
  .Call(`_unpack_opts`,
        parent,
        df,
        simplify,
        parent.env(environment()),
        max_size,
        max_depth,
        underflow_handler,
        buf);
}

# Come up with a name when a non-string is used as key.
repr <- function(x) {
  paste0(deparse(x, control = c("keepNA")), collapse="")
}
