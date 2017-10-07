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
  .Call(`_unpack_msg`, x, unpackOpts(...))
}

#' Extract a number of msgpack messages from a raw object.
#'
#' @param n How many messages to read. An "NA" here means to read as
#'   much as possible.
#' @return `unpackMsgs(r, n)` returns a list `X` with three elements:
#'   `X$msgs` is a list of the messages unpacked. `X$remaining` is a
#'   [raw] vector of data that was not unpacked. `x$status` is a
#'   character value indicating why parsing stopped.
#'
#' Some status values you may want to check for are `"ok"` meaning
#' that the requested number of messages was read; `"end of input"`
#' meaning a smaller number of messages was read; and
#' `"buffer underflow"` meaning that we were in the middle of a
#' message when the end of input was reached. Other status values
#' indicate errors encountered in parsing.
#'
#' @examples
#' x <- packMsgs(list("one", "two", "three"))
#' unpackMsgs(x, 2)
#' @useDynLib msgpack _unpack_msg_partial
#' @rdname unpackMsg
#' @export
unpackMsgs <- function(x, n=NA, ...) {
  nmsgs <- 0
  offset <- 0
  storeMessages <- catenator(list())
  status <- "ok"

  opts = unpackOpts(...)
  if (is.na(n)) {
      n <- .Machine$integer.max
  }
  tryCatch(
    while(nmsgs < n) {
        result <- .Call(`_unpack_msg_partial`, x, offset, opts)
        status <- result[[3]]
        if (status == "ok") {
            offset <- result[[2]]
            storeMessages(list(result[[1]]))
            nmsgs <- nmsgs + 1
        } else {
            break()
        }
    },
    error = function(e) {
      status <<- e$message
    }
  )
  list(msgs = storeMessages(action="read"),
       remaining = if (offset < length(x))
                     x[(offset+1):length(x)]
                   else raw(0),
       status = status)
}

#' [unpackOpts()] interprets the options that are common to
#' [unpackMsgs()], [unpackMsg()], and [msgConnection()]. It is not
#' exported.
#'
#' @param parent When an environment is given, (such as [emptyenv()]),
#'     unpack msgpack dicts into environment objects, with the given
#'     value as parent. This option overrides
#'     `use_df=TRUE`. Otherwise, unpack dicts into named vectors /
#'     lists.
#' @param df When `TRUE`, convert msgpack dicts, whose elements are
#'     all arrays having the same length, into [data.frame()]s.
#' @param simplify If `TRUE`, simplify msgpack lists into primitive
#'     vectors.
#' @param max_size The maximum length of message to decode.
#' @param max_depth The maximum degree of nesting to support.
#' @param underflow_handler Advanced usage. A `function(r, x)` where
#'     `r` is a raw object and `x < length(r)` is an offset. Its
#'     return value should be a new raw object, beginning with the
#'     same object, or NULL if more data could not be read.
#' @rdname unpackMsg
#' @useDynLib msgpack _unpack_opts
unpackOpts <- function(parent = NULL,
                       df = TRUE,
                       simplify = TRUE,
                       max_size = NA,
                       max_depth = NA,
                       underflow_handler = NULL) {
    #' TODO: make this actually initialize the whole msg-reader struct
  .Call(`_unpack_opts`,
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
