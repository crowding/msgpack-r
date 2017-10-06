#' Read and write msgpack messages using a connection.
#'
#' @param conn A [connection] object open in binary mode.
#' @param max_size The largest partial uncompleted message to
#'   store. `NA` means do not enforce a limit.
#' @param read_size How many bytes to read at a time.
#' @param ... Unpacking options (see [unpackMsg]).
#' @return [msgConnection()] returns an object of class 'msgConnection'
#'
#' Because msgpack messages have unpredictable length, the decoder
#' reads ahead in chunks, then finds the boundaries between messages.
#' Therefore when reading over a socket or a fifo it is best to use a
#' nonblocking connection, and it will not work to mix readMsg and
#' readBin on the same connection.
#'
#' In the present implementation, decoding must start over at the
#' beginning of the message when it is split across two or more
#' chunks. So `read_size` should be set to something larger than the
#' typical message recieved.
#'
#' If you are reading data from an untrusted source you will probably want
#' to set option `max_size` (see [unpackOpts]).  This
#' protects against stack overflows and out-of-memory errors that
#' could be caused by some messages.
#'
#' Reading from connections is somewhat experimental at the
#' moment. Please report problems you encounter.
#' @examples
#' out <- rawConnection(raw(0), open="wb")
#' apply(quakes, 1, function(x) writeMsg(x, out))
#' length(rawConnectionValue(out))
#' inn <- msgConnection(rawConnection(rawConnectionValue(out), open="rb"))
#' readMsg(inn)
#' readMsgs(inn, 3)
#' @export
msgConnection <- function(conn, read_size=2^16, max_size=NA, ...) {
  partial = raw(0)
  status = "ok"

  readMsgs <- function(n=NA, read_size=parent.env(environment())$read_size) {
    collect <- catenator(list())
    if (is.na(n)) n <- .Machine$integer.max
    while ((nm <- collect(action="length")) < n) {
      switch(status,
             "ok" = {
               if (length(partial) == 0)
                 partial <<- readRaw(conn, read_size)
             },
             "buffer underflow" = {
               tmp <- readRaw(conn, read_size)
               if (length(tmp) == 0) {
                 break()
               } else if (!is.na(max_size)
                          && length(partial) + length(tmp) > max_size) {
                 status <<- "buffer overflow"
                 break()
               } else {
                 partial <<- c(partial, tmp)
               }
             },
             "end of input" = {
               buf <- readRaw(conn, read_size)
             },
             #default
             break()
             )
      if (length(partial) > 0) {
        msgs_bytes <- unpackMsgs(partial, n - nm, max_size = max_size, ...)
        collect(msgs_bytes[[1]])
        partial <<- msgs_bytes$remaining
        status <<- msgs_bytes$status
      } else {
        break()
      }
    }
    collect(action="read")
  }

  #msgConnection object is just the orig object with this
  #environment dangled off it
  attr(conn, "reader") <- environment();
  addClass(conn, "msgConnection")
}

# I get errors thrown (sometimes) when reading at the end of
# a nonblocking fifo.
readRaw <- function(conn, n) {
  tryCatch(readBin(conn, 'raw', n),
           error=function(e) {
             raw(0)
             })
}

catenator <- function(val=c()) {
  n <- length(val)
  function(x=c(), action="store") {
    switch(action,
           store =
             {
               lx <- length(x)
               if (lx > 0) {
                 l <- length(val)
                 if (lx + n > l) {
                   length(val) <<- max(l + lx, 2 * l);
                 }
                 val[(n+1):(n+lx)] <<- x
                 n <<- n + lx
               }
             },
           read =
             {
               length(val) <<- n
               val
             },
           length =
             {
               n
             })
  }
}

addClass <- function(x, classes) structure(x, class = c(classes, class(x)))

#' @rdname msgConnection
#' @export
partial <- function(conn) UseMethod("partial")

#' @rdname msgConnection
#' @export
partial.msgConnection <- function(conn) {
  attr(conn, "reader")$partial
}

#' @rdname msgConnection
#' @export
#' @param n The maximum number of messages to read. A value of NA
#'   means to parse all available messages up until the end of input.
#' @return `readMsgs(conn, n)` returns a list of some length between 0
#'   and n, containing the decoded messages.
readMsgs <- function(conn, n = NA, ...) {
  UseMethod("readMsgs")
}

#' @export
readMsgs.msgConnection <- function(conn, n = NA, ...) {
  attr(conn, "reader")$readMsgs(n, ...)
}

#' @rdname msgConnection
#' @return `status(conn)` returns the status of msgpack decoding on the
#'   connection. A value of `"ok"` indicates all requested messages
#'   were read, `"buffer underflow"` indicates a partial message is on
#'   the line, `"end of input"` means the last available message has
#'   been read.  Other values indicate errors encountered in decoding,
#'   which will effectively halt reading.
#' @export
status <- function(conn) UseMethod("status")

#' @rdname msgConnection
#' @export
status.msgConnection <- function(conn) {
  attr(conn, "reader")$status
}

#' @rdname msgConnection
#' @return `readMsg(conn)` returns exactly one message, or throws an error.
#' @export
readMsg <- function(conn, ...) {
  x <- readMsgs(conn, 1, ...)
  if (length(x) < 1) {
    stop(status(conn))
  }
  x[[1]]
}

#' @rdname msgConnection
#' @export
#' @param obj An R object.
writeMsg <- function(obj, conn, ...) {
  writeBin(packMsg(obj, ...), conn)
}

#' ...
#'
#' `writeMsgs(l, conn)` writes a list of
#'   messages to a connection. That is, `writeMsg(1:10, conn)` writes one
#'   message containing an array, while `writeMsgs(1:10, conn)` writes
#'   ten consecutive messages each containing one integer.
#' @rdname msgConnection
#' @export
#' @param objs A list of R objects.
writeMsgs <- function(objs, conn, ...) {
  writeBin(packMsgs(objs), conn)
}
