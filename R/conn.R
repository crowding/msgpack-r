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
#' If you are reading data from a not completely trusted source you
#' will probably want to give options `max_size` and `max_depth` (see
#' [unpackOpts]). Without it, some deeply nested or cleverly designed
#' messages can cause a stack overflow or out-of-memory error.  With
#' these options set, you will get an R exception instead.
#'
#' @examples
#' out <- rawConnection(raw(0), open="wb")
#' apply(quakes, 1, function(x) writeMsg(x, out))
#' length(rawConnectionValue(out))
#' inn <- msgConnection(rawConnection(rawConnectionValue(out), open="rb"))
#' readMsg(inn)
#' readMsgs(inn, 3)
#' @export
msgConnection <- function(con, read_size=2^16, max_size=NA, ...) {
  partial <- raw(0)
  status <- "ok"
  bread <- 0
  bwrite <- 0

  reader <- function(x) {
    readRaw(con, read_size)
  }

  readMsgs <- function(n=NA, read_size = parent.env(environment())$read_size) {
    if (is.na(n)) n <- .Machine$integer.max
    msgs_bytes <- unpackMsgs(partial, n, max_size = max_size, reader = reader)
    partial <<- msgs_bytes$remaining
    status <<- msgs_bytes$status
    bread <<- bread + msgs_bytes$bread
    msgs_bytes$msgs
  }

  doClose <- function(...) {
    close(con, ...)
  }

  #msgConnection object is just the orig object with this
  #environment dangled off it.
  structure(addClass(con, "msgConnection"), reader = environment())
}

summary.msgConnection <- function(con, ...) {
  s <- NextMethod("summary")
  c(s, list(status = status(con)))
}

# TODO: read raw method, to interleave with readMsgs.

#' @rdname msgConnection
#' @export
close.msgConnection <- function(con, ...) {
    attr(con, "reader")$doClose(...)
}

catenator <- function(val=c()) {
  n <- length(val)
  function(x=c(), action="store") {
    switch(action,
           store = {
             lx <- length(x)
             if (lx > 0) {
               l <- length(val)
               if (lx + n > l) {
                 length(val) <<- max(l + lx, 2 * l);
               }
               val[ (n + 1):(n + lx) ] <<- x
               n <<- n + lx
             }
             x
           },
           read = {
             length(val) <<- n
             val
           },
           length = {
             n
           },
           reset = {
             val <<- x
             n <<- length(x)
           })
  }
}

lister <- function(val = list()) {
  n <- length(val)
  function(x, action="store") {
    switch(action,
           store = {
             if (n > length(val))
               length(val) <<- max(1, 2 * length(val))
             n <<- n + 1
             val[[n]] <<- x
           },
           read = {
             length(val) <<- n
             val
           },
           length = {
             n
           },
           clear = {
             n <<- 0
             length(val) <<- 0
           }
           )
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
#'     means to parse all available messages until end of input.
#' @return A list of up to `n` decoded messages.
readMsgs <- function(conn, n = NA, ...) {
  UseMethod("readMsgs")
}

#' @export
readMsgs.msgConnection <- function(conn, n = NA, ...) {
  attr(conn, "reader")$readMsgs(n, ...)
}

#' ...
#'
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


#' ...
#'
#' `seek(con)` returns the number of bytes that have been successfully
#' read or written.
#' @rdname msgConnection
#' @export
seek.msgConnection <- function(con, where=NA, origin = "start", rw = summary(con)$mode) {
  if (!is.na(where)) {
    stop("Can't seek a msgConnection")
  }
  rw <- pmatch(rw, c("read", "write"), 0L)
  switch(rw,
         attr(con, "reader")$bread,
         attr(con, "reader")$bwrite,
         )
}

#' ...
#'
#' @rdname msgConnection
#' @return `readMsg(conn)` returns exactly one message, or throws an error.
#' @export
readMsg <- function(con, ...) {
  UseMethod("readMsg", con)
}

readMsg.msgConnection <- function(conn, ...) {
  x <- readMsgs(conn, 1, ...)
  if (length(x) < 1) {
    stop(status(conn))
  }
  x[[1]]
}

#' ...
#'
#' @rdname msgConnection
#' @export
writeMsg <- function(obj, con, ...) {
  UseMethod("writeMsg", con)
}

writeMsg.connection <- function(obj, conn, ...) {
  writeMsgs(list(obj), conn, ...)
}

writeMsg.msgConnection <- function(obj, conn, ...) {
  writeMsgs(list(obj), conn, ...)
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
writeMsgs <- function(objs, con, ...) {
  UseMethod("writeMsgs", con)
}

writeMsgs.connection <- function(objs, conn, ...) {
  writeRaw(packMsgs(objs, ...), conn)
}

writeMsgs.msgConnection <- function(objs, conn, ...) {
  buf <- packMsgs(objs, ...)
  result <- writeRaw(buf, conn)
  attr(conn, "reader")$bwrite <- attr(conn, "reader")$bwrite + length(buf)
  invisible(result)
}


## To support test harness, we use "readRaw" and "writeRaw"
## internally, instead of "readBin" which is not an S3 method
readRaw <- function(con, n, ...) {
  UseMethod("readRaw")
}


writeRaw <- function(object, con, ...) {
  UseMethod("writeRaw", con)
}


readRaw.connection <- function(conn, n) {
  # I get errors thrown (sometimes) when reading at the end of
  # a nonblocking fifo.
  tryCatch(readBin(conn, 'raw', n),
           error=function(e) {
             #warning("Ignoring ", e)
             raw(0)
           })
}


writeRaw.connection <- function(object, con, ...) {
    writeBin(object, con, ...)
}


## An inefficient double ended byte buffer for test harness purposes
rawBuffer <- function(object = raw(0)) {
    open <- "r"
    bytes <- length(object)
    buf <- rawConnection(object, open = "r")
    object <- NULL

    write <- function(object) {
      switch(open,
             "w" = {
               writeBin(object, buf)
             },
             "r" = {
               data <- readBin(buf, 'raw', bytes - seek(buf))
               close(buf)
               buf <<- rawConnection(data,
                                     open = "w")
               open <<- "w"
               writeBin(data, buf)
               write(object)
             }
      )
    }

    read <- function(n) {
      switch(open,
             "r" = {
               readBin(buf, 'raw', n)
             },
             "w" = {
               ##convert a write buffer into a read buffer
               val <<- rawConnectionValue(buf)
               close(buf)
               buf <<- rawConnection(val, open = "r")
               bytes <<- length(val)
               open <<- "r"
               read(n)
             }
      )
    }

    doClose <- function(n) {
      close(buf)
      buf <- NULL
    }

    structure(list(environment()), class = "rawBuffer")
}

writeRaw.rawBuffer <- function(object, con) {
    con[[1]]$write(object)
}

readRaw.rawBuffer <- function(con, n) {
    con[[1]]$read(n)
}

close.rawBuffer <- function(con) {
    con[[1]]$doClose()
}
