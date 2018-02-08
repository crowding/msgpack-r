#' Read and write msgpack formatted messages over R connections.
#'
#' A `msgConnection` object decodes msgpack messages from an
#' underlying R raw connection.
#'
#' @param con A [connection] object open in binary mode.
#' @param max_size The largest partial message to store, in
#'   bytes. `NA` means do not enforce a limit.
#' @param read_size How many bytes to read at a time.
#' @param ... Unpacking options (see [unpackMsg]).
#' @return `msgConnection()` returns an object of class
#'   `msgConnection`.
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

  reader <- function(desired) {
    # ignore "desired" and just read non-blockingly.
    readRaw(con, read_size)
  }

  readMsgs <- function(n=NA, read_size = parent.env(environment())$read_size) {
    if (is.na(n)) n <- .Machine$integer.max
    msgs_bytes <- unpackMsgs(partial, n, max_size = max_size, reader = reader)
    partial <<- msgs_bytes$remaining
    status <<- msgs_bytes$status
    bread <<- bread + msgs_bytes$bytes_read
    msgs_bytes$msgs
  }

  doClose <- function(...) {
    close(con, ...)
  }

  #msgConnection object is just the orig object with this
  #environment dangled off it.
  structure(addClass(con, "msgConnection"), reader = environment())
}

#' @export
summary.msgConnection <- function(object, ...) {
  s <- NextMethod("summary")
  c(s, list(status = status(object)))
}


#' @rdname msgConnection
#' @export
close.msgConnection <- function(con, ...) {
    attr(con, "reader")$doClose(...)
}

catenator <- function(val=c()) {
  # An in-memory FIFO type object.
  #tracemem(val)
  start <- 0
  end <- length(val)
  function(x, action="store", ..., opts) {

    switch(action,
           store = {
             lx <- length(x)
             l <- length(val)
             if (lx > 0) {
               #check for overflow
               if (end + lx > l && start > 0) {
                 # rotate back to start
                 if (start > 0 && end != start) {
                   val[1:(end-start)] <- val[(start+1):end]
                 }
                 end <<- end - start
                 start <<- 0
               }
               if (end + lx > l) {
                 # double array length
                 length(val) <<- max(end + lx, 2 * l);
               }

               #inject new values
               val[ (end + 1):(end + lx) ] <<- x
               end <<- end + lx
             }
             dbg("lx", lx, "start", start, "end", end, "\n")
             x
           },

           read = {
             if (end > start) {
               val[(start+1):end]
             } else val[c()]
           },

           buf = {
             val
           },

           start = start,

           length = end - start,

           end = end,

           contents = {
             list(val, start, end)
           },

           reset = {
             val <<- x
             start <<- 0
             end <<- length(x)
           },

           drop = {
             if (x <= end - start && x >= 0) {
               start <<- start + x
             } else {
               stop("have ", end - start, ", can't drop ",  x)
             }
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

#' @return `partial(con)` returns any data that has been read ahead of
#'   the last decoded message.
#' @rdname msgConnection
#' @export
partial <- function(con) UseMethod("partial")

#' @rdname msgConnection
#' @export
partial.msgConnection <- function(con) {
  attr(con, "reader")$partial
}

#' @rdname msgConnection
#' @export
#' @param n The maximum number of messages to read. A value of NA
#'     means to parse all available messages until end of input.
#' @return `readMsgs(con, n)` returns a list of up to `n` decoded messages.
readMsgs <- function(con, n = NA, ...) {
  UseMethod("readMsgs")
}

#' @export
readMsgs.msgConnection <- function(con, n = NA, ...) {
  attr(con, "reader")$readMsgs(n, ...)
}

#' @rdname msgConnection
#' @return `status(con)` returns the status of msgpack decoding on the
#'   connection. A value of `"ok"` indicates all requested messages
#'   were read, `"buffer underflow"` for a non-blocking connection
#'   indicates that only part of a message has been received, and
#'   `"end of input"` means the last available message has been read.
#'   Other values indicate errors encountered in decoding, which will
#'   effectively halt reading.
#' @export
status <- function(con) UseMethod("status")

#' @rdname msgConnection
#' @export
status.msgConnection <- function(con) {
  attr(con, "reader")$status
}


#' @rdname msgConnection
#' @return `seek(con)` returns the number of bytes that have been
#'   successfully read or written, depending on the mode of the
#'   connection. (Repositioning is not supported.)
#' @param rw See `seek()`.
#' @export
seek.msgConnection <- function(con, rw = summary(con)$mode, ...) {
  rw <- pmatch(rw, c("read", "write"), 0L)
  switch(rw,
         attr(con, "reader")$bread,
         attr(con, "reader")$bwrite,
         )
}

#' `readMsg(con)` reads exactly one message from a
#'   msgConnection, or throws an error.
#'
#' @rdname msgConnection
#' @return `readMsg(con)` returns one decoded message.
#' @export
readMsg <- function(con, ...) {
  UseMethod("readMsg", con)
}

#' @export
readMsg.msgConnection <- function(con, ...) {
  x <- readMsgs(con, 1, ...)
  if (length(x) < 1) {
    stop(status(con))
  }
  x[[1]]
}

#' `writeMsg(x, con)` writes a single message to a msgConnection.
#'
#' @rdname msgConnection
#' @param obj An R object.
#' @export
writeMsg <- function(obj, con, ...) {
  UseMethod("writeMsg", con)
}

#' @export
writeMsg.connection <- function(obj, con, ...) {
  writeMsgs(list(obj), con, ...)
}

#' @export
writeMsg.msgConnection <- function(obj, con, ...) {
  writeMsgs(list(obj), con, ...)
}

#' `writeMsgs(l, conn)` writes a list of
#'   messages to a connection. That is, `writeMsg(1:10, conn)` writes one
#'   message containing an array, while `writeMsgs(1:10, conn)` writes
#'   ten consecutive messages each containing one integer.
#'
#' `writeMsg` will work with anyR connection inraw mode, but reading
#' requires a msgConnection object.
#'
#' Because msgpack messages have unpredictable length, the decoder
#' reads ahead in chunks, then finds the boundaries between messages.
#' Therefore when reading over a socket or a fifo it is best to use a
#' nonblocking connection, and it will not work to mix readMsg and
#' readBin on the same connection.
#'
#' If you are reading data from a not completely trusted source you
#' should specify options `max_size` and `max_depth` (see
#' [unpackOpts]). Without it, some deeply nested or cleverly designed
#' messages can cause a stack overflow or out-of-memory error.  With
#' these options set, you will get an R exception instead.

#' @rdname msgConnection
#' @param objs A list of R objects.
#' @export
writeMsgs <- function(objs, con, ...) {
  UseMethod("writeMsgs", con)
}

#' @export
writeMsgs.connection <- function(objs, con, ...) {
  writeRaw(packMsgs(objs, ...), con)
}

#' @export
writeMsgs.msgConnection <- function(objs, con, ...) {
  buf <- packMsgs(objs, ...)
  result <- writeRaw(buf, con)
  attr(con, "reader")$bwrite <- attr(con, "reader")$bwrite + length(buf)
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

readRaw.connection <- function(con, n) {
  # I get errors thrown (sometimes) when reading at the end of
  # a nonblocking fifo.
  tryCatch({
    readBin(con, 'raw', n)
  },
  error = function(e) {
    # warning("Ignoring ", e)
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
               val <- rawConnectionValue(buf)
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
