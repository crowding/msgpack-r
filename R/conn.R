#' Wrap a connection so that it can read msgpack objects.
#'
#' @param conn A connection object that supports readBytes.
#' @param max_size The maximum size of an incoming message, in bytes.
#' @param read_size The default read size.
#' @param ... Packing options (see pack[])
#' @return An object of class 'msgConnection'
#' @export
msgConnection <- function(conn, read_size=NA, max_size=NA, ...) {

  bufstate = "empty"
  buf = raw(0)

  readMsgs <- function(n, bytes=read_size) {
    new = readBin(conn, "byte", bytes)
    if (length(new) > 0) {
      buf <- c(buf, new)
      msgs_bytes <- unpack_msgs(buf)
      buf <<- msgs_bytes[[2]]
    }
  }

  partial <- function() buf

  attr(con, "reader") <- environment();
  class(con) <- c("msgConnection", class(con))
}

#' @export
readMsgs <- function(conn, ...) {
  UseMethod("readMsgs");
}

#' @export
readMsgs.default <- function(conn, ...) {
  stop("readMsg() requires a msgConnection()") 
}

#' @export
readMsgs.msgConnection <- function(conn, ...) {
  attr(conn, "reader")$read_msgs()
}

#' @export
writeMsg <- function(obj, conn, ...) {
  writeBin(packMsg(obj, ...), conn)
}

#' @export
writeMsg <- function(conn, ...) {
  useMethod("write_msg")
}

