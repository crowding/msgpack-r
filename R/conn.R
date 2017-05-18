#' Wrap a connection so that it can read or write msgpack objects.
#'
#' @param conn A connection object that supports readBytes.
#' @param bufsize The size of the buffer to use to pack objects before sending. Optimise this for speed?
#' @param ... Packing options (see pack[])
#' @return A connection object with "class" and invisibly, the number of bytes written. NULL, invisibly.
#' @author Peter
msg_conn <- function(conn, ...) {
  #keep backlog of partially-parsed bufs....
  attr(con, "reader") <- reader(conn);
  class(con) <- c("msg_conn", class(con))
}


reader <- function(conn, bufsize=NA, ...) {
  buf <- raw(0)

  unpack <- function(buf) unpackb(buf, ...)
  read_msgs <- function(n, readsize=NA) {
    new = readBin(conn, "byte", readsize)
    if (length(new) > 0) {
      buf <- c(buf, new)
      msgs_bytes <- unpack_msgs(buf)
      buf <<- msgs_bytes[[2]]
    }
  }

  read_msg <- function() {
    new <- readBin(conn, "byte", readsize)
    if (length(new) > 0) {
      buf <<- c(buf, new)
      msgs_bytes <- unpack_msgs(buf, ...)
      new <<- msgs_bytes()
    }
  }

  partial <- function() buf
}

read_msgs <- function(conn, ...) {
  UseMethod("read_msgs");
}

read_msgs.connection <- function(conn, ...) {
  stop("read_msg needs do be called on a msg_conn")
}

read_msgs.msg_connection <- function(conn, ...) {
  attr(conn, "reader")$read_msgs()
}

write_msg <- function(obj, conn, ...) {
  writeBin(packb(obj, ...), conn)
}



write_msg <- function(conn, ...) {
  useMethod("write_msg")
}

read.msg_stream <- function(conn, ...) {
  stop("not written");
}
