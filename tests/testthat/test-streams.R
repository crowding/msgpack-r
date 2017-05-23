context("unpack from streams")

#most of these tests to be implemented using a raw in-memory connection

`%is%` <- expect_equal

msgs = as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f,
                0xa3, 0x61, 0x6e, 0x64,
                0xa7, 0x67, 0x6f, 0x6f))

test_that("consume N messages and return remaining data", {
  expect_equal(unpackMsgs(as.raw(1:10)),
               list(msgs = as.list(1:10),
                    remaining = raw(0),
                    status = "end of input"))

  expect_equal(unpackMsgs(msgs),
               list(msgs = list("hello", "and"),
                    remaining = as.raw(c(0xa7, 0x67, 0x6f, 0x6f)),
                    status = "buffer underflow"))

  expect_equal(unpackMsgs(as.raw(c(0xa7, 0x6f, 0x6f))),
               list(msgs = list(),
                    remaining = as.raw(c(0xa7, 0x6f, 0x6f)),
                    status = "buffer underflow"))

  expect_equal(unpackMsgs(as.raw(c(1:10)), 3),
               list(msgs = as.list(1:3),
                    remaining = as.raw(4:10),
                    status = "ok"))

  expect_equal(unpackMsgs(as.raw(c(1:10))),
               list(msgs = as.list(1:10),
                    remaining = raw(0),
                    status = "end of input"))
})

test_that("packMsgs round trip", {
  unpackMsgs(packMsgs(c(list(1:10), 1:10)))
})

test_that("Errors raised in parsing are caught", {
  unpackMsgs(as.raw(c(0xc1)))$status %is% "malformed input"
})

two_ends <- function(f, read_size = 2^16, max_buf = NA) {
  fnam <- tempfile()
  on.exit(unlink(fnam, force=TRUE))
  end_A <- msgConnection(file(fnam, open="wb", raw = TRUE))
  on.exit(close(end_A), add=TRUE)
  end_B <- msgConnection(file(fnam, open="rb", raw = TRUE))

  on.exit(close(end_B), add=TRUE)
  f(end_A, end_B)
}

test_that("read from a connection (blocking)", {
  conn <- msgConnection(rawConnection(msgs, open="r"))
  readMsg(conn) %is% "hello"
  readMsg(conn) %is% "and"
  expect_error(readMsg(conn))
  close(conn)

  conn <- msgConnection(rawConnection(msgs, open="r"))
  readMsgs(conn) %is% list("hello", "and")
  status(conn) %is% "buffer underflow"
  close(conn)
})

test_that("write to a connection", {
  conn <- rawConnection(raw(), open="w")
  writeMsg(1:10, conn)
  writeMsgs(1:10, conn)
  expect_equal(
    unpackMsgs(rawConnectionValue(conn))$msgs,
    c(list(1:10), as.list(1:10)))
})

test_that("write to and read from connections", {
  two_ends(function(A, B) {
    writeMsgs(1:10, A)
    flush(A)
    readMsgs(B)
  }) %is% list(1:10)
})

test_that("read from a connection (non-blocking)", {
  two_ends(function(endA, endB) {
    writeMsg(1, endA)
    partial <- packMsg("here is a partial message")
    writeBin(partial[1:10], endA)
    flush(endA)
    readMsgs(endB) %is% list(1)
    partial(endb) %is% partial[1:10]
    writeBin(partial[11:length(partial)], endA)
    readMsgs(endB) %is% list("here is a partial message", 2)
  })
})

library(parallel)
# util function: fork and give each process an open socket connection
fork <- function(parent, child, parent_blocks = FALSE, child_blocks=FALSE) {
  port <- 16969
  other <- parallel:::mcfork()
  if ("childProcess" %in% class(other)) {
    cat("parent opening\n")
    conn <- msgConnection(socketConnection(server=TRUE, port=port,
                                           open="a+b", blocking = parent_blocks))
    cat("parent opened\n")
    on.exit({
      cat("parent closing\n")
      close(conn)
      cat("parent closed\n")
    })
    #we are the parent
    parent(conn)
  } else if ("masterProcess" %in% class(other)) {
    Sys.sleep(0.5)
    cat("child opening\n")
    conn <- msgConnection(socketConnection(server=FALSE, port = port,
                                           open="a+b", blocking = child_blocks))
    cat("child opened\n")
    tryCatch({
      child(conn)
      cat("child closing1\n")
      close(conn)
      cat("child closed1\n")
      parallel:::mcexit(0)
    }, error = function(e) {
      cat("child error\n")
      print(e)
      cat("child closing2\n")
      close(conn)
      cat("child closed2\n")
      parallel:::mcexit(1)}
    )
  } else {
    stop(other)
  }
}

test_that("Fork works?", {

  fork(child = function(parent) {
    writeBin(as.raw(1:100), parent)
  },
  parent = function(child) {
    readBin(child, 'raw', 1000)
  })

 })

test_that("read/write over socket", {
  fork(child = function(parent) {
    writeMsg(1:10, parent)
  },
  parent = function(child) {
    print(readMsgs(child))
  })
})

fork(
child = function(parent) {
  for (i in 1:10) {
    cat("writing\n")
    writeBin(as.raw(i), parent)
    flush(parent)
    Sys.sleep(0.1)
  }
},
parent = function(child) {
  while(TRUE) {
    x <- readBin(child, "raw", 100)
    if (length(x) >= 1) {
      print(length(x))
    }
  }
})

# seems like we might have to include a select statement....

child = function(parent) {
  for (i in 1:10) {
    cat("writing\n")
    write
  }
}

test_that("read nonblocking", {

#this is blocking on read, why??

  fork(child = function(parent) {
    for (i in 1:10) {
      cat("writing\n")
      writeMsg(i, parent)
      Sys.sleep(1000);
    }
  },
  parent = function(child) {
    while(status(child) != "end of input") {
      debug(readMsgs)
      x <- readMsgs(child)
      cat("read ", length(x), " things\n")
      print(x)
    }
  })

})


test_that("read/write non-blocking", {

  fork(parent_blocks=FALSE,
       child = function(parent) {
    for (i in 1:10) {
      Sys.sleep(0.1);
      writeMsg(i, conn)
    }
  },
  parent = function(child) {
    while(isIncomplete(child)) {
      print(readMsgs(child))
    }
  })
})

test_that("respect a max buffer/object size and max read size", {

  pow <- function(n) {
    if (n == 0) {
      "pow"
    } else {
      t <- pow(n-1)
      list(t, t)
    }
  }

  stop("not written")

  two_ends(
    read_size = 128,
    max_size = 512,
    function(A, B) {
      writeMsgs(lapply(1:10, pow), A)
      flush(A)
      result <- readMsgs(B)
      readMsgs(B) %is% lapply(1:6, pow)
    }
  )
})

