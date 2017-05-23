context("unpack from streams")

#most of these tests to be implemented using a fifo connection.

`%is%` <- expect_equal

msgs = as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f,
                0xa3, 0x61, 0x6e, 0x64,
                0xa7, 0x67, 0x6f, 0x6f))

two_ends <- function(f, read_size = 2^16, max_buf = NA) {
  fnam <- tempfile()
  on.exit(unlink(fnam, force=TRUE))
  system(paste("mkfifo", fnam))
  end_B <- msgConnection(fifo(fnam, open="rb", blocking=FALSE))
  on.exit(close(end_B), add=TRUE)
  end_A <- msgConnection(fifo(fnam, open="wb", blocking=FALSE))
  on.exit(close(end_A), add=TRUE)
  f(end_A, end_B)
}

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
  }) %is% as.list(1:10)
})

test_that("read non-blocking with inomplete message", {
  two_ends(function(endA, endB) {
    writeMsg(1, endA)
    partial <- packMsgs(list("here is a partial message", 2))
    writeBin(partial[1:10], endA)
    flush(endA)
    readMsgs(endB) %is% list(1)
    status(endB) %is% "buffer underflow"
    partial(endB) %is% partial[1:10]
    writeBin(partial[11:length(partial)], endA)
    flush(endA)
    Sys.sleep(0.1)
    readMsgs(endB) %is% list("here is a partial message", 2)
  })
})

# I'd like to have some tests with reading/writing to a separate
# process. I fried with parallel/mcfork, makeForkCluster, but forking
# seems to cause weirdness with nonblocking. Subprocess package seems
# to not have tha prob. but the subprocess doesn't have my functions
# loaded.
