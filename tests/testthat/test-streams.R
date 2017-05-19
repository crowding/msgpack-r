context("unpack from streams")

#most of these tests to be implemented using a raw in-memory connection

`%is%` <- expect_equal

msgs = as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f,
                0xa3, 0x61, 0x6e, 0x64,
                0xa7, 0x67, 0x6f, 0x6f))

test_that("consume N messages and return remaining data", {
  expect_equal(unpackMsgs(as.raw(1:10)),
               list(messages = as.list(1:10),
                    remaining = raw(0),
                    status = "end of input"))

  expect_equal(unpackMsgs(msgs),
               list(messages = list("hello", "and"),
                    remaining = as.raw(c(0xa7, 0x67, 0x6f, 0x6f)),
                    status = "buffer underflow"))

  expect_equal(unpackMsgs(as.raw(c(0xa7, 0x6f, 0x6f))),
               list(messages = list(),
                    remaining = as.raw(c(0xa7, 0x6f, 0x6f)),
                    status = "buffer underflow"))

  expect_equal(unpackMsgs(as.raw(c(1:10)), 3),
               list(messages = as.list(1:3),
                    remaining = as.raw(4:10),
                    status = "ok"))

  expect_equal(unpackMsgs(as.raw(c(1:10))),
               list(messages = as.list(1:10),
                    remaining = raw(0),
                    status = "end of input"))
})

test_that("Errors raised in parsing are caught", {
  unpackMsgs(as.raw(c(0xc1)))$status %is% "malformed input"
  stop("not written")
})

test_that("consume from a connection (blocking)", {
  conn = rawConnection(msgs, open="r")
  readMsg(conn) %is% "hello"
  readMsg(conn) %is% "and"
  expect_error(readMsg(conn))

  conn = rawConnection(msgs, open="r")
  readMsgs(conn) %is% c("hello", "and")
})

test_that("read from a connection (non-blocking)", {
  conn = msg_connection(rawConnection(msgs, open="r"))
})

test_that("write to a connection", {
  conn = rawConnection(msgs, open = "w")
  write_msgs(conn, c("hello", "there"))
  write_msg(conn, c("you", "are"))

  expect_equal(rawValue(conn),
               packb(list("hello", "there", c("you", "are"))))
});

test_that("consume non-blocking from a (wrapped) connection", {
  conn = rawConnection(msgs, open = "r")
  stop("not implemented")
})

test_that("consume non-blocking from a (wrapped) connection", {
  stop("not implemented")
})

test_that("consume partial message non-blocking, then finish", {
  stop("not implemented")
})

test_that("readMsg/s blocking respects connection blocking", {

})

test_that("respect a max buffer/object size", {
  stop("not implemented")
})

