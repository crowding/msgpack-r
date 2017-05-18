context("unpack from streams")

#most of these tests to be implemented using a raw in-memory connection

msgs = as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f,
                0xa3, 0x61, 0x6e, 0x64,
                0xa7, 0x67, 0x6f, 0x6f))

test_that("consume N messages and return remaining data", {
  expect_equal(unpack_msgs(x),
               list(list("hello", "and"),
                    as.raw(c(0xa7, 0x67, 0x6f, 0x6f))))

  expect_equal(unpack_msgs(as.raw(c(0xa7, 0x6f, 0x6f))),
               list(list(), c(0xa7, 0x6f, 0x6f)))

  expect_equal(unpack_msgs(as.raw(c(1:10)), 3),
               list(1:3), as.raw(4:10))

  expect_equal(unpack_msgs(as.raw(c(1:10))),
               list(1:10), raw(0))

})

test_that("consume from a connection (blocking)", {
  conn = rawConnection(msgs, open="r")
  read_msg(conn) %is% "hello"
  read_msg(conn) %is% "and"
  expect_error(read_msg(conn))

  conn = rawConnection(msgs, open="r")
  read_msgs(conn) %is% c("hello", "and")
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

test_that("respect a max buffer/object size", {
  stop("not implemented")
})

