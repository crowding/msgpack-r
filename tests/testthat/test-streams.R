context("unpack from streams")

#most of these tests to be implemented using a fifo connection.

`%is%` <- expect_equal

msgs <- as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f,
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
                    status = "end of input",
                    bytes_read = 10))

  expect_equal(unpackMsgs(msgs),
               list(msgs = list("hello", "and"),
                    remaining = as.raw(c(0xa7, 0x67, 0x6f, 0x6f)),
                    status = "buffer underflow",
                    bytes_read = 10))

  expect_equal(unpackMsgs(as.raw(c(0xa7, 0x6f, 0x6f))),
               list(msgs = list(),
                    remaining = as.raw(c(0xa7, 0x6f, 0x6f)),
                    status = "buffer underflow",
                    bytes_read = 0))

  expect_equal(unpackMsgs(as.raw(c(1:10)), 3),
               list(msgs = as.list(1:3),
                    remaining = as.raw(4:10),
                    status = "ok",
                    bytes_read = 3))
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
  close(conn)
})

test_that("write to and read from connections", {
  con <- msgConnection(rawBuffer(raw(0)))
  writeMsgs(1:10, con)
  readMsgs(con) %is% as.list(1:10)
  close(con)
  two_ends(function(A, B) {
    writeMsgs(1:10, A)
    flush(A)
    readMsgs(B)
  }) %is% as.list(1:10)
})

test_that("read non-blocking with complete message", {
    test <- packMsgs(list("hello", "and"))
    conn <- msgConnection(
        rawConnection(packMsgs(list("hello", "and", "world")), open="r"),
        read_size = length(test))
    readMsgs(conn) %is% list("hello", "and", "world")
    close(conn)
})

test_that("read non-blocking with incomplete message", {
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

test_that("underflow at incomplete message (1-process)", {
    buf <- rawBuffer()
    partial <- packMsgs(list(1, "here is a partial message", 2))
    writeRaw(partial[1:10], buf)
    con <- msgConnection(buf)
    readMsgs(con) %is% list(1)
    writeRaw(partial[11:length(partial)], buf)
    readMsgs(con) %is% list("here is a partial message", 2)
    readMsgs(con) %is% list()
    close(con)
})

test_that("read non-blocking with array breaking over chunks", {
    ##this should at least trigger the underflow handler, no?
    partial <- packMsgs(list("hello", 1:2))
    full <- packMsgs(list("hello", 1:10))
    conn <- rawConnection(full, open="r")
    conn <- msgConnection(conn, read_size = length(partial))
    readMsgs(conn) %is% list("hello", 1:10)
    close(conn)
})

test_that("rawBuffer", {
    x <- rawBuffer(as.raw(1:5))
    tryCatch({
        readRaw(x, 3) %is% as.raw(1:3)
        writeRaw(as.raw(6:10), x)
        readRaw(x, 5) %is% as.raw(4:8)
        readRaw(x, 10) %is% as.raw(9:10)
        readRaw(x, 10) %is% raw(0)
        writeRaw(as.raw(11:25), x)
        readRaw(x, 100) %is% as.raw(11:25)
        writeRaw(as.raw(26:27), x)
        readRaw(x, 2) %is% as.raw(26:27)
        readRaw(x, 0) %is% raw(0)
        readRaw(x, 10) %is% raw(0)
    }, finally = {
        close(x)
    })
})

test_that("read non-blocking and underflow handling when variously interrupted",
{
    orig <- list("hello",  c("hello", "world"), list("hello", "world", c(1, 2, 3)))
    packed <- packMsgs(orig)

    cut <- 28
    for (cut in 1:(length(packed) - 1)) {
        firstChunk <- packed[1:cut]
        secondChunk <- packed[  (cut+1) : (length(packed)) ]
        con <- msgConnection(rawBuffer(firstChunk))
        expect_error(read1 <- readMsgs(con), NA, info = paste0("at cut ", cut))
        writeRaw(secondChunk, con)
        expect_error(read2 <- readMsgs(con), NA, info = paste0("at cut ", cut))
        expect_equal(c(read1, read2), orig, info = paste0("at cut ", cut))
        close(con)
    }
})

test_that("assembling an array > read_size", {
    mess <- (1:25) + rep(0, 1000)
    c <- msgConnection(rawBuffer(raw(0)), read_size=32)
    writeMsg(mess, c)
    readMsgs(c) %is% list(mess)
    close(c)
})

test_that("resume from interrupt when message >> read_size",
{
    mess <- (1:25) + rep(0, 1000)
    con <- msgConnection(rawBuffer(raw(0)), read_size=32)
    bin <- packMsg(mess)
    writeRaw(bin[1:100], con)
    readMsgs(con) %is% list()
    writeRaw(bin[101:200], con)
    readMsgs(con) %is% list()
    writeRaw(bin[201:length(bin)], con)
    readMsgs(con) %is% list(mess)
    close(con)
})

test_that("Assembling a string >> read size", {
    ## strings are individual messages that stretch over potentially many reads.
    mess <- paste0(letters[sample(26, 1000, replace=TRUE)], collapse="")
    con <- msgConnection(rawBuffer(raw(0)), read_size=32)
    writeMsgs(list(mess), con)
    readMsgs(con) %is% list(mess)
    expect_equal(seek(con, rw="r"), 1003)
    close(con)
})

test_that("seek method", {
    con = msgConnection(rawConnection(msgs, open="r"))
    readMsgs(con)
    seek(con) %is% 10
    close(con)

    con = msgConnection(rawConnection(msgs, open="w"))
    writeMsg("hello", con)
    seek(con) %is% 6
    close(con)

    con = msgConnection(rawBuffer())
    writeMsg("hello", con)
    expect_error(seek(con))
    seek(con, rw = "w") %is% 6
    seek(con, rw = "r") %is% 0
    readMsg(con)
    seek(con, rw = "r") %is% 6
    close(con)
})

`%@%` <- function(x, name) {
  attr(x, as.character(substitute(name)))
}

test_that("large blob under gctorture", {

  data <- sample(as.raw(0:255), 0x1000000, TRUE)
  con <- msgConnection(rawConnection(raw(0), open="wb"))
  packet <- packMsg(data)
  writeMsg(data, con)
  bytes <- rawConnectionValue(con)
  close(con)
  con2 <- msgConnection(rawConnection(bytes, open="rb"))
  as.read <- NULL
  local({
    gctorture(TRUE)
    on.exit(gctorture(FALSE))
    # getting Error: RAW() can only be applied to a 'raw', not a 'NULL'
    # I think perhaps because the buffer gets collected.
    # I should hold an external pointer to the buffer, then?
    # gctorture triggers an early error "2" so yeah.
    # any way to debug GC, though?
    as.read <<- readMsg(con2)
  })
  close(con2)
  expect_identical(as.read, data)

})



## When I single stepped through, what I got was this:

## handle_unpack_underflow:  Swapping buffers, ( 0x80005228[0:65536][5] -> 0x263fe028[0:16777221][5] ) @decode.c:163
## make_sexp_from_context:  Making sexp from a binary @decode.c:269
## _unpack_msg_partial: After:  buf = 0x263fe028[0:16777221][16777221], status = 'ok' @decode.c:218



# I'd like to have some tests with reading/writing to a separate
# process. I tried with parallel/mcfork, makeForkCluster, but forking
# seems to cause weirdness with nonblocking. (however this was before
# I sorted out partial reads.) Subprocess package seems to not have
# that prob. but the subprocess that is created doesn't have my
# functions loaded.

print.raw <- function(x, max.print = getOption("max.print"), ...) {
  # Display raw vectors as hex dumps. Debugging use, not exported.
  con <- pipe("hexdump -C", "wb")
  if (length(x) > max.print) {
    writeBin(x[1:max.print], con)
    close(con)
    cat(paste0("[ reached getOption(\"max.print\") -- omitted ",
               length(x) - max.print,
               " entries ] \n"))
  } else {
    writeBin(x, con[1:max.print])
    close(con)
  }
}
