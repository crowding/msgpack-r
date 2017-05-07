context("msgpackr static API")

`%is%` <- expect_equal


stringToRaw <- function(ch) {
  vapply(strsplit("hello", "")[[1]], charToRaw, raw(1))
}


pack_rt <- function(start) {
  "Pack and unpack and check that your data made the round trip (as defined by
   expect_equivalent)"
  bin <- packb(start)
  end <- unpackb(bin)
  expect_equivalent(start, end)
  return(bin)
}

test_that("pack singletons", {
  #null
  packb(NULL) %is% as.raw(0xc0)
  #NA is encoded as null?
  packb(NA) %is% as.raw(0xc0)

  #logical
  packb(FALSE) %is% as.raw(0xc2)
  packb(TRUE) %is% as.raw(0xc3)

  #int
  packb(12L) %is% as.raw(0x0c)

  # cwpack will use 32 bit float if precision is preserved.
  # float32 representation of 5:
  # 5 = 1.25 * 2^2
  #   = +1 * 2 ^  (129 - 127) * (1 + 0.25)
  #     ^sign      ^exponent     ^mantissa
  #     0          10000001         010000000000000000000000
  # 01000000 10100000 00000000 00000000
  # 40 A0 00 00
  expect_equal(packb(5),
               as.raw(c(0xCA, 0x40, 0xA0, 0x00, 0x00)))

  # float64:
  x <- 1.7976931348623157e308 # .Machine$double.xmax
  # 0 11111111110 1111111111111111111111111111111111111111111111111111
  packb(x) %is% as.raw(c(0xCB, 0x7F, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF))

  # character
  expect_equal(packb("hello"),
               as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f)))
  expect_equal(packb(NA_character_), as.raw(0xc0))
})

test_that("Pack vectors", {
  stop("not written")
})

test_that("Unpack singletons", {
  stop("not written")
  # just roundtrip the above...
})

test_that("pack zero length vectors", {
  stop("not written")
})

test_that("Vectors with NAs", {
  stop("not written")
})

test_that("NA and NaN are distinct doubles,", {
  stop("not written")
})

test_that("compatibility mode", {
  stop("not written")
})

test_that("use arrays for singletons", {
  stop("not written")
})

test_that("overflow handler works", {
  stop("not written")
})

test_that("Homepage example", {
  x <- packb(list(compact=TRUE, schema=0))
  cmp <- c(as.raw(c(0x82, 0xa7)),
           charToRaw("compact"),
           as.raw(c(0xc3, 0xa6)),
           charToRaw("schema"),
           as.raw(00))
  x %is% cmp
})
