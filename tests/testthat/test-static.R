context("msgpackr static API")

`%is%` <- expect_equal


stringToRaw <- function(ch) {
  vapply(strsplit("hello", "")[[1]], charToRaw, raw(1))
}


roundtrip <- function(start) {
  bin <- packb(start)
  end <- unpackb(bin)
  expect_identical(start, end)
  bin
}


pack_rt <- function (start, cmp) {
  "Pack and unpack and check that your data made the round trip (as defined by
   expect_equivalent)"
  bin <- packb(start)
  expect_equal(bin, cmp)
  end <- unpackb(bin)
  expect_equivalent(start, end)
}


test_that("pack singletons", {
  #null
  pack_rt(NULL, as.raw(0xc0)) #NA is encoded as null?
  packb(NA) %is% as.raw(0xc0)

  #logical
  pack_rt(FALSE, as.raw(0xc2))
  pack_rt(TRUE, as.raw(0xc3))

  #small ints
  pack_rt(12L, as.raw(0x0c))
  pack_rt(-4L, as.raw(0xfc))

  #32 bit ints
  pack_rt(2147483647L, as.raw(c(0xCE, 0x7f, 0xff, 0xff, 0xff)))

  # cwpack will use 32 bit float if precision is preserved.
  # float32 representation of 5:
  # 5 = 1.25 * 2^2
  #   = +1 * 2 ^  (129 - 127) * (1 + 0.25)
  #     ^sign      ^exponent     ^mantissa
  #     0          10000001         010000000000000000000000
  # 01000000 10100000 00000000 00000000
  # 40 A0 00 00
  pack_rt(5, as.raw(c(0xCA, 0x40, 0xA0, 0x00, 0x00)))

  # float64:
  x <- 1.7976931348623157e308 # .Machine$double.xmax
  # 0 11111111110 1111111111111111111111111111111111111111111111111111
  pack_rt(x, as.raw(c(0xCB, 0x7F, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)))

  # character
  pack_rt("hello",
          as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f)))

  # raw character
  pack_rt(as.raw(0xab),
          as.raw(c(0xc4, 0x01, 0xab)))

  packb(NA_character_)  %is% as.raw(0xc0) #does not round trip
})


test_that("unpack large ints to float", {
  bigint = as.raw(c(0xcf, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01))
  negint = as.raw(c(0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff))
  not_na = as.raw(c(0xd3, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00))
  bigfloat = 9007199254740993 #read as float loses precision
  negfloat = -2147483649
  not_nafloat = -2147483648
  expect_warning(unpackb(bigint) %is% float, "precision")
  unpackb(negint) %is% negfloat
  unpackb(not_na) %is% not_nafloat
  #
  # and all in a vector...
  unpackb(c(as.raw(0x93), bigint, negint, not_na)) %is% c(float, negfloat, not_na_float)
})


test_that("nice errors from unpack", {
  expect_error(unpackb(as.raw(c(0x92, 0xc0))),
               "end of input")
})


test_that("Pack raws", {
  roundtrip(as.raw(c(0xab, 0xbc, 0x00)))
})


test_that("Pack lists", {
  roundtrip(list(1, "what"))
  roundtrip(list("a", list("b", 4)))
})


test_that("unpack simplified vectors", {
    roundtrip(c(FALSE, NA)) #bool
    roundtrip(list(FALSE, 3L)) #list; don't coerce logicals (that aren't all NA)
    roundtrip(c(1L, NA)) #integer
    roundtrip(c(1L, NA, 1.0)) #real
    roundtrip(c("hello", NA)) #string
    roundtrip(list(1L, 2L, "hi")) #list, don't coerce to char
    roundtrip(list(c(1,2), c("hi", "bye"))) #list
})


test_that("unpack simplified vectors starting with NA", {
  roundtrip(c(NA, FALSE, TRUE))
  roundtrip(c(NA, 1L, 2L))
  roundtrip(c(NA, exp(0), pi))
  roundtrip(c(NA, "hi", "bye"))
})


test_that("pack zero length vectors", {
  roundtrip(c())
})


test_that("NA and NaN are distinct doubles,", {
  roundtrip(c(NA, NaN))
})


test_that("compatibility mode", {
  stop("not written")
})


test_that("use arrays for singletons", {
  stop("not written")
})


test_that("packing overflow handler works", {
  stop("not written")
})


test_that("unpack into envs", {
  stop("not written")
})


test_that("not simplifying", {
  stop("not written")
})


test_that("customize treatment of nil values", {
 stop("not written")
})

test_that("Homepage example", {
  pack_rt(list(compact=TRUE, schema=0),
          c(as.raw(c(0x82, 0xa7)),
            charToRaw("compact"),
            as.raw(c(0xc3, 0xa6)),
            charToRaw("schema"),
            as.raw(00)))
})


test_that("extension data types", {
  stop("not written")
})


test_that("recursion not allowed", {
  stop("not written")
});

## Local Variables:
## ess-r-package-info: ("msgpackr" . "~/msgpackr/")
## End:
