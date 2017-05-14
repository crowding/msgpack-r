context("msgpackr static API")

`%is%` <- expect_equal


stringToRaw <- function(ch) {
  vapply(strsplit("hello", "")[[1]], charToRaw, raw(1))
}


roundtrip <- function(start) {
  bin <- packb(start)
  end <- unpackb(bin)
  expect_equal(start, end)
  bin
}


pack_rt <- function (start, cmp, ...) {
  "Pack and unpack and check that your data made the round trip (as defined by
   expect_equivalent)"
  bin <- packb(start, ...)
  expect_equal(bin, cmp)
  end <- unpackb(bin, ...)
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

  # cwpack will use 32 bit float if precision is preserved. For example, Inf:
  pack_rt(Inf, as.raw(c(0xca, 0x7f, 0x80, 0x00, 0x00)))

  # and a float64:
  x <- 1.7976931348623157e308 # .Machine$double.xmax
  # 0 11111111110 1111111111111111111111111111111111111111111111111111
  pack_rt(x, as.raw(c(0xCB, 0x7F, 0xEF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF)))

  # character
  pack_rt("hello",
          as.raw(c(0xa5, 0x68, 0x65, 0x6c, 0x6c, 0x6f)))

  # raw bytes
  pack_rt(as.raw(0xab),
          as.raw(c(0xc4, 0x01, 0xab)))

  #NAs and NULL all collapse to nil
  packb(NA_character_) %is% as.raw(0xc0)
  packb(NA_real_) %is% as.raw(0xc0)
  packb(NA_integer_) %is% as.raw(0xc0)
  packb(NULL) %is% as.raw(0xc0)
})

test_that("unpack large ints to float", {
  bigint = as.raw(c(0xcf, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01))
  negint = as.raw(c(0xd3, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xff, 0xff, 0xff))
  not_na = as.raw(c(0xd3, 0xff, 0xff, 0xff, 0xff, 0x80, 0x00, 0x00, 0x00))
  bigfloat = 9007199254740993 #read as float loses precision
  negfloat = -2147483649
  not_na_float = -2147483648
  expect_warning(unpackb(bigint) %is% bigfloat, "precision")
  unpackb(negint) %is% negfloat
  unpackb(not_na) %is% not_na_float
  #
  # and all in a vector...
  expect_warning(
   unpackb(c(as.raw(0x93), bigint, negint, not_na)) %is%
     c(bigfloat, negfloat, not_na_float),
   "precision")
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


test_that("packing overflow handler works", {
  expect_true(length(packb(1:10000)) > 1000)
})

test_that("extension mechanism", {
  obj <- c(1, 2, 3)
  class(obj) <- c("reverse")
  assign(envir = globalenv(), "prepack.reverse", function(x) rev(x))
  unpackb(packb(obj)) %is% c(3, 2, 1)
})


test_that("recursive use of msgpack works", {
  assign(envir = globalenv(), "prepack.blob", function(x) packb(unclass(x)))

  obj <- "hello"
  class(obj) <- "blob"
  typeof(unpackb(packb(obj))) %is% "raw"
})


test_that("Max buffer size", {
  packb(300:400, max_size=306)
  expect_error(packb(300:401, max_size=306), "overflow")
})


test_that("NA and NaN are distinct doubles,", {
  roundtrip(c(NA, NaN))
})


test_that("compatibility mode", {
  packb(as.raw(c(1, 2, 3))) %is% as.raw(c(0xc4, 0x03, 0x01, 0x02, 0x03))
  packb(as.raw(c(1, 2, 3)), compatible=TRUE) %is% as.raw(c(0xa3, 0x01, 0x02, 0x03))
})


test_that("Unpackb: detect bad strings, warn, and return raw", {

  expect_warning(expect_equal(unpackb(as.raw(c(0xa3, 0x00, 0x62, 0x63))),
                              as.raw(c(0x00, 0x62, 0x63))),
                 "nul")

  #and check for malformed UTF8
  #3 byte sequence with last continuation byte missing
  expect_warning(expect_equal(unpackb(as.raw(c(0xa2, 0x30, 0x80))),
                              as.raw(c(0x30, 0x80))),
                 "UTF")

  #2 bytes of 3 byte sequence followed by space
  expect_warning(expect_equal(unpackb(as.raw(c(0xa3, 0x30, 0x80, 0x20))),
                              as.raw(c(0x30, 0x80, 0x20))),
                 "UTF")

  # illegal byte
  expect_warning(expect_equal(unpackb(as.raw(c(0xa1, 0xff))),
                              as.raw(c(0xff))),
                 "UTF")
  #also for malformed UTF8?

})

test_that("always emit strings in UTF8,", {
  x <- "fa\xE7ile"
  Encoding(x) <- "latin1"
  packb(x) %is% as.raw(c(0xa7, 0x66, 0x61, 0xc3, 0xa7, 0x69, 0x6c, 0x65))
})

test_that("use ints for integral floats under 32 bits", {
  packb(1) %is% packb(1L)

  length(packb(2^32)) %is% 5
  length(packb(2^32+1)) %is% 9
  length(packb(-2^31)) %is% 5
  length(packb(-2^31-1)) %is% 9
})


test_that("as_is uses arrays even for singletons", {
  length(packb(1)) %is% 1
  length(packb(1, as_is=TRUE)) %is% 2
  length(packb(list(1, 2, 3))) %is% 4
  length(packb(list(1, 2, 3), as_is = TRUE)) %is% 7
  unpackb(packb(list(1, 2, 3))) %is% c(1, 2, 3)
  unpackb(packb(list(1, 2, 3), as_is = TRUE)) %is% list(1, 2, 3)

  length(packb(I(1), as_is=FALSE)) %is% 2
})


test_that( "single row data frames also pack with asIs", {
  expect_true(  length(packb(data.frame( a=1, b=2)))
              > length(packb(list(a=1, b=2))))
})


test_that("pack named vectors into dicts", {
  unpackb(packb(list(a=1, b=NULL))) %is% c(a=1, b=NA)
  unpackb(packb(list(a=1, b=NULL), use_dict=FALSE)) %is% c(1, NA)
})


test_that("unpack dicts into envs", {
  typeof(mode(unpackb(packb(list2env(list(a=1, b=2))))) %is% "list")

  x <- new.env()
  e <- unpackb(packb(list(a = 1, b = NULL)), x)
  typeof(e) %is% "environment"
  as.list(e) %is% list(a = 1)
  parent.env(e) %is% x
})


test_that("pack envs into sorted dicts", {
  e <- list2env(list(c=3, b=1, d=4, a=2))
  unpackb(packb(e)) %is% c(a=2, b=1, c=3, d=4)
})


test_that("detect data frames", {
  expect_true(is.data.frame(unpackb(packb(data.frame(a=1, b=2)))))
})


test_that("Raw with names", {
  expect_warning(packb(structure(as.raw(c(1,2)), names=c("a", "b"))))
})


test_that("unpackb refusing to simplify", {
  unpackb(packb(list(1, 2))) %is% c(1L, 2L)
  unpackb(packb(list(1, 2)), simplify=FALSE) %is% list(1L, 2L)
})


test_that("unpack treatment of nil values", {
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


test_that("over 2GB output?", {
  stop("not written");
})


## Local Variables:
## ess-r-package-info: ("msgpackr" . "~/msgpackr/")
## End:
