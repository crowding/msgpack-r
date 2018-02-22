msgpack for R
================
Peter Meilstrup
February 22, 2018

[![CRAN version
badge](http://www.r-pkg.org/badges/version/msgpack)](https://cran.r-project.org/package=msgpack)
[![Travis build
status](http://travis-ci.org/crowding/msgpack-r.svg?branch=master)](https://travis-ci.org/crowding/msgpack-r)
[![Code
coverage](https://codecov.io/gh/crowding/msgpack-r/branch/master/graph/badge.svg)](https://codecov.io/gh/crowding/msgpack-r)

This is a high speed [msgpack](https://github.com/clwi/CWPack) encoder
and decoder for R, based on the [CWPack](https://msgpack.org) C
implementation.

`msgpack` is a binary data format with data structures similar to `JSON`
and a compact binary encoding. It can be a drop-in replacement for
`JSON` in most applications. It is designed to be fast to parse and
compact to transmit and store.

## Installation

From CRAN:

``` r
install.packages("msgpack")
```

From Github:

``` r
library(devtools)
install_github("crowding/msgpack-r")
```

## Usage

``` r
library(msgpack)
x <- packMsg( list(compact=TRUE, schema=0) )
x
##  [1] 82 a7 63 6f 6d 70 61 63 74 c3 a6 73 63 68 65 6d 61 00
dput(unpackMsg( x ))
## structure(list(compact = TRUE, schema = 0L), .Names = c("compact", 
## "schema"))
```

### Connections / Streaming

Write messages one or several at a time:

``` r
conOut <- rawConnection(raw(0), open = "w") # or socketConnection, etc
writeMsg("one", conOut)
writeMsgs(list(2, c(buckle=TRUE), c(owner="my", type="shoe")), conOut)
```

Use a `msgConnection` object to read messages one or several at a
time:

``` r
conIn <- msgConnection(rawConnection(rawConnectionValue(conOut), open = "r"))
dput(readMsgs(conIn, 2))
## list("one", 2L)
dput(readMsg(conIn))
## structure(TRUE, .Names = "buckle")
dput(readMsgs(conIn))
## list(structure(c("my", "shoe"), .Names = c("owner", "type")))
```

### Performance

Msgpack is fast and compact. See the [benchmarking
vignette](inst/doc/comparison.html).

![Plot of time taken to transmit dataset, vs size of dataset, for each
encoder under four conditions.](gh/space.svg)

![Comparison of space used by each encoder to encode a test
dataset.](gh/time.svg)
