---
title: "msgpack for R"
output: github_document
---

This is a high speed [msgpack][2] encoder and decoder for R, based on the
[CWPack][1] C implementation.
1: https://github.com/clwi/CWPack
2: https://msgpack.org

`msgpack` is a binary data format with compact encoding and data
structure support similar to JSON. It can be a drop-in replacement for
JSON in most applications. It is designed to be fast to parse and
compact to transmit and store.



## Installation


```r
library(devtools)
install_github("crowding/msgpack-r")
```

## Usage


```r
library(msgpack)
x <- packMsg( list(compact=TRUE, schema=0) )
x
##  [1] 82 a7 63 6f 6d 70 61 63 74 c3 a6 73 63 68 65 6d 61 00
dput(unpackMsg( x ))
## structure(list(compact = TRUE, schema = 0L), .Names = c("compact", 
## "schema"))
```

### Connections / Streaming


```r
conOut <- rawConnection(raw(0), open = "w") #or socketConnection, etc
writeMsg("one", conOut)
writeMsgs(list(2, c(buckle=TRUE), c(owner="my", type="shoe")), conOut)
conIn <- msgConnection(rawConnection(rawConnectionValue(conOut), open = "r"))
dput(readMsgs(conIn, 2))
## list("one", 2L)
dput(readMsg(conIn))
## structure(TRUE, .Names = "buckle")
dput(readMsgs(conIn))
## list(structure(c("my", "shoe"), .Names = c("owner", "type")))
```

### Performance

Msgpack is fast and compact. See the [benchmarking vignette](vignettes/comparison.html).

![Plot of time taken to transmit dataset, vs size of dataset, for each encoder under four conditions.](gh/space.svg)

![Comparison of space used by each encoder to encode a test dataset.](gh/time.svg)
