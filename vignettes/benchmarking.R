## ----- setup
library(knitr)
library(rmarkdown)
library(purrr)
library(dplyr)
library(magrittr)
library(tibble)
library(printr)
library(ggplot2)

## ---- dataset
library(nycflights13)
dataset <- as.list(as.environment("package:nycflights13"))

subsample <- function(dataset, rate) {
  lapply(dataset, function(df) {
    df[1:(1 + round(nrow(df) * rate)), ]
  })
}

onerow <- subsample(dataset, 0)

## ---- definitions
attach_env <- function(arg_, ...) {
  target <- new.env(parent = env(arg_))
  for (i in list(...)) {
    e <- as.environment(i)
    # think about "inject_env" here
    inject(e, target)
  }
  env(arg_) <- target
}

inject <- function(from, to_env) {
  f <- as.environment(from)
  dots2env(as.dots(f), envir = to_env)
}

forked <- function(ifParent, ifChild) {
  #   Fork the R process. In the parent process call ifParent(), in
  #   the child process call ifChild(). Return a list of return value of
  #   both functions, or error messages.
  other <- parallel:::mcfork()
  if (inherits(other, "masterProcess")) {
    # we are child process
    tryCatch({
      result <- ifChild(x)
      #cat("ran child\n")
      parallel:::mcexit(0, send=result)
    },
    error =
        function(err) {
            ##cat("child error\n")
            parallel:::mcexit(1, send=err)
      }
    )
  } else {
    # we are master process
    mine <- tryCatch(ifParent(x), error = identity)
    #cat("ran parent\n");
    theirs <- tryCatch(unserialize(parallel:::readChild(other)), error=identity)
    #cat("read from child\n");
    Sys.sleep(1) #wait for child to finish?
    child_pids <- vapply(parallel:::children(), function(x) x$pid, 0)
    if (other$pid %in% child_pids) {
      warning("Killing child process ", deparse(other))
      parallel:::mckill(other, tools::SIGTERM)
    }
    c(theirs, mine)
  }
}

return_result <- FALSE
# Function accepts a set of sys.time readings and computes timings
times <- function(start.write, end.write,
                  start.read, end.read,
                  bytes, result,
                  start.parent, end.parent, ...) {
  c(list(extra=list(list(...))),
    if (missing(end.read))
      list()
    else
      c(read.user = end.read[["user.self"]] - start.read[["user.self"]],
        read.sys = end.read[["sys.self"]] - start.read[["sys.self"]],
        read.elapsed = end.read[["elapsed"]] - start.read[["elapsed"]],
        write.user = end.write[["user.self"]] - start.write[["user.self"]],
        write.sys = end.write[["sys.self"]] - start.write[["sys.self"]],
        write.elapsed = end.write[["elapsed"]] - start.write[["elapsed"]],
        total.user = (end.read[["user.self"]] + end.write[["user.self"]]
          - start.write[["user.self"]] - start.read[["user.self"]]),
        total.sys = (end.read[["sys.self"]] + end.write[["sys.self"]]
          - start.write[["sys.self"]] - start.read[["sys.self"]]),
        total.elapsed = ( max(end.write[["elapsed"]], end.read[["elapsed"]])
          - min(start.write[["elapsed"]], start.read[["elapsed"]]))),
    bytes = if (missing(bytes)) list() else bytes,
    result = if (missing(result) || !return_result) list() else list(result),
    parent = if (missing(start.parent)) list()
             else c(user = (end.parent[["user.self"]] - start.parent[["user.self"]]),
                    sys = (end.parent[["sys.self"]] - start.parent[["sys.self"]]),
                    elapsed = (end.parent[["elapsed"]] - start.parent[["elapsed"]])))
}

bytes <- function(x) {
  switch(mode(x),
         raw = {
           length(x)
         },
         character = {
           length(x) + sum(nchar(x))
         })
}

timeConvert <- function(data,
                        from = unserialize,
                        to = function(data) serialize(data,NULL),
                        wrap = identity, ...) {
  force(data)
  start.write <- proc.time()
  enc <- to(data)
  end.write <- proc.time()
  as.read <- from(enc)
  end.read <- proc.time()
  bytes <- bytes(enc)
  times(start.write, end.write,
        end.write, end.read,
        bytes(enc), as.read)
}


timeConnection <- function(..., raw = TRUE) {
  if (raw) {
    timeRawConnection(...)
  } else {
    timeTextConnection(...)
  }
}

timeRawConnection <- function(data,
                              reader = unserialize,
                              writer = serialize,
                              wrap = identity, ...) {
  force(data)
  conn <- wrap(rawConnection(raw(0), open="wb"))
  on.exit(close(conn), add=TRUE)
  start.write <- proc.time()
  writer(data, conn)
  end.write <- proc.time()
  bytes <- rawConnectionValue(conn)
  conn2 <- wrap(rawConnection(bytes, open="rb"))
  on.exit(close(conn2), add=TRUE)
  start.read <- proc.time()
  as.read <- reader(conn2)
  end.read <- proc.time()
  times(start.write, end.write,
        end.write, end.read,
        length(bytes), as.read)
}

timeTextConnection <- function(data,
                               reader = function(x) source(x, TRUE),
                               writer = function(x, conn) dump("x", conn),
                               wrap = identity, ...) {
  force(data)
  theText <- character(0)
  conn <- textConnection(NULL, open="w")
  on.exit(close(conn), add=TRUE)
  start.write <- proc.time()
  writer(data, conn)
  end.write <- proc.time()
  theText <- textConnectionValue(conn)
  nbytes <- bytes(theText)
  conn2 <- wrap(textConnection(theText, open="r"))
  on.exit(close(conn2), add=TRUE)
  start.read <- proc.time()
  as.read <- reader(conn2)
  end.read <- proc.time()
  times(start.write, end.write,
        end.write, end.read,
        nbytes, as.read)
}

timeFileIO <- function(data,
                       reader = unserialize,
                       writer = serialize,
                       raw = TRUE,
                       wrap = identity, ...) {
  force(data)
  fnam <- tempfile()
  on.exit(unlink(fnam, force=TRUE))
  con <- file(fnam, open=paste0("w", if(raw) "b" else ""), raw=raw)
  start.write <- proc.time()
  writer(data, con)
  nbytes <- seek(con)
  close(con)
  end.write <- proc.time()
  con <- wrap(file(fnam, open=paste0("r", if(raw) "b" else ""), raw=raw))
  on.exit(close(con), add=TRUE)
  start.read <- proc.time()
  as.read <- reader(con)
  end.read <- proc.time()
  times(start.write, end.write,
        start.read, end.read,
        nbytes, as.read)
}

port <- 42170
timeSocketTransfer <- function(data,
                               reader = unserialize,
                               writer = serialize,
                                wrap = identity,
                               raw = TRUE, ...) {
  force(data)
  doRead <- function(other) {
    conn <- wrap(socketConnection(port = port,
                                  server = TRUE,
                                  blocking = TRUE,
                                  open = paste0("r", if(raw) "b" else "")))
    on.exit(close(conn))
    start.read <- proc.time()
    as.read <- reader(conn)
    end.read <- proc.time()
    list(start.read = start.read,
         end.read = end.read,
         result = as.read)
  }
  doWrite <- function(other) {
    conn <- wrap(socketConnection(port = port, server = FALSE,
                                  blocking = TRUE,
                                  open = paste0("w", if(raw) "b" else "")))
    start.write <- proc.time()
    on.exit(close(conn))
    writer(data, conn)
    flush(conn)
    end.write <- proc.time()
    list(start.write = start.write,
         end.write = end.write, bytes=NA)
  }
  start.parent <- proc.time()
  results <- forked(ifChild = doWrite, ifParent = doRead)
  end.parent <- proc.time()

  do.call(times, c(results,
                   list(start.parent = start.parent, end.parent = end.parent)),
          quote=TRUE)
}

timeFifoTransfer <- function(data,
                             reader = unserialize,
                             writer = serialize,
                             wrap = identity, ...) {
  force(data)
  fnam <- tempfile()
  on.exit(unlink(fnam, force = TRUE))
  system(paste("mkfifo", fnam))

  doRead <- function(other) {
    Sys.sleep(1)
    conn <- wrap(fifo(fnam, open = "rb", blocking = TRUE))
    on.exit({
      close(conn)
    })
    start.read <- proc.time()
    as.read <- reader(conn)
    end.read <- proc.time()
    list(start.read = start.read,
         end.read = end.read,
         result = as.read)
  }
  doWrite <- function(other) {
    conn <- fifo(fnam, open = "wb", blocking = TRUE)
    on.exit({
      close(conn)
    })
    start.write <- proc.time()
    writer(data, conn)
    flush(conn)
    end.write <- proc.time()
    list(start.write = start.write,
         end.write = end.write,
         bytes = NA)
  }

  start.parent <- proc.time()
  results <- forked(ifChild = doWrite, ifParent = doRead)
  end.parent <- proc.time()

  do.call(times, c(results,
                   list(start.parent = start.parent,
                        end.parent = end.parent)),
          quote=TRUE)
}

timeCurve <- function(dataset,
                      method,
                      timeout = 60,
                      start = 0.01,
                      max = 1, ...
                      ) {
  results <- data_frame()
  current <- start
  if (missing(method)) {
    message("method missing???")
    return(data.frame())
  }
  while (current <= max) {
    message(paste0("size = ", current))
    data <- subsample(dataset, current)
    result <- method(data, ...)
    results <- bind_rows(results, as_tibble(c(size = current, result)))
    if ("total.elapsed" %in% names(result)) {
      if (result$total.elapsed > timeout) break() else NULL
    } else  {
      break()
    }
    if (current == max) break()
    current = min(current * sqrt(2), max)
  }
  results
}


#e.g.,
quote(
    tests <- list(
        strategy = list(
          ,  convert = timeConvert
          ,  connection = timeRawConnection
          ,  file = timeFileIO
          ,  fifo = timeFifoTransfer
          ,  socket = timeSocketTransfer
          ,  wire = timeWireTransfer
        ),
        encoder = list(
            serialize = list(
                unserialize
              , function(data) serialize(data, NULL))
          , dput = list(
                deparse
              , function(t) eval(parse(text=t)))
          , jsonlite = list(
                jsonlite::fromJSON
              , jsonlite::toJSON)
          , msgpack = list(
                msgpack::packMsg
              , msgpack::unpackMsg)
          , RJSONIO = list(
                RJSONIO::fromJSON,
                RJSONIO::toJSON))
      , dataset = list(nycflights13=list(dataset)))
)


arg_df <- function(tests) (tests
  # produces an arg data frame: the labels in columns and the argument
  # data structure in column "args"
  %>% map(names)
  %>% cross_df()
  %>% pmap_dfr(function(...) {
    labels <- list(...)
    arglist <- pmap(list(labels, names(labels)),
                    function(label, column) list(tests[[column]][[label]]))
    c(labels, args = list(list(arglist)))
  })
  %>% mutate(args = (args # unlist the args twice
    %>% map(.
            %>% unlist(recursive = FALSE, use.names = FALSE)
            %>% unlist(recursive = FALSE, use.names = TRUE))))
)


`%+%` <- union
`%-%` <- setdiff
`%*%` <- intersect
# `%/%` <- dusjunctive union ???

run_tests <- function(arg_df) (arg_df
  %>% pmap_dfr(function(..., args) {
    labels <- list(...)
    message(paste0(collapse = "\n", deparse(labels, control=c())))
    # what I want here is a bind syntax like
    ## bind({
    ##   list(strategy = ?fun, ??args) <- args
    ##   fun(!!args)
    ## })
    fun <- args$strategy
    args <- args[names(args) != "strategy"]
    the_env <- list2env(args, parent=environment())
    # this dance is to avoid calling do.call with a giant unnamed dataset
    # (which causes R to spin several minutes on writing a traceback)
    call <- as.call(c(quote(fun),
                      structure(map(names(args), as.name),
                                names = names(args))))
    results <- eval(call, the_env)
    results <- do.call(fun, args, quote = TRUE, envir = environment())
    as_data_frame(c(labels, results))
  })
)

benchmarks <- data.frame()
store <- function(data) {
  # key on all character columns the two tables hav in common
  existing.keys <- names(benchmarks)[map_lgl(benchmarks, is.character)]
  new.keys <- names(data)[map_lgl(data, is.character)]
  keys <- existing.keys %*% new.keys
  message(paste("Replacing on keys:", paste0("\"", keys, "\"", collapse=", ")))
  if (length(keys) > 0) {
    benchmarks <<- (benchmarks          # update benchmarks
      %>% anti_join(data, by=keys)
      %>% bind_rows(data)
    )
  } else {
    benchmarks <<- data
  }
  data
}

common.options <- list(
  strategy = list(
    timeCurve = list(strategy = timeCurve, timeout = 10, start = 0.001)),
  dataset = list(
    nycflights13 = list(data = dataset)))

conversion.methods <- list(
  method = list(
    convert = list(method = timeConvert))
)

connection.methods <- list(
  method = list(
    conn = list(method = timeConnection),
    file = list(method = timeFileIO),
    fifo = list(method = timeFifoTransfer),
    socket = list(method = timeSocketTransfer))
)

convert.common.options <- c(
  common.options,
  conversion.methods
)

raw.common.options <- c(
  common.options,
  conversion.methods,
  etc = list(raw = list(raw = TRUE))
)

text.common.options <- c(
  common.options,
  conversion.methods,
  etc = list(raw = list(raw = FALSE))
)

all.common.options <- c(
  common.options,
  list(
    method = c(connection.methods$method,
               conversion.methods$method))
)

`%but%` <- function(l, r) {
  l[names(r)] <- r
  l
}

showTimings <- function(timings, label, ...) {
  stats <- with(timings, c(
    "total.elapsed (s)" = total.elapsed,
    "read.cpu (s)" = read.user + read.sys,
    "read.elapsed (s)" = read.elapsed,
    "write.cpu (s)" = write.user + write.sys,
    "write.elapsed (s)" = write.elapsed,
    "data size (MB)" = bytes / 0x100000
  ))
  kable(stats, col.names = label, digits=2)
}

## list(x = ?first, ??rest) -> list()

## f <- function(a, call) bind({
##   list(?first, ??rest) <- callList
## })

## Local Variables:
## ess-r-package-info: ("msgpack" . "~/msgpack/")
## End:
