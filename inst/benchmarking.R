## ----- setup
ensureExists <- function(packages){
  installed <- library()$results[,1]
  needs.install <- setdiff(packages, installed)
  if(length(needs.install) > 0) {
    install.packages(needs.install)
  }
}

if (exists("clust") && !is.null(clust)) {
  stopCluster(clust)
  clust <- NULL
}

ensureExists(c(
  "knitr",
  "rmarkdown",
  "purrr",
  "dplyr",
  "magrittr",
  "tibble",
  "printr",
  "ggplot2",
  "nycflights13",
  "parallel",
  "ssh.utils",
  "tools",
  "msgpack",
  "jsonlite",
  "msgpackR",
  "rjson",
  "RJSONIO",
  "yaml",
  "broom"
))

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

remoteHost <- "paradoxus.local"
port <- 42170

bufferBytes <- function(f) function(con, chunk = 65536L) {
  buf <- rawConnection(raw(0), open="w")
  repeat {
    result <- readBin(con = con, what = "raw", n = chunk)
    if (length(result) == 0) break
    writeBin(result, buf)
  }
  f(rawConnectionValue(buf))
}

bufferBytes2 <- function(f) function(con, chunk = 65536L) {
  buf <- msgpack:::catenator(raw(0)) # this is faster
  repeat {
    result <- readBin(con = con, what = "raw", n = chunk)
    if (length(result) == 0) break
    buf(result)
  }
  f(buf(action="read"))
}

bufferRawConn <- function(f) function(con, chunk = 65536L) {
  buf <- msgpack:::catenator(raw(0)) # this is faster
  repeat {
    result <- readBin(con = con, what = "raw", n = chunk)
    if (length(result) == 0) break
    buf(result)
  }
  f(rawConnection(buf(action="read"), open="r"))
}


bufferLines <- function(reader) function(con) {
  lines <- msgpack:::catenator(character(0))
  repeat {
    l <- readLines(con)
    if (length(l) == 0) break()
    lines(l)
  }
  buf <- textConnection(lines(action="read"))
  on.exit(close(buf))
  reader(buf)
}

forked <- function(ifParent, ifChild, catch=TRUE) {
  #   Fork the R process. In the parent process call ifParent(), in
  #   the child process call ifChild(). Return a list of return value of
  #   both functions, or error messages.
  other <- parallel:::mcfork()
  if (inherits(other, "masterProcess")) {
    # we are child process
    if (catch) {
      tryCatch({
        result <- ifChild(x)
        parallel:::mcexit(0, send = result)
      },
      error =
        function(e) {
          parallel:::mcexit(1, send = list(error=e, calls=sys.calls()))
        }
      )
    } else {
      result <- ifChild(x)
      parallel:::mcexit(0, send = result)
    }
  } else {
    # we are master process
    if (catch) {
      mine <- tryCatch(ifParent(x),
                       error = if (catch) function(e) list(error=e, calls = sys.calls()) else NULL)
    } else {
      mine <- ifParent(x)
    }
    theirs <- tryCatch(
      unserialize(parallel:::readChild(other)),
      error = if(catch) function(e) list(error=e, calls=sys.calls()) else NULL)
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
    if (missing(end.read) || missing(end.write))
      list()
    else
      c(read.user = end.read[["user.self"]] - start.read[["user.self"]],
        read.sys = end.read[["sys.self"]] - start.read[["sys.self"]],
        read.elapsed = end.read[["elapsed"]] - start.read[["elapsed"]],
        write.user = end.write[["user.self"]] - start.write[["user.self"]],
        write.sys = end.write[["sys.self"]] - start.write[["sys.self"]],
        write.elapsed = end.write[["elapsed"]] - start.write[["elapsed"]],
        total.user =
          (   end.read[["user.self"]] + end.write[["user.self"]]
            - start.write[["user.self"]] - start.read[["user.self"]]),
        total.sys =
          (   end.read[["sys.self"]] + end.write[["sys.self"]]
            - start.write[["sys.self"]] - start.read[["sys.self"]]),
        total.elapsed = max(end.read[["elapsed"]] - start.read[["elapsed"]],
                            end.write[["elapsed"]] - start.write[["elapsed"]])),
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

timeSocketTransfer <- function(data,
                               reader = unserialize,
                               writer = serialize,
                               wrap = identity,
                               raw = TRUE,
                               catch = FALSE, ...) {
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
         end.read = end.read)
  }
  doWrite <- function(other) {
    Sys.sleep(0.5)
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
  results <- forked(ifChild = doWrite, ifParent = doRead, catch = catch)
  end.parent <- proc.time()

  do.call(times, c(results,
                   list(start.parent = start.parent, end.parent = end.parent)),
          quote=TRUE)
}

timeFifoTransfer <- function(data,
                             reader = unserialize,
                             writer = serialize,
                             wrap = identity,
                             catch = FALSE, ...) {
  force(data)
  fnam <- tempfile()
  on.exit(unlink(fnam, force = TRUE))
  system(paste("mkfifo", fnam))

  doRead <- function(other) {
    Sys.sleep(0.5)
    conn <- wrap(fifo(fnam, open = "rb", blocking = TRUE))
    on.exit({
      close(conn)
    })
    start.read <- proc.time()
    as.read <- reader(conn)
    end.read <- proc.time()
    list(start.read = start.read,
         end.read = end.read)
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
  results <- forked(ifChild = doWrite, ifParent = doRead, catch = catch)
  end.parent <- proc.time()

  do.call(times, c(results,
                   list(start.parent = start.parent,
                        end.parent = end.parent)),
          quote=TRUE)
}

clust <<- NULL
startRemote <- function() {
  message("starting remote")
  ssh.utils::run.remote("killall R", remoteHost)
  clust <- makePSOCKcluster(nnodes=1, c(remoteHost), rscript = "Rscript")
  rtmp <- clusterCall(clust, tempdir)
  message("rtmp is ", rtmp)
  message("getwd is ", getwd())

  ssh.utils::cp.remote("", "../inst/benchmarking.R",
                       remoteHost, paste0(rtmp, "/benchmarking.R"))
  clusterCall(clust, options, repos = getOption("repos"))
  parallel::clusterCall(clust, source, paste0(rtmp, "/benchmarking.R"))
  clust <<- clust
  message("started remote")
}

doRemoteWrite <- function(data, host, port, wrap, raw, writer) {
  tryCatch({
    Sys.sleep(1)
    conn <- wrap(socketConnection(host = host,
                                  port = port,
                                  server = FALSE,
                                  blocking = TRUE,
                                  open = paste0("w", if(raw) "b" else "")))
    start.write <- proc.time()
    on.exit(close(conn))
    writer(data, conn)
    flush(conn)
    end.write <- proc.time()
    list(start.write = start.write,
         end.write = end.write, bytes=NA)
  },
  error = function(e) list(error=e, calls=sys.calls()))
}

doRemoteRead <- function(port, wrap, raw, reader) {
  # place logging statements in here...

  tryCatch({
    sock <- socketConnection(port = port,
                             server = TRUE,
                             blocking = TRUE,
                             open = paste0("r", if(raw) "b" else ""),
                             timeout = 60)
    conn <- wrap(sock)
    on.exit(close(conn))
    start.read <- proc.time()
    as.read <- reader(conn)
    end.read <- proc.time()
    list(start.read = start.read,
         end.read = end.read)
  },
  error = function(e) list(error=e, calls=sys.calls())
  )
}

timeRemoteTransfer <- function(data,
                               reader = bufferBytes(unserialize),
                               writer = serialize,
                               wrap = identity,
                               raw = TRUE,
                               ...) {
  force(data)
  if (is.null(clust)) {
    startRemote()
  }

  parallel:::sendCall(clust[[1]],
                      doRemoteRead,
                      list(port, wrap, raw, reader))
  local_results <- tryCatch(
    doRemoteWrite(data, remoteHost, port, wrap, raw, writer),
    error = function(e) list(error = e, calls = sys.calls()))
  remote_results <- parallel:::recvResult(clust[[1]])
  do.call(times, c(local_results, remote_results), quote=TRUE)
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

`%*%` <- intersect

run_tests <- function(arg_df) {
  pmap_dfr(arg_df, function(..., args) {
    labels <- list(...)
    message(paste0(collapse = "\n", deparse(labels, control=c())))
    # what I want is a bind syntax like
    ## bind({
    ##   list(strategy = ?fun, ??args) <- args
    ##   fun(!!args)
    ## })
    fun <- args$strategy
    args <- args[names(args) != "strategy"]
    the_env <- list2env(args, parent = environment())

    # this dance is to avoid calling do.call with a giant unnamed dataset
    # (which causes R to spin several minutes on writing a traceback)
    call <- as.call(c(quote(fun),
                      structure(map(names(args), as.name),
                                names = names(args))))
    results <- eval(call, the_env)
    as_data_frame(c(labels, results))
  })
}

store <- function(data, dataset) {
  # key on all character columns the two tables hav in common
  existing.keys <- names(benchmarks)[map_lgl(benchmarks, is.character)]
  new.keys <- names(data)[map_lgl(data, is.character)]
  keys <- intersect(existing.keys, new.keys)
  message(paste("Replacing on keys:", paste0("\"", keys, "\"", collapse=", ")))
  if (length(keys) > 0) {
    dataset <- (dataset          # update benchmarks
      %>% anti_join(data, by=keys)
      %>% bind_rows(data)
    )
  } else {
    dataset <- data
  }
  dataset
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

synchronous.methods <- list(
  method = list(
    conn = list(method = timeConnection),
    file = list(method = timeFileIO)
))

concurrent.methods <- list(
  method = list(
    remote = list(method = timeRemoteTransfer),
    socket = list(method = timeSocketTransfer),
    fifo = list(method = timeFifoTransfer))
)

connection.methods <- list(
  method = c(concurrent.methods$method,
             synchronous.methods$method)
)

convert.common.options <- c(
  common.options,
  conversion.methods
)

raw.common.options <- c(
  common.options,
  conversion.methods,
  raw = list('TRUE' = list(raw = TRUE))
)

text.common.options <- c(
  common.options,
  conversion.methods,
  raw = list('FALSE' = list(raw = FALSE))
)

all.common.options <- c(
  common.options,
  list(
    method = c(connection.methods$method,
               conversion.methods$method))
)

buffer_read_options <- function(reader = identity,
                              raw = FALSE,
                              buffer = (if (raw) bufferBytes2 else bufferLines)) {
  c(common.options,
    list(raw = structure(list(list(raw = raw)), names = as.character(raw))),
    list(method = list(
      conn = list(method = timeConnection,
                  reader = reader),
      file = list(method = timeFileIO,
                  reader = reader),
      fifo = list(method = timeFifoTransfer,
                  reader = buffer(reader)),
      socket = list(method = timeSocketTransfer,
                    reader = buffer(reader)),
      remote = list(method = timeRemoteTransfer,
                    reader = buffer(reader))
    ))
    )
}

combine_opts <- function(x, ...) {
  for (l in list(...)) {
    for (n in names(l)) {
      if (n %in% names(x)) {
        x[[n]] <- c(x[[n]], l[[n]])
      } else {
        x[[n]] <- c(x[[n]], l[[n]])
      }
    }
  }
  x;
}

`%but%` <- function(l, r) {
  l[names(r)] <- r;
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
