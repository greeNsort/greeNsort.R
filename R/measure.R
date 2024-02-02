# greeNsort old measurement (obsolete)
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

.sudopw <- NULL

#' Measuring a single expresssion
#'
#' This function spawns a seperated process which runs the linux \code{perf} tool and collects
#' interesting statistics including RAPL measurements
#'
#' The sleep-time for linux perf is calculated as \code{offset + slope*expected}
#'
#' @param pass the root password for \code{sudo}
#' @param file the name of the file into which \code{perf} is redirected
#' @param expr an expression calling a sorting function with output suitable to \code{\link{perf}} to be measured
#' @param expected the expected runtime in seconds
#' @param offset security offset in seconds
#' @param slope security factor
#' @param wait seconds waited between spawning \code{perf} and evaluating \code{expr} and also waited after the sleep time
#'
#' @return a vector with elements
#' \item{instructions}{instructions (count)}
#' \item{cpu-cycles}{cpu-cycles (count)}
#' \item{bus-cycles}{bus-cycles (count)}
#' \item{page-faults}{page-faults (count)}
#' \item{cache-references}{cache-references (count)}
#' \item{cache-misses}{cache-misses (count)}
#' \item{L1-dcache-loads}{L1-dcache-loads (count)}
#' \item{L1-dcache-stores}{L1-dcache-stores (count)}
#' \item{branch-instructions}{branch-instructions (count)}
#' \item{branch-misses}{branch-misses (count)}
#' \item{energy-pkg}{power/energy-pkg/ (Joule)}
#' \item{energy-cores}{power/energy-cores/ (Joule)}
#' \item{energy-ram}{power/energy-ram/ (Joule)}
#' \item{seconds-elapsed}{the time perf measured for sleep (seconds)}
#' \item{memory}{memory (\code{\link{size}})}
#' \item{runtime}{runtime (\code{\link{secs}})}
#' \item{footprint}{footprint (\code{\link{tFootprint}})}
#' @note \code{perf} requires
#' sudo apt-get install linux-tools-generic
#' sudo apt-get remove linux-tools-5.3.0-28-generic
#' sudo apt-get install linux-tools-5.3.0-40-generic
#'
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' x <- testdb$func$tieboot(2^23)
#' y <- x[]; s <- secs(Frogsort1(y))
#' y <- x[]; a <- measures(.sudopw, ".perf.txt", quote(Frogsort1(y)), s)
#' b <- measures(.sudopw, ".perf.txt", quote(Sys.sleep(s)), s)
#' d <- debias(sleep=b, task=a)
#' print(d)
#' rm(.sudopw)
#' }
measures <- function(pass, file, expr, expected, offset = 0.2, slope = 1.1, wait = 0.1){
  stat <- c( "instructions"
             ,"cpu-cycles"
             ,"bus-cycles"
             ,"page-faults"
             ,"cache-references"
             ,"cache-misses"
             ,"L1-dcache-loads"
             ,"L1-dcache-stores"
             ,"branch-instructions"
             ,"branch-misses"
             ,"power/energy-pkg/"  # this includes cores, ram and gpu
             ,"power/energy-cores/"
             ,"power/energy-ram/"
  )
  sleep <- wait + offset + slope*expected
  system(paste('sudo -S -b perf stat -a -e ', paste(stat, sep = '', collapse = ','), ' sleep ', sleep, ' 2> ', file, sep = ''), input = paste(pass,'\n', sep = ''))
  Sys.sleep(wait)
  tim <- perf(eval.parent(expr))
  if (is.null(tim))
    tim <- c(size = 0, secs = 0)
  finalwait <- sleep - tim[["secs"]] + wait
  if (finalwait > 0)
    Sys.sleep(finalwait)
  lines <- readLines(file, warn = FALSE)
  lines <- sub(",",".",gsub("[.]","",sub("^[ ]+","",sub("[ ]+$","",grep("[0123456789.,]+.+$", substr(lines, 1, 52), value = TRUE)))))
  ret <- as.numeric(gsub("([ ].*$)", "", lines))
  retnam <- sub("/$", "", sub("^[ ]+", "", gsub("^[0123456789.]+[ ]", "", lines)))
  retnam <- sub("Joules power/", "", retnam)  # fix naming of RAPL measurements
  names(ret) <- sub("seconds time elapsed", "seconds-elapsed", retnam)
  ret <- c(ret
           , "memory" = tim[["size"]]
           , "runtime" = tim[["secs"]]
           , "footprint" = tim[["size"]]*tim[["secs"]]
  )
  ret
}

#' Measuring replications of one algorithm in one data setting
#'
#' For each replication \code{r in 0:repl} test \code{data} sets is created with \code{set.seed(r)}.
#' Data number 0 is used to determine the expected runtime of \code{algo}.
#' For each of the remaining \code{1:repl} data sets \code{\link{measure1}} is called twice,
#' once measuring a sleep command of the expected duration
#' and once measuring algorithm \code{algo} on the data.
#'
#' @param algo label of the algorithm as in \code{\link{algodb}$label}
#' @param data name of the test data pattern as in \code{\link{testdb}$name}
#' @param size    number of test elements to be sorted
#' @param repl number of replications
#' @param pass the root password for \code{sudo}
#' @param file the name of the file into which \code{perf} is redirected
#' @param \dots further arguments passed to the algorithm
#'
#' @return a list with two components
#' \item{sleep}{the sleep measurement: a dataframe with columns returned by \code{\link{measures}} and \code{repl} rows}
#' \item{task}{the algorithm mesasurement: a dataframe with columns returned by \code{\link{measures}} and \code{repl} rows}
#'
#' @note The recommended correction for background
#' If the algorithm is size-varying the data is coerced to integer and then to character.
#' Note that the size-varing algorithms return their pure sorting time
#' but have relevant overhead for pre- and post-processing the strings
#' which is included in this measurement,
#' to correct for this bias measure also \code{\link{UVWoverhead}}.
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' m <- measurer(.sudopw, ".perf.txt", "Frog1", "permut", 2^23, 5)
#' rm(.sudopw)
#' }
measurer <- function(pass , file, algo, data, size, repl=2, ...){
  cat(0, data, algo, 1)
  set.seed(0)
  x <- testdb$func[[data]](size)
  if (algodb[algo,"varying"])
    x <- format(as.integer(x))
  sortexpr <- as.call(c(list(as.name(algodb[algo,"name"]), as.name("x")), list(...)))
  expected <- secs(eval(sortexpr))
  cat(" secs=", expected, "  ", paste(names(list(...)), list(...), sep = "="), "\n", sep = "")
  sleepexpr <- call("Sys.sleep", expected)
  retemp <- rettmp <- NULL
  for (r in seq_len(repl)) {
    cat(r, data, algo)
    set.seed(r)
    x <- testdb$func[[data]](size)
    if (algodb[algo,"varying"])
      x <- format(as.integer(x))
    emp <- tmp <- NULL
    i <- 0L
    while (length(emp) <= 3 || length(tmp) <= 3) {
      i <- i + 1L
      cat("", i)
      y <- force(x[])
      sortexpr <- as.call(c(list(as.name(algodb[algo,"name"]), as.name("y")), list(...)))
      emp <- measures(pass = pass, file = file, expr = sleepexpr, expected = expected)
      tmp <- measures(pass = pass, file = file, expr = sortexpr, expected = expected)
    }
    deb <- debias(sleep = emp, task = tmp)
    cat(" secs=", tmp["runtime"], " sleep=", emp["energy-pkg"], " task=", tmp["energy-pkg"], " debiased=", deb["energy-pkg"], "\n", sep = "")
    retemp <- rbind(retemp, emp)
    rettmp <- rbind(rettmp, tmp)
  }
  rownames(retemp) <- 1:r
  rownames(rettmp) <- 1:r
  list(sleep = retemp, task = rettmp)
}



#' Measuring one algorithm in one data setting
#'
#' For each replication \code{r in 0:repl} test \code{data} sets is created with \code{set.seed(r)}.
#' Data number 0 is used to determine the expected runtime of \code{algo}.
#' For each of the remaining \code{1:repl} data sets \code{\link{measure1}} is called twice,
#' once measuring a sleep command of the expected duration
#' and once measuring algorithm \code{algo} on the data.
#'
#' @param algo label of the algorithm as in \code{\link{algodb}$label}
#' @param data name of the test data pattern as in \code{\link{testdb}$name}
#' @param size number of test elements to be sorted
#' @param repl the replications number (for \code{\link{set.seed}})
#' @param pass the root password for \code{sudo}
#' @param file the name of the file into which \code{perf} is redirected
#' @param expected the expected runtime in seconds
#' @param \dots further arguments passed to the algorithm
#'
#' @return a list with two components
#' \item{sleep}{the sleep measurement: a vector returned by \code{\link{measures}}}
#' \item{task}{the algorithm mesasurement: a vector returned by \code{\link{measures}}}
#'
#' @note The recommended correction for background
#' If the algorithm is size-varying the data is coerced to integer and then to character.
#' Note that the size-varing algorithms return their pure sorting time
#' but have relevant overhead for pre- and post-processing the strings
#' which is included in this measurement,
#' to correct for this bias measure also \code{\link{UVWoverhead}}.
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' n <- 2^23
#' a <- "Frog1"
#' d <- "tieboot"
#' m1 <- measure1(.sudopw, ".perf.txt", a, d, n, 1)
#' m2 <- measure1(.sudopw, ".perf.txt", a, d, n, 2, expected=m1$task["runtime"])
#' m1 <- measure1(.sudopw, ".perf.txt", "VFrog1", "tieboot", 2^23, 1)
#' m2 <- measure1(.sudopw, ".perf.txt", "VFrog1", "tieboot", 2^23, 2, expected=m1$task["runtime"])
#' rm(.sudopw)
#' }

measure1 <- function(pass , file, algo, data, size, repl=1, expected=NULL, ...){
  if (is.null(expected)){
    cat(0, data, algo, 1)
    set.seed(0)
    x <- testdb$func[[data]](size)
    if (algodb[algo,"varying"])
      x <- num2str(x)
    sortexpr <- as.call(c(list(as.name(algodb[algo,"name"]), as.name("x")), list(...)))
    expected <- secs(eval(sortexpr))
    cat(" secs=", expected, "  ", paste(names(list(...)), list(...), sep = "="), "\n", sep = "")
  }
  sleepexpr <- call("Sys.sleep", expected)
  cat(repl, data, algo)
  set.seed(repl)
  x <- testdb$func[[data]](size)
  if (algodb[algo,"varying"])
    x <- num2str(x)
  emp <- tmp <- NULL
  i <- 0L
  while (length(emp) <= 3 || length(tmp) <= 3) {
    i <- i + 1L
    cat("", i)
    y <- force(x[])
    sortexpr <- as.call(c(list(as.name(algodb[algo,"name"]), as.name("y")), list(...)))
    emp <- measures(pass = pass, file = file, expr = sleepexpr, expected = expected)
    tmp <- measures(pass = pass, file = file, expr = sortexpr, expected = expected)
  }
  deb <- debias(sleep = emp, task = tmp)
  cat(" expe=", expected, " secs=", tmp["runtime"], " sleep=", emp["energy-pkg"], " task=", tmp["energy-pkg"], " debiased=", deb["energy-pkg"], "\n", sep = "")
  list(sleep = emp, task = tmp)
}


#' Measuring one algorithm in one data setting
#'
#' For each replication \code{r in 0:repl} test \code{data} sets is created with \code{set.seed(r)}.
#' Data number 0 is used to determine the expected runtime of \code{algo}.
#' For each of the remaining \code{1:repl} data sets \code{\link{measure1}} is called.
#'
#' @param pass the root password for \code{sudo}
#' @param file the name of the file into which \code{perf} is redirected
#' @param algo label of the algorithm as in \code{\link{algodb}$label}
#' @param data name of the test data pattern as in \code{\link{testdb}$name}
#' @param size    number of test elements to be sorted
#' @param repl the replications id (only for reporting)
#' @param ret optional a list with partial results
#' @param imag NULL or a filename for saving the image
#' @param \dots further arguments passed to the algorithm
#'
#' @return a list with \code{repl} components
#' each with \code{data} components
#' each with \code{algo} components
#' each with two components
#' \item{sleep}{the sleep measurement: a vector returned by \code{\link{measures}}}
#' \item{task}{the algorithm mesasurement: a vector returned by \code{\link{measures}}}
#'
#' @note The recommended correction for background
#' If the algorithm is size-varying the data is coerced to integer and then to character.
#' Note that the size-varing algorithms return their pure sorting time
#' but have relevant overhead for pre- and post-processing the strings
#' which is included in this measurement,
#' to correct for this bias measure also \code{\link{UVWoverhead}}.
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' l <- measurea(.sudopw, ".perf.txt", "Frog1", "asclocal", 2^23, 2, imag="measurea.RData")
#' l <- measurea(.sudopw, ".perf.txt", c("Frog1", "Frog2"), c("asclocal", "ascglobal"), 2^23, 3, imag="measurea.RData", ret=l)
#' l <- measurea(.sudopw, ".perf.txt", "Ska", "tiesqrt", 2^24, 50)
#' rm(.sudopw)
#' }

measurea <- function(pass, file, algo, data, size, repl, ret = list(), imag = NULL, ...){
  names(algo) <- algo
  names(data) <- data
  delayedAssign("lalg", lapply(algo, function(x)NULL))
  delayedAssign("ldat", lapply(data, function(x)lalg))
  lret <- length(ret)
  if (!lret){
    ret <- lapply(seq_len(repl), function(x)ldat)
  }else{
    tmp <- vector("list", repl)
    for (r in seq_len(repl)){
      if (lret < r || is.null(ret[[r]])){
        tmp[[r]] <- ldat
      }else{
        for (d in data){
          if (is.null(ret[[r]][[d]])){
            tmp[[r]][[d]] <- lalg
          }else{
            for (a in algo){
              if (!is.null(ret[[r]][[d]][[a]])){
                tmp[[r]][[d]][[a]] <- ret[[r]][[d]][[a]]
              }
            }
          }
        }
      }
    }
    ret <- tmp
    rm(tmp)
  }
  gc()
  for (r in seq_len(repl)){
    for (d in data){
      for (a in sample(algo)){
        if (is.null(ret[[r]][[d]][[a]])){
          if (!is.null(ret[[1]][[d]][[a]])){
            expected <- median(unlist(lapply(ret[seq_len(r)], function(x)x[[d]][[a]]$task[["runtime"]])), na.rm=TRUE)
          }else{
            expected <- NA
          }
          if (is.na(expected)){
            ret[[r]][[d]][[a]] <- measure1(pass = pass, file = file, algo = a, data = d, size = size, repl = r, ...)
          }else{
            ret[[r]][[d]][[a]] <- measure1(pass = pass, file = file, algo = a, data = d, size = size, repl = r, expected = expected, ...)
          }
        }
      }
      if (!is.null(imag))
        save(ret, file=imag)
      gc()
    }
  }
  ret
}


#' Debias measurement
#'
#' Removes sleep-cost from task-cost
#' and optionally assigns 1/p of proportional sleep costs during runtime
#' where p is the number of physical cores (not hyperthreaded cores)
#'
#' @param measurement either a list with two components \code{sleep} and \code{task} or the following two parameters
#' @param sleep background-costs: a vector of measurements from \code{\link{measure1}} or a matrix of measurements from \code{\link{measures}}
#' @param task algorithm-costs: a vector of measurements from \code{\link{measure1}} or a matrix of measurements from \code{\link{measures}}
#' @param with.background TRUE to add back 1/p of the sleep costs during runtime (default FALSE)
#' @param used.cores number of cores used (default 1)
#' @param available.cores number of cores available (default number of physical cores, see \code{\link{perfcores}})
#'
#' @return the debiased task input (without \code{seconds-elapsed})
#' @note that this measurement and correction requires relatively large sorting tasks
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' m <- measure("Frog1","tieboot",2^22,5,.sudopw,".perf.txt")
#' d <- debias(m)
#' rbind(
#'   s=apply(m$sleep,2,median)
#' , t=apply(m$task,2,median)
#' , d=apply(d,2,median)
#' )
#' rm(.sudopw)
#' }
debias <- function(
  measurement = NULL
, sleep=measurement$sleep
, task=measurement$task
, with.background=FALSE
, used.cores = 1
, available.cores = perfcores(FALSE)
){
  if (is.matrix(sleep)) {
    msleep <- apply(sleep, 2, median)
  }else{
    msleep <- sleep
  }
  if (is.matrix(task)) {
    sleep <- t(task)
    sleep[] <- msleep
    sleep <- t(sleep)
    sleep <- sleep*(task[,"seconds-elapsed"]/sleep[,"seconds-elapsed"])
    ret <- task - sleep + if (with.background) sleep*(task[,"runtime"]/sleep[,"seconds-elapsed"]*used.cores/available.cores) else 0
    rel <- ret / pmax(task, sleep)
    rel <- rel[,-match(c("seconds-elapsed"), colnames(rel))]
    ret <- ret[,-match(c("seconds-elapsed"), colnames(ret))]
    chk <- as.matrix(apply(rel,2,function(x){summary(x)[c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")]}))
    neg <- !is.na(chk) & chk < -0.01
    if (any(neg)){
      warning("negative values indicate overcorrection, try larger samplesize ", paste(outer(rownames(chk), colnames(chk), function(i,j){ifelse(neg[cbind(i,j)],paste("|",i,j,sep = ""),"")  }), collapse = ""), "\n")
    }
  }else{
    msleep <- msleep*(task["seconds-elapsed"]/sleep["seconds-elapsed"])
    ret <- task - msleep + if (with.background) msleep*(task["runtime"]/msleep["seconds-elapsed"]*used.cores/available.cores) else 0
    ret <- ret[-match(c("seconds-elapsed"), names(task))]
  }
  ret
}



#' Debias varying (UWV) measurement
#'
#' Removes overhead costs from measuerment costs (except for the mesaurements from \code{\link{perf}})
#'
#' @param measurement a vector of measurements from \code{\link{measure1}} or a matrix of measurements from \code{\link{measures}} of a size varying algorithm
#' @param overhead a vector of measurements from \code{\link{measure1}} or a matrix of measurements from \code{\link{measures}} of \code{\link{UVWoverhead}}
#'
#' @return the debiased measurement input
#' @note that this measurement and correction requires relatively large sorting tasks
#' @export
#'
#' @examples
#' \donttest{
#' .sudopw <- rstudioapi::askForPassword("sudopw")
#' m <- debias(measure("WQuick2","tieboot",2^22,3,.sudopw,".perf.txt"))
#' o <- debias(measure("UVW","tieboot",2^22,3,.sudopw,".perf.txt"))
#' d <- debiasUVW(m,o)
#' rbind(
#'   m=apply(m,2,median)
#' , o=apply(o,2,median)
#' , d=apply(d,2,median)
#' )
#' rm(.sudopw)
#' }
debiasUVW <- function(measurement, overhead) {
  p <- c("memory","runtime","footprint")
  if (is.matrix(overhead)) {
    overhead <- apply(overhead, 2, median)
  }
  if (is.matrix(measurement)) {
    ret <- t(t(measurement) - overhead)
    ret[,p] <- measurement[,p]
    chk <- as.matrix(apply(ret,2,function(x){summary(x)[c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")]}))
    neg <- !is.na(chk) & chk < 0
    if (any(neg))
      warning("negative values indicate overcorrection, try larger samplesize ", paste(outer(rownames(chk), colnames(chk), function(i,j){ifelse(neg[cbind(i,j)],paste("|",i,j,sep = ""),"")  }), collapse = ""), "\n")
  }else{
    ret <- measurement - overhead
    ret[p] <- measurement[p]
  }
  ret
}
