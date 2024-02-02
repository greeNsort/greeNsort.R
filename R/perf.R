# greeNsort performance measurement
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

#' Default number of cores
#'
#' central interface defining the number of available cores
#'
#' @param logical TRUE to get hyperthreads, false to get physical cores (at least under linux)
#'
#' @return integer number of cores
#' @seealso \code{\link{perf}}
#' @export
#'
#' @examples
#' perfcores()
#' perfcores(FALSE)
#' perfcores(TRUE)
#' #PKnuthsort(runif(), threads = perfcores(FALSE))
#'
perfcores <- function(logical=TRUE){
  if (logical)
    parallel::detectCores()
  else
    RhpcBLASctl::get_num_cores()
}


#' Powercap paths on this machine
#'
#' Return paths to the powercap rapl directories on this machine
#'
#' Currently checks for four potential paths on a one-socket Intel or AMD machine.
#' Non-existing paths are returned as `NA`.
#'
#' @return a character vector of checked paths with the discovered names attached
#' @seealso \code{\link{perfcompiled}},  \code{\link{perfcheck}},  \code{\link{perfconfig}},  \code{\link{perf}}
#' @export
#'
#' @examples
#' perfmachine()
perfmachine <- function(){
  rapls <- c("package","core","uncore","dram")
  paths <- c("/sys/class/powercap/intel-rapl/intel-rapl:0/"
             , "/sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:0/"
             , "/sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:1/"
             , "/sys/class/powercap/intel-rapl/intel-rapl:0/intel-rapl:0:2/"
  )
  rapl_names <- sapply(paths, function(p){
    x <- tryCatch(readLines(paste0(p, "name"))
                  , warning = function(e) {
                    message(paste("NOTE: powercap path", p, "set to NA"))
                    return(NA)
                  }
    )
  })
  rapl_names <- sub("-.+", "", rapl_names)
  config <- paths[match(rapls, rapl_names)]
  names(config) <- rapls
  config
}


#' Compiled powercap paths
#'
#' Return compiled paths to the powercap rapl directories
#'
#' @return a character vector with three-elements
#' \item{package}{path to directory of PACKAGE counter}
#' \item{cores}{path to directory of CORES counter}
#' \item{dram}{path to directory of DRAM counter}
#' @seealso \code{\link{perfmachine}},  \code{\link{perfcheck}},  \code{\link{perfconfig}},  \code{\link{perf}}
#' @export
#'
#' @examples
#' perfcompiled()
perfcompiled <- function(
)
{
  ret <- .Call(C_r_perfpaths)
  names(ret) <- c("package","core","uncore","dram")
  ret
}


#' Checking performance configuration
#'
#' Throws a warning if the powercap rapl configuration does not fit to the machine
#'
#' @return an `TRUE` if the compiled comfiguration matches the machine, otherwise `FALSE`
#' @seealso \code{\link{perfmachine}},  \code{\link{perfcompiled}},  \code{\link{perfconfig}},  \code{\link{perf}}
#' @export
#'
#' @examples
#' perfcheck()
perfcheck <- function(){
  machine <- perfmachine()
  compiled <- perfcompiled()
  identical(machine, compiled)
}


#' Configure performance configuration
#'
#' determine the paths for `package`, `core`, `dram` and overwrite the
#' C-header file which configures the paths to the powercap rapl directories
#'
#' Call this in the package directory, then re-compile the package.
#'
#' @return path to the written config file
#' @seealso \code{\link{perf}}
#' @export
#'
#' @examples
#' \dontrun{
#'   perfconfig()
#' }
perfconfig <- function(){
  config <- perfmachine()
  reconfig <- ifelse(is.na(config)
                     , paste0('#undef PATH_POWERCAP_', toupper(names(config)))
                     , paste0('#define PATH_POWERCAP_', toupper(names(config)), ' "', config, '"')
  )
  writeLines(reconfig, "src/powercap_config.h")
  cat(reconfig, sep="\n")
  file.path(getwd(), "src/powercap_config.h")
}


#' Granting access to performance counters
#'
#' Determines the configured paths to the powercap rapl directories and executes the grants with `sudo`.
#' You need to know the sudo password.
#'
#' After rebooting a machine the grants on the virtual powercap rapl directories are lost and need to be granted again.
#' This can either be done by root in a startup-script or here by calling `perfgrants`.
#'
#' @return invisible vector of grants
#' @seealso \code{\link{perf}}
#' @export
#'
#' @examples
#' \dontrun{
#'   perfgrants()
#' }
perfgrants <- function(){
  paths <- perfcompiled()
  paths <- paths[!is.na(paths)]
  grants <-paste0("sudo chmod 444 ", paths, "energy_uj", ";\n", collapse="")
  invisible(system2(command = "pkexec"
  , args = paste0("env DISPLAY=$DISPLAY XAUTHORITY=$XAUTHORITY bash -c "
    ,"'",grants, "'")
    , stdout = TRUE
    , stderr = TRUE)
  )
  cat(grants)
  invisible(grants)
}


#' Return performance maximum
#'
#' Return the maximum powercap RAPL counters
#'
#' @return a double vector with five elements
#' \item{time}{\code{\link{Inf}}}
#' \item{package}{PACKAGE maximum in Joule}
#' \item{core}{CORE maximum in Joule}
#' \item{uncore}{UNCORE maximum in Joule}
#' \item{dram}{DRAM maximum in Joule}
#' @seealso \code{\link{perfnow}}, \code{\link{perfmon}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' perfmax()
perfmax <- function(
)
{
  ret <- .Call(C_r_perfmax)
  names(ret) <- c("time","package","core","uncore","dram")
  ret
}




#' Get performance counters
#'
#' Return the current powercap RAPL counters
#'
#' @return a double vector with four elements
#' \item{time}{\code{\link{proc.time}[[3]]} in seconds}
#' \item{package}{PACKAGE counter in Joule}
#' \item{core}{CORE counter in Joule}
#' \item{uncore}{UNCORE counter in Joule}
#' \item{dram}{DRAM counter in Joule}
#' @seealso \code{\link{perfmax}}, \code{\link{perfmon}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' perfnow()
perfnow <- function(
)
{
  ret <- .Call(C_r_perfnow)
  ret[1] <- proc.time()[[3]]
  names(ret) <- c("time","package","core","uncore","dram")
  ret
}

#' Measure end of interval and return perf
#'
#' Return time in seconds and energies in Joule since interval begin `x`
#'
#' Handles RAPL counter overflow assuming it won't happen twice
#'
#' @param x a measurement of interval begin returned by \code{\link{perfnow}}
#' @param rowname optional algoname for the returned matrix
#' @param n optional number of elements
#' @param b optional number of bytes per element
#' @param p optional number of processes
#' @param t optional number of threads
#' @param size optional \%RAM
#' @return one-row matrix like \code{\link{rawperf}}
#' @seealso  \code{\link{perfcalibrate}} and \code{\link{perf}}
#' @seealso \code{\link{perfmax}}, \code{\link{perfmon}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' message('Custom idle measurement')
#' p <- perfnow()
#' Sys.sleep(2)
#' perfthen(p)
perfthen <- function(
  x
, rowname = NA
, n = NA
, b = 8
, p = 1
, t = 1
, size = NA
)
{
  force(x) # in case perfnow() is called within x
  y <- .Call(C_r_perfnow)
  y[1] <- proc.time()[[3]]
  d <- y-x
  # handle wrap-around from x to y assuming it will not happen twice
  i <- d<0
  if (any(i)){
    if (d[1]<0)
      warning("time overflow not handled properly (Inf)")
    d[i] <- y[i] - (x[i] - perfmax()[i])
  }
  ret <- c(n, b, p, t, size, d)
  dim(ret) <- c(1, 10)
  dimnames(ret) <- list(as.character(rowname), c("n","b","p","t","size","secs","base","core","unco","dram"))
  ret[,"base"] <- ret[,"base"] - sum(ret[,c("core","unco")])
  ret
}


#' Performance Monitor
#'
#' A simple Performance Monitor
#'
#' The first `refsecs` seconds it generates a baseline measurement,
#' and then reports each `monsecs` seconds until interrupted
#'
#' @param refsec number of seconds to generate baseline estimate before monitoring
#' @param monsec number of seconds to sleep between reporting change
#' @param maxsec maximum number of seconds to monitor
#'
#' @return not returned but printed: a matrix reporting percentages with 2 rows
#' \item{curr/max}{Percent current values relative to maximum values: allows to monitor counter overflow (<<100 expected)}
#' \item{diff/base}{Percent incremental values relative to baseline: allows to monitor current energy (>=100 expected)}
#' and 4 columns
#' \item{package}{RAPL PACKAGE (socket including CORE+UNCORE+REST)}
#' \item{package}{RAPL CORE (CPU only)}
#' \item{package}{RAPL UNCORE (GPU only)}
#' \item{package}{RAPL DRAM (RAM only)}
#' @seealso \code{\link{perfnow}}, \code{\link{perfmax}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' perfmon(refsec=30, monsec=1, maxsec=3)
perfmon <- function(refsec=5, monsec=1, maxsec=Inf){
  m <- perfmax()
  nam <- names(m)
  now <- perfnow()
  Sys.sleep(refsec)
  lst <- perfnow()
  bas <- (lst - now)/refsec*monsec
  stasec <- proc.time()[[3]]
  while((proc.time()[[3]]-stasec) < maxsec){
    Sys.sleep(monsec)
    now <- perfnow()
    fil <- now/m
    fil <- fil[!is.na(fil)]
    if (any(fil > 0.99))
      warning("close to overflow")
    dif <- now - lst
    print(round(rbind("curr/max"=100*now/m, "diff/base"=100*dif/bas)))
    lst <- now
  }
}



#' Make perf return value
#'
#' takes a return value with measurements of the R/C-glue layer and returns it with proper \code{\link{dim}} and \code{\link{dimnames}}
#'
#' @param x the return value of a C-level sorting call with an attribute `perf`
#' @param rowname the rowname to be used
#'
#' @return measurements as a matrix with columns
#' \item{n}{number of elements}
#' \item{b}{(average) number of bytes per element}
#' \item{p}{number of cores}
#' \item{size}{average \%RAM}
#' \item{secs}{runTime in Seconds}
#' \item{base}{base energy in Joules}
#' \item{core}{core energy in Joules}
#' \item{unco}{uncore (GPU) energy in Joules}
#' \item{dram}{DRAM energy in Joules}
#' @export
#'
retperf <- function(x, rowname=""){
  structure(as.vector(attr(x, 'perf')), dim=c(1, 10), dimnames=list(rowname, c("n","b","p","t","size","secs","base","core","unco","dram")))
}



#' Measure background energy
#'
#' Measure background energy of a sleep time
#'
#' @param x number of seconds to sleep
#' @return like \code{\link{rawperf}}
#' @seealso  \code{\link{perfcalibrate}} and \code{\link{perf}}
#' @export
#'
#' @examples
#' perfsleep(1)
#'
perfsleep <- function(
    x
)
{
  stopifnot(is.numeric(x) && length(x)==1 && x >= 0)
  ret <- .Call(C_r_perfsleep, x=as.double(x))
  ret <- retperf(ret, "perfsleep")
  ret
}


#' Calibrate for background energy
#'
#' Performs `n` sleep measures with \code{\link{perfsleep}},
#' fits linear models \code{\link{lm}} (without intercept) predicting energy-measurements
#' from (measured) sleep seconds.
#'
#' The return object is saved to the \code{\link{globalenv}} under the name \code{.perfbackground}
#'
#' @param n number of measurements (length of \code{\link{seq}})
#' @param minsec minimum seconds to measure (begin of \code{\link{seq}})
#' @param maxsec maximum seconds to measure (end of \code{\link{seq}})
#' @param plot TRUE for plotting the models FALSE for not
#' @param save TRUE for saving .perfbackground in 'perfbackground.RData'
#'
#' @return \code{invisible} with side-effect in \code{\link{globalenv}} hidden environment \code{.perfbackground} with three components
#' \item{data}{a dataframe with columns of \code{\link{retperf}}}
#' \item{fits}{a list with three elements \code{base}, \code{core} and \code{dram} containing the linear models }
#' \item{pred}{a list with three elements \code{base}, \code{core} and \code{dram} containing the prediction functions }
#' @export
#'
#' @examples
#' perfcalibrate(n=10, minsec=0, maxsec=2, plot = TRUE)
perfcalibrate <- function(n=300, minsec=0, maxsec=24, plot = TRUE, save = FALSE){
  s <- sample(seq(minsec, maxsec, length=n))
  data <- as.data.frame(do.call("rbind", lapply(s, function(s)perfsleep(s))), row.names=FALSE)
  .perfbackground <- new.env()
  assign("data", data, envir = .perfbackground)
  rm(data)
  assign("fits", list(
    base = lm(base~secs-1, data=.perfbackground$data)
    , core= lm(core~secs-1, data=.perfbackground$data)
    , dram = lm(dram~secs-1, data=.perfbackground$data)
  ), envir = .perfbackground)
  assign("pred", list(
    base = function(x)predict(.perfbackground$fits$base, data.frame(secs=x))
  , core= function(x)predict(.perfbackground$fits$core, data.frame(secs=x))
  , dram = function(x)predict(.perfbackground$fits$dram, data.frame(secs=x))
  ), envir = .perfbackground)
  assign(".perfbackground"
         , .perfbackground
         , globalenv()
  )
  if (save)
    save(.perfbackground, file="perfbackground.RData")
  rm(.perfbackground)
  if (plot){
    matplot(.perfbackground$data[,"secs"], .perfbackground$data[,c("base","core","dram")]
            , pch=c("b","c","d")
            , col=1:3
            , xlim = c(0, max(.perfbackground$data[,"secs"]))
            , ylim = c(0, max(.perfbackground$data[,c("base","core","dram")]))
            , xlab="secs", ylab="joule"
    )
    abline(.perfbackground$fits$base, col=1)
    abline(.perfbackground$fits$core, col=2)
    abline(.perfbackground$fits$dram, col=3)
  }
  invisible()
}



#' Extract raw energy measurement
#'
#' The energy values stem from RAPL counters
#' of the powercap kernel module. RAPL delivers
#' a `dram` measure, a `core` measure,
#' and a `package` (socket) measure which contains
#' the `core` measure. `base` is calculated as
#' `package - core`.
#'
#' Note that the RAPL counters are incremented at certain points
#' in time, hence the measured counter difference can be zero.
#'
#' Note that the RAPL counters can overflow for long-running tasks.
#' Our code detects and corrects overflow, for a single overflow
#' this still leads to correct results, for multiple overflows not.
#'
#' Note that the kernel updates the counters in different points
#' in time, hence for short measurements `base` can become negative.
#' You can handle that either statistically aggregating over many
#' measurements or by measuring larger tasks.
#'
#' @param x see \code{\link{perf}}
#' @return a \code{\link{retperf}}
#' @seealso \code{\link{rawperf}},  \code{\link{bacperf}}, \code{\link{difperf}}, \code{\link{adjperf}}, \code{\link{amdperf}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' rawperf(Insertionsort(runif(1000)))
#' # statistical median and mean are not negative
#' summary(do.call("rbind", lapply(1:1000, function(i){
#'   rawperf(Insertionsort(runif(100)))
#' }))[,-(1:3)])
#' # bigger task has no negative values
#' summary(do.call("rbind", lapply(1:100, function(i){
#'   rawperf(Insertionsort(runif(10000)))
#' }))[,-(1:3)])
#'
rawperf <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    x
  else
    a
}

#' Extract background energy cost estimation
#'
#' The energy values are estimates of `.perfbackground`
#' created by \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return raw measurements, see \code{\link{retperf}}
#' @seealso \code{\link{rawperf}},  \code{\link{bacperf}}, \code{\link{difperf}}, \code{\link{adjperf}}, \code{\link{amdperf}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' ret <- Insertionsort(runif(1000))
#' rbind(
#'   rawperf(ret)
#' , bacperf(ret)
#' )[,c("base","core","dram")]
bacperf <- function(x){
  if (!exists(".perfbackground", envir=globalenv())){
    if (file.exists("perfbackground.RData")){
      load("perfbackground.RData", envir = globalenv())
    }else{
      if (basename(getwd()) == "greeNsort.Rcheck"){
        message("dummy calibration for examples during R CMD check")
        perfcalibrate(10, 0, 2, save=TRUE)
      }else
        stop("neither .perfbackground in globalenv() nor perfbackground.RData, calibrate your own machine!")
    }
  }
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  s <- a[,"secs"]
  background <- cbind(
    base=.perfbackground$pred$base(s)
    , core=.perfbackground$pred$core(s)
    , dram=.perfbackground$pred$dram(s)
  )
  a[,c("base","core","dram")] <- background
  a
}

#' Extract energy cost minus background
#'
#' The energy values are the difference between
#' raw measurements (\code{\link{rawperf}}) and
#' background estimates (\code{\link{bacperf}})
#'
#' @param x see \code{\link{perf}}
#' @return measurements minus background in the form of \code{\link{retperf}}
#' @seealso \code{\link{rawperf}},  \code{\link{bacperf}}, \code{\link{difperf}}, \code{\link{adjperf}}, \code{\link{amdperf}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' e <- c("base","core","dram")
#' ret <- Insertionsort(runif(1000))
#' rbind(
#'   rawperf(ret) - bacperf(ret)
#' , difperf(ret)
#' )[,e]
#' all.equal(sum(rawperf(ret)[,e]), sum(difperf(ret)[,e]))
difperf <- function(x){
  if (!exists(".perfbackground", envir=globalenv())){
    if (file.exists("perfbackground.RData")){
      load("perfbackground.RData", envir = globalenv())
    }else{
      if (basename(getwd()) == "greeNsort.Rcheck"){
        message("dummy calibration for examples during R CMD check")
        perfcalibrate(10, 0, 2, save=TRUE)
      }else
        stop("neither .perfbackground in globalenv() nor perfbackground.RData, calibrate your own machine!")
    }
  }
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  s <- a[,"secs"]
  baseline <- cbind(
    base=.perfbackground$pred$base(s)
    , core=.perfbackground$pred$core(s)
    , dram=.perfbackground$pred$dram(s)
  )
  measure <- a[,c("base","core","dram")]
  adj <- measure - baseline
  a[,c("base","core","dram")] <- adj
  a
}

#' Extract adjusted energy cost
#'
#' The energy values are adjusted such that
#' background is shifted to the `base` component,
#' i.e. `core` and `dram` are minus background
#' and `base` is plus background of `core` and `dram`.
#'
#' @param x see \code{\link{perf}}
#' @return measurements adjusted for background in the form of \code{\link{retperf}}
#' @seealso \code{\link{rawperf}},  \code{\link{bacperf}}, \code{\link{difperf}}, \code{\link{adjperf}}, \code{\link{amdperf}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' e <- c("base","core","dram")
#' ret <- Insertionsort(runif(1000))
#' rbind(
#'   rawperf(ret)
#' , bacperf(ret)
#' , difperf(ret)
#' , adjperf(ret)
#' )[,e]
#' all.equal(sum(rawperf(ret)[,e]), sum(adjperf(ret)[,e]))
adjperf <- function(x){
  if (!exists(".perfbackground", envir=globalenv())){
    if (file.exists("perfbackground.RData")){
      load("perfbackground.RData", envir = globalenv())
    }else{
      if (basename(getwd()) == "greeNsort.Rcheck"){
        message("dummy calibration for examples during R CMD check")
        perfcalibrate(10, 0, 2, save=TRUE)
      }else
        stop("neither .perfbackground in globalenv() nor perfbackground.RData, calibrate your own machine!")
    }
  }
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  s <- a[,"secs"]
  baseline <- cbind(
    base=.perfbackground$pred$base(s)
    , core=.perfbackground$pred$core(s)
    , dram=.perfbackground$pred$dram(s)
  )
  measure <- a[,c("base","core","dram")]
  adj <- measure - baseline
  adj[,"base"] <- adj[,"base"] + rowSums(baseline)
  a[,c("base","core","dram")] <- adj
  a
}

#' Extract adjusted energy cost for AMD
#'
#' The energy values are adjusted such that
#' the `base` component is added to the `core` component
#' and `base` is set to zero, in measurement and baseline
#' then the same adjustment of  \code{\link{adjperf}} is done.
#'
#' @param x see \code{\link{perf}}
#' @return measurements adjusted for background in the form of \code{\link{retperf}}
#' @seealso \code{\link{rawperf}},  \code{\link{bacperf}}, \code{\link{difperf}}, \code{\link{adjperf}}, \code{\link{amdperf}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' e <- c("base","core","dram")
#' ret <- Insertionsort(runif(1000))
#' rbind(
#'   rawperf(ret)
#' , bacperf(ret)
#' , difperf(ret)
#' , adjperf(ret)
#' , amdperf(ret)
#' )[,e]
#' all.equal(sum(rawperf(ret)[,e]), sum(adjperf(ret)[,e]))
amdperf <- function(x){
  if (!exists(".perfbackground", envir=globalenv())){
    if (file.exists("perfbackground.RData")){
      load("perfbackground.RData", envir = globalenv())
    }else{
      if (basename(getwd()) == "greeNsort.Rcheck"){
        message("dummy calibration for examples during R CMD check")
        perfcalibrate(10, 0, 2, save=TRUE)
      }else
        stop("neither .perfbackground in globalenv() nor perfbackground.RData, calibrate your own machine!")
    }
  }
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  s <- a[,"secs"]
  baseline <- cbind(
    base=0
    , core=.perfbackground$pred$base(s) + .perfbackground$pred$core(s)
    , dram=.perfbackground$pred$dram(s)
  )
  measure <- cbind(
      base=0
    , core=a[,"base"] + a[,"core"]
    , dram=a[,"dram"]
  )
  adj <- measure - baseline
  adj[,"base"] <- adj[,"base"] + rowSums(baseline)
  a[,c("base","core","dram")] <- adj
  a
}



#' Automatically switch energy calculation
#'
#' Depending on \code{\link{getOption}} `greensort_perf_calc`
#' in `c('raw','dif','adj','amd')` extracts one of \code{\link{rawperf}},
#' \code{\link{difperf}} or \code{\link{adjperf}}
#'
#' @param x see \code{\link{perf}}
#' @return \code{\link{perf}}
#' @seealso   \code{\link{bacperf}}, \code{\link{perfcalibrate}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' getOption('greensort_perf_calc')
#' options(greensort_perf_calc='raw')
#' optperf(Insertionsort(runif(1000)))
optperf <- function(x){
  switch( as.character(getOption("greensort_perf_calc", default="raw"))
          , raw = rawperf(x)
          , dif = difperf(x)
          , adj = adjperf(x)
          , amd = amdperf(x)
          , stop("option greensort_perf_calc is not one of 'raw'|'dif'|'adj'|'amd'")
  )
}



#' Measurement names (without units)
#'
#' Returns Measurement names without units
#'
#' @param x a a vector of performance names such as \code{\link{colnames}} from \code{\link{perf}}
#'
#' @return a vector of names without units
#' @seealso \code{\link{perfnames}} and \code{\link{perflabels}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' p <- aperf(Insertionsort(runif(10000)))
#' data.frame(perf=colnames(p), labels=perflabels(colnames(p)), labels=perfnames(colnames(p)), labels=perfunits(colnames(p)))
#' p
#' colnames(p) <- perfnames(colnames(p))
#' p
perfnames <- function(x){
  ret        <- c("%RAM", "runTime", "lifeTime", "cpuTime", "bEnergy", "cEnergy", "dEnergy", "cdEnergy", "pcdEnergy", "bcdEnergy", "tFootprint", "cdFootprint", "pcdFootprint", "bcdFootprint")
  names(ret) <- c("%RAM", "runTime", "lifeTime", "cpuTime", "bEnergy", "cEnergy", "dEnergy", "cdEnergy", "pcdEnergy", "bcdEnergy", "tFootprint", "cdFootprint", "pcdFootprint", "bcdFootprint")
  ret[x]
}

#' Measurement labels
#'
#' Returns short Measurement labels
#'
#' @param x a a vector of performance names such as \code{\link{colnames}} from \code{\link{perf}}
#'
#' @return a vector of names without units
#' @seealso \code{\link{perfnames}} and \code{\link{perflabels}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' p <- aperf(Insertionsort(runif(10000)))
#' data.frame(perf=colnames(p), labels=perflabels(colnames(p)), labels=perfnames(colnames(p)), labels=perfunits(colnames(p)))
#' p
#' colnames(p) <- perfnames(colnames(p))
#' p
perflabels <- function(x){
  ret        <- c("%M"  , "rT"     , "lT"      , "cT"     , "bE"     , "cE"     , "dE"     , "cdE"     , "pcdE"     , "bcdE"     , "tF"        , "cdF"        , "pcdF"        , "bcdF"        )
  names(ret) <- c("%RAM", "runTime", "lifeTime", "cpuTime", "bEnergy", "cEnergy", "dEnergy", "cdEnergy", "pcdEnergy", "bcdEnergy", "tFootprint", "cdFootprint", "pcdFootprint", "bcdFootprint")
  ret[x]
}



#' Measurement names with units
#'
#' Returns long Measurement names including units
#'
#' @param x a a vector of performance names such as \code{\link{colnames}} from \code{\link{perf}}
#'
#' @return a vector of names with units
#' @seealso \code{\link{perfnames}} and \code{\link{perflabels}}, \code{\link{perf}}
#' @export
#'
#' @examples
#' p <- aperf(Insertionsort(runif(10000)))
#' data.frame(perf=colnames(p), labels=perflabels(colnames(p)), labels=perfnames(colnames(p)), labels=perfunits(colnames(p)))
#' p
#' colnames(p) <- perfunits(colnames(p))
#' t(p)
perfunits <- function(x){
  ret <- c("%RAM ((data+buffer)/data)", "runTime (Seconds)", "lifeTime (Seconds)", "cpuTime (Seconds)"
           , "bEnergy (Joules)", "cEnergy (Joules)", "dEnergy (Joules)", "cdEnergy (Joules)", "pcdEnergy (Joules)", "bcdEnergy (Joules)"
           , "tFootprint (%RAM*Seconds)", "cdFootprint (%RAM*Joules)", "pcdFootprint (%RAM*Joules)", "bcdFootprint (%RAM*Joules)")
  names(ret) <- c("%RAM", "runTime", "lifeTime", "cpuTime", "bEnergy", "cEnergy", "dEnergy", "cdEnergy", "pcdEnergy", "bcdEnergy", "tFootprint", "cdFootprint", "pcdFootprint", "bcdFootprint")
  ret[x]
}



#' Add (raw) performance measurements
#'
#' \code{\link{rbind}s} all arguments and aggregates them to a single row.
#' Components \code{c("n","b","p","t")} are taken from the first row  and
#' component \code{\link{size}} is taken as a  \code{\link{secs}} weighed average.
#'
#' @param ... one or more matrices of one or more rows of \code{\link{rawperf}}
#' @param rowname rowname for output, NULL means default, i.e. first rowname of input
#'
#' @return \code{\link{retperf}}
#' @seealso \code{\link{perfminus}}
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' y <- x[]
#' p0 <- UVWoverhead(y)
#' p1 <- VKnuthsort(y)
#' p2 <- perfnow()
#' x <- sort(x)
#' p2 <- perfthen(p2, rowname="Rsort", size=1) # optimistic size
#' print(rbind(
#'   p0
#' , p1
#' , perfplus(p1, p0, rowname="Total")
#' , Test = perfminus(perfplus(p1, p0), p0, rowname="Test")
#' ))
#' rbind(
#'   sperf(perfplus(p1, p0))
#' , sperf(p2)
#' )
#' @export
perfplus <- function(..., rowname = NULL){
  p <- do.call("rbind", list(...))
  s <- secs(p)
  s <- sum(size(p)*s)/sum(s)
  ret <- colSums(p)
  ret['size'] <- s
  const <- c("n","b","p","t")
  ret[const] <- p[1,const]
  if (is.null(rowname))
    rowname <- rownames(p)[1]
  matrix(ret, nrow=1, dimnames=list(rowname, colnames(p)))
}


#' Subtract (raw) performance measurements
#'
#' \code{\link{rbind}s} all arguments and aggregates all but the first them to a single row
#' and substracts the result from the first row.
#' Components \code{c("n","b","p","t")} are taken as is from the first row  and
#' component \code{\link{size}} is calculated like a  \code{\link{secs}} weighed average.
#'
#' @param ... one or more matrices of one or more rows of \code{\link{rawperf}}
#' @param rowname rowname for output, NULL means default, i.e. first rowname of input
#'
#' @return \code{\link{retperf}}
#' @seealso \code{\link{perfplus}}
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' y <- x[]
#' p0 <- UVWoverhead(y)
#' p1 <- VKnuthsort(y)
#' p2 <- perfnow()
#' x <- sort(x)
#' p2 <- perfthen(p2, rowname="Rsort", size=1) # optimistic size
#' print(rbind(
#'   p0
#' , p1
#' , perfplus(p1, p0, rowname="Total")
#' , Test = perfminus(perfplus(p1, p0), p0, rowname="Test")
#' ))
#' rbind(
#'   sperf(perfplus(p1, p0))
#' , sperf(p2)
#' )
#' @export
perfminus <- function(..., rowname = NULL){
  p <- do.call("rbind", list(...))
  d <- perfplus(p[-1,,drop=FALSE])
  p1 <- p[1,,drop=FALSE]
  ret <- p1 - d
  s <- (p1[,"size"]*p1[,"secs"] - d[,"size"]*d[,"secs"])/(ret[,"secs"])
  ret[,"size"] <- s
  const <- c("n","b","p","t")
  ret[,const] <- p1[,const]
  if (is.null(rowname))
    rowname <- rownames(p)[1]
  matrix(ret, nrow=1, dimnames=list(rowname, colnames(p)))
}




#' Extract \%RAM
#'
#' Returns the average RAM consumption over \code{\link{runTime}} divided by data size
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} '%RAM'
#' @seealso \code{\link{perf}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' size(ret)
#' "%RAM"(ret)
size <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"size", drop=FALSE]
  colnames(ret) <- "%RAM"
  ret
}

#' @describeIn size alternative function name for `size`
#' @export
"%RAM" <- size


#' Extract runTime
#'
#' Returns the runTime in seconds
#'
#' runTime is elapsed time, it grows if concurring processes delay processing.
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'runTime'
#' @seealso \code{\link{perf}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' secs(ret)
#' runTime(ret)
secs <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"secs", drop=FALSE]
  colnames(ret) <- "runTime"
  ret
}

#' @describeIn secs alternative function name for `secs`
#' @export
runTime <- secs

#' Extract lifeTime
#'
#' Returns the lifeTime in seconds
#'
#' lifeTime is \code{\link{runTime}} divided by the number of parallel processes that share the hardware for amortization
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'lifeTime'
#' @seealso \code{\link{perf}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' secs(ret)
#' runTime(ret)
lifeTime <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"secs", drop=FALSE] / a[,"p", drop=FALSE]
  colnames(ret) <- "lifeTime"
  ret
}


#' Extract cpuTime
#'
#' Returns the cpuTime in seconds.
#'
#' Note that cpuTime is not measured but estimated
#' as the \code{\link{runTime}} multiplied by the estimated effective number of threads,
#' assuming no thread waits up to the number of physical cores.
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'cpuTime'
#' @seealso \code{\link{perf}}
#' @export
#' @examples
#' ret <- PKnuthsort(runif(1e6), threads=2)
#' perf(ret)
#' secs(ret)
#' runTime(ret)
#' cpuTime(ret)
cpuTime <- function(x){
  p <- perfcores(logical=FALSE)
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"secs", drop=FALSE] * a[,"t", drop=FALSE]  / pmax(1, (a[,"t", drop=FALSE]*a[,"p", drop=FALSE]/p))
  colnames(ret) <- "cpuTime"
  ret
}


#' Extract tFootprint
#'
#' Returns the tFootprint in %RAM times seconds
#'
#' This is \code{\link{size}} times \code{\link{secs}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'tFootprint'
#' @seealso \code{\link{perf}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' sizesecs(ret)
#' tFootprint(ret)
sizesecs <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"size", drop=FALSE] * a[,"secs", drop=FALSE]
  colnames(ret) <- "tFootprint"
  ret
}

#' @describeIn sizesecs alternative function name for `sizesecs`
#' @export
tFootprint <- sizesecs



#' Extract GPU Energy
#'
#' Returns the RAPL uncore Energy in Joule
#'
#' \code{\link{unco}} is the RAPL uncore energy
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'unco'
#' @seealso \code{\link{retperf}}, \code{\link{base}}, \code{\link{core}}, \code{\link{unco}}, \code{\link{dram}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' ret
#' core(ret)
#' unco(ret)
unco <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"core", drop=FALSE]
  colnames(ret) <- "unco"
  ret
}


#' Extract bEnergy
#'
#' Returns the baseline Energy in Joule
#'
#' \code{\link{base}} is the RAPL packet energy minus the core energy,
#' \code{\link{bEnergy}} is the same *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'base' or 'bEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' base(ret)
#' bEnergy(ret)
base <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"base", drop=FALSE]
  colnames(ret) <- "base"
  ret
}

#' @describeIn base the baseline-adjusted equivalent to `base`
#' @export
bEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- a[,"base", drop=FALSE] / a[,"p", drop=FALSE]
  colnames(ret) <- "bEnergy"
  ret
}


#' Extract cEnergy
#'
#' Returns the RAPL core Energy in Joule
#'
#' \code{\link{core}} is the RAPL core energy,
#' \code{\link{cEnergy}} is the same *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'core' or 'cEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' core(ret)
#' cEnergy(ret)
core <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"core", drop=FALSE]
  colnames(ret) <- "core"
  ret
}

#' @describeIn core the baseline-adjusted equivalent to `core`
#' @export
cEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- a[,"core", drop=FALSE] / a[,"p", drop=FALSE]
  colnames(ret) <- "cEnergy"
  ret
}


#' Extract dEnergy
#'
#' Returns the RAPL DRAM in Joule
#'
#' \code{\link{dram}} is the RAPL DRAM energy,
#' \code{\link{dEnergy}} is the same *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'dram' or 'dEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' dram(ret)
#' dEnergy(ret)
dram <- function(x){
  a <- attr(x, 'perf')
  if (is.null(a))
    a <- x
  ret <- a[,"dram", drop=FALSE]
  colnames(ret) <- "dram"
  ret
}

#' @describeIn dram the baseline-adjusted equivalent to `dram`
#' @export
dEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- a[,"dram", drop=FALSE] / a[,"p", drop=FALSE]
  colnames(ret) <- "dEnergy"
  ret
}


#' Extract cdEnergy
#'
#' Returns the RAPL CORE + DRAM in Joule
#'
#' \code{\link{cdEnergy}} is \code{\link{cEnergy}} + \code{\link{dEnergy}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'cdEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' cEnergy(ret)
#' dEnergy(ret)
#' cdEnergy(ret)
cdEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- (a[,"core", drop=FALSE] + a[,"dram", drop=FALSE] ) / a[,"p", drop=FALSE]
  colnames(ret) <- "cdEnergy"
  ret
}

#' Extract pcdEnergy
#'
#' Returns the RAPL thread-partial BASE + CORE + DRAM in Joule
#'
#' \code{\link{pcdEnergy}} is a part of \code{\link{bEnergy}} + \code{\link{cEnergy}} + \code{\link{dEnergy}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'pcdEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}, \code{\link{bcdEnergy}}, \code{\link{pcdFootprint}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' bEnergy(ret)
#' cEnergy(ret)
#' dEnergy(ret)
#' bcdEnergy(ret)
#' pcdEnergy(ret)
pcdEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  t <- perfcores(logical=FALSE)
  ret <- (a[,"core", drop=FALSE] + a[,"dram"] + a[,"base"]*pmin(a[,"p"]*a[,"t"], t)/t) / a[,"p"]
  colnames(ret) <- "pcdEnergy"
  ret
}


#' Extract bcdEnergy
#'
#' Returns the RAPL BASE + CORE + DRAM Energy  in Joule
#'
#' \code{\link{bcdEnergy}} is \code{\link{bEnergy}} + \code{\link{cEnergy}} + \code{\link{dEnergy}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'bcdEnergy'
#' @seealso \code{\link{perf}}, \code{\link{aEnergy}}, \code{\link{pcdEnergy}}, \code{\link{bcdFootprint}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' bEnergy(ret)
#' cEnergy(ret)
#' dEnergy(ret)
#' bcdEnergy(ret)
bcdEnergy <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- ( a[,"core", drop=FALSE] + a[,"dram", drop=FALSE] + a[,"base", drop=FALSE] ) / a[,"p", drop=FALSE]
  colnames(ret) <- "bcdEnergy"
  ret
}


#' Extract cdFootprint
#'
#' Returns the RAPL CORE + DRAM Footprint
#'
#' \code{\link{cdFootprint}} is \code{\link{cdEnergy}} * \code{\link{\%RAM}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'cdFootprint'
#' @seealso \code{\link{perf}}, \code{\link{aFootprint}}, \code{\link{cdEnergy}}, \code{\link{bcdFootprint}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' "%RAM"(ret)
#' cdEnergy(ret)
#' cdFootprint(ret)
cdFootprint <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- (a[,"core", drop=FALSE] + a[,"dram", drop=FALSE])  / a[,"p", drop=FALSE] * a[,"size", drop=FALSE]
  colnames(ret) <- "cdFootprint"
  ret
}


#' Extract pcdFootprint
#'
#' Returns the RAPL thread-partial BASE + CORE + DRAM Footprint
#'
#' \code{\link{pcdFootprint}} is \code{\link{pcdEnergy}} * \code{\link{\%RAM}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'pcdFootprint'
#' @seealso \code{\link{perf}}, \code{\link{aFootprint}}, \code{\link{pcdEnergy}}, \code{\link{bcdFootprint}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' "%RAM"(ret)
#' pcdEnergy(ret)
#' pcdFootprint(ret)
pcdFootprint <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  t <- perfcores(logical=FALSE)
  ret <- (a[,"core", drop=FALSE] + a[,"dram"] + a[,"base"]*pmin(a[,"p"]*a[,"t"], t)/t) / a[,"p"] * a[,"size", drop=FALSE]
  colnames(ret) <- "pcdFootprint"
  ret
}


#' Extract bcdFootprint
#'
#' Returns the RAPL BASE + CORE + DRAM Footprint
#'
#' \code{\link{pcdFootprint}} is \code{\link{pcdEnergy}} * \code{\link{\%RAM}}.
#' This is *after* the adjustment specified in \code{\link{options}} `greensort_perf_calc` with default 'raw',
#' see \code{\link{perfcalibrate}}
#'
#' @param x see \code{\link{perf}}
#' @return a double column with \code{\link{colnames}} 'pcdFootprint'
#' @seealso \code{\link{perf}}, \code{\link{aFootprint}}, \code{\link{bcdEnergy}}, \code{\link{pcdFootprint}}
#' @export
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' "%RAM"(ret)
#' bcdEnergy(ret)
#' bcdFootprint(ret)
bcdFootprint <- function(x){
  a <- optperf(x)
  if (is.null(a))
    a <- x
  ret <- (a[,"core", drop=FALSE] + a[,"dram", drop=FALSE] + a[,"base", drop=FALSE]) / a[,"p", drop=FALSE] * a[,"size", drop=FALSE]
  colnames(ret) <- "bcdFootprint"
  ret
}



#' Extract all Energy measurements
#'
#' @param x see \code{\link{perf}}
#'
#' @return a matrix with different measurements in columns
#' @export
#'
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' aEnergy(ret)
aEnergy <- function(x){
  cbind(bEnergy(x), cEnergy(x), dEnergy(x), cdEnergy(x), pcdEnergy(x), bcdEnergy(x))
}


#' Extract all time measurements
#'
#' @param x see \code{\link{perf}}
#'
#' @return a matrix with different measurements in columns
#' @export
#'
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' aTime(ret)
aTime <- function(x){
  cbind(runTime(x), lifeTime(x), cpuTime(x))
}


#' Extract all Footprint measurements
#'
#' @param x see \code{\link{perf}}
#'
#' @return a matrix with different measurements in columns
#' @export
#'
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' aFootprint(ret)
aFootprint <- function(x){
  cbind(tFootprint(x), cdFootprint(x), pcdFootprint(x), bcdFootprint(x))
}

#' Extract all measurements
#'
#' @param x see \code{\link{perf}}
#'
#' @return a matrix with different measurements in columns
#' @export
#'
#' @examples
#' ret <- Insertionsort(runif(1000))
#' perf(ret)
#' aperf(ret)
aperf <- function(x){
  cbind("%RAM"(x), aTime(x), aEnergy(x), aFootprint(x))
}



#' Performance measurements
#'
#' The `perf` function returns the default performance KPIs
#'
#' @param x an object that has a `perf` \code{\link{attr}} or which is a perf attribute or which is a \code{\link{rbind}} \code{\link{matrix}} or \code{\link{data.frame}} of multiple such attributes
#' @return a matrix or data.frame with \code{\link{rownames}} of the input and the following columns:
#' \item{\%RAM \code{\link{size}} or \code{\link{\%RAM}}}{the maximum memory used for execution (the size of data and buffer relative to the size of the data only)}
#' \item{runTime \code{\link{secs}} or \code{\link{runTime}}}{the execution time measured in seconds}
#' \item{lifeTime \code{\link{lifeTime}}}{the execution time measured in seconds corrected for parallel processes}
#' \item{cpuTime \code{\link{cpuTime}}}{the cpuTime estimated from runTime, number of threads and number of processes}
#' \item{bEnergy \code{\link{bEnergy}}}{the \code{\link{optperf}} corrected `base` energy }
#' \item{cdEnergy \code{\link{cdEnergy}}}{the \code{\link{optperf}} corrected `core` + `dram` energy }
#' \item{pcdEnergy \code{\link{pcdEnergy}}}{the \code{\link{optperf}} corrected thread adjusted partiel `base` + `core` + `dram` energy }
#' \item{bcdEnergy \code{\link{bcdEnergy}}}{the \code{\link{optperf}} corrected `base` + `core` + `dram` energy }
#' \item{cdFootprint \code{\link{cdFootprint}}}{ \code{\link{cdEnergy}} * \code{\link{\%RAM}} }
#' \item{pcdFootprint \code{\link{pcdFootprint}}}{ \code{\link{pcdEnergy}} * \code{\link{\%RAM}} }
#' \item{bcdFootprint \code{\link{bcdFootprint}}}{ \code{\link{bcdEnergy}} * \code{\link{\%RAM}} }
#' \item{tFootprint \code{\link{sizesecs}} or \code{\link{tFootprint}}}{the integral of memory size over execution time where size is measured as number of elements}
#'
#' @seealso \code{\link{perfgrants}}, \code{\link{retperf}}, \code{\link{aTime}}, \code{\link{aEnergy}}, \code{\link{aFootprint}}, \code{\link{perfcalibrate}}
#' @examples
#' N <- 1024
#' z <- Insertionsort(runif(N))
#' perf(z)
#' perf(rbind(z,z))
#' @export

perf <- function(x){
  cbind("%RAM"(x), runTime(x), lifeTime(x), cpuTime(x), bEnergy(x), cdEnergy(x), bcdEnergy(x), pcdEnergy(x), cdFootprint(x), bcdFootprint(x), pcdFootprint(x), tFootprint(x))
}

#' @describeIn perf a simple version of `perf` which returns only `\%RAM`, runTime`, `bcdEnergy`, `bcdFootprint`, `tFootprint`
#' @export
sperf <- function(x){
  cbind("%RAM"(x), runTime(x), bcdEnergy(x), bcdFootprint(x), tFootprint(x))
}

