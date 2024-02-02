# greeNsort testing
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

#' Test select algorithms
#'
#' We assume that selection is unstable (but handle the case of stable testing where `current_keys(0.5) != 0.5` and the test data have digits after the decimal dot)
#'
#' @param R vector of random seeds for replications
#' @param N vector of samplesizes
#' @param S vector of situations in c("insitu","exsitu"), non-existing will be skipped automatically
#' @param A vector of algorithm labels from \code{\link{algodb}}
#' @param D vector of data labels from \code{\link{testdb}}
#' @param out output destination: "" for console or filename
#' @param interrupt TRUE for interrupting at first error, FALSE for continuing all tests
#' @param verbose TRUE to cat all tests, FALSE for silent testing
#'
#' @seealso  \code{\link{current_keys}}, \code{\link{testpart}}, \code{\link{testsort}}
#'
#' @return TRUE if all tests were OK otherwise FALSE
#' @export
#'
#' @examples
#' testselect(out = "", verbose = TRUE)

testselect <- function(
  R = 0
, N = c(1:7, insertionsort_limit() - 1L, insertionsort_limit(), insertionsort_limit() + 1L, 127L, 128L, 129L, 255L, 256L, 257L)
, S = c("insitu", "exsitu")
, A = algodb[algodb$task %in% c("select","SELECT"), "label"]
, D = testdb$label
, out = file(paste("test", r, sep = ""), open = "w")
, interrupt = out == ""
, verbose = !interrupt
){
  stopifnot(all(algodb[A, "task"] %in% c("select","SELECT")))
  OK <- TRUE
  for (r in R) {
    for (n in N) {
      if (n > 1 || r == R[1])  # skip repetitions for n==1
        for (d in {if (n == 1) "tieall" else D}) {
          set.seed(r)
          x <- testdb$func[[d]](n)
          if (current_keys(0.5) != 0.5) {
            ox <- order(current_keys(x))
            sx <- current_keys(x[ox])
          }else{
            sx <- sort(x)
          }
          K <- sort(if (n > 9) c(1L, sample(2:(n - 1), 8), n) else seq_len(n))
          for (k in K) {
            xk <- sx[k]
            rk <- range(seq_len(n)[sx == xk])
            for (a in A) {
              f <- algodb[a,"func"][[1]]
              task <- algodb[a,"task"]
              for (s in S) {
                if (s %in% unlist(as.list(as.list(f)$situation)[-1])) {
                  msg <- paste("n=", n, "; k=", k, "; kl=", rk[1], "; kr=", rk[2], "; r=", r, "; d='", d, "'; a='", a, "'; s='", s, "';", sep = "")
                  if (verbose)
                    cat(msg, file = out)
                  force(z <- x[])
                  ret <- f(z, k, situation = s)
                  if (task == 'select') {
                    if (current_keys(0.5) != 0.5) {
                      res <- identical(xk, current_keys(as.vector(ret)))
                    }else{
                      res <- identical(xk, as.vector(ret))
                    }
                  }else if (task == 'SELECT') {
                    res <- identical(rk, as.vector(ret))
                  }else stop("wrong task")
                  if (res) {
                    if (verbose)
                      cat(" passed\n", file = out)
                  }else{
                    OK <- FALSE
                    if (verbose)
                      cat(" failed\n", file = out)
                    if (interrupt) {
                      if (verbose)
                        cat("-- interrupted --\n", file = out)
                      else
                        cat(msg, file = out)
                      return(FALSE)
                    }
                  }
                }
              }
            }
          }
        }
    }
  }
  if (OK)
    if (verbose)
      cat("-- OK --\n", file = out)
  else
    if (verbose)
      cat("-- NOT OK --\n", file = out)
  return(OK)
}


#' Test partial sorting algorithms
#'
#' We assume that partial sorting is unstable (but handle the case of stable testing where `current_keys(0.5) != 0.5` and the test data have digits after the decimal dot)
#'
#' @param R vector of random seeds for replications
#' @param N vector of samplesizes
#' @param S vector of situations in c("insitu","exsitu"), non-existing will be skipped automatically
#' @param A vector of algorithm labels from \code{\link{algodb}}
#' @param D vector of data labels from \code{\link{testdb}}
#' @param out output destination: "" for console or filename
#' @param interrupt TRUE for interrupting at first error, FALSE for continuing all tests
#' @param verbose TRUE to cat all tests, FALSE for silent testing
#'
#' @seealso \code{\link{current_keys}}, \code{\link{testselect}}, \code{\link{testsort}}
#'
#' @return TRUE if all tests were OK otherwise FALSE
#' @export
#'
#' @examples
#' testpart(out = "", verbose = TRUE)

testpart <- function(
    R = 0
  , N = c(1:7, insertionsort_limit() - 1L, insertionsort_limit(), insertionsort_limit() + 1L, 127L, 128L, 129L, 255L, 256L, 257L)
  , S = c("insitu", "exsitu")
  , A = algodb[algodb$task %in% c("partial","PARTIAL"), "label"]
  , D = c("ascall", "descall", "permut", "tiesqrt","tielog2")
  , out = file(paste("test", r, sep = ""), open = "w")
  , interrupt = out == ""
  , verbose = !interrupt
){
  stopifnot(all(algodb[A, "task"] %in% c("partial","PARTIAL")))
  OK <- TRUE
  for (r in R) {
    for (n in N) {
      if (n > 1 || r == R[1])  # skip repetitions for n==1
        for (d in {if (n == 1) "tieall" else D}) {
          set.seed(r)
          x <- testdb$func[[d]](n)
          if (current_keys(0.5) != 0.5) {
            ox <- order(current_keys(x))
            sx <- current_keys(x[ox])
          }else{
            sx <- sort(x)
          }
          K1 <- sort(if (n > 9) c(1L, sample(2:(n - 1), 8), n) else seq_len(n))
          for (k1 in K1) {
            xk1 <- sx[k1]
            kl <- min(seq_len(n)[sx == xk1])
            K2 <- sort(if (n-k1+1 > 9) c(k1, sample((k1+1):(n-1), 8), n) else seq_len(n)[k1:n])
            for (k2 in K2) {
              xk2 <- sx[k2]
              kr <- max(seq_len(n)[sx == xk2])
              for (a in A){
              isleft <- length(grep("left",algodb[a,"name"]))
              isright <- length(grep("right",algodb[a,"name"]))
              if ( (isleft&&k1==1) || (isleft&&k2==n) || (!isleft && !isright) ){
                f <- algodb[a,"func"][[1]]
                task <- algodb[a,"task"]
                for (s in S) {
                  if (s %in% unlist(as.list(as.list(f)$situation)[-1])) {
                    msg <- paste("n=", n, "; k1=", k1, "; k2=", k2, "; kl=", kl, "; kr=", kr, "; r=", r, "; d='", d, "'; a='", a, "'; s='", s, "';", sep = "")
                    if (verbose)
                      cat(msg, file = out)
                    force(z <- x[])
                    if (isleft){
                      ret <- f(z, k2, situation = s)
                    }else if (isright){
                      ret <- f(z, k1, situation = s)
                    }else{
                      ret <- f(z, c(k1, k2), situation = s)
                    }
                    if (task == 'partial') {
                      if (current_keys(0.5) != 0.5) {
                        res <- identical(sx[k1:k2], current_keys(z[k1:k2]))
                      }else{
                        res <- identical(sx[k1:k2], z[k1:k2])
                      }
                    }else if (task == 'PARTIAL') {
                      if (current_keys(0.5) != 0.5) {
                        res <- identical(sx[kl:kr], current_keys(z[kl:kr]))
                      }else{
                        res <- identical(sx[kl:kr], z[kl:kr])
                      }
                    }else stop("wrong task")
                    if (res) {
                      if (verbose)
                        cat(" passed\n", file = out)
                    }else{
                      OK <- FALSE
                      if (verbose)
                        cat(" failed\n", file = out)
                      if (interrupt) {
                        if (verbose)
                          cat("-- interrupted --\n", file = out)
                        else
                          cat(msg, file = out)
                        return(FALSE)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  if (OK)
    if (verbose)
      cat("-- OK --\n", file = out)
  else
    if (verbose)
      cat("-- NOT OK --\n", file = out)
  return(OK)
}


#' Test sorting algorithms
#'
#' @param R vector of random seeds for replications
#' @param N vector of samplesizes
#' @param M vector of methods in c("index","pointer"), non-existing will be skipped automatically
#' @param S vector of situations in c("insitu","exsitu"), non-existing will be skipped automatically
#' @param A vector of algorithm labels from \code{\link{algodb}}
#' @param D vector of data labels from \code{\link{testdb}}
#' @param T scalar with number of parallel cores
#' @param out output destination: "" for console or filename
#' @param interrupt TRUE for interrupting at first error, FALSE for continuing all tests
#' @param verbose TRUE to cat all tests, FALSE for silent testing
#'
#' @seealso \code{\link{current_keys}}, \code{\link{testselect}}, \code{\link{testpart}}, \code{\link{vtestsort}}
#'
#' @return TRUE if all tests were OK otherwise FALSE
#' @export
#'
#' @examples
#' testsort(out = "", verbose = TRUE)
testsort <- function(
    R = 0
  , N = c(1:7, insertionsort_limit() - 1L, insertionsort_limit(), insertionsort_limit() + 1L, 127L, 128L, 129L, 255L, 256L, 257L)
  , M=c("pointer", "index")
  , S = c("insitu", "exsitu")
  , A = algodb[algodb$task %in% c("sort","SORT")  & !(algodb$label %in% c("IPS4o", "IS4o")), "label"]
  , D = testdb$label
  , T = 1:perfcores()
  , out = file(paste("test", r, sep = ""), open = "w")
  , interrupt = out == ""
  , verbose = !interrupt
){
  OK <- TRUE
  stopifnot(!any(is.na(algodb[A, "stable"])))
  hasVarying <- any(algodb[A,"varying"])
  hasEqual <- any(!algodb[A,"varying"])
  isCompiledStable <- current_keys(0.5) != 0.5
  for (r in R) {
    for (n in N) {
      if (n > 1 || r == R[1]){  # skip repetitions for n==1
        for (d in {if (n == 1) "tieall" else D}) {
          hasTestStable   <- isCompiledStable && (   testdb[d,"stable"]  && any( algodb[A,"teststable"]) )
          hasTestUnstable <- isCompiledStable && ( (!testdb[d,"stable"]) || any(!algodb[A,"teststable"]) )
          set.seed(r)
          x <- testdb$func[[d]](n)
          if (hasVarying){  # always uses identical()
            vx <- num2str(x)
            svx <- sort(vx)
          }
          if (isCompiledStable) {
            if (hasEqual)  # only stably compiled uses identical
              sx <- sort(x)
            if (hasTestStable){
              if (hasEqual){
                #generic but slow
                #ox <- order(current_keys(x))
                #gsx <- x[ox]
                # let's trust Knuthsort
                gx <- x[]
                Knuthsort(gx)
              }
              if (hasVarying){
                #ovx <- order(current_keys(vx))
                #gvx <- vx[ovx]
                gvx <- vx[]
                VKnuthsort(gvx)
              }
            }
            if (hasTestUnstable){
              if (hasEqual)
                rx <- current_keys(sx)
              if (hasVarying)
                rvx <- current_keys(svx)
            }
          }
          for (a in A) {
            f <- algodb[a,"func"][[1]]
            hasThreads <- !is.null(as.list(f)$threads)
            isTestStable <- isCompiledStable && testdb[d,"stable"]  && algodb[a,"teststable"]
            isMatrix <- algodb[[a,"matrix"]]
            for (m in M) {
              for (s in S) {
                if (s %in% unlist(as.list(as.list(f)$situation)[-1]) && m %in% unlist(as.list(as.list(f)$method)[-1])) {
                  for (t in T){
                    if (t==1 || hasThreads){
                  msg <- paste("r=", r, "; n=", n, "; d='", d, "'; a='", a, "'; m='", m, "'; s='", s, "'; t=", t, ";", sep = "")
                  if (verbose)
                    cat(msg, file = out)
                  if (algodb[a, "varying"])
                    force(z <- vx[])
                  else
                    force(z <- x[])
                  if (isMatrix){
                    z <- rbind(
                      as.integer(trunc(z))
                    , as.integer((z-trunc(z))*2^31)
                    )
                  }
                  if (hasThreads)
                    ret <- f(z, method = m, situation = s, threads = t)
                  else
                    ret <- f(z, method = m, situation = s)
                  if (isCompiledStable){
                    if (isTestStable){
                      if (algodb[a, "varying"]){
                        res <- identical(z,gvx)
                      }else{
                        if (isMatrix){
                          res <- identical(z,rbind(
                              as.integer(trunc(gx))
                            , as.integer((gx-trunc(gx))*2^31)
                          ))
                        }else{
                          res <- identical(z,gx)
                        }
                      }
                    }else{
                      if (algodb[a, "varying"]){
                        res <- identical(current_keys(z),rvx)
                      }else{
                        if (isMatrix){
                          res <- identical(z[1,], as.integer(trunc(rx)))
                        }else{
                          #res <- identical(current_keys(z),rx)  #slower
                          res <- !is.unsorted(current_keys(z))  #faster
                        }
                      }
                    }
                  }else{
                    if (algodb[a, "varying"]){
                      res <- identical(z,svx)
                    }else{
                      if (isMatrix)
                        z <- z[1,]
                      #res <- identical(z,sx)  # slower
                      res <- !is.unsorted(z)  # faster
                    }
                  }
                  if (res) {
                    if (verbose)
                      cat(" passed\n", file = out)
                  }else{
                    OK <- FALSE
                    if (verbose)
                      cat(" failed\n", file = out)
                    # print(x)
                    # print(sx)
                    # print(z)
                    if (interrupt) {
                      if (verbose)
                        cat("-- interrupted --\n", file = out)
                      else
                        cat(msg, file = out)
                      return(FALSE)
                    }
                  }
                  }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  if (OK)
    if (verbose)
      cat("-- OK --\n", file = out)
  else
    if (verbose)
      cat("-- NOT OK --\n", file = out)
  return(OK)
}


#' Varying Test sorting algorithms
#'
#' @param R vector of random seeds for replications
#' @param M vector of methods in c("index","pointer"), non-existing will be skipped automatically
#' @param S vector of situations in c("insitu","exsitu"), non-existing will be skipped automatically
#' @param A vector of algorithm labels from \code{\link{algodb}}
#' @param D vector of data labels from \code{\link{vtestdb}}
#' @param out output destination: "" for console or filename
#' @param interrupt TRUE for interrupting at first error, FALSE for continuing all tests
#' @param verbose TRUE to cat all tests, FALSE for silent testing
#'
#' @seealso \code{\link{current_keys}}, \code{\link{testsort}}
#'
#' @return TRUE if all tests were OK otherwise FALSE
#' @export
#'
#' @examples
#' vtestsort(out = "", verbose = TRUE)
vtestsort <- function(
  R = 0
  , M=c("pointer", "index")
  , S = c("insitu", "exsitu")
  , A = algodb[algodb$varying & algodb$task %in% c("sort", "SORT"), "label"]
  , D = vtestdb$label
  , out = file(paste("test", r, sep = ""), open = "w")
  , interrupt = out == ""
  , verbose = !interrupt
){
  if (current_keys(0.5) != 0.5)
    stop("real strings are not compatible with STABLE_TEST compiled code")
  OK <- TRUE
  for (r in R) {
    for (d in {if (r == R[1]) D else D[union(grep("p", D), grep("b", D))]}) {
      set.seed(r)
      vx <- vtestdb[[d,"func"]]()
      n <- length(vx)
      n2 <- sum(nchar(vx))
      for (a in {if (length(grep("KP", d))) A else intersect(A, algodb[algodb$varying & algodb$task == "SORT", "label"])}) {
        f <- algodb[[a,"func"]]
        for (m in M) {
          for (s in S) {
            if (s %in% unlist(as.list(as.list(f)$situation)[-1]) && m %in% unlist(as.list(as.list(f)$method)[-1])) {
              msg <- paste("n=", n, " n2=", n2, "; r=", r, "; d='", d, "'; a='", a, "'; m='", m, "'; s='", s, "';", sep = "")
              if (verbose)
                cat(msg, file = out)
                force(z <- vx[])
              ret <- f(z, method = m, situation = s)
              res <- current_issorted(z)
              res <- TRUE
              if (res) {
                if (verbose)
                  cat(" passed\n", file = out)
              }else{
                OK <- FALSE
                if (verbose)
                  cat(" failed\n", file = out)
                if (interrupt) {
                  if (verbose)
                    cat("-- interrupted --\n", file = out)
                  else
                    cat(msg, file = out)
                  return(FALSE)
                }
              }
            }
          }
        }
      }
    }
  }
  if (OK)
    if (verbose)
      cat("-- OK --\n", file = out)
  else
    if (verbose)
      cat("-- NOT OK --\n", file = out)
  return(OK)
}
