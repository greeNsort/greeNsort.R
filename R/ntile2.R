# greeNsort ntile calculation
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk


#' ntiles of two sorted arrays from LEFT to RIGHT
#'
#' expensive version based on full sorting
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param k scalar number of elements to be separated on the LEFT
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = 2^7
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N-skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' k = N%/%4
#' Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
#'
Rntile2_asc_asc_left_to_right_expensive  <- function(L, R, k=(Ln+Rn)%/%2){
  Ln <- length(L)
  Rn <- length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  X = c(L,R)
  X = X[order(current_keys(X))]
  xlr = c(l=sum(current_keys(L) < current_keys(X[k])), r=sum(current_keys(R) < current_keys(X[k])))
  xlr["l"] = min(sum(current_keys(L) <= current_keys(X[k])), k - xlr["r"])
  xlr["r"] = k - xlr["l"]
  xlr
}


#' ntiles of two sorted arrays from RIGHT to LEFT
#'
#' expensive version based on full sorting
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param k number of elements to be separated on the RIGHT
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = 2^7
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N-skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' k = N%/%4
#' Rntile2_asc_asc_right_to_left_expensive(L,R,k=k)
#'
Rntile2_asc_asc_right_to_left_expensive  <- function(L, R, k=(Ln+Rn)%/%2){
  Ln <- length(L)
  Rn <- length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  X = c(L,R)
  X = X[order(current_keys(X))]
  kr = length(X) - k
  xlr = c(l=sum(current_keys(L) > current_keys(X[kr])), r=sum(current_keys(R) > current_keys(X[kr])))
  xlr["r"] = min(sum(current_keys(R) >= current_keys(X[kr])), k - xlr["l"])
  xlr["l"] = k - xlr["r"]
  xlr
}



#' ntiles of two sorted arrays (ASC ASC from LEFT to RIGHT)
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param k number of elements to be separated on the LEFT
#' @param verbose TRUE for reporting the steps
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' ycol  = c("red","blue")
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' ylim = range(c(L,R))
#' oldpar <- par(mfrow=c(2,2))
#' k = as.integer(N*1/12)
#' xlr = Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
#' lr = Rntile2_asc_asc_left_to_right(L,R,k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)>lr[1])])
#' m = max(L[lr[1]], R[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(R, ylim=ylim, col=ycol[1+(seq_along(R)>lr[2])])
#' abline(h=m, col=ycol[1])
#' k = as.integer(N*11/12)
#' xlr = Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
#' lr = Rntile2_asc_asc_left_to_right(L,R,k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)>lr[1])])
#' m = max(L[lr[1]], R[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(R, ylim=ylim, col=ycol[1+(seq_along(R)>lr[2])])
#' abline(h=m, col=ycol[1])
#' par(oldpar)
#'

Rntile2_asc_asc_left_to_right <- function(L,R, k=(Ln+Rn)%/%2, verbose=FALSE){
  # /b//
  Ln=length(L)
  Rn=length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  lmax = min(k,Ln)
  rmax = min(k,Rn)
  lmin = k - rmax
  rmin = k - lmax
  if (verbose)
    cat("Task: Ln=", Ln, "  Rn=", Rn, "  k=", k, "  lmin=", lmin, "  lmax=", lmax, "  rmin=", rmin, "  rmax=", rmax, "\n", sep="")
  if (lmax == 0L || lmin == Ln)
    return(c(l=lmin, r=rmax))
  if (rmax == 0L || rmin == Rn)
    return(c(l=lmax, r=rmin))
  l = l1 = lmin
  l2 = lmax
  if (verbose)
    cat("Initial:  l1=", l1, "  l2=", l2, "\n", sep="")
  while((l1+1) < l2){
    l = l1 + as.integer((l2-l1)%/%2)
    if (verbose)
      cat("loop:  l1=", l1, "  l=", l, "  l2=", l2, "\n", sep="")
    if (current_keys(L[l]) <= current_keys(R[k - l + 1])){
      l1 = l
    }else{
      l2 = l
    }
  }
  stopifnot(l1+1 == l2)
  if (verbose)
    cat("converged:  rmax=", rmax,"  lmin=", lmin,"  l1=", l1, "  l=", l, "  l2=", l2, "  lmax=", lmax, "  rmin=", rmin, "\n", sep="")
  if (l==l2){
    # last step was narrowing from right: try the left remaining border
    if (current_keys(L[l]) > current_keys(R[k - l])){
        l = l1
      if (verbose)
        cat("corrected:  l=", l, "\n", sep="")
    }
  }else{
    # last step was narrowing from left: try the right remaining border
      if (current_keys(L[l2]) <= current_keys(R[k - l])){
        l = l2
        if (verbose)
          cat("corrected:  l=", l, "\n", sep="")
      }
  }
  return(c(l = l, r = k - l))
}


#' ntiles of two sorted arrays (ASC REVASC from LEFT to RIGHT)
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (reverse ascending)
#' @param k number of elements to be separated on the LEFT
#' @param verbose TRUE for reporting the steps
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' ycol  = c("red","blue")
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' ylim = range(c(L,R))
#' oldpar <- par(mfrow=c(2,2))
#' k = as.integer(N*1/12)
#' xlr = Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
#' lr = Rntile2_asc_revasc_left_to_right(L,rev(R),k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)>lr[1])])
#' m = max(L[lr[1]], R[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(rev(R), ylim=ylim, col=ycol[1+(rev(seq_along(R))>lr[2])])
#' abline(h=m, col=ycol[1])
#' k = as.integer(N*11/12)
#' xlr = Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
#' lr = Rntile2_asc_revasc_left_to_right(L,rev(R),k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)>lr[1])])
#' m = max(L[lr[1]], R[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(rev(R), ylim=ylim, col=ycol[1+(rev(seq_along(R))>lr[2])])
#' abline(h=m, col=ycol[1])
#' par(oldpar)
#'

Rntile2_asc_revasc_left_to_right <- function(L,R, k=(Ln+Rn)%/%2, verbose=FALSE){
  # /b\\
  Ln=length(L)
  Rn=length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  lmax = min(k,Ln)
  rmax = min(k,Rn)
  lmin = k - rmax
  rmin = k - lmax
  if (verbose)
    cat("Task: Ln=", Ln, "  Rn=", Rn, "  k=", k, "  lmin=", lmin, "  lmax=", lmax, "  rmin=", rmin, "  rmax=", rmax, "\n", sep="")
  if (lmax == 0L || lmin == Ln)
    return(c(l=lmin, r=rmax))
  if (rmax == 0L || rmin == Rn)
    return(c(l=lmax, r=rmin))
  l = l1 = lmin
  l2 = lmax
  Rn = Rn + 1L
  if (verbose)
    cat("Initial:  l1=", l1, "  l2=", l2, "\n", sep="")
  while((l1+1) < l2){
    l = l1 + as.integer((l2-l1)%/%2)
    if (verbose)
      cat("loop:  l1=", l1, "  l=", l, "  l2=", l2, "\n", sep="")
    if (current_keys(L[l]) <= current_keys(R[Rn - (k - l + 1)])){
      l1 = l
    }else{
      l2 = l
    }
  }
  stopifnot(l1+1 == l2)
  if (verbose)
    cat("converged:  rmax=", rmax,"  lmin=", lmin,"  l1=", l1, "  l=", l, "  l2=", l2, "  lmax=", lmax, "  rmin=", rmin, "\n", sep="")
  if (l==l2){
    # last step was narrowing from right: try the left remaining border
    if (current_keys(L[l]) > current_keys(R[Rn - (k - l)])){
      l = l1
      if (verbose)
        cat("corrected:  l=", l, "\n", sep="")
    }
  }else{
    # last step was narrowing from left: try the right remaining border
    if (current_keys(L[l2]) <= current_keys(R[Rn - (k - l)])){
      l = l2
      if (verbose)
        cat("corrected:  l=", l, "\n", sep="")
    }
  }
  return(c(l = l, r = k - l))
}



#' ntiles of two sorted arrays (ASC ASC from RIGHT to LEFT)
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param k number of elements to be separated on the LEFT
#' @param verbose TRUE for reporting the steps
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' ycol  = c("red","blue")
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' ylim = range(c(L,R))
#' oldpar <- par(mfrow=c(2,2))
#' k = as.integer(N*1/12)
#' xlr = Rntile2_asc_asc_right_to_left_expensive(L, R, k=k)
#' lr = Rntile2_asc_asc_right_to_left(L,R,k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)<(Ln-lr[1]+1))])
#' m = min(rev(L)[lr[1]], rev(R)[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(R, ylim=ylim, col=ycol[1+(seq_along(R)<(Rn-lr[2]+1))])
#' abline(h=m, col=ycol[1])
#' k = as.integer(N*11/12)
#' xlr = Rntile2_asc_asc_right_to_left_expensive(L, R, k=k)
#' lr = Rntile2_asc_asc_right_to_left(L,R,k=k, verbose=TRUE)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)<(Ln-lr[1]+1))])
#' m = min(rev(L)[lr[1]], rev(R)[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(R, ylim=ylim, col=ycol[1+(seq_along(R)<(Rn-lr[2]+1))])
#' abline(h=m, col=ycol[1])
#' par(oldpar)
#'

Rntile2_asc_asc_right_to_left <- function(L,R, k=(Ln+Rn)%/%2, verbose=FALSE){
  # //b/
  Ln=length(L)
  Rn=length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  lmax = min(k,Ln)
  rmax = min(k,Rn)
  lmin = k - rmax
  rmin = k - lmax
  if (verbose)
    cat("Task: Ln=", Ln, "  Rn=", Rn, "  k=", k, "  lmin=", lmin, "  lmax=", lmax, "  rmin=", rmin, "  rmax=", rmax, "\n", sep="")
  if (lmax == 0L || lmin == Ln)
    return(c(l=lmin, r=rmax))
  if (rmax == 0L || rmin == Rn)
    return(c(l=lmax, r=rmin))
  r = r1 = rmin
  r2 = rmax
  Ln = Ln + 1L
  Rn = Rn + 1L
  if (verbose)
    cat("Initial:  r1=", r1, "  r2=", r2, "\n", sep="")
  while((r1+1) < r2){
    r = r1 + as.integer((r2-r1)%/%2)
    if (verbose)
      cat("loop:  r1=", r1, "  r=", r, "  r2=", r2, "\n", sep="")
    if (current_keys(L[Ln - (k - r + 1)]) <= current_keys(R[Rn - (r)])){
      r1 = r
    }else{
      r2 = r
    }
  }
  stopifnot(r1+1 == r2)
  if (verbose)
    cat("converged:  rmax=", rmax,"  lmin=", lmin,"  r1=", r1, "  r=", r, "  r2=", r2, "  lmax=", lmax, "  rmin=", rmin, "\n", sep="")
  if (r==r2){
    # last step was narrowing from left: try the right remaining border
    if (current_keys(L[Ln - (k - r)]) > current_keys(R[Rn - (r)])){
      r = r1
      if (verbose)
        cat("corrected:  r=", r, "\n", sep="")
    }
  }else{
    # last step was narrowing from right: try the left remaining border
    if (current_keys(L[Ln - (k - r)]) <= current_keys(R[Rn - (r2)])){
      r = r2
      if (verbose)
        cat("corrected:  r=", r, "\n", sep="")
    }
  }
  return(c(l = k - r, r = r))
}





#' ntiles of two sorted arrays (ASC REVASC from RIGHT to LEFT)
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (reverse ascending)
#' @param k number of elements to be separated on the LEFT
#' @param verbose TRUE for reporting the steps
#'
#' @return two-element vector c(l,r) giving the number of elements from L and R
#' @export
#'
#' @examples
#' ycol  = c("red","blue")
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' ylim = range(c(L,R))
#' oldpar <- par(mfrow=c(2,2))
#' k = as.integer(N*1/12)
#' xlr = Rntile2_asc_asc_right_to_left_expensive(L, R, k=k)
#' lr = Rntile2_asc_revasc_right_to_left(L,rev(R),k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)<(Ln-lr[1]+1))])
#' m = min(rev(L)[lr[1]], rev(R)[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(rev(R), ylim=ylim, col=ycol[1+(rev(seq_along(R))<(Rn-lr[2]+1))])
#' abline(h=m, col=ycol[1])
#' k = as.integer(N*11/12)
#' xlr = Rntile2_asc_asc_right_to_left_expensive(L, R, k=k)
#' lr = Rntile2_asc_revasc_right_to_left(L,rev(R),k=k)
#' stopifnot(identical(lr, xlr))
#' plot(L, ylim=ylim, col=ycol[1+(seq_along(L)<(Ln-lr[1]+1))])
#' m = min(rev(L)[lr[1]], rev(R)[lr[2]])
#' abline(h=m, col=ycol[1])
#' plot(rev(R), ylim=ylim, col=ycol[1+(rev(seq_along(R))<(Rn-lr[2]+1))])
#' abline(h=m, col=ycol[1])
#' par(oldpar)
#'

Rntile2_asc_revasc_right_to_left <- function(L,R, k=(Ln+Rn)%/%2, verbose=FALSE){
  # //b\
  Ln=length(L)
  Rn=length(R)
  k = as.integer(k)
  stopifnot( k <= Ln+Rn)
  lmax = min(k,Ln)
  rmax = min(k,Rn)
  lmin = k - rmax
  rmin = k - lmax
  if (verbose)
    cat("Task: Ln=", Ln, "  Rn=", Rn, "  k=", k, "  lmin=", lmin, "  lmax=", lmax, "  rmin=", rmin, "  rmax=", rmax, "\n", sep="")
  if (lmax == 0L || lmin == Ln)
    return(c(l=lmin, r=rmax))
  if (rmax == 0L || rmin == Rn)
    return(c(l=lmax, r=rmin))
  r = r1 = rmin
  r2 = rmax
  Ln = Ln + 1L
  if (verbose)
    cat("Initial:  r1=", r1, "  r2=", r2, "\n", sep="")
  while((r1+1) < r2){
    r = r1 + as.integer((r2-r1)%/%2)
    if (verbose)
      cat("loop:  r1=", r1, "  r=", r, "  r2=", r2, "\n", sep="")
    if (current_keys(L[Ln - (k - r + 1)]) <= current_keys(R[r])){
      r1 = r
    }else{
      r2 = r
    }
  }
  stopifnot(r1+1 == r2)
  if (verbose)
    cat("converged:  rmax=", rmax,"  lmin=", lmin,"  r1=", r1, "  r=", r, "  r2=", r2, "  lmax=", lmax, "  rmin=", rmin, "\n", sep="")
  if (r==r2){
    # last step was narrowing from left: try the right remaining border
    if (current_keys(L[Ln - (k - r)]) > current_keys(R[r])){
      r = r1
      if (verbose)
        cat("corrected:  r=", r, "\n", sep="")
    }
  }else{
    # last step was narrowing from right: try the left remaining border
    if (current_keys(L[Ln - (k - r)]) <= current_keys(R[r2])){
      r = r2
      if (verbose)
        cat("corrected:  r=", r, "\n", sep="")
    }
  }
  return(c(l = k - r, r = r))
}



#' ntiles of two ASC ASC sorted arrays from LEFT to RIGHT
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param threads scalar number of threads
#' @param P vector number of split fractions from the LEFT (default: parameter 'threads' equally sized fractions)
#' @param K vector number of elements to be separated on the LEFT (default: derived from parameter P)
#'
#' @return a 2xlength(K) matrix
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' threads <- 8
#' lr = ntile2_asc_asc_left_to_right(L,R,threads)
#' ylim = range(c(L,R))
#' lrna = lr; lrna[lrna==0] = NA
#' m = pmax(L[lrna["l",]], R[lrna["r",]], na.rm=TRUE)
#' oldpar <- par(mfrow=c(1,2), bg="grey40")
#' plot(L, ylim=ylim, col=rep(seq_len(threads), diff(c(0,lr['l',]))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' plot(R, ylim=ylim, col=rep(seq_len(threads), diff(c(0,lr['r',]))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' par(oldpar)

ntile2_asc_asc_left_to_right <- function(L, R, threads=2, P = seq_len(threads)/threads, K=round(P*N))
{
  stopifnot(threads >= 2)
  N <- length(L) + length(R)
  K <- as.integer(K)
  KU <- unique(K)
  ret <- .Call(C_r_Ntile2_asc_asc_left_to_right
  , L = L
  , R = R
  , K = KU
  )
  dim(ret) <- c(2L, length(KU))
  ret <- ret[,match(K, KU), drop=FALSE]
  dimnames(ret) <- list(c("l","r"), K)
  ret
}


#' ntiles of two ASC REVASC sorted arrays from LEFT to RIGHT
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (reverse ascending)
#' @param threads scalar number of threads
#' @param P vector number of split fractions from the LEFT (default: parameter 'threads' equally sized fractions)
#' @param K vector number of elements to be separated on the LEFT (default: derived from parameter P)
#'
#' @return a 2xlength(K) matrix
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' threads <- 8
#' lr1 = ntile2_asc_asc_left_to_right(L,R,threads)
#' lr = ntile2_asc_revasc_left_to_right(L,rev(R),threads)
#' ylim = range(c(L,R))
#' lrna = lr; lrna[lrna==0] = NA
#' m = pmax(L[lrna["l",]], R[lrna["r",]], na.rm=TRUE)
#' oldpar <- par(mfrow=c(1,2), bg="grey40")
#' plot(L, ylim=ylim, col=rep(seq_len(threads), diff(c(0,lr['l',]))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' plot(rev(R), ylim=ylim, col=rev(rep(seq_len(threads), diff(c(0,lr['r',])))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' par(oldpar)

ntile2_asc_revasc_left_to_right <- function(L, R, threads=2, P = seq_len(threads)/threads, K=round(P*N))
{
  stopifnot(threads >= 2)
  N <- length(L) + length(R)
  K <- as.integer(K)
  KU <- unique(K)
  ret <- .Call(C_r_Ntile2_asc_revasc_left_to_right
               , L = L
               , R = R
               , K = KU
  )
  dim(ret) <- c(2L, length(KU))
  ret <- ret[,match(K, KU), drop=FALSE]
  dimnames(ret) <- list(c("l","r"), K)
  ret
}



#' ntiles of two ASC ASC sorted arrays from RIGHT to LEFT
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param threads scalar number of threads
#' @param P vector number of split fractions from the RIGHT (default: parameter 'threads' equally sized fractions)
#' @param K vector number of elements to be separated on the RIGHT (default: derived from parameter P)
#'
#' @return a 2xlength(K) matrix
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' threads <- 8
#' lr = ntile2_asc_asc_right_to_left(L,R,threads)
#' ylim = range(c(L,R))
#' lrna = lr; lrna[lrna==0] = NA
#' m = pmin(L[Ln+1-lrna["l",]], R[Rn+1-lrna["r",]], na.rm=TRUE)
#' oldpar <- par(mfrow=c(1,2), bg="grey40")
#' plot(L, ylim=ylim, col=rev(rep(seq_len(threads), diff(c(0,lr['l',])))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' plot(R, ylim=ylim, col=rev(rep(seq_len(threads), diff(c(0,lr['r',])))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' par(oldpar)

ntile2_asc_asc_right_to_left <- function(L, R, threads=2, P = seq_len(threads)/threads, K=round(P*N))
{
  stopifnot(threads >= 2)
  N <- length(L) + length(R)
  K <- as.integer(K)
  KU <- unique(K)
  ret <- .Call(C_r_Ntile2_asc_asc_right_to_left
               , L = L
               , R = R
               , K = KU
  )
  dim(ret) <- c(2L, length(KU))
  ret <- ret[,match(K, KU), drop=FALSE]
  dimnames(ret) <- list(c("l","r"), K)
  ret
}


#' ntiles of two ASC REVASC sorted arrays from RIGHT to LEFT
#'
#' @param L left sorted vector (ascending)
#' @param R right sorted vector  (ascending)
#' @param threads scalar number of threads
#' @param P vector number of split fractions from the RIGHT (default: parameter 'threads' equally sized fractions)
#' @param K vector number of elements to be separated on the RIGHT (default: derived from parameter P)
#'
#' @return a 2xlength(K) matrix
#' @export
#'
#' @examples
#' Ln = 2^12
#' Rn = 2^12
#' N = Ln+Rn
#' skew = runif(1, -2^8, 2^8)
#' L = rchisq(Ln, N-skew)
#' R = rchisq(Rn, N+skew)
#' L = L[order(current_keys(L))]
#' R = R[order(current_keys(R))]
#' threads <- 8
#' lr = ntile2_asc_revasc_right_to_left(L,rev(R),threads)
#' ylim = range(c(L,R))
#' lrna = lr; lrna[lrna==0] = NA
#' m = pmin(L[Ln+1-lrna["l",]], rev(R)[lrna["r",]], na.rm=TRUE)
#' oldpar <- par(mfrow=c(1,2), bg="grey40")
#' plot(L, ylim=ylim, col=rev(rep(seq_len(threads), diff(c(0,lr['l',])))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' plot(rev(R), ylim=ylim, col=rep(seq_len(threads), diff(c(0,lr['r',]))), pch='.')
#' abline(h=m, col=seq_len(threads))
#' par(oldpar)

ntile2_asc_revasc_right_to_left <- function(L, R, threads=2, P = seq_len(threads)/threads, K=round(P*N))
{
  stopifnot(threads >= 2)
  N <- length(L) + length(R)
  K <- as.integer(K)
  KU <- unique(K)
  ret <- .Call(C_r_Ntile2_asc_revasc_right_to_left
               , L = L
               , R = R
               , K = KU
  )
  dim(ret) <- c(2L, length(KU))
  ret <- ret[,match(K, KU), drop=FALSE]
  dimnames(ret) <- list(c("l","r"), K)
  ret
}




if (FALSE){

  require(greeNsort)

  for (N in 0:32){
    for (nl in 0:N){
      nr = N - nl
        for(r in 1:(8*N)){
          set.seed(r)
        N = nl+nr
        skew = runif(1, -N/16, N/16)
        L = rchisq(nl, N-skew)
        R = rchisq(nr, N+skew)
        X = testdb$func$stabletie(nl+nr)
        L = X[seq_len(nl)]
        R = X[seq_len(nr)+nl]
        rm(X)
        L = L[order(current_keys(L))]
        R = R[order(current_keys(R))]
        for (k in 0:N){
          cat("r=", r, "  nl=", nl, "  nr=", nr, "k=", k, "\n")
          xlr = Rntile2_asc_asc_left_to_right_expensive(L,R,k=k)
          lr = Rntile2_asc_asc_left_to_right(L,R,k=k)
          stopifnot(identical(lr, xlr))
        }
      }
    }
  }

}
