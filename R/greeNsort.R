# greeNsort algorithms, datagenerators and databases
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

# tools::package_native_routine_registration_skeleton('c:/tmp/green/greeNsort','c:/tmp/green/greeNsort/src/init.c')


#' greeNsort color
#'
#' @format A scalar character color value
#' @export
greensort_color <- rgb(0, 104, 55, maxColorValue = 255)


#' cran_compatible
#'
#' Show compilation choice for memory management and random number generation
#'
#' @return \code{TRUE} if the package has beed compiled under R's memory management (Calloc,Free) for CRAN conpatibility,
#' and returns \code{FALSE} if the package has beed compiled for with the standard C memory management (malloc,free).
#' @seealso \code{\link{greeNsort-package}}
#' @examples
#'   cran_compatible()
#' @export

cran_compatible <- function()
{
  .Call(C_r_cran_compatible)
}


#' string check
#'
#' find positions in string that equal the BUFFER_CHAR
#'
#' @param x a vector of \code{\link{character}}
#'
#' @return vector of pos < nchar(x), where pos==0 for strings that are OK
#' @export
#'
#' @examples
#' stopifnot(all(string_check(vtestdb$func$KafkaWords())==0))
#' stopifnot(all(string_check(vtestdb$func$KafkaParts())==0))
#' stopifnot(all(string_check(vtestdb$func$BibleWords())==0))
#' stopifnot(all(string_check(vtestdb$func$BibleVerses())==0))
#' stopifnot(all(string_check(vtestdb$func$DictWords())==0))
string_check <- function(x)
{
  if (typeof(x) != "character")
    stop("x must be string")
  .Call(C_r_string_check, x = x)
}


#' stable_keys
#'
#' Convert Keys to Keys used in stable testing
#'
#' doubles are truncated towards zero
#' strings must be shorter equal 20 and are truncated from the first dot to the right
#'
#' @param x a double or character vector to be rounded (or not)
#' @return The rounded
#' @seealso \code{\link{greeNsort-package}}
#' @examples
#'   stable_keys(seq(-10, 10, 0.5))
#'   stable_keys(num2str(seq(-10, 10, 0.5)))
#' @export

stable_keys <- function(x)
{
  if (typeof(x) == "double"){
    .Call(C_r_stable_keys, x = x)
  }else if (typeof(x) == "character"){
    .Call(C_r_stable_keys, x = x)
  }else stop("x must be double or double converted to character")
}


#' current_keys
#'
#' Show compilation choice for stability testing
#'
#' The package allows rounding of key values before comparision for testing of sorting stability:
#' the sorting comparator does not see decimals
#' and the sporting algorithm must not change the order of decimals within ties.
#' This provides an API into the rounding (or not rounding) of the key values.
#'
#' @param x a double or character vector to be rounded (or not)
#' @return The rounded or not-rounded input values
#' @seealso \code{\link{greeNsort-package}}
#' @examples
#'   current_keys(seq(-10, 10, 0.5))
#'   current_keys(num2str(seq(-10, 10, 0.5)))
#' @export

current_keys <- function(x)
{
  if (typeof(x) == "double"){
    .Call(C_r_current_keys, x = x)
  }else if (typeof(x) == "character"){
    if (.Call(C_r_current_keys, x = 0.5) == 0){
      sub("[.].*", "", x)
    }else{
      x
    }
  }else stop("x must be double or double converted to character")
}



#' string compare
#'
#' access to the 3-way string compare function compiled into the size-varying (string) sorting routines
#'
#' @param x a vector of \code{\link{character}}
#' @param y a vector of \code{\link{character}}
#'
#' @return -1,0,+1 for x<y,x==y,x>y
#' @export
#'
#' @examples
#' current_compare("1.1","1.1")
#' current_compare("1.1","1.9")
#' current_compare("1.9","1.1")
#' current_compare(1.1,1.1)
#' current_compare(1.1,1.9)
#' current_compare(1.9,1.1)
current_compare <- function(x, y)
{
  if (length(y)==1 && length(x)>1)
    y <- rep(y, length=length(x))
  .Call(C_r_current_compare, x = x, y = y)
}

#' check sortedness of string
#'
#' check sortedness of string given the definition used in \code{\link{current_compare}}
#'
#' @param x a vector of \code{\link{double}} or \code{\link{character}}
#'
#' @return logical
#' @export
#'
#' @examples

#' current_issorted(c("1.1","1.1"))
#' current_issorted(c("1.1","1.9"))
#' current_issorted(c("1.9","1.1"))
#' current_issorted(c(1.1,1.1))
#' current_issorted(c(1.1,1.9))
#' current_issorted(c(1.9,1.1))
#' current_issorted(c("a","\u00bbWas","z.b","z.a"))
#' !is.unsorted(c("a","\u00bbWas","z.b","z.a"))
#' current_issorted(c(1.9, 1.1, 9.9, 9.1))
#' !is.unsorted(c(1.9, 1.1, 9.9, 9.1))
current_issorted <- function(x){
  # n <- length(x)
  # if (n < 2)
  #   return(TRUE)
  # all(current_compare(x[-n], x[-1]) <= 0)
  .Call(C_r_current_issorted, x = x)
}


#' Convert numeric test data to string test data
#'
#' @param x a numeric vector
#'
#' @return a character vector
#' @export
#'
#' @examples
#' num2str(testdb$func$stable(12))
num2str <- function(x)
{
  s <- format(x, scientific = FALSE)
  neg <- grep("-", s)
  if (length(neg)){
    s <- gsub("[ -]","0",s)
    s[neg] <- paste0("-", substr(s[neg], 2, nchar(s[neg])))
    paste0(ifelse(seq_along(s) %in% neg, "-", "+"), substr(s, 2, nchar(s)))
  }else{
    gsub(" ","0",s)
  }
}


#' insertionsort_limit
#'
#' Show compilation choice for insertion sort tuning
#'
#' The package allows compiling with different choices for insertion sort tuning,
#' namely the max value for which recursion is stopped and sorting is delegated to \code{\link{Insertionsort}}.
#' A value of 0 has a special meaning (may compile to somewhat different code for the no-tuning-case).
#'
#' @return the max value for which recursion is stopped and sorting is delegated to \code{\link{Insertionsort}}.
#' @seealso \code{\link{greeNsort-package}}
#' @examples
#'   insertionsort_limit()
#' @export

insertionsort_limit <- function(){
  .Call(C_r_insertionsort_limit)
}

#' Symmetric search
#'
#' Symmetric binary search
#'
#' @param x a vector of data to search in
#' @param vec a scalar to search for
#' @param cmp one of c("WL","L","LT","LE","EQL","EQR","GE","GT","R","WR")
#' @param decreasing FALSE = x is ASC, TRUE = x is DESC
#'
#' @details finds the 'first' element from the decision border defined by cmp the search value.
#' There are two "EQ": "EQL" searches for the left most in case of ties, "EQR" searches for the rightmost.
#' Non-matches return -1 or n on the suitable side, cmp "EQL" and "EQR" return -1 for non-matches
#'
#' @return a one length integer vector giving the found position with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @export
#'
#' @examples
#' h <- seq(-3.5, 3.5, 0.5)
#' col <- rep(c("red","blue"), length.out=length(h))
#' oldpar <- par(mfcol=c(2,2))
#' for (cmp in c("WL","WR")){
#'   x <- sort(rep(-3:3, 10), decreasing=FALSE)
#'   n <- length(x)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("ASC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#'   x <- sort(rep(-3:3, 10), decreasing=TRUE)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("DESC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#' }
#' par(oldpar)
#'
#' h <- seq(-3.5, 3.5, 0.5)
#' col <- rep(c("red","blue"), length.out=length(h))
#' oldpar <- par(mfcol=c(2,2))
#' for (cmp in c("L","R")){
#'   x <- sort(rep(-3:3, 10), decreasing=FALSE)
#'   n <- length(x)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("ASC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#'   x <- sort(rep(-3:3, 10), decreasing=TRUE)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("DESC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#' }
#' par(oldpar)
#'
#' h <- seq(-3.5, 3.5, 0.5)
#' col <- rep(c("red","blue"), length.out=length(h))
#' oldpar <- par(mfrow=c(3,4))
#' for (cmp in c("LT","LE","EQL","EQR","GE","GT")){
#'   x <- sort(rep(-3:3, 10), decreasing=FALSE)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("ASC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#'   x <- sort(rep(-3:3, 10), decreasing=TRUE)
#'   v <- sapply(h, symsearch, x, cmp)
#'   i <- v>=1 & v<=n
#'   plot(x, main=paste("DESC", cmp), ylim=range(h))
#'   abline(h=h[!i], col=col[!i], lty=3)
#'   points(v[i],h[i],col=col[i], lwd=2, pch=1, cex=2)
#'   y <- x[v[i]]
#'   segments(v[i], h[i], v[i], y, col=col[i], lwd=2)
#' }
#' par(oldpar)

symsearch <- function(x, vec, cmp = c("WL","L","LT","LE","EQL","EQR","GE","GT","R","WR")[5], decreasing=NA){
  cmp <- match(cmp, c("WL","L","LT","LE","EQL","","EQR","GE","GT","R","WR")) - 6L
  if (is.na(decreasing)){
    decreasing <- vec[1] > vec[length(vec)]
  }else{
    if (length(decreasing)!=1)
      stop("illegal decreasing")
    decreasing <- as.logical(decreasing)
  }
  if (length(cmp)!=1 || is.na(cmp) || cmp=="")
    stop("illegal cmp")
  ret <- .Call(
    C_r_symsearch
    , x = as.double(vec)
    , v = as.double(x)
    , cmp = cmp
    , decreasing = decreasing
  )
  ret <- retperf(ret, "symsearch")
  ret
}


#' Chunksort
#'
#' a utility for test data generation which sorts sqrt(N) chunks of sqrt(N) elements using \code{\link{Quicksort2}}
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param b an integer block size (default sqrt(N))
#' @param direction an integer: -1 is descending, 1 is ascending, 0 is ascending-descending
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param perf FALSE to avoid measurement at startup (when grants might still be missing)
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{order}} for standard R ordering, \code{\link{Insertionsort}} for the sorting version,
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' perf(Chunksort(x))
#' oldpar <- par(pty="s")
#' plot(x)
#' lines(x)
#' par(oldpar)
#' @export
Chunksort <- function(
    x
    , b = sqrt(length(x))
    , direction = 1L
    , situation=c("insitu", "exsitu")
    , perf = TRUE
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  b <- max(1L, as.integer(b))
  direction <- as.integer(sign(direction))
  stopifnot(b >= 1 && b <= length(x))
  situation <- match.arg(situation)
  if (perf){
  if (situation == "insitu") {
    ret <- .Call(C_r_Chunksort_insitu
                 , x = x
                 , b = b
                 , d = direction
    )
  }else{
    ret <- .Call(C_r_Chunksort_exsitu
                 , x = x
                 , b = b
                 , d = direction
    )
  }
  ret <- retperf(ret, "Chunksort")
  }else{
    ret <- .Call(C_r_Chunksort_noperf
                 , x = x
                 , b = b
                 , d = direction
    )
  }
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Sel'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Insertionsort}} for the classic Insertionsort with squared cost
#' \code{\link{Selectionsort2}} for a square-root Selectionsort with less than squared cost
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Insertionsort(y))}
#' ,{y <- x[]; sperf(Selectionsort(y))}
#' ,{y <- x[]; sperf(Selectionsort2(y))}
#' )
#' @export

Selectionsort <- function(x
                     , situation=c(
                       "insitu"
                       ,"exsitu"
                     )
                     , method=c(
                       "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Selectionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Selectionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Selectionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Sel2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Insertionsort}} for the classic Insertionsort with squared cost
#' \code{\link{Selectionsort}} for the classic Selectionsort with squared cost
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Insertionsort(y))}
#' ,{y <- x[]; sperf(Selectionsort(y))}
#' ,{y <- x[]; sperf(Selectionsort2(y))}
#' )
#' @export

Selectionsort2 <- function(x
                          , situation=c(
                            "insitu"
                            ,"exsitu"
                          )
                          , method=c(
                            "index"
                          )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Selectionsort2_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Selectionsort2_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Selectionsort2")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Ins'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param direction "l2r" for reading and writing stable ties from left to right, "r2l" or "rev" for writing (hence reversing) stable ties from right to left, i.e. the result is reverse ascening or reverse descending.
#' @param decreasing default \code{FALSE} for ascending,  \code{TRUE} for descending
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{sort}} for standard R sorting,
#' \code{\link{Selectionsort}} for Selectionsort,
#' \code{\link{RInsertionsort}} and \code{\link{SInsertionsort}} for indirect versions,
#' \code{\link{UInsertionsort}} and \code{\link{WInsertionsort}} for indirect size-varying versions,
#' and \code{\link{VInsertionsort}} for a direct size-varying version.
#' Finally  \code{\link{MInsertionsort}} for sorting of matrix-columns by first row-values
#' and \code{\link{NInsertionsort}} for sorting such matrices by sorting separated keys and then permuting the columns.
#' @examples
#' x <- testdb$func$permut(2^6)
#' sperf(Insertionsort(x))
#' x
#' @export

Insertionsort <- function(
    x
    , direction=c("l2r", "r2l","rev")
    , decreasing = FALSE
    , situation=c("insitu", "exsitu")
    , method=c("index")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  direction <- match.arg(direction)
  situation <- match.arg(situation)
  if (situation == "insitu") {
    if (decreasing) {
      if (direction == "l2r") {
        ret <- .Call(C_r_Insertionsort_desc_l2r_insitu
                     , x = x
        )
      }else{
        ret <- .Call(C_r_Insertionsort_desc_r2l_insitu
                     , x = x
        )
      }
    }else{
      if (direction == "l2r") {
        ret <- .Call(C_r_Insertionsort_l2r_insitu
                     , x = x
        )
      }else{
        ret <- .Call(C_r_Insertionsort_r2l_insitu
                     , x = x
        )
      }
    }
  }else{
    if (decreasing) {
      if (direction == "l2r") {
        ret <- .Call(C_r_Insertionsort_desc_l2r_exsitu
                     , x = x
        )
      }else{
        ret <- .Call(C_r_Insertionsort_desc_r2l_exsitu
                     , x = x
        )
      }
    }else{
      if (direction == "l2r") {
        ret <- .Call(C_r_Insertionsort_l2r_exsitu
                     , x = x
        )
      }else{
        ret <- .Call(C_r_Insertionsort_r2l_exsitu
                     , x = x
        )
      }
    }
  }
  ret <- retperf(ret, "Insertionsort")
  ret
}



#' Insertionorder
#'
#' like \code{\link{Insertionsort}} but sorting an index instead of the data (random access to the data)
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param o a vector with integer positions in x to be sorted with x
#' @param direction "l2r" for reading and writing stable ties from left to right, "r2l" or "rev" for writing (hence reversing) stable ties from right to left, i.e. the result is reverse ascening or reverse descending.
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{order}} for standard R ordering, \code{\link{Insertionsort}} for the sorting version,
#' @examples
#' x <- testdb$func$permut(2^6)
#' i <- seq_along(x)
#' sperf(Insertionorder(x, i))
#' x
#' x[i]
#' @export

Insertionorder <- function(
    x
    , o=seq_along(x)
    , direction=c("l2r", "r2l","rev")
    , situation=c("insitu", "exsitu")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  if (typeof(o) != "integer")
    stop("o must be integer")
  direction <- match.arg(direction)
  situation <- match.arg(situation)
  if (situation == "insitu") {
    if (direction == "l2r") {
      ret <- .Call(C_r_Insertionorder_l2r_insitu
                   , x = x
                   , o = o
      )
    }else{
      ret <- .Call(C_r_Insertionorder_r2l_insitu
                   , x = x
                   , o = o
      )
    }
  }else{
    if (direction == "l2r") {
      ret <- .Call(C_r_Insertionorder_l2r_exsitu
                   , x = x
                   , o = o
      )
    }else{
      ret <- .Call(C_r_Insertionorder_r2l_exsitu
                   , x = x
                   , o = o
      )
    }
  }
  ret <- retperf(ret, "Insertionorder")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'RIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Insertionsort}} for the standard,
#' \code{\link{SInsertionsort}} for a version that avoids random access
#' \code{\link{UInsertionsort}} and \code{\link{WInsertionsort}} for versions for size-varying elemen
#' \code{\link{RQuicksort2}} for an algorithm that uses \code{RInsertionsort}
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(RInsertionsort(y))}
#' ,{y <- x[]; sperf(SInsertionsort(y))}
#' ,{y <- x[]; sperf(Insertionsort(y))}
#' )
#' @export

RInsertionsort <- function(x
                           , situation=c("insitu", "exsitu")
                           , method=c("index")
)
{
  if (!is.double(x))
    stop("only double vectors implemented")
  method <- match.arg(method)
  situation <- match.arg(situation)
  if (situation == 'insitu') {
    ret <- .Call(C_r_RInsertionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_RInsertionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "RInsertionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'SIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Insertionsort}} for the standard,
#' \code{\link{RInsertionsort}} for a version sorts pointers to doubles (random access on doubles)
#' \code{\link{SQuicksort2}} for an algorithm that uses \code{SInsertionsort}
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(SInsertionsort(y))}
#' ,{y <- x[]; sperf(RInsertionsort(y))}
#' ,{y <- x[]; sperf(Insertionsort(y))}
#' )
#' @export

SInsertionsort <- function(x
                           , situation=c("insitu", "exsitu")
                           , method=c("index")
)
{
  if (!is.double(x))
    stop("only double vectors implemented")
  method <- match.arg(method)
  situation <- match.arg(situation)
  if (situation == 'insitu') {
    ret <- .Call(C_r_SInsertionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_SInsertionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "SInsertionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'UIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Insertionsort}} for the standard,
#' \code{\link{UZacksort}} for an algorithm that uses \code{UInsertionsort}
#' \code{\link{WInsertionsort}} for a version that is explicitely stabilized
#' \code{\link{VInsertionsort}} for a version sorts elements of varying size directly (moving elements, not pointers)
#' \code{\link{RInsertionsort}} for a version that sorts doubles via pointers
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- vtestdb$func$KafkaWords()[1:n]
#' rbind(
#'  {y <- x[]; sperf(UInsertionsort(y))}
#' ,{y <- x[]; sperf(WInsertionsort(y))}
#' )
#' @export

UInsertionsort <- function(x
                           , situation=c("insitu", "exsitu")
                           , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  method <- match.arg(method)
  situation <- match.arg(situation)
  if (situation == 'insitu') {
    ret <- .Call(C_r_UInsertionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_UInsertionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "UInsertionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'WIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Insertionsort}} for the standard,
#' \code{\link{WQuicksort2}} for an algorithm that uses \code{WInsertionsort}
#' \code{\link{UInsertionsort}} for a version that is not explicitely stabilized (but also stable)
#' \code{\link{VInsertionsort}} for a version sorts elements of varying size directly (moving elements, not pointers)
#' \code{\link{RInsertionsort}} for a version that sorts doubles via pointers
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- vtestdb$func$KafkaWords()[1:n]
#' rbind(
#'  {y <- x[]; sperf(WInsertionsort(y))}
#' ,{y <- x[]; sperf(UInsertionsort(y))}
#' )
#' @export

WInsertionsort <- function(x
                           , situation=c("insitu", "exsitu")
                           , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  method <- match.arg(method)
  situation <- match.arg(situation)
  if (situation == 'insitu') {
    ret <- .Call(C_r_WInsertionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_WInsertionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "WInsertionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'VIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Insertionsort}} for the standard,
#' \code{\link{VKnuthsort}} and \code{\link{VFrogsort1}} for algorithms that use \code{VInsertionsort}
#' \code{\link{UInsertionsort}} and \code{\link{WInsertionsort}} for versions that sort elements of varying size indirectly (moving pointers, not elements)
#' \code{\link{RInsertionsort}} for a version that sorts doubles via pointers
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- vtestdb$func$KafkaWords()[1:n]
#' rbind(
#'  {y <- x[]; sperf(VInsertionsort(y))}
#' ,{y <- x[]; sperf(UInsertionsort(y))}
#' ,{y <- x[]; sperf(WInsertionsort(y))}
#' )
#' @export

VInsertionsort <- function(x
                           , situation=c("insitu", "exsitu")
                           , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  method <- match.arg(method)
  situation <- match.arg(situation)
  if (situation == 'insitu') {
    ret <- .Call(C_r_VInsertionsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_VInsertionsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "VInsertionsort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'MIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{integer}} matrix to be sorted columnwise by key in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Insertionsort}} for standard Insertionsort of vectors and
#' \code{\link{NInsertionsort}} for sorting matrices by sorting separated keys and then permuting the columns.
#' \code{\link{MKnuthsort}} for a scalable algorithm for sorting matrices using this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(MInsertionsort(x))
#' x
#' @export

MInsertionsort <- function(
    x
    , situation=c("insitu", "exsitu")
    , method=c("index")
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  if (situation == "insitu") {
    ret <- .Call(C_r_MInsertionsort_l2r_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_MInsertionsort_l2r_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "MInsertionsort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'NIns'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a integer matrix, columns to be sorted by the values in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Insertionsort}} for standard Insertionsort of vectors and
#' \code{\link{MInsertionsort}} for sorting of matrix-columns including non-separated keys
#' \code{\link{NKnuthsort}} for a scalable algorithm for sorting matrices using this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(NInsertionsort(x))
#' x
#' @export

NInsertionsort <- function(
    x
    , situation=c("insitu", "exsitu")
    , method=c("index")
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  if (situation == "insitu") {
    ret <- .Call(C_r_NInsertionsort_l2r_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_NInsertionsort_l2r_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "NInsertionsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'aMd'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the approximate median as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{median}} for R's standard median, \code{\link{Pickselect}} for an reliable exact P&P selection algorithm using it
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; m <- approxMedian(y)
#' x
#' y
#' median(x)
#' m
#' @export

approxMedian <- function(x
                         , situation=c("insitu","exsitu")
                         , method=c("index")
) {
  situation <- match.arg(situation)
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  if (situation == "insitu")
    ret <- .Call(C_r_approxMedian_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_approxMedian_exsitu
                 , x = x
    )
  attr(ret, 'perf') <- retperf(ret, "approxMedian")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'bfprt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Pickselect}} for a faster version using \code{\link{approxMedian}}
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; s <- BFPRTselect(y, k=n/2)
#' x
#' y
#' m <- approxMedian(x)
#' quantile(x, 0.5, type=1)
#' s
#' m
#' @export

BFPRTselect <- function(x, k=length(x)/2
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_BFPRTselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_BFPRTselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "BFPRTselect")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'pick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{BFPRTselect}} for a slower algorithm by Blum, M.; Floyd, R. W.; Pratt, V. R.; Rivest, R. L.; Tarjan, R. E. (1973)
#' \code{\link{Quickselect}} for a usually faster version with random pivots
#' \code{\link{Zickselect}} for the greeNsort version giving more informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; s <- Pickselect(y, k=n/2)
#' x
#' y
#' m <- approxMedian(x)
#' quantile(x, 0.5, type=1)
#' s
#' m
#' @export

Pickselect <- function(x, k=length(x)/2
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Pickselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Pickselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Pickselect")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Pickselect}} for a prior-art version giving less informative output
#' \code{\link{Zackselect}} for a usually faster version with random pivots
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' k <- n%/%2
#' y <- x[]; p <- Pickselect(y, k)
#' z <- x[]; v <- Zickselect(z, k)
#' plot(sort(x), main="fully sorted data")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(y, main="partially sorted data (Pickselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(z, main="partially sorted data (Zickselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' @export

Zickselect <- function(x, k=length(x)/2
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Zickselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Zickselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Zickselect")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'quick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}}, like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Pickselect}} for a usually slower version with \code{\link{approxMedian}} pivots
#' \code{\link{Zackselect}} for the greeNsort version giving more informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; q <- Quickselect(y)
#' y <- x[]; z <- Zackselect(y)
#' y <- x[]; p <- Pickselect(y)
#' q
#' z
#' p
#' @export

Quickselect <- function(x, k=length(x)/2
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Quickselect2_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Quickselect2_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Quickselect")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zack'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Quickselect}} for a prior-art version giving less informative output
#' \code{\link{Zuckselect}} for a similar algorithm that only switches direction on one branch
#' \code{\link{Zickselect}} for a usually slower version with \code{\link{approxMedian}} pivots
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' k <- n%/%2
#' y <- x[]; p <- Quickselect(y, k)
#' z <- x[]; v <- Zackselect(z, k)
#' plot(sort(x), main="fully sorted data")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(y, main="partially sorted data (Quickselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(z, main="partially sorted data (Zackselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' @export

Zackselect <- function(x, k=length(x)/2
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Zackselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Zackselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Zackselect")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zuck'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Quickselect}} for a prior-art version giving less informative output
#' \code{\link{Zackselect}} for a similar algorithm switching direction on both branches
#' \code{\link{Duckselect}} for a version that is adaptive to presorted data
#' \code{\link{ZuckselectB}} for a version reducing branch-misprediction using block tuning
#' \code{\link{Zickselect}} for a usually slower version with \code{\link{approxMedian}} pivots
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' k <- n%/%2
#' y <- x[]; p <- Quickselect(y, k)
#' z <- x[]; v <- Zuckselect(z, k)
#' plot(sort(x), main="fully sorted data")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(y, main="partially sorted data (Quickselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(z, main="partially sorted data (Zuckselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' @export

Zuckselect <- function(x, k=length(x)/2
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Zuckselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Zuckselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Zuckselect")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'duck'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Zuckselect}} for a optimal version non-adaptive to presorting
#' \code{\link{DuckselectB}} for a version reducing branch-misprediction using block tuning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' k <- n%/%2
#' y <- x[]; p <- Quickselect(y, k)
#' z <- x[]; v <- Duckselect(z, k)
#' plot(sort(x), main="fully sorted data")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(y, main="partially sorted data (Quickselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' plot(z, main="partially sorted data (Duckselect)")
#' abline(h=p, col="blue")
#' abline(v=v, col=greensort_color)
#' abline(v=k, col="red")
#' @export

Duckselect <- function(x, k=length(x)/2
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Duckselect_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Duckselect_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Duckselect")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zuckB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Zuckselect}} for the version without block-tuning
#' \code{\link{DuckselectB}} for a version that is adaptive to presorted data
#' @examples
#' message("use bigger n, eg.g. 2^20, to see timing difference")
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; z <- Zuckselect(y)
#' y <- x[]; b <- ZuckselectB(y)
#' z
#' b
#' @export

ZuckselectB <- function(x, k=length(x)/2
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_ZuckselectB_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_ZuckselectB_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "ZuckselectB")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'duckB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return a integer pair of positions k_min, k_max with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{ZuckselectB}} for a version not tuned for presorted data,
#' \code{\link{Duckselect}} for the version not tuned to minimize branch-misprediction,
#' @examples
#' message("use bigger n, eg.g. 2^20, to see timing difference")
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; z <- Duckselect(y)
#' y <- x[]; b <- DuckselectB(y)
#' z
#' b
#' @export

DuckselectB <- function(x, k=length(x)/2
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_DuckselectB_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_DuckselectB_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "DuckselectB")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'quickpl'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Quickselect}} for selection
#' \code{\link{Quickpart}} for partial sorting on both ends
#' \code{\link{Quickpartright}} for partial sorting on the right end
#' \code{\link{Zackpartleft}} for the greeNsort version giving more informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; q <- Quickpartleft(y);
#' y <- x[]; q <- Quickpartright(y)
#' y <- x[]; q <- Zackpartleft(y)
#' y <- x[]; q <- Quickselect(y)
#' q
#' @export

Quickpartleft <- function(x, k=length(x)/2
                          , situation=c("insitu","exsitu")
                          , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Quickpartleft2_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Quickpartleft2_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Quickpartleft")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'quickpr'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Quickselect}} for select
#' \code{\link{Quickpart}} for partial sorting on both ends
#' \code{\link{Quickpartleft}} for partial sorting on the left end
#' \code{\link{Zackpartright}} for the greeNsort version giving more informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; q <- Quickpartright(y)
#' y <- x[]; q <- Quickpartleft(y)
#' y <- x[]; q <- Zackpartright(y)
#' y <- x[]; q <- Quickselect(y)
#' q
#' @export

Quickpartright <- function(x, k=length(x)/2
                           , situation=c("insitu","exsitu")
                           , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Quickpartright2_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Quickpartright2_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Quickpartright")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zackpl'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Zackselect}} for selection
#' \code{\link{Zackpart}} for partial sorting on both ends
#' \code{\link{Zackpartright}} for partial sorting on the right end
#' \code{\link{Quickpartleft}} for the classic version giving less informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; q <- Zackpartleft(y)
#' y <- x[]; q <- Zackpartright(y)
#' y <- x[]; q <- Quickpartleft(y)
#' y <- x[]; q <- Zackselect(y)
#' q
#' @export

Zackpartleft <- function(x, k=length(x)/2
                         , situation=c("insitu","exsitu")
                         , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Zackpartleft_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Zackpartleft_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Zackpartleft")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zackpr'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a double vector for searching (re-arranged with situation='insitu')
#' @param k the position within sorted x for which we ask the value
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return the value \code{x[k]}  as a double scalar with an attribute \code{\link{perf}} like \code{\link{rawperf}}
#' @seealso \code{\link{quantile}} for R's standard quantiles,
#' \code{\link{Zackselect}} for select
#' \code{\link{Zackpart}} for partial sorting on both ends
#' \code{\link{Zackpartleft}} for partial sorting on the left end
#' \code{\link{Quickpartright}} for the classic version giving less informative output
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; q <- Zackpartright(y)
#' y <- x[]; q <- Zackpartleft(y)
#' y <- x[]; q <- Quickpartright(y)
#' y <- x[]; q <- Zackselect(y)
#' q
#' @export

Zackpartright <- function(x, k=length(x)/2
                          , situation=c("insitu","exsitu")
                          , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  method <- match.arg(method)
  k <- as.integer(k)
  if (k < 1 || k > length(x))
    stop("k must be in [1,n] where n=length(x)")
  situation <- match.arg(situation)
  if (situation == "insitu")
    ret <- .Call(C_r_Zackpartright_insitu
                 , x = x
                 , k = k
    )
  else
    ret <- .Call(C_r_Zackpartright_exsitu
                 , x = x
                 , k = k
    )
  attr(ret, 'perf') <- retperf(ret, "Zackpartright")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'dotnetpt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quickpart}} for a proper quick partial sorting algorithm
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Dotnetpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Quickpart(y, c(0.33, 0.67)*n)); plot(y)
#' @export


Dotnetpart <- function(x
                       , partial=c(1L, length(x))
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Dotnetpart_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_Dotnetpart_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "Dotnetpart")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'quickpt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quickpartleft}} for a proper quick partial sorting algorithm
#' \code{\link{Dotnetpart}} for the weird beginners algorithm used in dot.net
#' \code{\link{Zackpart}} for a greeNsort algorithm that extends the sorted part to all ties
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Quickpartleft(y, 0.67*n)); plot(y)
#' y <- x[]; sperf(Dotnetpart(y, c(1, 0.67*n))); plot(y)
#' y <- x[]; sperf(Dotnetpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Quickpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Zackpart(y, c(0.33, 0.67)*n)); plot(y)
#' @export


Quickpart <- function(x
                      , partial=c(1L, length(x))
                      , situation=c("insitu","exsitu")
                      , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Quickpart2_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_Quickpart2_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "Quickpart")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zackpt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quickpart}} for the classic quick algorithm that does not guarantee to include all ties
#' \code{\link{Zuckpart}} for a version that switches direction only on the required branch
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Quickpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Zackpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Zuckpart(y, c(0.33, 0.67)*n)); plot(y)
#' @export

Zackpart <- function(x
                     , partial=c(1L, length(x))
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Zackpart_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_Zackpart_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "Zackpart")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zuckpt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zackpart}} for a version that switches direction on both branches
#' \code{\link{ZuckpartB}} for a version using block-tuning
#' \code{\link{Duckpart}} for a version tuned to presorted data
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Zackpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Zuckpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Duckpart(y, c(0.33, 0.67)*n)); plot(y)
#' @export


Zuckpart <- function(x
                     , partial=c(1L, length(x))
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Zuckpart_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_Zuckpart_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "Zuckpart")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'zuckptB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zuckpart}} for a version not block-tuned
#' \code{\link{DuckpartB}} for a version tuned to presorted data
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Zuckpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(ZuckpartB(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(DuckpartB(y, c(0.33, 0.67)*n)); plot(y)
#' @export


ZuckpartB <- function(x
                      , partial=c(1L, length(x))
                      , situation=c("insitu","exsitu")
                      , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_ZuckpartB_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_ZuckpartB_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "ZuckpartB")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'duckpt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zuckpart}} for a version not tuned for presorted data
#' \code{\link{DuckpartB}} for a block-tuned version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(Zuckpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Duckpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(DuckpartB(y, c(0.33, 0.67)*n)); plot(y)
#' @export

Duckpart <- function(x
                     , partial=c(1L, length(x))
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Duckpart_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_Duckpart_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "Duckpart")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'duckptB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param partial a double vector with two elements for lower and upper limit of partial sorting
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{ZuckpartB}} for a version not tuned to presorted data
#' \code{\link{Duckpart}} for a version not block-tuned
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$tiesqrt(n)
#' y <- x[]; sperf(DuckpartB(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(Duckpart(y, c(0.33, 0.67)*n)); plot(y)
#' y <- x[]; sperf(ZuckpartB(y, c(0.33, 0.67)*n)); plot(y)
#' @export


DuckpartB <- function(x
                      , partial=c(1L, length(x))
                      , situation=c("insitu","exsitu")
                      , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  partial <- as.integer(partial)
  stopifnot(length(partial) == 2 && partial[1]<=partial[2] & partial[1] >= 1 & partial[2] <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_DuckpartB_insitu
                 , x = x, partial = partial
    )
  }else{
    ret <- .Call(C_r_DuckpartB_exsitu
                 , x = x, partial = partial
    )
  }
  ret <- retperf(ret, "DuckpartB")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Lomuto'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort1}} for Hoares original version
#' \code{\link{Quicksort2}} for the established binary version of Quicksort
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Lomutosort(y))}
#' ,{y <- x[]; sperf(Quicksort1(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

Lomutosort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Lomutosort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Lomutosort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Lomutosort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Quick1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Lomutosort}} for Bentley's version with Lomuto-partitioning
#' \code{\link{Quicksort2}} for the established binary version of Quicksort
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort1(y))}
#' ,{y <- x[]; sperf(Lomutosort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

Quicksort1 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Quicksort1_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Quicksort1_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Quicksort1")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Quick2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Picksort}} for a version that uses  \code{\link{approxMedian}} pivots
#' \code{\link{Quicksort3}} for Bentley&McIlroy's algorithm that invests extra operations for isolating a (third) partition with pivot-ties for early termination.
#' \code{\link{Dupisort}} for Yaroslavskyi's 'Dual-Pivot-Quicksort' which invests extra operations for isolating a true third partition
#' \code{\link{Quicksort2B}} for a version that is tuned to avoid branch-(mis)predictions
#' \code{\link{Chicksort}} for a related algorithm that avoids tie swaps
#' \code{\link{Zocksort}} for an asymmetric algorithm that provides early termination on ties but is vulnerable to certain input patterns
#' \code{\link{Zacksort}} for an algorithm with similar costs for non-tied keys that provides early termination on ties
#' \code{\link{SQuicksort2}} for a stabilized version (directly)
#' \code{\link{RQuicksort2}} for a stabilized version (indirectly)
#' \code{\link{WQuicksort2}} for a version sorting strings indirectly (stablized)
#' \code{\link{PQuicksort2}} for a branch-parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(RQuicksort2(y))}
#' ,{y <- x[]; sperf(SQuicksort2(y))}
#' ,{y <- x[]; sperf(Chicksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Chicksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' @export

Quicksort2 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index","pointer")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Quicksort2P_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Quicksort2_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Quicksort2P_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Quicksort2_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Quicksort2")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Quick3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2}} for Hoare's original algorithm without tuning for early termination on ties.
#' \code{\link{Dupisort}} for Yaroslavskyi's 'Dual-Pivot-Quicksort' which invests extra operations for isolating a true third partition
#' \code{\link{Zacksort}} for a algorithm with lower costs for non-tied keys that also provides early termination on ties
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' @export

Quicksort3 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index","pointer")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Quicksort3P_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Quicksort3_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Quicksort3P_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Quicksort3_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Quicksort3")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Dupi'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Tricksort}} for a slightly improved version avoiding some redundand operations
#' \code{\link{Quicksort3}} for Bentley&McIlroy's slower threeway algorithm with tuning for early termination on ties.
#' \code{\link{Quicksort2}} for Hoare's binary algorithm without tuning for early termination on ties.
#' \code{\link{Zacksort}} for a simpler binary greeNsort algorithm with low costs for non-tied keys that also provides early termination on ties but is easier parallelized
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' @export

Dupisort <- function(x
                     , situation=c("insitu","exsitu")
                     , method=c("index","pointer")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_DupisortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Dupisort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_DupisortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Dupisort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Dupisort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Trick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Dupisort}} for Yaroslavskyi's version (but with random pivots and still with redundant operations)
#' \code{\link{Quicksort2}} for Hoare's binary algorithm without tuning for early termination on ties.
#' \code{\link{Quicksort3}} for Bentley&McIlroy's slower threeway algorithm with tuning for early termination on ties.
#' \code{\link{Zacksort}} for a simpler binary algorithm with low costs for non-tied keys that also provides early termination on ties but is easier parallelized
#' \code{\link{Quicksort2B}} and \code{\link{ZacksortB}} as a benchmark
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Dupisort(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}#'
#' )
#' @export

Tricksort <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index","pointer")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_TricksortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Tricksort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_TricksortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Tricksort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Tricksort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Quick2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2}} for the classic version without branch tuning
#' \code{\link{ZacksortB}} for a algorithm with similar costs for non-tied keys that provides early termination on ties
#' \code{\link{SQuicksort2B}} for a stabilized version (directly)
#' \code{\link{RQuicksort2B}} for a stabilized version (indirectly)
#' \code{\link{WQuicksort2B}} for a version sorting strings indirectly (stabilized)
#' \code{\link{PQuicksort2B}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(RQuicksort2B(y))}
#' ,{y <- x[]; sperf(SQuicksort2B(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' )
#' @export

Quicksort2B <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")

  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Quicksort2B_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Quicksort2B_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Quicksort2B")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Plug'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{BFPRTsort}} the sorting analog to \code{\link{BFPRTselect}},
#' \code{\link{Pugsort}} for an algorithm that uses \code{\link{Pickselect}} without following partitioning
#' \code{\link{Picksort}} for an algorithm that uses \code{\link{approxMedian}} followed by partitioning
#' \code{\link{Slicksort}} for an algorithm that uses \code{\link{Quickselect}} followed by partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Plugsort(y))}
#' ,{y <- x[]; sperf(Pugsort(y))}
#' ,{y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Slicksort(y))}
#' )
#' @export

Plugsort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_Plugsort_insitu
               , x = x
               , PACKAGE = "greeNsort"
  )
  ret <- retperf(ret, "Plugsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Pug'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Plugsort}} for an algorithm that uses \code{\link{Pickselect}} followed by partitioning
#' \code{\link{Slicksort}} for an algorithm that uses \code{\link{Quickselect}} followed by partitioning
#' \code{\link{Sicksort}} for an algorithm that uses \code{\link{Quickselect}} without following partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Pugsort(y))}
#' ,{y <- x[]; sperf(Plugsort(y))}
#' ,{y <- x[]; sperf(Slicksort(y))}
#' ,{y <- x[]; sperf(Sicksort(y))}
#' )
#' @export

Pugsort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_Pugsort_insitu
               , x = x
  )
  ret <- retperf(ret, "Pugsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Slick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Plugsort}} for an algorithm that uses \code{\link{Pickselect}} followed by partitioning
#' \code{\link{Sicksort}} for an algorithm that uses \code{\link{Quickselect}} without following partitioning
#' \code{\link{Quicksort2}} for an algorithm that uses a random pivot followed by partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Slicksort(y))}
#' ,{y <- x[]; sperf(Plugsort(y))}
#' ,{y <- x[]; sperf(Sicksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

Slicksort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_Slicksort_insitu
               , x = x
  )
  ret <- retperf(ret, "Slicksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Sick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Pugsort}} for an algorithm that uses \code{\link{Pickselect}} without following partitioning
#' \code{\link{Slicksort}} for an algorithm that uses \code{\link{Quickselect}} followed by partitioning
#' \code{\link{Quicksort2}} for an algorithm that uses random pivots followed by partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Sicksort(y))}
#' ,{y <- x[]; sperf(Pugsort(y))}
#' ,{y <- x[]; sperf(Slicksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

Sicksort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_Sicksort_insitu
               , x = x
  )
  ret <- retperf(ret, "Sicksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'BFPRT'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Plugsort}} for an algorithm that uses \code{\link{BFPRTselect}} followed by partitioning
#' \code{\link{Quicksort2}} for quicksort that uses random pivots followed by partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Plugsort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

BFPRTsort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_BFPRTsort_insitu
               , x = x
  )
  ret <- retperf(ret, "BFPRTsort")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Pick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Plugsort}} for an algorithm that uses \code{\link{Pickselect}} followed by partitioning
#' \code{\link{Quicksort2}} for quicksort that uses random pivots followed by partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Plugsort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' )
#' @export

Picksort <- function(x, situation=c("insitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  ret <- .Call(C_r_Picksort_insitu
               , x = x
  )
  ret <- retperf(ret, "Picksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'RQuick2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param reorder_inplace \code{FALSE} allocates 100\% buffer to speed up final reordering, \code{TRUE} reorders inplace
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{SQuicksort2}} for a stable version that avoids random access
#' \code{\link{Quicksort2}} for the unstable version
#' \code{\link{RQuicksort2B}} for a version that is tuned to avoid branch-(mis)prediction
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(RQuicksort2(y))}
#' ,{y <- x[]; sperf(SQuicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(RQuicksort2B(y))}
#' )
#' @export

RQuicksort2 <- function(x
                        , situation=c("insitu","exsitu")
                        , reorder_inplace = FALSE
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_RQuicksort2_insitu
                 , x = x
                 , reorder_inplace = as.logical(reorder_inplace)
    )
  }else{
    ret <- .Call(C_r_RQuicksort2_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "RQuicksort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'RQuick2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param reorder_inplace \code{FALSE} allocates 100\% buffer to speed up final reordering, \code{TRUE} reorders inplace
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{RQuicksort2}} for the algorithm without tuning
#' \code{\link{SQuicksort2B}} for a tuned stable algorithm that avoids random access
#' \code{\link{Quicksort2B}} for a tuned unstable version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(RQuicksort2B(y))}
#' ,{y <- x[]; sperf(RQuicksort2(y))}
#' ,{y <- x[]; sperf(SQuicksort2B(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' )
#' @export

RQuicksort2B <- function(x
                         , situation=c("insitu","exsitu")
                         , reorder_inplace = FALSE
                         , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_RQuicksort2B_insitu
                 , x = x
                 , reorder_inplace = as.logical(reorder_inplace)
    )
  }else{
    ret <- .Call(C_r_RQuicksort2B_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "RQuicksort2B")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'SQuick2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{RQuicksort2}} for an algorithm using pointers but random access
#' \code{\link{Quicksort2}} for the unstable algorithm
#' \code{\link{SQuicksort2B}} for a version that is tuned to avoid branch-(mis)prediction
#' \code{\link{Knuthsort}} for a stable mergesort using 100\% buffer
#' \code{\link{Frogsort1}} for a stable algorithm using 50\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(SQuicksort2(y))}
#' ,{y <- x[]; sperf(RQuicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(SQuicksort2B(y))}
#' ,{y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(Frogsort1(y))}
#' )
#' @export

SQuicksort2 <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_SQuicksort2_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_SQuicksort2_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "SQuicksort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'SQuick2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{RQuicksort2B}} for an algorithm using pointers but random access
#' \code{\link{Quicksort2B}} for the unstable algorithm
#' \code{\link{SQuicksort2}} for a version that is not tuned to avoid branch-(mis)prediction
#' \code{\link{KatasortB}} for a stable tuned mergesort using 100\% buffer
#' \code{\link{Frogsort1B}} for a stable tuned algorithm using 50\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(SQuicksort2B(y))}
#' ,{y <- x[]; sperf(RQuicksort2B(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(SQuicksort2(y))}
#' ,{y <- x[]; sperf(KatasortB(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' )
#' @export

SQuicksort2B <- function(x
                         , situation=c("insitu","exsitu")
                         , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_SQuicksort2B_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_SQuicksort2B_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "SQuicksort2B")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Chick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2}} for Hoare's original quicksort which swaps ties in order to achieve a balanced partitioning
#' \code{\link{Zacksort}} for for a different approach to symmetry
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Chicksort(y))}
#' )
#' @export

Chicksort <- function(x, situation=c("insitu"), method=c("index","pointer")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (method == "pointer")
    ret <- .Call(C_r_ChicksortP_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Chicksort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Chicksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Zock'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2}} for Hoare's original quicksort which swaps ties in order to achieve a balanced partitioning
#' \code{\link{Zacksort}} for the related symmetric greeNsort algorithm without such vulnerability
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Zocksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Zocksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tieperm1(n)
#' rbind(
#'  {y <- x[]; sperf(Zocksort(y))}     # OK
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' x <- testdb$func$tieperm2(n)
#' rbind(
#'  {y <- x[]; sperf(Zocksort(y))}     # degenerated
#' ,{y <- x[]; sperf(Zacksort(y))}
#' )
#' @export

Zocksort <- function(x
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Zocksort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Zocksort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Zocksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Zack'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zocksort}} for the related asymmetric greeNsort algorithm with vulnerability
#' \code{\link{Zucksort}} for a variant doing zig-zag only on the branch at risk (the branch potentially containing pivot-ties)
#' \code{\link{Zicksort}} for a version using \code{\link{approxMedian}} pivots
#' \code{\link{Quicksort2}} for Hoare's original quicksort which swaps ties in order to achieve a balanced partitioning
#' \code{\link{Quicksort3}} for Bentley&McIlroy's algorithm that invests extra operations for isolating a (third) partition with pivot-ties for early termination.
#' \code{\link{Tricksort}} for a slightly improved version avoiding some redundant operations in Yaroslavskyi's 'Dual-Pivot-Quicksort' which invests extra operations for isolating a true third partition
#' \code{\link{ZacksortB}} for a greeNsort version tuned to avoid branch-(mis)prediction
#' \code{\link{Frogsort2}} for the fastest stable algorithm with O(N*log(N)) worst-case
#' \code{\link{UZacksort}} for a version sorting strings indirectly (not stabilized, stabilization removes ties)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zocksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zocksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zocksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' @export

Zacksort <- function(x
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Zacksort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Zacksort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Zacksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ZackB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zacksort}} for the untuned version
#' \code{\link{Quicksort2B}} for Hoare's quicksort tuned to avoid branch-(mis)prediction
#' \code{\link{Frogsort1B}} for a stable greeNsort algorithm
#' \code{\link{Tricksort}} for a slightly improved version avoiding some redundand operations in Yaroslavskyi's 'Dual-Pivot-Quicksort' which invests extra operations for isolating a true third partition
#' \code{\link{Frogsort2}} for the fastest stable greeNsort algorithm with O(N*log(N)) worst-case
#' \code{\link{UZacksortB}} for a version sorting strings indirectly (not stabilized, stabilization removes ties)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(ZacksortB(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Tricksort(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' @export

ZacksortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_ZacksortB_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_ZacksortB_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "ZacksortB")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Zuck'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zacksort}} for a variant that switches partition directon on both branches
#' \code{\link{ZucksortA}} for a variant tuned for presorting before recursion
#' \code{\link{Ducksort}} for a variant tuned for presorting during recursion
#' \code{\link{ZucksortB}} for a block-tuned variant
#' \code{\link{ZucksortD}} for a variant using a deterministic pivot
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(Quicksort3(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' @export

Zucksort <- function(x
                     , situation=c("insitu","exsitu")
                     , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Zucksort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Zucksort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Zucksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ZuckA'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zucksort}} for the untuned version
#' \code{\link{Ducksort}} for a version recognizing presorting in the recursion
#' \code{\link{Frogsort1A}} for a stable greeNsort algorithm
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' )
#' @export

ZucksortA <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_ZucksortA_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_ZucksortA_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "ZucksortA")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ZuckB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zucksort}} for the untuned version
#' \code{\link{Quicksort2B}} for Hoare's quicksort tuned to avoid branch-(mis)prediction
#' \code{\link{Frogsort1B}} for a stable greeNsort algorithm
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortB(y))}
#' ,{y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(Frogsort1B(y))}
#' )
#' @export

ZucksortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_ZucksortB_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_ZucksortB_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "ZucksortB")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ZuckD'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zucksort}} for the greeNsort algorithm doing zig-zag on both branches
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortD(y))}
#' )
#' @export

ZucksortD <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index")
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_ZucksortD_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_ZucksortD_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "ZucksortD")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Duck'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zucksort}} the greeNsort algorithm for early termination on ties
#' \code{\link{ZucksortA}} for the simple asymmetric basis for this
#' \code{\link{DucksortB}} for the block-tuned version
#' \code{\link{Pdqsort}} for Pattern Defeating Quicksort, not branchless
#' \code{\link{PDucksort}} for parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' @export


Ducksort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Ducksort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Ducksort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Ducksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'DuckB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Ducksort}} for the non-block-tuned version
#' \code{\link{Zucksort}} the greeNsort algorithm for early termination on ties
#' \code{\link{ZucksortA}} for the simple asymmetric basis for this
#' \code{\link{Pdqsort}} for Patterm Defeating Quicksort (branchless)
#' \code{\link{PDucksortB}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(Zucksort(y))}
#' ,{y <- x[]; sperf(ZucksortA(y))}
#' ,{y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(DucksortB(y))}
#' )
#' @export

DucksortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_DucksortB_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_DucksortB_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "DucksortB")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Zick'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Zacksort}} for the version with random pivots
#' \code{\link{Picksort}} for Hoare's quicksort with \code{\link{approxMedian}} as pivot
#' \code{\link{Frogsort2}} for the fastest stable greeNsort algorithm with O(N*log(N)) worst-case
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$tiesqrt(n)
#' rbind(
#'  {y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Zicksort(y))}
#' ,{y <- x[]; sperf(Zacksort(y))}
#' ,{y <- x[]; sperf(Picksort(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' )
#' @export

Zicksort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Zicksort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Zicksort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Zicksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Copy'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Nocosort}} for Sedgewick's 'no copy merge sort' that avoid one copy pass per merge using the ping-pong optmization (but three loop checks)
#' \code{\link{Simplsort}} simple mergesort with 1 move pass and 2 loop checks (N elements buffer)
#' \code{\link{Bimesort}} for Sedgewicks 'bitonic' mergesort with single move pass, sentinel and single loop check (N elements buffer) but stable
#' \code{\link{Knuthsort}} for a ping-pong algorithm with Knuth's merge (one loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Copysort(y))}    # extra copy pass, 1 loop check
#' ,{y <- x[]; sperf(Nocosort(y))}    # p-merging, 3 loop checks
#' ,{y <- x[]; sperf(Simplsort(y))}   # p-merging, 2 loop checks, inline comparison
#' ,{y <- x[]; sperf(Bimesort(y))}    # for Sedgewick's 'bitonic Mergesort' with one loop check
#' ,{y <- x[]; sperf(Knuthsort(y))}   # for a version with Knuth's merge (one loop check)
#' )
#' @export

Copysort <- function(x
                     , situation=c(
                       "insitu"   # allocate n, copy to aux, sort
                       , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                       ,  "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_CopysortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Copysort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_CopysortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Copysort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Copysort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Noco'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Copysort}} for a (faster) algorithm that does not user the ping-pong optmization
#' \code{\link{Bimesort}} for Sedgewick's 'bitonic Mergesort' with one loop check
#' \code{\link{Simplsort}} for a simple version with two loop checks
#' \code{\link{Knuthsort}} for a version with Knuth's merge (one loop check)
#' \code{\link{Katasort}} for a version with Katajainen's tuned merge (half loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Nocosort(y))}    # p-merging, 3 loop checks
#' ,{y <- x[]; sperf(Copysort(y))}    # extra copy pass, 1 loop check
#' ,{y <- x[]; sperf(Simplsort(y))}   # p-merging, 2 loop checks, inline comparison
#' ,{y <- x[]; sperf(Bimesort(y))}    # o-merging, 1 loop check
#' ,{y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' ,{y <- x[]; sperf(Katasort(y))}    # p-merging, tuned for 1/2 loop check
#' )
#' @export

Nocosort <- function(x
                     , situation=c(
                       "insitu"   # allocate n, copy to aux, sort
                       , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                       ,  "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_NocosortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Nocosort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_NocosortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Nocosort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Nocosort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Simpl'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Nocosort}} for Sedgewick's 'no copy merge sort' that avoid one copy pass per merge using the ping-pong optmization (but three loop checks)
#' \code{\link{Copysort}} for a (faster) algorithm that does not user the ping-pong optmization
#' \code{\link{Bimesort}} for Sedgewick's 'bitonic Mergesort' with one loop check
#' \code{\link{Knuthsort}} for a version with Knuth's merge (one loop check)
#' \code{\link{Katasort}} for a version with Katajainen's tuned merge (half loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Nocosort(y))}    # p-merging, 3 loop checks
#' ,{y <- x[]; sperf(Copysort(y))}    # extra copy pass, 1 loop check
#' ,{y <- x[]; sperf(Simplsort(y))}   # p-merging, 2 loop checks, inline comparison
#' ,{y <- x[]; sperf(Bimesort(y))}    # o-merging, 1 loop check
#' ,{y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' ,{y <- x[]; sperf(Katasort(y))}    # p-merging, tuned for 1/2 loop check
#' )
#' @export

Simplsort <- function(x
                      , situation=c(
                        "insitu"   # allocate n, copy to aux, sort
                        , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                        , "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_SimplsortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Simplsort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_SimplsortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Simplsort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Simplsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Bime'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Nocosort}} for Sedgewick's nocopy mergesort with three loop checks
#' \code{\link{Knuthsort}} for a ping-pong algorithm with Knuth's merge (also one loop check)
#' \code{\link{BimesortB}} for a version tuned to avoid branch-misprediction
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(BimesortB(y))}   # o-merging, 1 loop check
#' ,{y <- x[]; sperf(Bimesort(y))}    # o-merging, 1 loop check
#' ,{y <- x[]; sperf(Nocosort(y))}    # p-merging, 3 loop checks
#' ,{y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' )
#' @export

Bimesort <- function(x
                     , situation=c(
                       "insitu"   # allocate n, copy to aux, sort
                       , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                       ,  "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_BimesortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Bimesort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_BimesortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Bimesort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Bimesort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'BimeB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Bimesort}} for a version without tuning
#' \code{\link{KatasortB}} for a tuned ping-pong algorithm with Katajainen's merge (also one loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(BimesortB(y))}   # o-merging, 1 loop check, block tuned
#' ,{y <- x[]; sperf(Bimesort(y))}    # o-merging, 1 loop check (not tuned)
#' ,{y <- x[]; sperf(KatasortB(y))}   # p-merging, 1 loop check, block tuned
#' )
#' @export

BimesortB <- function(x
                      , situation=c(
                        "insitu"   # allocate n, copy to aux, sort
                        , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                        ,  "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_BimesortBP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_BimesortB_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_BimesortBP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_BimesortB_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "BimesortB")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Knuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Simplsort}} for a simple ping-pongalgorithm with two loop check
#' \code{\link{Nocosort}} for Sedgewick's ping-pong sort with three loop checks
#' \code{\link{Bimesort}} for Sedgewick's 'bitonic Mergesort' with one loop check
#' \code{\link{Katasort}} for a version with Katajainen's tuned merge (on average half check per loop traversal)
#' \code{\link{Ninisort}} for a version that requries only 50\% buffer (one loop check)
#' \code{\link{Frogsort1}} for a s-merging algorithm that reduces distance and requries only 50\% buffer (one loop check)
#' \code{\link{Knuth3sort}} and \code{\link{Knuth4sort}} for k-ary versions
#' \code{\link{KnuthsortA}} for an adaptive version
#' \code{\link{GKnuthsort}} for a gapped version
#' \code{\link{MKnuthsort}} for a version sorting matrix columns by keys in first row
#' \code{\link{UKnuthsort}} for a version sorting strings indirectly
#' \code{\link{VKnuthsort}} for a version sorting strings directly
#' \code{\link{PKnuthsort}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' ,{y <- x[]; sperf(KnuthsortA(y))}  # adaptive version
#' ,{y <- x[]; sperf(Nocosort(y))}    # p-merging, 3 loop checks
#' ,{y <- x[]; sperf(Bimesort(y))}    # o-merging, 1 loop check
#' ,{y <- x[]; sperf(Katasort(y))}    # p-merging, tuned for 1/2 loop check
#' ,{y <- x[]; sperf(Ninisort(y))}    # p-merging with only 50\% buffer, tuned for 1/2 loop check
#' ,{y <- x[]; sperf(Frogsort1(y))}   # a s-merging algorithm, only 50\% buffer (one loop check)
#' ,{y <- x[]; sperf(Knuth3sort(y))}  # p-merging, 1 loop check, log3 S&M
#' ,{y <- x[]; sperf(Knuth4sort(y))}  # p-merging, 1 loop check, log4 S&M
#' )
#' @export

Knuthsort <- function(x
                      , situation=c(
                        "insitu"   # allocate n, copy to aux, sort
                        , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                        ,  "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_KnuthsortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Knuthsort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_KnuthsortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Knuthsort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Knuthsort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'KnuthA'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the version without this tuning (one loop check)
#' \code{\link{KatasortA}} for Katajainen's the version with this tuning (half loop check)
#' \code{\link{Frogsort1A}} for a s-merging algorithm with non-overlap check that requries only 50\% buffer (one loop check)
#' \code{\link{VKnuthsortA}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' ,{y <- x[]; sperf(KnuthsortA(y))}  # p-merging, 1 loop check, tuned A
#' ,{y <- x[]; sperf(KatasortA(y))}   # p-merging, 1/2 loop check, tuned A
#' ,{y <- x[]; sperf(Frogsort1A(y))}  # a s-merging algorithm, 1 loop check, tuned A
#' )
#' @export

KnuthsortA <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                         ,  "index"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_KnuthsortAP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KnuthsortA_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_KnuthsortAP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KnuthsortA_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "KnuthsortA")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kata'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for a version with Knuth's merge (one check per loop traversal)
#' \code{\link{KatasortA}} for a version tuned for less initial copying and non-overlap check
#' \code{\link{KatasortB}} for a version tuned to avoid branch-misprediction
#' \code{\link{Kata3sort}} and \code{\link{Kata4sort}} for k-ary versions
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}   # p-merging, 1 loop check
#' ,{y <- x[]; sperf(Katasort(y))}    # p-merging, tuned for 1/2 loop check
#' ,{y <- x[]; sperf(KatasortA(y))}   # p-merging, tuned for 1/2 loop check, tuned A
#' ,{y <- x[]; sperf(KatasortB(y))}   # p-merging, tuned for 1/2 loop check, tuned B
#' ,{y <- x[]; sperf(Kata3sort(y))}   # p-merging, tuned for 1/2 loop check, log3 S&M
#' ,{y <- x[]; sperf(Kata4sort(y))}   # p-merging, tuned for 1/2 loop check, log4 S&M
#' )
#' @export

Katasort <- function(x
                     , situation=c(
                       "insitu"   # allocate n, copy to aux, sort
                       , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                       ,  "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_KatasortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Katasort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_KatasortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Katasort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Katasort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'KataA'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Katasort}} for the version without this tuning (half loop check)
#' \code{\link{KnuthsortA}} for an algorithm with Knuth's merge with this tuning (one loop check)
#' \code{\link{Frogsort1A}} for a s-merging algorithm with non-overlap check that requries only 50\% buffer (one loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Katasort(y))}    # p-merging, 1/2 loop check
#' ,{y <- x[]; sperf(KatasortA(y))}   # p-merging, 1/2 loop check, tuned A
#' ,{y <- x[]; sperf(KnuthsortA(y))}  # p-merging, 1 loop check, tuned A
#' ,{y <- x[]; sperf(Frogsort1A(y))}  # s-merging algorithm, 50\% buffer, 1 loop check, tuned A
#' )
#' @export

KatasortA <- function(x
                      , situation=c(
                        "insitu"   # allocate n, copy to aux, sort
                        , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                        ,  "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_KatasortAP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KatasortA_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_KatasortAP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KatasortA_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "KatasortA")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'KataB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Katasort}} for the version without this tuning (half loop check)
#' \code{\link{BimesortB}} for Sedgewick's 'bitonic mergesort' with this tuning (one loop check)
#' \code{\link{Frogsort1B}} for a tuned s-merging algorithm that requires only 50\% buffer (one loop check)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(KatasortB(y))}   # p-merging, 1/2 loop check, tuned B
#' ,{y <- x[]; sperf(Katasort(y))}    # p-merging, 1/2 loop check
#' ,{y <- x[]; sperf(BimesortB(y))}   # o-merging with sentinel stop, 1 loop check, tuned B
#' ,{y <- x[]; sperf(Frogsort1B(y))}  # s-merging algorithm, 50\% buffer, 1 loop check, tuned B
#' )
#' @export

KatasortB <- function(x
                      , situation=c(
                        "insitu"   # allocate n, copy to aux, sort
                        , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                        ,  "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_KatasortBP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KatasortB_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_KatasortBP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_KatasortB_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "KatasortB")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Knuth3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the binary version
#' \code{\link{Knuth4sort}} for a log4 version
#' \code{\link{Kata3sort}} for a version with Katajainen's tuned merge (on average half check per loop traversal)
#' \code{\link{Croco3sort}} for a r-merging algorithm that reduces distance
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(Knuth3sort(y))}
#' ,{y <- x[]; sperf(Knuth4sort(y))}
#' ,{y <- x[]; sperf(Kata3sort(y))}
#' ,{y <- x[]; sperf(Croco3sort(y))}
#' )
#' @export

Knuth3sort <- function(x, situation=c("insitu","exsitu"), method=c("pointer")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Knuth3sort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Knuth3sort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Knuth3sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Knuth4'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the binary version
#' \code{\link{Knuth3sort}} for a log3 version
#' \code{\link{Kata4sort}} for a version with Katajainen's tuned merge (on average half check per loop traversal)
#' \code{\link{Croco4sort}} for a r-merging algorithm that reduces distance
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(Knuth3sort(y))}
#' ,{y <- x[]; sperf(Knuth4sort(y))}
#' ,{y <- x[]; sperf(Kata4sort(y))}
#' ,{y <- x[]; sperf(Croco4sort(y))}
#' )
#' @export

Knuth4sort <- function(x, situation=c("insitu","exsitu"), method=c("pointer")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Knuth4sort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Knuth4sort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Knuth4sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kata3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Katasort}} for the binary version
#' \code{\link{Kata4sort}} for a log4 version
#' \code{\link{Knuth3sort}} for a version with Katajainen's tuned merge (on average half check per loop traversal)
#' \code{\link{Kroco3sort}} for a r-merging algorithm that reduces distance
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Kata3sort(y))}
#' ,{y <- x[]; sperf(Katasort(y))}
#' ,{y <- x[]; sperf(Kata4sort(y))}
#' ,{y <- x[]; sperf(Knuth3sort(y))}
#' ,{y <- x[]; sperf(Kroco3sort(y))}
#' )
#' @export

Kata3sort <- function(x, situation=c("insitu","exsitu"), method=c("pointer")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Kata3sort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Kata3sort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Kata3sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kata4'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Katasort}} for the binary version
#' \code{\link{Kata3sort}} for a log4 version
#' \code{\link{Knuth4sort}} for a version with Katajainen's tuned merge (on average half check per loop traversal)
#' \code{\link{Kroco4sort}} for a r-merging algorithm that reduces distance
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Katasort(y))}
#' ,{y <- x[]; sperf(Kata3sort(y))}
#' ,{y <- x[]; sperf(Kata4sort(y))}
#' ,{y <- x[]; sperf(Knuth4sort(y))}
#' ,{y <- x[]; sperf(Kroco4sort(y))}
#' )
#' @export

Kata4sort <- function(x, situation=c("insitu","exsitu"), method=c("pointer")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Kata4sort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Kata4sort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Kata4sort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'GKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the classic ping-pong algorithm with distant buffer
#' \code{\link{TKnuthsort}} for a t-merging algorithm using transport moves on both input streams
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(GKnuthsort(y))}
#' ,{y <- x[]; sperf(TKnuthsort(y))}
#' )
#' @export

GKnuthsort <- function(x
                       , situation=c(
                         "insitu"  # allocate n, pre to aux + dat, sort, post from dat+aux
                         ,"exsitu"  # allocate 2n, pre to aux, sort, post from aux
                       )
                       , method=c(
                         "index"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_GKnuthsort_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_GKnuthsort_exsitu
                 , x = x
    )
  ret <- retperf(ret, "GKnuthsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'TKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the classic ping-pong algorithm with distant buffer
#' \code{\link{GKnuthsort}} for the gapped ping-pong algorithm
#' \code{\link{Crocosort}} for a r-merging algorithm using relocation moves of only one input stream (and semi-inplace merging)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(GKnuthsort(y))}
#' ,{y <- x[]; sperf(TKnuthsort(y))}
#' ,{y <- x[]; sperf(Crocosort(y))}
#' )
#' @export

TKnuthsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_TKnuthsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_TKnuthsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "TKnuthsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Croco'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the classic ping-pong algorithm with distant buffer
#' \code{\link{TKnuthsort}} for the algorithm with relocation-moves of both input-streams (and Knuth's merge)
#' \code{\link{Crocosort}} for a r-merging algorithm using relocation moves of only one input stream (and semi-inplace merging)
#' \code{\link{Croco3sort}} and \code{\link{Croco4sort}} for k-ary versions with partial-inplace merging
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(TKnuthsort(y))}
#' ,{y <- x[]; sperf(Crocosort(y))}
#' ,{y <- x[]; sperf(Croco3sort(y))}
#' ,{y <- x[]; sperf(Croco4sort(y))}
#' )
#' @export

Crocosort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_Crocosort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Crocosort_insitu
                 , x = x
    )
  ret <- retperf(ret, "Crocosort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Croco3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Crocosort}} for the binary version
#' \code{\link{Croco4sort}} for the 4-ary version
#' \code{\link{Kroco3sort}} for the 3-ary version with Katajainen's loop check
#' \code{\link{Knuth3sort}} for the 3-ary ping-pong version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Crocosort(y))}
#' ,{y <- x[]; sperf(Croco3sort(y))}
#' ,{y <- x[]; sperf(Croco4sort(y))}
#' ,{y <- x[]; sperf(Kroco3sort(y))}
#' ,{y <- x[]; sperf(Knuth3sort(y))}
#' )
#' @export

Croco3sort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Croco3sort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Croco3sort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Croco3sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Croco4'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Croco3sort}} for the 3-ary version
#' \code{\link{Kroco4sort}} for the 4-ary version with Katajainen's loop check
#' \code{\link{Knuth4sort}} for the 4-ary ping-pong version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Croco3sort(y))}
#' ,{y <- x[]; sperf(Croco4sort(y))}
#' ,{y <- x[]; sperf(Kroco4sort(y))}
#' ,{y <- x[]; sperf(Knuth4sort(y))}
#' )
#' @export

Croco4sort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Croco4sort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Croco4sort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Croco4sort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kroco3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Kroco4sort}} for the 4-ary version
#' \code{\link{Croco3sort}} for the 3-ary version with Knuths's loop check
#' \code{\link{Kata3sort}} for the 3-ary ping-pong version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Kroco4sort(y))}
#' ,{y <- x[]; sperf(Kroco3sort(y))}
#' ,{y <- x[]; sperf(Croco3sort(y))}
#' ,{y <- x[]; sperf(Kata3sort(y))}
#' )
#' @export

Kroco3sort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Kroco3sort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Kroco3sort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Kroco3sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kroco4'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Kroco3sort}} for the 3-ary version
#' \code{\link{Croco4sort}} for the 4-ary version with Knuths's loop check
#' \code{\link{Kata4sort}} for the 4-ary ping-pong version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Kroco3sort(y))}
#' ,{y <- x[]; sperf(Kroco4sort(y))}
#' ,{y <- x[]; sperf(Croco4sort(y))}
#' ,{y <- x[]; sperf(Kata4sort(y))}
#' )
#' @export

Kroco4sort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Kroco4sort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Kroco4sort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Kroco4sort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Inin'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the classic ping-pong algorithm with 100\% distant buffer
#' \code{\link{Ninisort}} for a version with Knuth's merge and using a register (one loop check)
#' \code{\link{Frogsort1}} for a s-merging algorithm that requires only 50\% buffer and parallelizes
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(Ininsort(y))}
#' ,{y <- x[]; sperf(Ninisort(y))}
#' ,{y <- x[]; sperf(Frogsort1(y))}
#' )
#' @export

Ininsort <- function(x
                     , situation=c(
                       "insitu"  # allocate 0.5n, sort
                       ,"exsitu"  # allocate 1.5n, copy to aux, sort, copy from aux
                     )
                     , method=c(
                       "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Ininsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Ininsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Ininsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Nini'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the classic ping-pong algorithm with 100\% distant buffer
#' \code{\link{Ininsort}} for the original version by Lang with (two loop checks)
#' \code{\link{Frogsort1}} for a s-merging algorithm that requires only 50\% buffer and parallelizes
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(Ininsort(y))}
#' ,{y <- x[]; sperf(Ninisort(y))}
#' ,{y <- x[]; sperf(Frogsort1(y))}
#' )
#' @export

Ninisort <- function(x
                     , situation=c(
                       "insitu"  # allocate 0.5n, sort
                       ,"exsitu"  # allocate 1.5n, copy to aux, sort, copy from aux
                     )
                     , method=c(
                       "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Ninisort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Ninisort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Ninisort")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog0'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for a more general version that handles odd and even numbers of elements without a dummy element
#' \code{\link{Geckosort0}} for a version with symmetric adaptivity
#' \code{\link{PFrogsort0}} for a more general parallel version that special cases incomplete triplets
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort0(y))}
#' ,{y <- x[]; sperf(Geckosort0(y))}  # a version with symmetric adaptivity
#' ,{y <- x[]; sperf(Frogsort1(y))}   # a version that handles odd and even numbers of elements
#' )
#' @export

Frogsort0 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index", "pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort0P_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort0_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort0P_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort0_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Frogsort0")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Gecko0'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for a more general version that handles odd and even numbers of elements
#' \code{\link{Geckosort0}} for a version with symmetric adaptivity
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Geckosort0(y))}
#' ,{y <- x[]; sperf(Frogsort0(y))}    # a version with asymmetric adaptivity
#' ,{y <- x[]; sperf(Geckosort1(y))}   # a version that handles odd and even numbers of elements
#' )
#' @export

Geckosort0 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Geckosort0_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Geckosort0_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Geckosort0")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Gecko1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Geckosort0}} for the basic version operating on triplets
#' \code{\link{Frogsort1}} for the symmetrically adaptive version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Geckosort1(y))}
#' ,{y <- x[]; sperf(Geckosort0(y))}   # the root version using triplets
#' ,{y <- x[]; sperf(Frogsort1(y))}    # a version with asymmetric adaptivity
#' )
#' @export

Geckosort1 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer","index")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Geckosort1P_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Geckosort1_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Geckosort1P_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Geckosort1_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Geckosort1")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for p-merging with 100\% buffer
#' \code{\link{Omitsort}} for a p-merging with 100\% buffer tuned for adaptivity
#' \code{\link{Ninisort}} for a less general algorithm with 50\% buffer
#' \code{\link{Geckosort1}} for the version that is 25\% bi-adaptive
#' \code{\link{Frogsort0}} for the root version using triplets
#' \code{\link{Frogsort1A}} for a version tuned with non-overlap testing for adaptivity on presorted data
#' \code{\link{Frogsort1B}} for a version tuned to avoid branch-(mis)prediction
#' \code{\link{Frogsort2}} for an unbalanced version using F&M requiring even less buffer and often even faster
#' \code{\link{Frogsort3}} for an unbalanced version using A&M requiring even less buffer
#' \code{\link{Frogsort6}} for a parametrized generalization of Frogsort1,2,3
#' \code{\link{Frogsort4}} for a bottom-up version starting with sqrt(N) buffer, finalizing with N/2 buffer
#' \code{\link{Frogsort5}} for a bottom-up version with optimized initial buffer size
#' \code{\link{Squidsort1}} for a version with adaptivity for presorted and reverse-sorted
#' \code{\link{VFrogsort1}} for a version that sorts strings (directly)
#' \code{\link{PFrogsort1}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}   # a p-merging with 100\% buffer
#' ,{y <- x[]; sperf(Omitsort(y))}    # a p-merging with 100\% buffer tuned for adaptivity
#' ,{y <- x[]; sperf(Ninisort(y))}    # a less general algorithm with 50\% buffer
#' ,{y <- x[]; sperf(Frogsort0(y))}   # the root version using triplets
#' ,{y <- x[]; sperf(Geckosort1(y))}  # the version that is 25\% bi-adaptive
#' ,{y <- x[]; sperf(Frogsort1(y))}   # the basic balanced version
#' ,{y <- x[]; sperf(Frogsort1A(y))}  # the balanced version tuned for adaptivity
#' ,{y <- x[]; sperf(Frogsort1B(y))}  # the balanced version tuned to avoid branch-(mis)prediction
#' ,{y <- x[]; sperf(Frogsort2(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort3(y))}   # the unbalanced version using A&M
#' ,{y <- x[]; sperf(Frogsort6(y))}   # a parametrized generalization of Frogsort1,2,3
#' ,{y <- x[]; sperf(Frogsort4(y))}   # a bottom-up version starting with sqrt buffer then 50% buffer
#' ,{y <- x[]; sperf(Frogsort5(y))}   # a bottom-up version with optimized buffer size
#' ,{y <- x[]; sperf(PFrogsort1(y))}  # a parallel version
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}   # a p-merging with 100\% buffer
#' ,{y <- x[]; sperf(Omitsort(y))}    # a p-merging with 100\% buffer tuned for adaptivity
#' ,{y <- x[]; sperf(Ninisort(y))}    # a less general algorithm with 50\% buffer
#' ,{y <- x[]; sperf(Geckosort1(y))}  # the version that is 25\% bi-adaptive
#' ,{y <- x[]; sperf(Frogsort1(y))}   # the basic balanced version
#' ,{y <- x[]; sperf(Frogsort1A(y))}  # the unbalanced version tuned for adaptivity
#' ,{y <- x[]; sperf(Squidsort1(y))}  # a version with adaptivity for presorted and reverse-sorted
#' )
#' @export

Frogsort1 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c(
                        "pointer"
                        ,  "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1P_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1P_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Frogsort1")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog1A'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for p-merging with 100\% buffer
#' \code{\link{KnuthsortA}} for p-merging with 100\% buffer
#' \code{\link{Omitsort}} for a p-merging with 100\% buffer tuned for adaptivity
#' \code{\link{Frogsort1A}} for a version tuned with non-overlap testing for adaptivity on presorted data
#' \code{\link{Squidsort1}} for a version with adaptivity for presorted and reverse-sorted
#' \code{\link{VFrogsort1A}} for a version sorting strings (directly)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}   # the untuned balanced version
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' )
#' @export

Frogsort1A <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c(
                         "pointer"
                         ,  "index"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1AP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1A_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1AP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1A_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Frogsort1A")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog1B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}}  for the basic balanced version
#' \code{\link{KatasortB}}  for a tuned p-merging with 100\% buffer
#' \code{\link{BimesortB}}  for a tuned p-merging with 100\% buffer
#' \code{\link{Simplsort}}  for a p-merging with 100\% buffer that also improves on B
#' \code{\link{Frogsort1A}} for a version tuned for adaptivity
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1B(y))}
#' ,{y <- x[]; sperf(Frogsort1(y))}   # the basic balanced version
#' ,{y <- x[]; sperf(KatasortB(y))}   # a tuned p-merging with 100\% buffer
#' ,{y <- x[]; sperf(BimesortB(y))}   # a tuned o-merging with 100\% buffer
#' ,{y <- x[]; sperf(Simplsort(y))}   # another p-merging with 100\% buffer
#' ,{y <- x[]; sperf(Frogsort1A(y))}  # a version tuned for adaptivity
#' )
#' @export

Frogsort1B <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c(
                         "pointer"
                         ,  "index"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1BP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1B_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort1BP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Frogsort1B_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Frogsort1B")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}}  for the basic balanced version
#' \code{\link{Frogsort2A}} for a version tuned for presorting
#' \code{\link{Frogsort2B}} for a version tuned against branch-misprediction
#' \code{\link{Frogsort3}}  for an unbalanced version using A&M requiring even less buffer
#' \code{\link{Frogsort6}}  for a parametrized generalization of Frogsort1,2,3
#' \code{\link{MFrogsort2}}  for a version sorting matrix columns by keys in first row (moves keys and payload)
#' \code{\link{NFrogsort2}}  for a version sorting matrix columns by keys in first row (moves keys and pointers)
#' \code{\link{PFrogsort2}}  for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort1(y))}   # the basic balanced version
#' ,{y <- x[]; sperf(Frogsort1B(y))}  # a balanced version tuned to avoid branch-(mis)prediction
#' ,{y <- x[]; sperf(Frogsort3(y))}   # the unbalanced version using A&M
#' ,{y <- x[]; sperf(Frogsort6(y))}   # a parametrized generalization of Frogsort1,2,3
#' )
#' @export

Frogsort2 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("pointer","index")
                      , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2P_insitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2_insitu
                   , x = x
                   , p = as.double(p)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2P_exsitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2_exsitu
                   , x = x
                   , p = as.double(p)
      )
  }
  ret <- retperf(ret, "Frogsort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog2A'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the untuned version
#' \code{\link{Frogsort1A}}  for the balanced version
#' \code{\link{Frogsort2B}} for a version tuned to avoid branch-(mis)prediction
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort2A(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort1A(y))}   # the basic balanced version
#' )
#' @export

Frogsort2A <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer","index")
                       , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2AP_insitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2A_insitu
                   , x = x
                   , p = as.double(p)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2AP_exsitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2A_exsitu
                   , x = x
                   , p = as.double(p)
      )
  }
  ret <- retperf(ret, "Frogsort2A")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the untuned version
#' \code{\link{Frogsort1B}} for the balanced version
#' \code{\link{Frogsort3B}} for an unbalanced version using A&M requiring even less buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}   # the untuned version
#' ,{y <- x[]; sperf(Frogsort1B(y))}  # a balanced version tuned to avoid branch-(mis)prediction
#' ,{y <- x[]; sperf(Frogsort2B(y))}  # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort3B(y))}  # the unbalanced version using A&M
#' )
#' @export

Frogsort2B <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer","index")
                       , p = 1/3
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2BP_insitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2B_insitu
                   , x = x
                   , p = as.double(p)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort2BP_exsitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort2B_exsitu
                   , x = x
                   , p = as.double(p)
      )
  }
  ret <- retperf(ret, "Frogsort2B")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for the balanced version
#' \code{\link{Frogsort2}} for an unbalanced version using F&M requiring even less buffer and often even faster
#' \code{\link{Frogsort3B}} for version tuned against branch misprediction
#' \code{\link{Frogsort6}} for a parametrized generalization of Frogsort1,2,3
#' \code{\link{PFrogsort3}} for a parallel version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}   # the balanced version
#' ,{y <- x[]; sperf(Frogsort2(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort3(y))}   # the unbalanced version using A&M
#' ,{y <- x[]; sperf(Frogsort6(y))}   # a parametrized generalization of Frogsort1,2,3
#' )
#' @export

Frogsort3 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("pointer","index")
                      , p = 1/8
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort3P_insitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort3_insitu
                   , x = x
                   , p = as.double(p)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort3P_exsitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort3_exsitu
                   , x = x
                   , p = as.double(p)
      )
  }
  ret <- retperf(ret, "Frogsort3")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog3B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort3}} for the untuned version
#' \code{\link{Frogsort1B}} for the underlying balanced version using triplets
#' \code{\link{Frogsort2B}} for an unbalanced version using F&M requiring even less buffer and often even faster
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort3(y))}    # the untuned version
#' ,{y <- x[]; sperf(Frogsort1B(y))}   # the balanced version
#' ,{y <- x[]; sperf(Frogsort2B(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort3B(y))}   # the unbalanced version using A&M
#' )
#' @export

Frogsort3B <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer","index")
                       , p = 1/8
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort3BP_insitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort3B_insitu
                   , x = x
                   , p = as.double(p)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort3BP_exsitu
                   , x = x
                   , p = as.double(p)
      )
    else
      ret <- .Call(C_r_Frogsort3B_exsitu
                   , x = x
                   , p = as.double(p)
      )
  }
  ret <- retperf(ret, "Frogsort3B")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog6'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p3 fraction of initial (alternated) buffer relative to data size
#' @param p2 fraction of initial (forked) buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort3}} for the underlying unbalanced version using A&M requiring even less buffer
#' \code{\link{Frogsort2}} for the underlying unbalanced version using F&M requiring even less buffer and often even faster
#' \code{\link{Frogsort1}} for the balanced version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort6(y))}   # a parametrized generalization of Frogsort1,2,3
#' ,{y <- x[]; sperf(Frogsort3(y))}   # the unbalanced version using A&M
#' ,{y <- x[]; sperf(Frogsort2(y))}   # the unbalanced version using F&M
#' ,{y <- x[]; sperf(Frogsort1(y))}   # the balanced version
#' )
#' @export

Frogsort6 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("pointer","index")
                      , p3 = 1/16  # with p3 < p2 better than Frogsort2 (which has p=1/8)
                      , p2 = 1/7   # with optimal p2 better than Frogsort3 (which has p2=1/2)
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p3 > 0 && p3 < 1)
  if (p3 > p2)
    stop("p3 must be LE p2")
  if (p3 > 0.5 || p2 > 0.5)
    warning("p > 0.5 doesn't help")
  n <- length(x)
  if (n < 5)
    p3 <- 0.5
  else if (p3 < 2/n)
    p3 <- 2/n
  if (p2 < p3)
    p2 <- p3
  else
    stopifnot(p2 > 0 && p2 < 1)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort6P_insitu
                   , x = x
                   , p3 = as.double(p3)
                   , p2 = as.double(p2)
      )
    else
      ret <- .Call(C_r_Frogsort6_insitu
                   , x = x
                   , p3 = as.double(p3)
                   , p2 = as.double(p2)
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_Frogsort6P_exsitu
                   , x = x
                   , p3 = as.double(p3)
                   , p2 = as.double(p2)
      )
    else
      ret <- .Call(C_r_Frogsort6_exsitu
                   , x = x
                   , p3 = as.double(p3)
                   , p2 = as.double(p2)
      )
  }
  ret <- retperf(ret, "Frogsort6")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog4'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of initial buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param verbose TRUE reports buffer fraction p, absolute buffer size b and data size n
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort5}} for a bottom-up version with optimized (bigger) buffer size delaying N/2 buffer need
#' \code{\link{Frogsort1}} for the top-down version with 50\% buffer
#' \code{\link{Jumpsort}}  for an algorithm with sqrt(N) buffer all the time, also distance-reducing
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(Frogsort4(y))}
#' ,{y <- x[]; sperf(Frogsort5(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' )
#' @export

Frogsort4 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index")
                      , p = 1/sqrt(length(x))
                      , verbose=FALSE)
{
  b <- max(1, round(p*length(x)))
  if (verbose)
    cat("p=", p, "  b=", b, "  n=", length(x), "\n", sep = "")
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(b >= 1 && b <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Frogsort4_insitu
                 , x = x
                 , b = as.integer(b)
    )
  }else{
    ret <- .Call(C_r_Frogsort4_exsitu
                 , x = x
                 , b = as.integer(b)
    )
  }
  ret <- retperf(ret, "Frogsort4")
  ret
}


#' @describeIn Frogsort5 calculate the footprint-optimal number of top-levels in the binary merge tree that use N/2 buffer
#' @export
Frogtop5 <- function(n, situation=c("insitu","exsitu")) {
  situation <- match.arg(situation)
  l <- log2(n)
  if (situation == "insitu")
    approx(c(0L,1L, 2L, 5L, 10L, 19L, 36L, 69L), 0:7, xout = l)$y  # findInterval(l,c(1L, 2L, 5L, 10L, 19L, 36L, 69L))
  else
    approx(c(0L,1L, 4L, 9L, 18L, 35L, 68L), 0:6, xout = l)$y  # findInterval(l,c(1L, 4L, 9L, 18L, 35L, 68L))
}

#' @describeIn Frogsort5 calculate the footprint-optimal number of bottom-levels in the binary merge tree that use the initial buffer
#' @export
Frogbot5 <- function(n, situation=c("insitu","exsitu")) {
  log2(n) - Frogtop5(n, situation = situation)
}


#' @describeIn Frogsort5 calculate the footprint-optimal buffer-fraction
#' @export
Frogbuf5 <- function(n, situation=c("insitu","exsitu")) {
  2^(Frogbot5(n, situation = situation) - 1L)/n  # Frogbot-1 ~ b/2
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Frog5'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param n size of the vector to be sorted
#' @param p fraction of initial buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param verbose TRUE reports buffer fraction p, absolute buffer size b and data size n
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for the top-down version with 50\% buffer
#' \code{\link{Frogsort4}} for the bottom-up version with sqrt(N) initial buffer size
#' \code{\link{Jumpsort}}  for an algorithm with sqrt(N) buffer all the time, also distance-reducing
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(Frogsort4(y))}
#' ,{y <- x[]; sperf(Frogsort5(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' )
#' @export

Frogsort5 <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index")
                      , p = Frogbuf5(length(x), situation = situation)
                      , verbose=FALSE
)
{
  b <- max(1, round(p*length(x)))
  if (verbose)
    cat("p=", p, "  b=", b, "  n=", length(x), "\n", sep = "")
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(b >= 1 && b <= length(x))
  if (situation == "insitu") {
    ret <- .Call(C_r_Frogsort4_insitu
                 , x = x
                 , b = as.integer(b)
    )
  }else{
    ret <- .Call(C_r_Frogsort4_exsitu
                 , x = x
                 , b = as.integer(b)
    )
  }
  ret <- retperf(ret, "Frogsort5")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Omit'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the version without this tuning (one loop check)
#' \code{\link{KnuthsortA}} for a version with simple tuning just skipping merging (but not copying)
#' \code{\link{Frogsort1}} for a s-merging algorithm without tuning that requires only 50\% buffer (one loop check)
#' \code{\link{Frogsort1A}} for a s-merging algorithm with non-overlap check that requires only 50\% buffer (one loop check)
#' \code{\link{Frogsort2}} for a memory-reduced s-merging algorithm that requires only 14\% buffer (one loop check)
#' \code{\link{Frogsort2A}} for a tuned memory-reduced s-merging algorithm that requires only 14\% buffer (one loop check)
#' \code{\link{Timsort}} for an algorithm that is tuned for the best case of presorted data
#' \code{\link{Insertionsort}} for an simple algorithm that performs very well on presorted data
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(KnuthsortA(y))}
#' ,{y <- x[]; sperf(Omitsort(y))}
#' ,{y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' ,{y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(Frogsort2A(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(Insertionsort(y))}
#' )
#' @export

Omitsort <- function(x
                     , situation=c(
                       "insitu"   # allocate n, copy to aux, sort
                       , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                       ,  "index"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    if (method == "pointer")
      ret <- .Call(C_r_OmitsortP_insitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Omitsort_insitu
                   , x = x
      )
  }else{
    if (method == "pointer")
      ret <- .Call(C_r_OmitsortP_exsitu
                   , x = x
      )
    else
      ret <- .Call(C_r_Omitsort_exsitu
                   , x = x
      )
  }
  ret <- retperf(ret, "Omitsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Octo'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{GKnuthsort}} for the non-tuned version
#' \code{\link{KnuthsortA}} for the tuned ping-pong algorithm
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$descnorm(n)
#' rbind(
#'  {y <- x[]; sperf(KnuthsortA(y))}
#' ,{y <- x[]; sperf(Omitsort(y))}
#' ,{y <- x[]; sperf(Octosort(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' ,{y <- x[]; sperf(Frogsort2A(y))}
#' ,{y <- x[]; sperf(Squidsort1(y))}
#' ,{y <- x[]; sperf(Squidsort2(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(gfxTimsort(y))}
#' ,{y <- x[]; sperf(Peeksort(y))}
#' ,{y <- x[]; sperf(Powersort(y))}
#' ,{y <- x[]; sperf(Powersort4(y))}
#' )
#' @export

Octosort <- function(x
                     , situation=c(
                       "insitu"  # allocate n, pre to aux + dat, sort, post from dat+aux
                       ,"exsitu"  # allocate 2n, pre to aux, sort, post from aux
                     )
                     , method=c(
                       "pointer"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_OctosortP_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_OctosortP_exsitu
                 , x = x
    )
  ret <- retperf(ret, "Octosort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Squid1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for the undelying untuned version
#' \code{\link{Geckosort1}} for an untuned symmetrically version
#' \code{\link{Frogsort1A}} for the undelying tuned version
#' \code{\link{Squidsort2}} for an unbalanced memory-reduced version
#' \code{\link{Timsort}} for an algorithm tuned for the best cases of presorting
#' \code{\link{gfxTimsort}} for an algorithm tuned for the best cases of presorting
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(Geckosort1(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' ,{y <- x[]; sperf(Squidsort1(y))}
#' ,{y <- x[]; sperf(Squidsort2(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(gfxTimsort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(Geckosort1(y))}
#' ,{y <- x[]; sperf(Frogsort1A(y))}
#' ,{y <- x[]; sperf(Squidsort1(y))}
#' ,{y <- x[]; sperf(Squidsort2(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(gfxTimsort(y))}
#' )
#' @export

Squidsort1 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Squidsort1P_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Squidsort1P_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Squidsort1")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Squid2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the undelying untuned version
#' \code{\link{Frogsort2A}} for the undelying tuned version
#' \code{\link{Squidsort1}} for an balanced version using 50\% buffer
#' \code{\link{Timsort}} for an algorithm tuned for the best cases of presorting
#' \code{\link{gfxTimsort}} for an algorithm tuned for the best cases of presorting
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(Frogsort2A(y))}
#' ,{y <- x[]; sperf(Squidsort1(y))}
#' ,{y <- x[]; sperf(Squidsort2(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(gfxTimsort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(Frogsort2A(y))}
#' ,{y <- x[]; sperf(Squidsort1(y))}
#' ,{y <- x[]; sperf(Squidsort2(y))}
#' ,{y <- x[]; sperf(Timsort(y))}
#' ,{y <- x[]; sperf(gfxTimsort(y))}
#' )
#' @export

Squidsort2 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
                       , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    ret <- .Call(C_r_Squidsort2P_insitu
                 , x = x
                 , p = as.double(p)
    )
  }else{
    ret <- .Call(C_r_Squidsort2P_exsitu
                 , x = x
                 , p = as.double(p)
    )
  }
  ret <- retperf(ret, "Squidsort2")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'IMerge'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for a classic mergesort using 100\% buffer.
#' \code{\link{Grailsort}} for a faster stable inplace merge sort,
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' )
#' @export

IMergesort <- function(x
                       , situation=c(
                         "insitu"   # sort
                         , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_InplaceMergesortP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_InplaceMergesortP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "IMergesort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Grail'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{IMergesort}} for the classic stable inplace merge sort,
#' \code{\link{Grailsqrt}} for a faster version leveraging \code{sqrt} buffer,
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' )
#' @export

Grailsort <- function(x
                      , situation=c(
                        "insitu"   # sort
                        , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_GrailsortP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_GrailsortP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Grailsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Grailsqrt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Grailsort}} for the true inplace version with only constant buffer,
#' \code{\link{Sqrtsort}} for a faster algorithm also needing \code{sqrt} buffer.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' )
#' @export

Grailsqrt <- function(x
                      , situation=c(
                        "insitu"   # sort
                        , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                      )
                      , method=c(
                        "pointer"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_GrailsqrtP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_GrailsqrtP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Grailsqrt")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Sqrt'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Grailsqrt}} for version of \code{\link{Grailsort}} also needing \code{sqrt} buffer,
#' \code{\link{Walksort}} for a faster greeNsort algorithm also needing \code{sqrt} buffer.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' )
#' @export

Sqrtsort <- function(x
                     , situation=c(
                       "insitu"   # sort
                       , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                     )
                     , method=c(
                       "pointer"
                     )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_SqrtsortP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_SqrtsortP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Sqrtsort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Walk'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of buffer relative to data
#' @param b absolute buffer size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param verbose TRUE reports buffer fraction p, absolute buffer size b and data size n
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Sqrtsort}} for the fastest \code{sqrt(N)} buffer algorithm from Astrelin
#' \code{\link{Jumpsort}} for a distance-reducing version (costs extra moves)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' )
#' @export

Walksort <- function(x
                     , situation=c(
                       "insitu"   # sort
                       , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                     )
                     , method=c(
                       "index"
                     )
                     , p = sqrt(n) / n
                     , b = max(1, round(p*n))
                     , verbose= FALSE
)
{
  n <- length(x)
  if (verbose)
    cat("2*p=", (b + n)/n, "  b=", b, "  n=", length(x), "\n", sep = "")
  stopifnot(b > 0)
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- ret <- .Call(C_r_Walksort_insitu
                        , x = x
                        , b = as.integer(b)
    )
  }else{
    ret <- ret <- .Call(C_r_Walksort_exsitu
                        , x = x
                        , b = as.integer(b)
    )
  }
  ret <- retperf(ret, "Walksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Jump'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of buffer relative to data
#' @param b absolute buffer size
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param verbose TRUE reports buffer fraction p, absolute buffer size b and data size n
#' @return like \code{\link{rawperf}}
#' @seealso \code{\link{Walksort}} for a version without relocation moves
#' \code{\link{Walksort}} for the version without extra moves for distance-reduction
#' \code{\link{Frogsort4}} for a Frogsort using initially only \code{sqrt(N)} buffer and then finalizing merges with \code{N/2} buffer
#' \code{\link{Frogsort5}} for a Frogsort using initially ideal \code{\link{Frogbuf5}} buffer and then finalizing merges with \code{N/2} buffer
#' \code{\link{Frogsort6}} for an algorithm that can be parametrized for small buffer (6\%)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' ,{y <- x[]; sperf(Frogsort4(y))}
#' ,{y <- x[]; sperf(Frogsort5(y))}
#' ,{y <- x[]; sperf(Frogsort6(y))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' ,{y <- x[]; sperf(Frogsort4(y))}
#' ,{y <- x[]; sperf(Frogsort5(y))}
#' ,{y <- x[]; sperf(Frogsort6(y))}
#' )
#' x <- testdb$func$descall(n)
#' rbind(
#'  {y <- x[]; sperf(IMergesort(y))}
#' ,{y <- x[]; sperf(Grailsort(y))}
#' ,{y <- x[]; sperf(Grailsqrt(y))}
#' ,{y <- x[]; sperf(Sqrtsort(y))}
#' ,{y <- x[]; sperf(Walksort(y))}
#' ,{y <- x[]; sperf(Jumpsort(y))}
#' ,{y <- x[]; sperf(Frogsort4(y))}
#' ,{y <- x[]; sperf(Frogsort5(y))}
#' ,{y <- x[]; sperf(Frogsort6(y))}
#' )
#' @export

Jumpsort <- function(x
                     , situation=c(
                       "insitu"   # sort
                       , "exsitu"   # allocate n, copy to aux, sort, copy from aux
                     )
                     , method=c(
                       "index"
                     )
                     , p = sqrt(n) / n
                     , b = max(1, round(p*n))
                     , verbose= FALSE
)
{
  n <- length(x)
  if (verbose)
    cat("2*p=", (b + n)/n, "  b=", b, "  n=", n, "\n", sep = "")
  stopifnot(b > 0)
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_Jumpsort_insitu
                 , x = x
                 , b = as.integer(b)
    )
  }else{
    ret <- .Call(C_r_Jumpsort_exsitu
                 , x = x
                 , b = as.integer(b)
    )
  }
  ret <- retperf(ret, "Jumpsort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ACP'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietACPTsort}} for a t-partition algorithm
#' \code{\link{DietCPsort}} for a ping-pong algorithm using random pivots (sacrificing the O(N*log)(N))) worst-case for greater speed.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' )
#' @export

DietACPsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietACPsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietACPsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietACPsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'CP'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietACPsort}} for a ping-pong algorithm with \code{\link{approxMedian}} pivots
#' \code{\link{DietCPTsort}} for an algorithm with relocation moves for distance reduction.
#' \code{\link{DietcP2sort}} for a ping-pong algorithm using a more expensive call-structure as in \code{\link{DietPcsort}}.
#' \code{\link{DietPcsort}} for a ping-pong algorithm saving the extra counting pass by integrating it into the previous partitioning.
#' \code{\link{Kiwisort}} for a ping-pong algorithm saving the extra counting pass by wing-partitioning
#' \code{\link{DietCPsortB}} for a version tuned to avoid branch-(mis)prediction.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(DietCPsortB(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(DietCPsortB(y))}
#' )
#' @export

DietCPsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietCPsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietCPsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietCPsort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'CPB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietCPsort}} for the untuned version
#' \code{\link{Kiwisort}} for the untuned ping-pong algorithm saving the extra counting pass by wing-partitioning
#' \code{\link{KiwisortB}} for the similar tuned ping-pong algorithm saving the extra counting pass by wing-partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPsortB(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(KiwisortB(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPsortB(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(KiwisortB(y))}
#' )
#' @export

DietCPsortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietCPsortB_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietCPsortB_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietCPsortB")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'cP2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietCPsort}} for a ping-pong algorithm using a less expensive call-structure.
#' \code{\link{DietPcsort}} for a ping-pong algorithm saving the extra counting pass by integrating it into the previous partitioning.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' )
#' @export

DietcP2sort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietcP2sort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietcP2sort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietcP2sort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Pc'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietCPsort}} for a ping-pong algorithm with an extra counting pass and simpler call-structure.
#' \code{\link{DietcP2sort}} for a ping-pong algorithm with a similar expensive call-structure but an extra counting pass.
#' \code{\link{Kiwisort}} for a faster ping-pong algorithm saving the extra counting pass by wing-partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietcP2sort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' )
#' @export

DietPcsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietPcsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietPcsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietPcsort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'ACPT'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietACPsort}} for a ping-pong algorithm without relocation moves.
#' \code{\link{DietCPTsort}} for a t-partition algorithm using random pivots (sacrificing the O(N*log)(N))) worst-case for greater speed.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietACPsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' )
#' @export

DietACPTsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietACPTsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietACPTsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietACPTsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'CPT'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietACPTsort}} for a t-partition algorithm using \code{\link{approxMedian}} as pivot.
#' \code{\link{DietCPsort}} for a ping-pong algorithm without relocation moves.
#' \code{\link{Swansort}} for a faster b-partition algorithm saving the extra counting pass by wing-partitioning
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(Swansort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(Swansort(y, situation = 'exsitu'))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietACPTsort(y))}
#' ,{y <- x[]; sperf(DietCPTsort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(Swansort(y))}
#' ,{y <- x[]; sperf(DietCPsort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(Swansort(y, situation = 'exsitu'))}
#' )
#' @export


DietCPTsort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "exsitu")
    ret <- .Call(C_r_DietCPTsort_exsitu
                 , x = x
    )
  else
    ret <- .Call(C_r_DietCPTsort_insitu
                 , x = x
    )
  ret <- retperf(ret, "DietCPTsort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Kiwi'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietCPsort}} for a ping-pong algorithm with an extra counting pass,
#' \code{\link{DietPcsort}} for a ping-pong algorithm which also tries to avoid an extra counting pass (less successfully),
#' \code{\link{KiwisortA}} for a version tuned for presorted data
#' \code{\link{KiwisortB}} for a version tuned to avoid branch-(mis)prediction.
#' \code{\link{Swansort}} for a b-partition algorithm (slower in 'insitu' setting)
#' \code{\link{SwansortB}} for a b-partition algorithm block-tuned (slower in 'insitu' setting)
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietPcsort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(Swansort(y))}
#' ,{y <- x[]; sperf(Swansort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPsort(y))}
#' ,{y <- x[]; sperf(DietPcsort(y))}
#' ,{y <- x[]; sperf(Kiwisort(y))}
#' ,{y <- x[]; sperf(Swansort(y))}
#' ,{y <- x[]; sperf(Swansort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' @export

Kiwisort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_Kiwisort_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Kiwisort_exsitu
                 , x = x
    )
  ret <- retperf(ret, "Kiwisort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'KiwiA'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Kiwisort}} for the untuned version
#' \code{\link{KiwisortB}} for a version tuned to avoid branch-(mis)prediction.
#' \code{\link{SwansortB}} for a distance-reducing version tuned to avoid branch-(mis)prediction.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' @export

KiwisortA <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_KiwisortA_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_KiwisortA_exsitu
                 , x = x
    )
  ret <- retperf(ret, "KiwisortA")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'KiwiB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Kiwisort}} for the untuned version
#' \code{\link{KiwisortA}} for a version tuned for presorted data
#' \code{\link{SwansortB}} for a distance-reducing version tuned to avoid branch-(mis)prediction.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' x <- testdb$func$ascall(n)
#' rbind(
#'  {y <- x[]; sperf(Kiwisort(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortA(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = 'exsitu'))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = 'exsitu'))}
#' )
#' @export

KiwisortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_KiwisortB_insitu
                 , x = x
    )
  }else
    ret <- .Call(C_r_KiwisortB_exsitu
                 , x = x
    )
  ret <- retperf(ret, "KiwisortB")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Swan'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DietCPTsort}} for a t-partition algorithm with an extra counting pass,
#' \code{\link{Kiwisort}} for a ping-pong algorithm (faster in 'insitu' setting)
#' \code{\link{Storksort}} for a t-partition algorithm which requires only 50\% buffer,
#' \code{\link{SwansortB}} for a version tuned to avoid branch-(mis)prediction.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPTsort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Swansort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "'insitu'"))}
#' )
#' rbind(
#'  {y <- x[]; sperf(DietCPTsort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Swansort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(StorksortB(y, situation = "exsitu"))}
#' )
#' @export


Swansort <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_Swansort_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_Swansort_exsitu
                 , x = x
    )
  ret <- retperf(ret, "Swansort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'SwanB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{StorksortB}} for the similar tuned t-partition algorithm which requires only 50\% buffer,
#' \code{\link{KiwisortB}} for the similar tuned ping-pong algorithm (faster in 'insitu' setting)
#' \code{\link{Swansort}} for the untuned version
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DietCPTsort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Swansort(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = "insitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "'insitu'"))}
#' )
#' rbind(
#'  {y <- x[]; sperf(DietCPTsort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Kiwisort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(KiwisortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Swansort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(StorksortB(y, situation = "exsitu"))}
#' )
#' @export

SwansortB <- function(x, situation=c("insitu","exsitu"), method=c("index")) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_SwansortB_insitu
                 , x = x
    )
  }else
    ret <- .Call(C_r_SwansortB_exsitu
                 , x = x
    )
  ret <- retperf(ret, "SwansortB")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'Stork'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Swansort}} for the more general t-partition P&P algorithm which needs 100\% buffer,
#' \code{\link{StorksortB}} for the version tuned to avoid branch-(mis)prediction,
#' \code{\link{Frogsort1}} for the much more general F&M algorithm which also needs 50\% buffer.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' y <- x[]; sperf(Storksort(y))
#' z <- x[]; sperf(Storksort(z, situation = "exsitu"))
#' oldpar <- par(mfrow=c(1,2))
#' plot(y, pch='.', main="'insitu'")
#' plot(z, pch='.', main="exsitu")
#' par(oldpar)
#' rbind(
#'  {y <- x[]; sperf(Swansort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(StorksortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Frogsort1(y, situation = "exsitu"))}
#' )
#' @export

Storksort <- function(x
                      , situation=c(
                        "'insitu'" # we use quoted 'insitu' here to not be classified as real inplace in algodb
                        , "exsitu"   # allocate 150% aux, receive from x in aux, partition in aux, send sorted data to x
                      )
                      , method=c(
                        "index"
                      )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  if (situation == "insitu")
    warning("this is not a complete insitu sort, use situation='exsitu' for a complete sort")
  method <- match.arg(method)
  if (situation == "'insitu'") {
    ret <- .Call(C_r_Storksort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_Storksort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "Storksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'StorkB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Storksort}} for the untuned version,
#' \code{\link{SwansortB}} for the similar tuned more general t-partition P&P algorithm which needs 100\% buffer,
#' \code{\link{Frogsort1B}} for the similar tuned much more general S&M algorithm which also needs 50\% buffer.
#' \code{\link{Frogsort2}} for the fastest S&M algorithm which also needs even less buffer and is even faster.
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Swansort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(SwansortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Storksort(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(StorksortB(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Frogsort1(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Frogsort1B(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Frogsort2(y, situation = "exsitu"))}
#' ,{y <- x[]; sperf(Frogsort2B(y, situation = "exsitu"))}
#' )
#' @export

StorksortB <- function(x
                       , situation=c(
                         "exsitu"   # allocate 150% aux, receive from x in aux, partition in aux, send sorted data to x
                       )
                       , method=c(
                         "index"
                       )
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  if (situation == "insitu")
    warning("this is not a complete insitu sort, use situation='exsitu' for a complete sort")
  method <- match.arg(method)
  ret <- .Call(C_r_StorksortB_exsitu
               , x = x
  )
  ret <- retperf(ret, "StorksortB")
  ret
}



#' String overhead
#'
#' Function for measuring the overhead of the string pre- and postprocessing in the UVWsort functions
#'
#' While the UVWsort functions measures the cost of sorting *wihtout* the overhead of counting string length and transforming R-string to C-strings and back,
#' `UVWoverhead` returns the timing of said overhead.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{VFrogsort1}} for direct frogsort of size-varying elements (strings),
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

UVWoverhead <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  ret <- .Call(C_r_UVWoverhead, x = x)
  ret <- retperf(ret, "UVWoverhead")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'UKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable)
#' \code{\link{VKnuthsort}} for a direct Mergesort for size-varying elements (strings, stable)
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

UKnuthsort <- function(x
                       , situation=c("exsitu", "insitu")
                       , method=c("pointer")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_UKnuthsortP_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_UKnuthsortP_exsitu
                 , x = x
    )
  ret <- retperf(ret, "UKnuthsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'UZack'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' \code{\link{UKnuthsort}} for an indirect mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{UZacksortB}} for a block-tuned version,
#' \code{\link{RQuicksort2}} for an indirect Quicksort for equally-sized elements,
#' \code{\link{Zacksort}} for a Zacksort for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

UZacksort <- function(x
                      , situation=c("exsitu", "insitu")
                      , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_UZacksort_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_UZacksort_exsitu
                 , x = x
    )
  ret <- retperf(ret, "UZacksort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'UZackB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{WQuicksort2B}} for a tuned indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' \code{\link{UZacksort}} for the untuned version,
#' \code{\link{RQuicksort2B}} for a tuned indirect Quicksort for equally-sized elements,
#' \code{\link{ZacksortB}} for the version for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

UZacksortB <- function(x
                       , situation=c("exsitu", "insitu")
                       , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_UZacksortB_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_UZacksortB_exsitu
                 , x = x
    )
  ret <- retperf(ret, "UZacksortB")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'WQuick2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' \code{\link{UKnuthsort}} for an indirect mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{WQuicksort2B}} for a block-tuned version,
#' \code{\link{RQuicksort2}} for an indirect Quicksort for equally-sized elements,
#' \code{\link{Quicksort2}} for the standard Quicksort for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

WQuicksort2 <- function(x
                        , situation=c("exsitu", "insitu")
                        , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_WQuicksort2_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_WQuicksort2_exsitu
                 , x = x
    )
  ret <- retperf(ret, "WQuicksort2")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'WQuick2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksortB}} for a tuned indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' \code{\link{WQuicksort2}} for the untuned version,
#' \code{\link{RQuicksort2B}} for a tuned indirect Quicksort for equally-sized elements,
#' \code{\link{Quicksort2B}} for a Quicksort for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

WQuicksort2B <- function(x
                         , situation=c("exsitu", "insitu")
                         , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_WQuicksort2B_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_WQuicksort2B_exsitu
                 , x = x
    )
  ret <- retperf(ret, "WQuicksort2B")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'VKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' \code{\link{UKnuthsort}} for an indirect mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{VFrogsort1}} for direct Frogsort of size-varying elements (strings),
#' \code{\link{VKnuthsortA}} for an adaptive tuned version,
#' \code{\link{Knuthsort}} for the version for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

VKnuthsort <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == 'insitu') {
    ret <- .Call(C_r_VKnuthsort_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_VKnuthsort_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "VKnuthsort")
  ret
}



#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'VKnuthA'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{VKnuthsort}} for the untuned version,
#' \code{\link{VFrogsort1A}} for a tuned direct frogsort of size-varying elements (strings),
#' \code{\link{KnuthsortA}} for the version for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

VKnuthsortA <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == 'insitu') {
    ret <- .Call(C_r_VKnuthsortA_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_VKnuthsortA_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "VKnuthsortA")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'VFrog1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{VFrogsort1A}} for an adaptive tuned version,
#' \code{\link{Frogsort1}} for the version for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

VFrogsort1 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_VFrogsort1_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_VFrogsort1_exsitu
                 , x = x
    )
  ret <- retperf(ret, "VFrogsort1")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'VFrog1A'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a character vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{VFrogsort1}} for the untuned version,
#' \code{\link{VKnuthsortA}} for a tuned direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' \code{\link{Frogsort1A}} for the version for equally-sized elements,
#' @examples
#' x <- vtestdb$func$KafkaWords()
#' rbind(
#'  {y <- x[]; sperf(UZacksort(y))}
#' ,{y <- x[]; sperf(WQuicksort2(y))}
#' ,{y <- x[]; sperf(UZacksortB(y))}
#' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' ,{y <- x[]; sperf(UKnuthsort(y))}
#' ,{y <- x[]; sperf(VKnuthsort(y))}
#' ,{y <- x[]; sperf(VFrogsort1(y))}
#' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' )
#' @export

VFrogsort1A <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
)
{
  if (!is.character(x))
    stop("only character vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu")
    ret <- .Call(C_r_VFrogsort1A_insitu
                 , x = x
    )
  else
    ret <- .Call(C_r_VFrogsort1A_exsitu
                 , x = x
    )
  ret <- retperf(ret, "VFrogsort1A")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'MKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{integer}} matrix to be sorted columnwise by key in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the standard algoritm for sorting a double vector
#' \code{\link{NKnuthsort}} for an algorithm sorting separated keys and pointers to records
#' \code{\link{MFrogsort2}} for a Frogsort2 doing this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(NInsertionsort(x))
#' print(x)
#' n <- getOption("greensort_example_size")
#' x <- as.integer(testdb$func$permut(n))
#' x <- rbind(x, -x, -x)
#' rbind(
#'  {y <- x[]; sperf(MKnuthsort(y))}
#' ,{y <- x[]; sperf(NKnuthsort(y))}
#' ,{y <- x[]; sperf(MFrogsort2(y))}
#' ,{y <- x[]; sperf(NFrogsort2(y))}
#' )
#' @export

MKnuthsort <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_MKnuthsortP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_MKnuthsortP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "MKnuthsort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'NKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{integer}} matrix to be sorted columnwise by key in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the standard algoritm for sorting a double vector
#' \code{\link{MKnuthsort}} for an algorithm sorting by mving complete columns
#' \code{\link{NFrogsort2}} for a Frogsort2 doing this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(NInsertionsort(x))
#' print(x)
#' n <- getOption("greensort_example_size")
#' x <- as.integer(testdb$func$permut(n))
#' x <- rbind(x, -x, -x)
#' rbind(
#'  {y <- x[]; sperf(MKnuthsort(y))}
#' ,{y <- x[]; sperf(NKnuthsort(y))}
#' ,{y <- x[]; sperf(MFrogsort2(y))}
#' ,{y <- x[]; sperf(NFrogsort2(y))}
#' )
#' @export

NKnuthsort <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_NKnuthsortP_insitu
                 , x = x
    )
  }else{
    ret <- .Call(C_r_NKnuthsortP_exsitu
                 , x = x
    )
  }
  ret <- retperf(ret, "NKnuthsort")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'MFrog2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{integer}} matrix to be sorted columnwise by key in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param p fraction of  buffer relative to data size
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the standard algoritm for sorting a double vector
#' \code{\link{NFrogsort2}} for an algorithm sorting separated keys and pointers to records
#' \code{\link{MKnuthsort}} for a Knuthsort doing this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(NInsertionsort(x))
#' print(x)
#' n <- getOption("greensort_example_size")
#' x <- as.integer(testdb$func$permut(n))
#' x <- rbind(x, -x, -x)
#' rbind(
#'  {y <- x[]; sperf(MKnuthsort(y))}
#' ,{y <- x[]; sperf(NKnuthsort(y))}
#' ,{y <- x[]; sperf(MFrogsort2(y))}
#' ,{y <- x[]; sperf(NFrogsort2(y))}
#' )
#' @export

MFrogsort2 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
                       , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    ret <- .Call(C_r_MFrogsort2P_insitu
                 , x = x
                 , p = as.double(p)
    )
  }else{
    ret <- .Call(C_r_MFrogsort2P_exsitu
                 , x = x
                 , p = as.double(p)
    )
  }
  ret <- retperf(ret, "MFrogsort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'NFrog2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{integer}} matrix to be sorted columnwise by key in the first row
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param p fraction of buffer relative to data size
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the standard algoritm for sorting a double vector
#' \code{\link{MKnuthsort}} for an algorithm sorting by moving complete columns
#' \code{\link{NKnuthsort}} for a Knuthsort doing this
#' @examples
#' x <- as.integer(testdb$func$permut(2^4))
#' x <- rbind(x,-x,-x)
#' sperf(NInsertionsort(x))
#' print(x)
#' n <- getOption("greensort_example_size")
#' x <- as.integer(testdb$func$permut(n))
#' x <- rbind(x, -x, -x)
#' rbind(
#'  {y <- x[]; sperf(MKnuthsort(y))}
#' ,{y <- x[]; sperf(NKnuthsort(y))}
#' ,{y <- x[]; sperf(MFrogsort2(y))}
#' ,{y <- x[]; sperf(NFrogsort2(y))}
#' )
#' @export

NFrogsort2 <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
                       , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
)
{
  if (typeof(x) != "integer")
    stop("only integer matrices implemented")
  if (!is.matrix(x))
    stop("x must be matrix")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    ret <- .Call(C_r_NFrogsort2P_insitu
                 , x = x
                 , p = as.double(p)
    )
  }else{
    ret <- .Call(C_r_NFrogsort2P_exsitu
                 , x = x
                 , p = as.double(p)
    )
  }
  ret <- retperf(ret, "NFrogsort2")
  ret
}






#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PQuick2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PQuicksort2 <- function(x
                        , situation=c("insitu","exsitu")
                        , method=c("index")
                        , threads=perfcores()
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PQuicksort2_insitu
                 , x = x
                 , t = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PQuicksort2_exsitu
                 , x = x
                 , t = as.double(threads)
    )
  }
  ret <- retperf(ret, "PQuicksort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PQuick2B'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Quicksort2B}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Quicksort2B(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PQuicksort2B <- function(x
                         , situation=c("insitu","exsitu")
                         , method=c("index")
                         , threads=perfcores()
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PQuicksort2B_insitu
                 , x = x
                 , t = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PQuicksort2B_exsitu
                 , x = x
                 , t = as.double(threads)
    )
  }
  ret <- retperf(ret, "PQuicksort2B")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PDuck'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Ducksort}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Ducksort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PDucksort <- function(x
                      , situation=c("insitu","exsitu")
                      , method=c("index")
                      , threads=perfcores()
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PDucksort_insitu
                 , x = x
                 , t = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PDucksort_exsitu
                 , x = x
                 , t = as.double(threads)
    )
  }
  ret <- retperf(ret, "PDucksort")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PDuckB'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{DucksortB}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(DucksortB(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(DucksortB(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(DucksortB(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PDucksortB <- function(x
                       , situation=c("insitu","exsitu")
                       , method=c("index")
                       , threads=perfcores()
) {
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PDucksortB_insitu
                 , x = x
                 , t = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PDucksortB_exsitu
                 , x = x
                 , t = as.double(threads)
    )
  }
  ret <- retperf(ret, "PDucksortB")
  ret
}





#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PKnuth'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Knuthsort}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Knuthsort(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PKnuthsort <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
                       , threads=perfcores()
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PKnuthsortP_insitu
                 , x = x
                 , threads = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PKnuthsortP_exsitu
                 , x = x
                 , threads = as.double(threads)
    )
  }
  ret <- retperf(ret, "PKnuthsort")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PFrog0'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort0}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort0(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort0(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort0(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PFrogsort0 <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
                       , threads=perfcores()
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PFrogsort0P_insitu
                 , x = x
                 , threads = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PFrogsort0P_exsitu
                 , x = x
                 , threads = as.double(threads)
    )
  }
  ret <- retperf(ret, "PFrogsort0")
  ret
}

#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PFrog1'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort1}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort1(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PFrogsort1 <- function(x
                       , situation=c(
                         "insitu"   # allocate n, copy to aux, sort
                         , "exsitu"   # allocate 2n, copy to aux+aux2, sort, copy from aux
                       )
                       , method=c(
                         "pointer"
                       )
                       , threads=perfcores()
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  if (situation == "insitu") {
    ret <- .Call(C_r_PFrogsort1P_insitu
                 , x = x
                 , threads = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PFrogsort1P_exsitu
                 , x = x
                 , threads = as.double(threads)
    )
  }
  ret <- retperf(ret, "PFrogsort1")
  ret
}




#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PFrog2'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort2}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PFrogsort2 <- function(x
                       , p = 1/5  # is best for speed, footprint and energy
                       , threads=perfcores()
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    ret <- .Call(C_r_PFrogsort2P_insitu
                 , x = x
                 , p = as.double(p)
                 , threads = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PFrogsort2P_exsitu
                 , x = x
                 , p = as.double(p)
                 , threads = as.double(threads)
    )
  }
  ret <- retperf(ret, "PFrogsort2")
  ret
}


#' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PFrog3'; greeNsort::algodb[thisalgo,'name']}}
#'
#' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#'
#' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#'
#' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#'
#' See \code{\link{algodb}} for the complete table of algorithms.
#'
#' @param x a \code{\link{double}} vector to be sorted
#' @param p fraction of  buffer relative to data size
#' @param threads the number of threads to use, default \code{\link{perfcores}}
#' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' @return like \code{\link{rawperf}}
#' @seealso
#' \code{\link{Frogsort3}} for the serial algorithm
#' \code{\link{PQuicksort2}} for a branch-parallel Quicksort
#' \code{\link{PQuicksort2}} for a branch-parallel block-tuned Quicksort
#' \code{\link{PDucksort}} for a branch-parallel Ducksort
#' \code{\link{PDucksort}} for a branch-parallel block-tuned Ducksort
#' \code{\link{PKnuthsort}} for a full-parallel Mergesort 100\% buffer
#' \code{\link{PFrogsort0}} for a full-parallel balanced F&M Frogsort 50\% buffer using triplet-setup
#' \code{\link{PFrogsort1}} for a full-parallel balanced F&M Frogsort 50\% buffer
#' \code{\link{PFrogsort2}} for a full-parallel balanced F&M Frogsort 14\% buffer
#' \code{\link{PFrogsort3}} for a full-parallel imbalanced A&M Frogsort 12\% buffer
#' @examples
#' n <- getOption("greensort_example_size")
#' x <- testdb$func$permut(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort3(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$tielog2(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort3(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' x <- testdb$func$ascnorm(n)
#' rbind(
#'  {y <- x[]; sperf(Frogsort3(y))}
#' ,{y <- x[]; sperf(PQuicksort2(y))}
#' ,{y <- x[]; sperf(PQuicksort2B(y))}
#' ,{y <- x[]; sperf(PDucksort(y))}
#' ,{y <- x[]; sperf(PDucksortB(y))}
#' ,{y <- x[]; sperf(PKnuthsort(y))}
#' ,{y <- x[]; sperf(PFrogsort0(y))}
#' ,{y <- x[]; sperf(PFrogsort1(y))}
#' ,{y <- x[]; sperf(PFrogsort2(y))}
#' ,{y <- x[]; sperf(PFrogsort3(y))}
#' )
#' @export

PFrogsort3 <- function(x
                       , p = 1/7  # is best for speed, 1/8 is best for footprint, 1/6 is best for energy  # golden ratio:  1 - ((1-sqrt(5))/-2)
                       , threads=perfcores()
                       , situation=c("insitu","exsitu")
                       , method=c("pointer")
)
{
  if (typeof(x) != "double")
    stop("only double vectors implemented")
  situation <- match.arg(situation)
  method <- match.arg(method)
  stopifnot(p > 0 && p < 1)
  if (p > 0.5)
    warning("p > 0.5 does n't help")
  n <- length(x)
  if (n < 5)
    p <- 0.5
  else if (p < 2/n)
    p <- 2/n
  if (situation == "insitu") {
    ret <- .Call(C_r_PFrogsort3P_insitu
                 , x = x
                 , p = as.double(p)
                 , threads = as.double(threads)
    )
  }else{
    ret <- .Call(C_r_PFrogsort3P_exsitu
                 , x = x
                 , p = as.double(p)
                 , threads = as.double(threads)
    )
  }
  ret <- retperf(ret, "PFrogsort3")
  ret
}







#' #' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- 'PVKnuth'; greeNsort::algodb[thisalgo,'name']}}
#' #'
#' #' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#' #'
#' #' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#' #'
#' #' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#' #'
#' #' See \code{\link{algodb}} for the complete table of algorithms.
#' #'
#' #' @param x a character vector to be sorted
#' #' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' #' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' #' @param threads the number of threads to use, default \code{\link{perfcores}}
#' #' @return like \code{\link{rawperf}}
#' #' @seealso
#' #' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' #' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' #' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' #' \code{\link{VFrogsort1}} for direct frogsort of size-varying elements (strings),
#' #' \code{\link{VKnuthsortA}} for an adaptive tuned version,
#' #' \code{\link{Knuthsort}} for the version for equally-sized elements,
#' #' @examples
#' #' x <- vtestdb$func$KafkaWords()
#' #' rbind(
#' #'  {y <- x[]; sperf(UZacksort(y))}
#' #' ,{y <- x[]; sperf(WQuicksort2(y))}
#' #' ,{y <- x[]; sperf(VKnuthsort(y))}
#' #' ,{y <- x[]; sperf(VFrogsort1(y))}
#' #' ,{y <- x[]; sperf(UZacksortB(y))}
#' #' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' #' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' #' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' #' )
#' #' @export
#'
#' PVKnuthsort <- function(x
#'                         , situation=c("insitu","exsitu")
#'                         , method=c("index")
#'                         , threads = perfcores()
#' )
#' {
#'   stop("not yet implemented in parallel")
#'   if (!is.character(x))
#'     stop("only character vectors implemented")
#'   situation <- match.arg(situation)
#'   method <- match.arg(method)
#'   if (situation == 'insitu') {
#'     ret <- .Call(C_r_PVKnuthsort_insitu
#'                  , x = x
#'                  , t = as.double(threads)
#'     )
#'   }else{
#'     ret <- .Call(C_r_PVKnuthsort_exsitu
#'                  , x = x
#'                  , t = as.double(threads)
#'     )
#'   }
#'   ret <- retperf(ret, "PVKnuthsort")
#'   ret
#' }
#'
#'
#'
#' #' \Sexpr[echo=FALSE, results=rd, stage=build]{{thisalgo <- '?PVFrog1'; greeNsort::algodb[thisalgo,'name']}}
#' #'
#' #' \Sexpr[echo=FALSE, results=rd, stage=build]{gsub("(.*)(like )(.+)( but)(.*)","\\\\1like \\\\\\\\code{\\\\\\\\link{\\\\3} } but\\\\5",greeNsort::algodb[thisalgo,'desc'])}
#' #'
#' #' \Sexpr[echo=FALSE, results=text, stage=build]{greeNsort::algodb[thisalgo,'expl']}
#' #'
#' #' \Sexpr[echo=FALSE, results=verbatim, stage=build]{{i <- !colnames(greeNsort::algodb) \%in\% c('func','desc','expl'); cat(paste(colnames(greeNsort::algodb)[i], ' - ',  greeNsort::algodb[thisalgo,i], '\n', sep=''), sep='')}}
#' #'
#' #' See \code{\link{algodb}} for the complete table of algorithms.
#' #'
#' #' @param x a character vector to be sorted
#' #' @param situation "insitu" will only allocate buffer memory and use the existing RAM for sorting, "exsitu" will allocate completely fresh RAM for data and buffer
#' #' @param method an attempt to classify the implementation,  "index" means the implementation rather indexes into arrays, "pointer" rather uses pointer arithmetic
#' #' @param threads the number of threads to use, default \code{\link{perfcores}}
#' #' @return like \code{\link{rawperf}}
#' #' @seealso
#' #' \code{\link{UZacksort}} for indirect Zacksort using pointers to size-varying elements (strings, not stable),
#' #' \code{\link{WQuicksort2}} for indirect stabilized quicksort using pointers  to size-varying elements (strings, no ties by definition),
#' #' \code{\link{VKnuthsort}} for direct mergesort of size-varying elements (strings) using Knuth's merge with one loop check,
#' #' \code{\link{VFrogsort1}} for direct frogsort of size-varying elements (strings),
#' #' \code{\link{VFrogsort1A}} for an adaptive tuned version,
#' #' \code{\link{Frogsort1}} for the version for equally-sized elements,
#' #' @examples
#' #' x <- vtestdb$func$KafkaWords()
#' #' rbind(
#' #'  {y <- x[]; sperf(UZacksort(y))}
#' #' ,{y <- x[]; sperf(WQuicksort2(y))}
#' #' ,{y <- x[]; sperf(VKnuthsort(y))}
#' #' ,{y <- x[]; sperf(VFrogsort1(y))}
#' #' ,{y <- x[]; sperf(UZacksortB(y))}
#' #' ,{y <- x[]; sperf(WQuicksort2B(y))}
#' #' ,{y <- x[]; sperf(VKnuthsortA(y))}
#' #' ,{y <- x[]; sperf(VFrogsort1A(y))}
#' #' )
#' #' @export
#'
#' PVFrogsort1 <- function(x
#'                         , situation=c("insitu","exsitu")
#'                         , method=c("index")
#'                         , threads = perfcores()
#' )
#' {
#'   stop("not yet implemented in parallel")
#'   if (!is.character(x))
#'     stop("only character vectors implemented")
#'   situation <- match.arg(situation)
#'   method <- match.arg(method)
#'   if (situation == "insitu")
#'     ret <- .Call(C_r_PVFrogsort1_insitu
#'                  , x = x
#'                  , t = as.double(threads)
#'     )
#'   else
#'     ret <- .Call(C_r_PVFrogsort1_exsitu
#'                  , x = x
#'                  , t = as.double(threads)
#'     )
#'   ret <- retperf(ret, "PVFrogsort1")
#'   ret
#' }







#----------------------------------------------------------

#' @describeIn vtestdb Kafka Paragraphs in natural sequence
#' @export
KafkaParts <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'Kafka_Die_Verwandlung.txt.gz'))
  ret <- readLines(con, encoding = 'UTF-8')[-(1:10)]
  close(con)
  ret <- strsplit(paste(ret, sep = "", collapse = " "), "  ")[[1]]
  ret
}

#' @describeIn vtestdb Kafka Words in natural sequence
#' @export
KafkaWords <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'Kafka_Die_Verwandlung.txt.gz'))
  ret <- scan(con, what = 'character', encoding = 'UTF-8', quiet = TRUE)
  close(con)
  ret <- gsub("[^abcdefghijklmnopqrstuvwxyz\u00e4\u00f6\u00fc\u00dfABCDEFGHIJKLMNOPQRSTUVWXYZ\u00c4\u00d6\u00dc']","",ret)
  ret[ret!=""]
}


#' @describeIn vtestdb Kafka Words sorted ascending
#' @export
KafkaWords_asc <- function() {
  sort(KafkaWords())
}

#' @describeIn vtestdb Kafka Words sorted descending
#' @export
KafkaWords_desc <- function() {
  sort(KafkaWords(), decreasing = TRUE)
}

#' @describeIn vtestdb Kafka Words randomly permuted
#' @export
KafkaWords_perm <- function() {
  sample(KafkaWords())
}


#' @describeIn vtestdb Kafka Parts sorted ascending
#' @export
KafkaParts_asc <- function() {
  sort(KafkaParts())
}

#' @describeIn vtestdb Kafka Parts sorted descending
#' @export
KafkaParts_desc <- function() {
  sort(KafkaParts(), decreasing = TRUE)
}

#' @describeIn vtestdb Kafka Parts randomly permuted
#' @export
KafkaParts_perm <- function() {
  sample(KafkaParts())
}




#' @describeIn vtestdb Bible Verse tokens in natural sequence
#' @export
BibleVerses <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'german_luther_1545_utf8.txt.gz'), encoding = "UTF-8")
  ret <- read.table(con, sep="\t", quote="", fileEncoding = "UTF-8", header = FALSE)[,4]
  ret <- ret[nchar(ret)>1]
  ret
}

#' @describeIn vtestdb Bible Word tokens in natural sequence
#' @export
BibleWords <- function() {
  ret <- unlist(strsplit(BibleVerses(), " "))
  ret <- gsub("[^abcdefghijklmnopqrstuvwxyz\u00e4\u00f6\u00fc\u00dfABCDEFGHIJKLMNOPQRSTUVWXYZ\u00c4\u00d6\u00dc']","",ret)
  ret <- ret[ret!=""]
  ret
}

#' @describeIn vtestdb Bible Words sorted ascending
#' @export
BibleWords_asc <- function() {
  sort(BibleWords())
}

#' @describeIn vtestdb Bible Words sorted descending
#' @export
BibleWords_desc <- function() {
  sort(BibleWords(), decreasing = TRUE)
}

#' @describeIn vtestdb Bible Words randomly permuted
#' @export
BibleWords_perm <- function() {
  sample(BibleWords())
}


#' @describeIn vtestdb Bible Verses sorted ascending
#' @export
BibleVerses_asc <- function() {
  sort(BibleVerses())
}

#' @describeIn vtestdb Bible Verses sorted descending
#' @export
BibleVerses_desc <- function() {
  sort(BibleVerses(), decreasing = TRUE)
}

#' @describeIn vtestdb Bible Verses randomly permuted
#' @export
BibleVerses_perm <- function() {
  sample(BibleVerses())
}




#' @describeIn vtestdb dictionary of german Words sorted
#' @export
GermanWords <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'german.dic.gz'))
  ret <- scan(con, what = 'character', encoding = 'UTF-8', quiet = TRUE)
  close(con)
  ret
}

#' @describeIn vtestdb dictionary of austrian Words sorted
#' @export
AustrianWords <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'austriazismen.txt.gz'))
  ret <- scan(con, what = 'character', encoding = 'UTF-8', quiet = TRUE)
  close(con)
  ret
}

#' @describeIn vtestdb dictionary of swiss Words sorted
#' @export
SwissWords <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'helvetismen.txt.gz'))
  ret <- scan(con, what = 'character', encoding = 'UTF-8', quiet = TRUE)
  close(con)
  ret
}

#' @describeIn vtestdb dictionary of variant Words sorted
#' @export
VariantWords <- function() {
  con <- gzfile(file.path(installed.packages()['greeNsort','LibPath'], 'greeNsort', 'extdata', 'variants.dic.gz'))
  ret <- scan(con, what = 'character', encoding = 'UTF-8', quiet = TRUE)
  close(con)
  ret
}


#' @describeIn vtestdb dictionary of German languages, concatenation of  4 subdictionaries (German, Austrian, Swiss, Variant), hence partially sorted
#' @export
DictWords <- function() {
  c(GermanWords(), AustrianWords(), SwissWords(), VariantWords())
}

#' @describeIn vtestdb dictionary ascending
#' @export
DictWords_asc <- function() {
  sort(DictWords())
}

#' @describeIn vtestdb dictionary descending
#' @export
DictWords_desc <- function() {
  sort(DictWords(), decreasing = TRUE)
}

#' @describeIn vtestdb dictionary randomly permuted
#' @export
DictWords_perm <- function() {
  sample(DictWords())
}

#' @describeIn vtestdb dictionary bootstrap sample (63% distinct, 37% duplicates)
#' @export
DictWords_boot <- function() {
  sample(DictWords(), replace = TRUE)
}


vtestdb <- tibble::tribble(
  ~name	,~label	,~major	,~minor	,~order, ~desc
  , "KafkaParts"	    , "KP", "K", "P", "n", "Kafka Paragraphs in Natural sequence"
  , "KafkaParts_asc"  , "KPa", "K", "P", "a", "Kafka Paragraphs sorted ascending"
  , "KafkaParts_desc" , "KPd", "K", "P", "d", "Kafka Paragraphs sorted descending"
  , "KafkaParts_perm" , "KPp", "K", "P", "p", "Kafka Paragraphs randomly permuted"
  , "KafkaWords"	    , "KW" , "K", "W", "n", "Kafka Words in Natural sequence"
  , "KafkaWords_asc"  , "KWa", "K", "W", "a", "Kafka Words sorted ascending"
  , "KafkaWords_desc" , "KWd", "K", "W", "d", "Kafka Words sorted descending"
  , "KafkaWords_perm"	, "KWp", "K",	"W", "p", "Kafka Words randomly permuted"
  , "BibleVerses"	    , "BV"	, "B"	, "V"	, "n", "Bible Verses in natural sequence"
  , "BibleVerses_asc"	, "BVa"	, "B"	, "V", "a"	, "Bible Verses sorted ascending"
  , "BibleVerses_desc"	, "BVd"	, "B"	, "V", "d"	, "Bible Verses sorted descending"
  , "BibleVerses_perm"	, "BVp"	, "B"	, "V", "p"	, "Bible Verses randomly permuted"
  , "BibleWords"	    , "BW"	, "B"	, "W"	, "n", "Bible Words  in natural sequence"
  , "BibleWords_asc"	, "BWa"	, "B"	, "W", "a"	, "Bible Words sorted ascending"
  , "BibleWords_desc"	, "BWd"	, "B"	, "W", "d"	, "Bible Words sorted descending"
  , "BibleWords_perm"	, "BWp"	, "B"	, "W", "p"	, "Bible Words randomly permuted"
  , "GermanWords"	    , "GW"	, "G"	,"W"	, "a", "dictionary of German Words sorted"
  , "AustrianWords"	  , "AW"	, "A"	,"W"  , "a", "dictionary of Austrian Words sorted"
  , "SwissWords"	    , "SW"	, "S"	,"W"	, "a", "dictionary of Swiss Words sorted"
  , "VariantWords"	  , "VW"	, "V"	,"W"	, "a", "dictionary of Variant Words sorted"
  , "DictWords"	      , "DW"	, "D"	,"W"	, "n", "dictionary of german languages, concatenation of  4 subdictionaries (german, autrian, swiss, variant), hence partially sorted"
  , "DictWords_asc"	  , "DWa"	, "D"	,"W"	, "a"	, "dictionary ascending"
  , "DictWords_desc"	, "DWd"	, "D"	,"W"	, "d"	, "dictionary descending"
  , "DictWords_perm"	, "DWp"	, "D"	,"W"	, "p"	, "dictionary randomly permuted"
  , "DictWords_boot"	, "DWb"	, "D"	,"W"	, "b"	, "dictionary bootstrap sample (63% distinct, 37% duplicates)"
)

vtestdb <- as.data.frame(vtestdb)
row.names(vtestdb) <- vtestdb$label
#vtestdb$func <- sapply(vtestdb$name, get)
vtestdb$func <- eval(parse(text = paste("c(", paste(vtestdb$name, collapse = ", "), ")", sep = "")))
names(vtestdb$func) <- vtestdb$name
vtestdb$code <- sapply(vtestdb$func, function(i){paste(deparse(i), collapse = "\n")})

vtestdb <- cbind(vtestdb
                 , structure(list(entropy = c(-6.63840950255377, -6.63840950255377,
                                              -6.63840950255377, -6.63840950255377, -9.54634416650865, -9.54634416650865,
                                              -9.54634416650865, -9.54634416650865, -14.8616148809665, -14.8616148809665,
                                              -14.8616148809665, -14.8616148809665, -9.86512898814041, -9.86512898814041,
                                              -9.86512898814041, -9.86512898814041, -20.9221038075773, -12.0687782779854,
                                              -11.4767462039395, -11.9450779951078, -20.9277291751745, -20.9277291751745,
                                              -20.9277291751745, -20.9277291751745, -20.1021184666074), relentropy = c(0.997025931025273,
                                                                                                                       0.997025931025273, 0.997025931025273, 0.997025931025273, 0.671072251562352,
                                                                                                                       0.671072251562352, 0.671072251562352, 0.671072251562352, 0.997735954829011,
                                                                                                                       0.997735954829011, 0.997735954829011, 0.997735954829011, 0.509293618509685,
                                                                                                                       0.509293618509685, 0.509293618509685, 0.509293618509685, 0.999999999999984,
                                                                                                                       1, 1, 1, 0.999885145497511, 0.999885145497511, 0.999885145497511,
                                                                                                                       0.999885145497511, 0.960439113080429), monotAscFrac = c(0.5,
                                                                                                                                                                               1, 0.01, 0.51, 0.508326807622031, 1, 0.793056643174106, 0.501174628034456,
                                                                                                                                                                               0.498490516505874, 1, 0.00830215921769377, 0.502986152129684,
                                                                                                                                                                               0.516064674832919, 1, 0.96699706493012, 0.503034676323327, 0.999985907844348,
                                                                                                                                                                               0.99697322467986, 0.998595998595999, 1, 0.999975976096216, 1,
                                                                                                                                                                               0.00119969369522675, 0.500250248997753, 0.50021271164809), strictAscFrac = c(0.5,
                                                                                                                                                                                                                                                            0.99, 0, 0.51, 0.507700339336988, 0.206943356825894, 0, 0.495797441921169,
                                                                                                                                                                                                                                                            0.498490516505874, 0.991697840782306, 0, 0.502986152129684, 0.515062707790109,
                                                                                                                                                                                                                                                            0.0330029350698795, 0, 0.496197985146097, 0.999985907844348,
                                                                                                                                                                                                                                                            0.99697322467986, 0.998595998595999, 1, 0.999975976096216, 0.998800306304773,
                                                                                                                                                                                                                                                            0, 0.500250248997753, 0.500212211150094), spearman = c(-0.147559697146185,
                                                                                                                                                                                                                                                                                                                   1, -0.999988351776354, -0.0101456027955737, 0.0225391285710389,
                                                                                                                                                                                                                                                                                                                   1, -0.999787047499194, 0.00354048697415988, -0.00438293185413531,
                                                                                                                                                                                                                                                                                                                   1, -0.999999973156686, -0.00373362889051942, 0.0154662635228889,
                                                                                                                                                                                                                                                                                                                   1, -0.999572730305453, 0.0083228927052873, 1, 0.9999997585934,
                                                                                                                                                                                                                                                                                                                   0.999999997926487, 1, 0.987441383421479, 1, -0.999999999999997,
                                                                                                                                                                                                                                                                                                                   -0.000221699960986286, 0.00054959882473941)), class = "data.frame", row.names = c("KafkaParts",
                                                                                                                                                                                                                                                                                                                                                                                                     "KafkaParts_asc", "KafkaParts_desc", "KafkaParts_perm", "KafkaWords",
                                                                                                                                                                                                                                                                                                                                                                                                     "KafkaWords_asc", "KafkaWords_desc", "KafkaWords_perm", "BibleVerses",
                                                                                                                                                                                                                                                                                                                                                                                                     "BibleVerses_asc", "BibleVerses_desc", "BibleVerses_perm", "BibleWords",
                                                                                                                                                                                                                                                                                                                                                                                                     "BibleWords_asc", "BibleWords_desc", "BibleWords_perm", "GermanWords",
                                                                                                                                                                                                                                                                                                                                                                                                     "AustrianWords", "SwissWords", "VariantWords", "DictWords", "DictWords_asc",
                                                                                                                                                                                                                                                                                                                                                                                                     "DictWords_desc", "DictWords_perm", "DictWords_boot"))
)

#rm(vstats)

# vstats <- as.data.frame(do.call('rbind', lapply(vtestdb$func, function(f) {
#   print(f)
#   x <- f()
#   n <- length(x)
#   t <- table(x)
#   p <- t[t > 0]/n
#   entropy <- sum(p*log2(p))
#   maxentropy <- log2(1/n)
#   c(
#     entropy = entropy
#     , relentropy = entropy/maxentropy
#     , monotAscFrac =  sum(ifelse(x[-1] >=  x[-n], 1, 0)) / (n - 1)
#     , strictAscFrac = sum(ifelse(x[-1] >  x[-n], 1, 0)) / (n - 1)
#     , spearman = if (length(p) == 1) NA else cor(seq_along(x), order(x), method = 'spearman')
#   )
# })))
# dput(vstats)


#' Testdata database (varying length strings)
#'
#' A dataset describing all testdata generators in this package.
#'
#' @name vtestdb
#' @format A data frame with one row per test generator and these columns:
#' \describe{
#'   \item{ name }{ full test name }
#'   \item{ label }{ abbreviated test label}
#'   \item{ desc }{ a description of the test }
#'   \item{ major }{ one letter classifiying the test as 'P'=Permutation, 'A' = Ascending, 'D' = Descending' 'a' = AscendingDescending, 'd'= DescendingAscending, 'T'= Ties, 'B' = Ascending with ties, 'S' = Stability }
#'   \item{ minor }{ one letter identifying the test }
#'   \item{ stable }{ \code{TRUE} if the data is suitable for stability testing via \code{\link{current_keys}}, \code{FALSE} otherwise }
#'   \item{ entropy }{ entropy }
#'   \item{ relentropy }{ relative entropy }
#'   \item{ monotAscFrac }{ fraction of ascending differences }
#'   \item{ spearman }{ Spearman correation between position and value }
#'   \item{ strictAscFrac }{  fraction of strictly ascending differences }
#'   \item{ code }{ the test generator code }
#'   \item{ func }{ the callable test generator function }
#'   }
#'
#' @seealso \code{\link{vtestdb}} and \code{\link{algodb}}
#'
#' @examples
#'   table(vtestdb$major)
#'   table(vtestdb$minor)
#'   summary(vtestdb$entropy)
#'   summary(vtestdb$relentropy)
#'   summary(vtestdb$monotAscFrac)
#'   summary(vtestdb$strictAscFrac)
#'   summary(vtestdb$spearman)
#'   \donttest{
#'     View(vtestdb)
#'   }
#'
#' @export
"vtestdb"



#' Testdata database (doubles)
#'
#' A dataset describing all testdata generators in this package.
#'
#' @name testdb
#' @format A data frame with one row per test generator and these columns:
#' \describe{
#'   \item{ name }{ full test name }
#'   \item{ label }{ abbreviated test label}
#'   \item{ desc }{ a description of the test }
#'   \item{ major }{ one letter classifiying the test as 'P'=Permutation, 'A' = Ascending, 'D' = Descending' 'a' = AscendingDescending, 'd'= DescendingAscending, 'T'= Ties, 'B' = Ascending with ties, 'S' = Stability }
#'   \item{ minor }{ one letter identifying the test }
#'   \item{ stable }{ \code{TRUE} if pattern is suitable for stability testing with \code{\link{current_keys}} }
#'   \item{ entropy }{ entropy }
#'   \item{ relentropy }{ relative entropy }
#'   \item{ monotAscFrac }{ fraction of ascending differences }
#'   \item{ strictAscFrac }{  fraction of strictly ascending differences }
#'   \item{ spearman }{ Spearman correation between position and value }
#'   \item{ code }{ the test generator code }
#'   \item{ func }{ the callable test generator function }
#'   }
#'
#' @seealso \code{\link{vtestdb}} and \code{\link{algodb}}
#'
#' @examples
#'   table(testdb$major)
#'   summary(testdb$entropy)
#'   summary(testdb$relentropy)
#'   summary(testdb$monotAscFrac)
#'   summary(testdb$strictAscFrac)
#'   summary(testdb$spearman)
#'   \donttest{
#'     View(testdb)
#'   }
#'
#' @export
"testdb"

testdb <- tibble::tribble(
  ~name	,~label	,~major	,~minor	,~stable	,~desc	,~code
  , "ascall"	, "ascall"	, "A"	, "A"	, FALSE	, "100% ascending"	, "function (n) as.double(1:n)"
  , "descall"	, "descall"	, "D"	, "D"	, FALSE	, "100% descending"	, "function (n) as.double(n:1)"
  , "ascdesc"	, "ascdesc"	, "a"	, "B"	, FALSE	, "50% ascending then 50% descending"	, "function(n)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; if (n %% 2) c(1:n2, 0, n2:1) else c(1:n2, n2:1) })"
  , "descasc"	, "descasc"	, "d"	, "C"	, FALSE	, "50% descending then 50% ascending"	, "function(n)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; if (n %% 2) c(n2:1, 0, 1:n2) else c(n2:1, 1:n2) })"
  , "asc90rev"	, "asc90rev"	, "A"	, "b"	, FALSE	, "90% ascending, random rest reversed"	, "function(n) {x <- as.double(1:n); i <- sample(n,n/10); x[i] <- x[rev(i)]; x}"
  , "desc90rev"	, "desc90rev"	, "D"	, "c"	, FALSE	, "90% descending, random rest reversed"	, "function(n) {x <- as.double(n:1); i <- sample(n,n/10); x[i] <- x[rev(i)]; x}"
  , "ascnorm"	, "ascnorm"	, "A"	, "a"	, TRUE	, "almost ascending with normal disturbance sd=1"	, "function (n, sd=1) { as.double(1:n)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "descnorm"	, "descnorm"	, "D"	, "d"	, TRUE	, "almost decending with normal disturbance sd=1"	, "function (n, sd=1) { as.double(n:1)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "asclog2"	, "asclog2"	, "A"	, "a"	, TRUE	, "almost ascending with normal disturbance sd=log2(n)"	, "function (n, sd=log2(n)) { as.double(1:n)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "desclog2"	, "desclog2"	, "D"	, "d"	, TRUE	, "almost decending with normal disturbance sd=log2(n)"	, "function (n, sd=log2(n)) { as.double(n:1)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "ascsqrt"	, "ascsqrt"	, "A"	, "a"	, TRUE	, "almost ascending with normal disturbance sd=sqrt(n)"	, "function (n, sd=sqrt(n)) { as.double(1:n)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "descsqrt"	, "descsqrt"	, "D"	, "d"	, TRUE	, "almost decending with normal disturbance sd=sqrt(n)"	, "function (n, sd=sqrt(n)) { as.double(n:1)  + rnorm(n, mean=9*sd, sd=sd) }"
  , "ascdescnorm"	, "ascdescnorm"	, "a"	, "B"	, TRUE	, "50% ascending then 50% descending with normal disturbance"	, "function(n, sd=1)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; if (n %% 2) c(1:n2, n2+1, n2:1) else c(1:n2, n2:1) }) + rnorm(n, mean=9*sd, sd=sd)"
  , "descascnorm"	, "descascnorm"	, "d"	, "C"	, TRUE	, "50% descending then 50% ascending with normal disturbance"	, "function(n, sd=1)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; if (n %% 2) c(n2:1, 0, 1:n2) else c(n2:1, 1:n2) }) + 9*sd + rnorm(n, mean=9*sd, sd=sd)"
  , "GlobalPermutLocalTie"	, "tieshuffle"	, "T"	, "T"	, FALSE	, "random shuffle of tied blocks"	, "function (n, b=ceiling(sqrt(n))) { x <-sample(seq(0,n,by=b)); rep(x, rep(b,length(x)))[1:n]}"
  , "GlobalPermutLocalAsc"	, "ascshuffle"	, "A"	, "A"	, FALSE	, "random shuffle of ascending blocks"	, "function (n, b=ceiling(sqrt(n))) { x <-sample(seq(0,n,by=b)); rep(x, rep(b,length(x)))[1:n] +  rep(1:b,length=n)}"
  , "GlobalPermutLocalDesc"	, "descshuffle"	, "D"	, "D"	, FALSE	, "random shuffle of descending blocks"	, "function (n, b=ceiling(sqrt(n))) { x <-sample(seq(0,n,by=b)); rep(x, rep(b,length(x)))[1:n] +  rep(b:1,length=n)}"
  , "GlobalPermutLocalAscDesc"	, "ascdescshuffle"	, "a"	, "D"	, FALSE	, "random shuffle of randomly ascending or descending blocks"	, "function (n, b=ceiling(sqrt(n))) { x <-sample(seq(0,n,by=b)); rep(x, rep(b,length(x)))[1:n]  + rep(c(1:b,b:1),length=n)}"
  , "GlobalPermutLocalTieAscDesc"	, "tieascdescshuffle"	, "T"	, "D"	, FALSE	, "random shuffle of randomly tied, ascending or descending blocks"	, "function (n, b=ceiling(sqrt(n))) { x <-sample(seq(0,n,by=b)); rep(x, rep(b,length(x)))[1:n] + rep(c(1:b,rep(1,b),b:1),length=n)}"
  , "GlobalAscLocalPermut"	, "ascglobal"	, "A"	, "G"	, TRUE	, "sqrt(N) ascending chunks with sqrt(N) random keys"	, "function(n, b=ceiling(sqrt(n))) {k <- n %/% b; r <- n %% b; k <- if (r) c(rep(b,k), r) else rep(b,k); rep(seq(1, n, b), k) + runif(n, 0, b)}"
  , "GlobalDescLocalPermut"	, "descglobal"	, "D"	, "G"	, TRUE	, "sqrt(N) descending chunks with sqrt(N) random keys"	, "function(n, b=ceiling(sqrt(n))) {k <- n %/% b; r <- n %% b; k <- if (r) c(rep(b,k), r) else rep(b,k); rep(seq(n, 1, -b), k) + runif(n, 0, b)}"
  , "GlobalTieLocalAsc"	, "asclocal"	, "A"	, "L"	, FALSE	, "sampled keys in sqrt(N) ascending chunks"	, "function(n, b=ceiling(sqrt(n))) {x <- as.double(sample(n)); Chunksort(x, b=b, direction=1, perf=FALSE); x}"
  , "GlobalTieLocalDesc"	, "desclocal"	, "D"	, "L"	, FALSE	, "sampled keys in sqrt(N) descending chunks"	, "function(n, b=ceiling(sqrt(n))) {x <- as.double(sample(n)); Chunksort(x, b=b, direction=-1, perf=FALSE); x}"
  , "GlobalTieLocalAscDesc"	, "ascdesclocal"	, "D"	, "L"	, FALSE	, "sampled keys in sqrt(N) descending chunks"	, "function(n, b=ceiling(sqrt(n))) {x <- as.double(sample(n)); Chunksort(x, b=b, direction=0, perf=FALSE); x}"
  , "GlobalAscLocalAsc"	, "ascwithasc"	, "A"	, "A"	, FALSE	, "sqrt(N) ascending chunks with sqrt(N) ascending keys (overlapping)"	, "function(n, b=ceiling(sqrt(n))) {as.double((1:n) + rep(1:b, length=n))}"
  , "GlobalAscLocalTie"	, "ascwithtie"	, "A"	, "T"	, FALSE	, "sqrt(N) ascending chunks with sqrt(N) tied keys"	, "function(n, b=ceiling(sqrt(n))) {as.double((1:n) + rep(b:1, length=n))}"
  , "GlobalAscLocalDesc"	, "ascwithdesc"	, "A"	, "R"	, FALSE	, "sqrt(N) ascending chunks with sqrt(N) descending keys"	, "function(n, b=ceiling(sqrt(n))) {(1:n) + (1+b) - rep(seq(2,2*b,2), length=n)}"
  , "GlobalDescLocalAsc"	, "descwithasc"	, "D"	, "R"	, FALSE	, "sqrt(N) descending chunks with sqrt(N) ascending keys"	, "function(n, b=ceiling(sqrt(n))) {(n:1) + rep(seq(2,2*b,2), length=n)}"
  , "GlobalDescLocalTie"	, "descwithtie"	, "D"	, "T"	, FALSE	, "sqrt(N) descending chunks with sqrt(N) tied keys"	, "function(n, b=ceiling(sqrt(n))) {as.double((n:1) + rep(1:b, length=n))}"
  , "GlobalDescLocalDesc"	, "descwithdesc"	, "D"	, "D"	, FALSE	, "sqrt(N) descending chunks with sqrt(N) descending keys (overlapping)"	, "function(n, b=ceiling(sqrt(n))) {as.double((n:1) + rep(b:1, length=n))}"
  , "weekdate"	, "weekdate"	, "A"	, "W"	, TRUE	, "ascending weeks with random times"	, "function (n) sort(sample(0:51, n, TRUE)) * 7 + runif(n, 0, 6.999)"
  , "weekday"	, "weekday"	, "B"	, "w"	, FALSE	, "ascending weeks with random days"	, "function (n) sort(sample(0:51, n, TRUE)) * 7 + sample(0:6, n, TRUE)"
  , "monthdate"	, "monthdate"	, "A"	, "M"	, TRUE	, "ascending months with random times"	, "function (n) sort(sample(0:11, n, TRUE)) * 31 + runif(n, 0, 30.999)"
  , "monthday"	, "monthday"	, "B"	, "m"	, FALSE	, "ascending months with random days"	, "function (n) sort(sample(0:11, n, TRUE)) * 31 + sample(0:30, n, TRUE)"
  , "permut"	, "permut"	, "P"	, "P"	, FALSE	, "random permuation"	, "function (n) as.double(sample(n))"
  , "stable"	, "stable"	, "S"	, "x"	, TRUE	, "50% sampled then 50% sampled + 0.5"	, "function(n)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; if (n %% 2) c(sample(n2), 0, sample(n2) + 0.5) else c(sample(n2), sample(n2) + 0.5) })"
  , "stableasc"	, "stableasc"	, "S"	, "y"	, TRUE	, "pairs of ascending integers {i,i+0.5}"	, "function(n)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; x <- as.vector(rbind((1:n2)+0.5, 1:n2)); if (n %% 2) c(0, x) else x})"
  , "stabledesc"	, "stabledesc"	, "S"	, "y"	, TRUE	, "pairs of descending integers {i,i+0.5}"	, "function(n)as.double(if (n < 2) rep(0,n) else {n2 <- n %/% 2; x <- as.vector(rbind(n2:1, (n2:1) + 0.5)); if (n %% 2) c(0, x) else x})"
  , "stabletie"	, "stabletie"	, "S"	, "z"	, TRUE	, "sampled {1,2} + ascending decimals"	, "function (n) sample(1:2, n, TRUE) + (0:(n - 1))/n"
  , "tieboot"	, "tieboot"	, "T"	, "t"	, FALSE	, "bootstrapsample (~63% distinct and 37% ties)"	, "function (n) as.double(sample(n, n, TRUE))"
  , "tielog2"	, "tielog2"	, "T"	, "g"	, FALSE	, "random sample from log2(N) distinct values"	, "function (n) as.double(sample(1:log2(n), n, TRUE))"
  , "tiesqrt"	, "tiesqrt"	, "T"	, "s"	, FALSE	, "random sample from sqrt(N) distinct values"	, "function (n) as.double(sample(1:sqrt(n), n, TRUE))"
  , "sample15"	, "sample15"	, "T"	, "5"	, FALSE	, "random sample from 5 distinct values"	, "function (n) as.double(sample(1:5, n, TRUE))"
  , "sample14"	, "sample14"	, "T"	, "4"	, FALSE	, "random sample from 4 distinct values"	, "function (n) as.double(sample(1:4, n, TRUE))"
  , "sample13"	, "sample13"	, "T"	, "3"	, FALSE	, "random sample from 3 distinct values"	, "function (n) as.double(sample(1:3, n, TRUE))"
  , "sample12"	, "sample12"	, "T"	, "2"	, FALSE	, "random sample from 2 distinct values"	, "function (n) as.double(sample(1:2, n, TRUE))"
  , "toggle12"	, "toggle12"	, "T"	, "o"	, FALSE	, "alternating 1 and 2"	, "function (n) rep(as.double(1:2), length.out = n)"
  , "toggle21"	, "toggle21"	, "T"	, "O"	, FALSE	, "alternating 2 and 1"	, "function (n) rep(as.double(2:1), length.out = n)"
  , "replic12"	, "replic12"	, "T"	, "e"	, FALSE	, "50% 1 then 50% 2"	, "function (n) c(rep(1, n%/%2), rep(2, n - n%/%2))"
  , "replic21"	, "replic21"	, "T"	, "E"	, FALSE	, "50% 2 then 50% 1"	, "function (n) c(rep(2, n%/%2), rep(1, n - n%/%2))"
  , "tieleft1"	, "tieleft1"	, "T"	, "l"	, FALSE	, "all 1 but rightmost 2"	, "function (n) c(rep(1, n - 1), 2)"
  , "tieleft2"	, "tieleft2"	, "T"	, "L"	, FALSE	, "all 2 but rightmost 1"	, "function (n) c(rep(2, n - 1), 1)"
  , "tieright1"	, "tieright1"	, "T"	, "r"	, FALSE	, "left 2 then all 1"	, "function (n) c(2, rep(1, n - 1))"
  , "tieright2"	, "tieright2"	, "T"	, "R"	, FALSE	, "left 1 then all 2"	, "function (n) c(1, rep(2, n - 1))"
  , "tieperm1"	, "tieperm1"	, "T"	, "p"	, FALSE	, "permutation of tied 1 and one 2"	, "function (n) sample(c(2, rep(1, n - 1)))"
  , "tieperm2"	, "tieperm2"	, "T"	, "P"	, FALSE	, "permutation of tied 2 and one 1"	, "function (n) sample(c(1, rep(2, n - 1)))"
  , "tieall"	, "tieall"	, "T"	, "T"	, FALSE	, "all 1"	, "function (n) rep(1, n)"
)


#ascchunk <- function(n) {b <- ceiling(sqrt(n));  k <- n %/% b; r <- n %% b; k <- if (r) c(rep(b,k), r) else rep(b,k); rep(seq(1, n, b), k) + runif(n, 0, b)}
#chunkasc <- function(n) {x <- runif(n, 0, n); Chunksort(x); x}

testdb <- as.data.frame(testdb)
testdb$func <- sapply(testdb$code, function(i)eval(parse(text = i)))
names(testdb$func) <- testdb$label
row.names(testdb) <- testdb$label

# work around early non-availability of current_keys()
evalOnLoad( parse(text = "
testdb <- cbind(testdb, as.data.frame(do.call('rbind', lapply(testdb$func, function(f) {
    n <- 1e4
    x <- current_keys(f(n))
    t <- tabulate(x,n)
    p <- t[t > 0]/n
    entropy <- sum(p*log2(p))
    maxentropy <- log2(1/n)
    c(
      entropy = entropy
    , relentropy = entropy/maxentropy
    , monotAscFrac = sum(diff(x) >= 0)/(n - 1)
    , strictAscFrac = sum(diff(x) > 0)/(n - 1)
    , spearman = if (length(p) == 1) NA else cor(seq_along(x), x, method = 'spearman')
    )
  }))))
") )


#' Algorithm database
#'
#' A dataset describing all algorithms in this package.
#'
#' @name algodb
#' @format A data frame with one row per algorithm and these columns:
#' \describe{
#'   \item{ name }{ full algorithm name }
#'   \item{ label }{ abbreviated algorithm label, labels of sorting Algorithms begin upper case, labels of selection algorithms begin lower case}
#'   \item{ major }{ single letter for major classification of the algorithms  }
#'   \item{ minor }{ single letter for minor classification of the algorithms  (unique within major) }
#'   \item{ color }{ a color to represent the algorithm }
#'   \item{ basic }{ the basic merge or partitioning }
#'   \item{ desc }{ a short description of the algorithm }
#'   \item{ expl }{ a longer explanation of the algorithm }
#'   \item{ class }{ a classification for the recursion, one of \code{S&M} Split&Merge, \code{P&P} Partition&Pool, \code{Divide&Conquer} Divide&Conquer which is neither \code{S&M} nor \code{P&P}, S&M (degenerated) Insertionsort is an extermely unbalanced special case of \code{S&M}}
#'   \item{ bufferclass }{ a classification of the buffer handling in the \code{S&M} recursion, one of \code{F&M} Fork&Merge (can be implemented concurrent) or Alternate&Merge \code{A&M} (only serial implementation possible)}
#'   \item{ task }{ the task the algorithm is performing, one of \code{select}  or \code{sort}}
#'   \item{ parallel }{ \code{TRUE} if algorithm is parallel, \code{FALSE} otherwise }
#'   \item{ varying }{ \code{TRUE} if the algorithm is implemented for elements of varying size (\code{\link{character}}) or \code{FALSE} if it is for fixed size (\code{\link{double}})}
#'   \item{ matrix }{ \code{TRUE} if the algorithm is implemented for sorting columns of integer matrices by keys in the first row }
#'   \item{ stable }{ \code{TRUE} if the algorithm is stable, \code{FALSE} otherwise}
#'   \item{ teststable }{ \code{TRUE} if the algorithm should be tested for stability, \code{FALSE} otherwise}
#'   \item{ buffer }{ some info about the buffer requirements  }
#'   \item{ adapt_ties }{ an attempt to classify adaptivity with regard to ties: one of 'no' (not adaptive), 'tuned' (explicitely tuned for adaptivity accepting a trade-off), 'hard' (algorithm is automaticaly adaptiv by design), 'soft' (algorithm is know to behave somewhat adaptive) }
#'   \item{ adapt_asc }{ an attempt to classify adaptivity with regard to presorting: one of 'no' (not adaptive), 'tuned' (explicitely tuned for adaptivity accepting a trade-off), 'hard' (algorithm is automatically adaptiv by design), 'soft' (algorithm is know to behave somewhat adaptive) }
#'   \item{ avoid_bm }{ an attempt to classify avoidance of branch-misprediction: one of 'no' (not avoiding), 'tuned' (explicitely tuned for avoiding accepting a trade-off), 'hard' (algorithm is automatically avoiding by design), 'soft' (algorithm is know to behave somewhat avoiding) }
#'   \item{ log }{ divide&conquer branching factor }
#'   \item{ balance }{ balance of the D&C, one of \code{balanced},  \code{skewed} or \code{random} }
#'   \item{ best }{ best case scaling }
#'   \item{ average }{average case scaling}
#'   \item{ worst }{worst case scaling}
#'   \item{ memory }{ type of memory access }
#'   \item{ recursion }{ type of recursion }
#'   \item{ symmasymm }{ type of symmetric recursion over which asymmetry }
#'   \item{ patent }{classification in the greeNsort patent, one of 'prior art', 'claimed', 'not claimed'}
#'   \item{ inventor }{name of inventor(s) where known}
#'   \item{ license }{licence where known}
#'   \item{ copyright }{copyright holder where known}
#'   \item{ source }{source of code, either 'greeNsort', or a different source}
#'   \item{ func }{the algorithm function}
#'   \item{ situation_exsitu }{\code{TRUE} if parameter \code{situation='exsitu'} is available, \code{FALSE} otherwise}
#'   \item{ situation_insitu }{\code{TRUE} if parameter \code{situation='insitu'} is available, \code{FALSE} otherwise}
#'   \item{ situation_default }{the default of parameter \code{situation}}
#'   \item{ method_pointer }{\code{TRUE} if parameter \code{method='pointer'} is available, \code{FALSE} otherwise}
#'   \item{ method_index }{\code{TRUE} if parameter \code{method='index'} is available, \code{FALSE} otherwise}
#'   \item{ method_default }{the default of parameter \code{method}}
#'   \item{ package }{which package, greeNsort or greeNsort.Rcpp}
#'   }
#'
#' @seealso \code{\link{testdb}} and \code{\link{vtestdb}}
#'
#' @examples
#'   \donttest{View(algodb)}
#'   table(algodb$class)
#'   table(algodb$task)
#'   table(algodb$varying)
#'   table(algodb$stable)
#'   unique(algodb$buffer)
#'   table(algodb$adapt_ties)
#'   table(algodb$adapt_asc)
#'   table(algodb$log)
#'   table(algodb$best)
#'   table(algodb$average)
#'   table(algodb$worst)
#'   table(algodb$patent)
#'   table(algodb$inventor)
#'   table(algodb$license)
#'   table(algodb$copyright)
#'   table(algodb$source)
#'
#' @export
"algodb"

algodb <- tibble::tribble(
  ~name	,~label	,~major	,~minor	,~color	,~basic	,~desc	,~expl	,~class	,~bufferclass	,~task	,~ parallel	,~ varying	,~ matrix	,~stable	,~teststable	,~buffer	,~adapt_ties	,~adapt_asc	,~avoid_bm	,~log	,~balance	,~best	,~average	,~worst	,~memory	,~recursion	,~symmasymm	,~patent	,~inventor	,~license	,~copyright	,~source	,~package
  ,"Selectionsort"	,"Sel"	,"I"	,"I"	,"black"	,"search&insert"	,"Selectionsort"	,"classic unstable implementation"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,FALSE	,FALSE	,TRUE	,"1"	,"no"	,"no"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"inplace scan"	,NA	,NA	,"prior art"	,"Friend (1956)"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Selectionsort2"	,"Sel2"	,"I"	,"I"	,"black"	,"search&insert"	,"Quadratic Selecting"	,"unstable implementation based on sqrt(N) slices, max is determined per slice, then max of slices gives global max. Following selects require only updating one slice."	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,FALSE	,FALSE	,TRUE	,"sqrt(N)"	,"no"	,"soft"	,"soft"	,NA	,"imbalanced"	,"2*sqrt(N)"	,"2*sqrt(N)"	,"2*sqrt(N)"	,"inplace scan"	,NA	,NA	,"prior art"	,"Friend (1956)"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Insertionsort"	,"Ins"	,"I"	,"I"	,"black"	,"search&insert"	,"Insertionsort"	,"classic stable implementation with sentinel"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"1"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"inplace scan (segmented)"	,NA	,NA	,"prior art"	,"Konrad Zuse (1945)"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"SInsertionsort"	,"SIns"	,"I"	,"S"	,"red"	,"search&insert"	,"stabilized Insertionsort"	,"stabilized Insertionsort (moves data and position vector, hence unlike Rinsertionsort avoids random access, useless on its own since Insertionsort is stable, but usefull within other stabilized algorithms)"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"1"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"scan (two vectors)"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"RInsertionsort"	,"RIns"	,"I"	,"R"	,"magenta"	,"search&insert"	,"indirect Insertionsort"	,"indirect Insertionsort (sorting of pointers to elements hence random access to elements, useful within other indirect algorithms)"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"1"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"pointer scan with random access"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"UInsertionsort"	,"UIns"	,"I"	,"U"	,"violet"	,"search&insert"	,"indirect insertion sort for variable elements"	,"indirect Insertionsort using pointers to size-varying elements (strings,stable)"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"m | m=max(n),N=sum(n)"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"pointer scan with random access"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"WInsertionsort"	,"WIns"	,"I"	,"W"	,"blue"	,"search&insert"	,"stabilized indirect insertion sort for variable elements"	,"indirect Insertionsort using pointers to size-varying elements (strings,stabilized,for use within other stabilized algorithms, useless on its own since Insertionsort is stable, just to measure the cost of stabilization)"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"m | m=max(n),N=sum(n)"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"pointer scan with random access"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"VInsertionsort"	,"VIns"	,"I"	,"V"	,"green"	,"search&insert"	,"greeeNsort: direct insertion sort for variable elements"	,"direct insertion sort of size-varying elements (strings,stable), truly inplace, note that this is slower  than RInsertionsort and SInsertionsort which require memory for pointers, note also that it is less sustainable than RInsertionsort and Sinsertionsort, but enables VFrogsort1."	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"m | m=max(n),N=sum(n)"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"inplace scan"	,NA	,NA	,"prior art"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"MInsertionsort"	,"MIns"	,"I"	,"I"	,"black"	,"search&insert"	,"like Insertionsort but for sorting of the colums of an 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"like Insertionsort but for sorting of the colums of an 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"1"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"inplace scan"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"NInsertionsort"	,"NIns"	,"I"	,"I"	,"black"	,"search&insert"	,"like MInsertionsort but sorting key+reference separately from the records, reordering the records in a final pass"	,"like MInsertionsort but sorting key+reference separately from the records, reordering the records in a final pass"	,"S&M (degenerated)"	,NA	,"sort"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"1"	,"no"	,"hard"	,"soft"	,NA	,"imbalanced"	,"N"	,"N\u00b2"	,"N\u00b2"	,"inplace scan (struct of two)"	,NA	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"approxMedian"	,"aMd"	,"M"	,"M"	,"black"	,NA	,"deterministic approximate median"	,"BFPRT Median of Medians: calculates Median for groups of five, then recurses"	,"D&C"	,NA	,"MEDIAN"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"N/5"	,"soft"	,"soft"	,"no"	,5	,"balanced"	,"N"	,"N"	,"N"	,"inplace zoom (insitu), back and forth (exsitu)"	,"standard"	,NA	,"prior art"	,"Jens Oehlschl\u00e4gel"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"BFPRTselect"	,"bfprt"	,"Q"	,"Q"	,"red"	,NA	,"deterministic exact median and other quantiles"	,"classic selection with approxMedian pivot (like Picksort but recursing only into one branch)"	,"P&P"	,NA	,"select"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N"	,"N"	,"N*log(N)"	,"inplace zoom"	,"single-side"	,NA	,"prior art"	,"Blum, M.; Floyd, R. W.; Pratt, V. R.; Rivest, R. L.; Tarjan, R. E. (1973), Time bounds for selection"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Pickselect"	,"pick"	,"Q"	,"Q"	,"red"	,NA	,"deterministic exact median and other quantiles"	,"classic selection with approxMedian pivot (like Picksort but recursing only into one branch)"	,"P&P"	,NA	,"select"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N"	,"N"	,"N*log(N)"	,"inplace zoom"	,"single-side"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quickselect"	,"quick"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic exact median and other quantiles"	,"classic selection with random pivot (like Quicksort2 but recursing only into one branch)"	,"P&P"	,NA	,"select"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"prior art"	,"Hoare (1961) Algorithm 65: Find"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zickselect"	,"zick"	,"Z"	,"Z"	,"blue"	,NA	,"greeNsort: first and last position of deterministic exact median and other quantiles"	,"DIET zig-zag selection with approxMedian pivot (like Zacksort but recursing only into one branch)"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"balanced"	,"N"	,"N"	,"N*log(N)"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zackselect"	,"zack"	,"z"	,"z"	,"green"	,NA	,"greeNsort: first and last position of probabilistic exact median and other quantiles"	,"DIET zig-zag selection with random pivot (like Zacksort but recursing only into one branch)"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zuckselect"	,"zuck"	,"z"	,"z"	,"green"	,NA	,"greeNsort: first and last position of probabilistic exact median and other quantiles"	,"DIET lazy zig-zag selection with random pivot (like Zucksort but recursing only into one branch)"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZuckselectB"	,"zuckB"	,"z"	,"z"	,"green"	,NA	,"like Zuckselect but tuned to reduce branch-misprediction"	,"like Zuckselect but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Duckselect"	,"duck"	,"z"	,"z"	,"green"	,NA	,"greeNsort: first and last position of probabilistic exact median and other quantiles"	,"POET lazy zig-zag selection with random pivot (like Zucksort but early termination on presorted data)"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DuckselectB"	,"duckB"	,"z"	,"z"	,"green"	,NA	,"like Duckselect but tuned to reduce branch-misprediction"	,"like Duckselect but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SELECT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"tuned"	,2	,"random"	,"N"	,"N"	,"N\u00b2"	,"inplace zoom"	,"single-side"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quickpartleft"	,"quickpl"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting at the left end derived from Quicksort2"	,"classic partial sorting from l..k with random pivot (derived from Quicksort2)"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N+M*log(M)"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"partial-side"	,NA	,"prior art"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quickpartright"	,"quickpr"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting at the right end derived from Quicksort2"	,"classic partial sorting from k..r with random pivot (derived from Quicksort2)"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"partial-side"	,NA	,"prior art"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zackpartleft"	,"zackpl"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting at the left end derived from Zacksort"	,"greeNsort partial sorting from l..k with random pivot (derived from Zacksort)"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"partial-side"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zackpartright"	,"zackpr"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting at the right end derived from Zacksort"	,"greeNsort partial sorting from l..k with random pivot (derived from Zacksort)"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"partial-side"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Dotnetpart"	,"dotnetpt"	,"q"	,"q"	,"magenta"	,NA	,"DotNet version of Quickpart (weird mixture of non-recursive and recursive, duplicated work, wrong loop conditions, deterministic pivot, no insertionsort tuning), here using random pivot, correct lopp condition but duplicated work and no insertionsort tuning"	,"The DotNet version of Partial Quicksort2 (weird mixture of non-recursive and recursive, deterministic pivot, no insertionsort tuning), here using random pivot but no insertionsort tuning"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Microsoft"	,"undefined"	,"ported to C by Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quickpart"	,"quickpt"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting in the middle derived from Quicksort2: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"probabilistic partial sorting in the middle derived from Quicksort2: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zackpart"	,"zackpt"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting in the middle derived from Zacksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"probabilistic partial sorting in the middle derived from Zacksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zuckpart"	,"zuckpt"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting in the middle derived from Zucksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"probabilistic partial sorting in the middle derived from Zucksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZuckpartB"	,"zuckptB"	,"q"	,"q"	,"magenta"	,NA	,"like Zuckpart but tuned to reduce branch-misprediction"	,"probabilistic partial sorting in the middle derived from Zucksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Duckpart"	,"duckpt"	,"q"	,"q"	,"magenta"	,NA	,"probabilistic partial sorting in the middle derived from Ducksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"probabilistic partial sorting in the middle derived from Ducksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DuckpartB"	,"duckptB"	,"q"	,"q"	,"magenta"	,NA	,"like Duckpart but tuned to reduce branch-misprediction"	,"probabilistic partial sorting in the middle derived from Ducksort: sorts l <= lpart <= rpart <= r such that x[i < lpart] <= x[lpart] and x[lpart..rpart] is sorted and x[rpart] <= x[rpart < i]"	,"P&P"	,NA	,"partial"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"tuned"	,"tuned"	,2	,"random"	,"N"	,"N+M*log(M)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"NOT: Chambers (1970) Algorithm 410: Partial Sorting"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Lomutosort"	,"Lomuto"	,"q"	,"q"	,"red"	,"s-partitioning"	,"Binary probabilistic quicksort by Bentley using Lomuto-partitioning (random pivot, no early termination, several errors??)"	,"Binary probabilistic quicksort by Bentley using Lomuto-partitioning (random pivot, no early termination, several errors??)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Nico Lomuto ()"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quicksort1"	,"Quick1"	,"q"	,"q"	,"red"	,"s-partitioning"	,"Binary probabilistic quicksort with pointer movment by Hoare 1961 (random pivot, early termination but vulnerable)"	,"Binary probabilistic quicksort with pointer movment by Hoare 1961 (random pivot, early termination but vulnerable)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Hoare (1961) Algorithm 64: Quicksort"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quicksort2"	,"Quick2"	,"q"	,"q"	,"red"	,"s-partitioning"	,"Binary probabilistic quicksort with pointer movment by Hoare 1961, tie-handling by  Singleton 1969 as recommended by Sedgewick 1977 (random pivot, swapping ties, no early terminating on ties)"	,"Binary probabilistic quicksort with pointer movment by Hoare 1961, tie-handling by  Singleton 1969 as recommended by Sedgewick 1977 (random pivot, swapping ties, no early terminating on ties)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Hoare (1961) Algorithm 64: Quicksort"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Quicksort3"	,"Quick3"	,"T"	,"T"	,"red"	,"t-partitioning"	,"Threeway quicksort by Wegner 1985 improved and popularized by Bentley&McIlroy 1993, early terminating on ties by isolating a third tie partition (but slower than Quicksort2)"	,"Threeway quicksort by Wegner 1985 improved and popularized by Bentley&McIlroy 1993, early terminating on ties by isolating a third tie partition (but slower than Quicksort2)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"tuned"	,"soft"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	, "Bentley, Jon L. and McIlroy, M. Douglas, 1993, Engineering a Sort Function"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Dupisort"	,"Dupi"	,"D"	,"D"	,"red"	,"3-partitioning"	,"Dual pivot quicksort by Yaroslavskyi 2009  but modified with random pivot, this creates three true partitions, the clever loop is very serial, difficult to parallelize, difficult to tune to reduce branch-mispredictions"	,"Dual pivot quicksort by Yaroslavskyi 2009  but modified with random pivot, this creates three true partitions, the clever loop is very serial, difficult to parallelize, difficult to tune to reduce branch-mispredictions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,3	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Vladimir Yaroslavskiy, 2009"	,"undefined"	,"(c) Vladimir Yaroslavskiy with Random Pivots by Jens Oehlschl\u00e4gel"	,"Vladimir Yaroslavskiy, 2009, Dual-Pivot Quicksort algorithm"	,"greeNsort"
  ,"Tricksort"	,"Trick"	,"t"	,"t"	,"red"	,"3-partitioning"	,"improved: like Dupisort but redundant operations removed"	,"improved: like Dupisort but redundant operations removed"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,3	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"not claimed"	,"Vladimir Yaroslavskiy, 2009, Dual-Pivot Quicksort algorithm"	,"greeNsort"	,"(c) Vladimir Yaroslavskiy with Random Pivots and Optimization by Jens Oehlschl\u00e4gel"	,"Vladimir Yaroslavskiy, 2009, Dual-Pivot Quicksort algorithm"	,"greeNsort"
  ,"Quicksort2B"	,"Quick2B"	,"q"	,"q"	,"red"	,"s-partitioning"	,"Block-quicksort, like Quicksort2 but tuned to reduce branch-misprediction and without their primitive early termination"	,"Block-quicksort following Edelkamp&Wei\u00df  to reduce branch-misprediction but without their primitive early termination"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	, "Stefan Edelkampand Armin Wei\u00df, 2016"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Plugsort"	,"Plug"	,"P"	,"P"	,"magenta"	,"s-partitioning"	,"stupid quicksort which plugs-in Pickselect for pivot, then partitions, then recurses"	,"quicksort which plugs-in pickselect for pivot determination (for didactic reasons only, shows how modularity can kill performance)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Pugsort"	,"Pug"	,"p"	,"p"	,"magenta"	,"s-partitioning"	,"stupid quicksort which plugs-in Pickselect for pivot, skips partitioning, then recurses"	,"sorting by calling pickselect (for didactic reasons only, shows how modularity can kill performance)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Slicksort"	,"Slick"	,"S"	,"S"	,"magenta"	,"s-partitioning"	,"stupid quicksort which plugs-in Quickselect for pivot, then partitions, then recurses"	,"quicksort which plugs-in quickselect for pivot determination (for didactic reasons only, shows how modularity can kill performance)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Sicksort"	,"Sick"	,"s"	,"s"	,"magenta"	,"s-partitioning"	,"stupid quicksort which plugs-in Quickselect for pivot, skips partitioning, then recurses"	,"sorting by calling quickselect (for didactic reasons only, shows how modularity can kill performance)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"BFPRTsort"	,"BFPRT"	,"Q"	,"Q"	,"red"	,"s-partitioning"	,"deterministic quicksort (like Quicksort2 but with BFPRT Median pivot)"	,"deterministic quicksort (like Quicksort2 but with approxMedian pivot)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Picksort"	,"Pick"	,"Q"	,"Q"	,"red"	,"s-partitioning"	,"deterministic quicksort (like Quicksort2 but with approxMedian pivot)"	,"deterministic quicksort (like Quicksort2 but with approxMedian pivot)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"RQuicksort2"	,"RQuick2"	,"R"	,"r"	,"red"	,"s-partitioning"	,"indirect stabilized quick sort (pointer\\% buffer)"	,"indirect stable quicksort (sorting of pointers to elements hence random access to elements, stabilized, no ties by definition, hence RZacksort would be overkill)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"pointer zoom with random access"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"RQuicksort2B"	,"RQuick2B"	,"R"	,"R"	,"red"	,"s-partitioning"	,"like RQuicksort2 but tuned to reduce branch-misprediction (pointer\\% buffer)"	,"RQuicksort2 tuned to reduce branch-misprediction (harms adaptivity)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"log(N)"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"pointer zoom with random access"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"SQuicksort2"	,"SQuick2"	,"Q"	,"q"	,"red"	,"s-partitioning"	,"direct stabilized quick sort (index\\% buffer)"	,"stabilized quicksort (stabilized with an extra position vector, no ties by definition, hence RZacksort would be overkill)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom (two vectors)"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"SQuicksort2B"	,"SQuick2B"	,"Q"	,"Q"	,"red"	,"s-partitioning"	,"like SQuicksort2 but tuned to reduce branch-misprediction (index\\% buffer)"	,"SQuicksort2 tuned to reduce branch-misprediction (harms adaptivity)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"log(N)"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom (two vectors)"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Chicksort"	,"Chick"	,"c"	,"c"	,"orange"	,"s-partitioning"	,"greeNsort: probabilistic quicksort that avoids swaping pivot-ties (unlike Singleton, on average O(N*log(N)) for all tied pattern, however, no early termination and slower than Quicksort2)"	,"quicksort avoiding tie-swaps and imbalance on ties by symmetric search for swap element in main loop (random pivot), however it lacks early-termiatin on ties and is slower in the von Neumann architecture (unless the machine has special CPU instructions for parallel symmetric movement and comparision)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zocksort"	,"Zock"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: probabilistic quicksort wit lean early termination and no overhead (DIET, expecetd O(N*log(N)) for many tied patterns, early termination for many tied patterns)"	,"quicksort with asymmetric partitioning and early terminating on ties with search for non-pivot in pre-loop and danger of degenerated behavior (random pivot), (see examples), 'zocken' is a German verb vor risky betting"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zacksort"	,"Zack"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: probabilistic quicksort wit lean early termination and no overhead (DIET, expecetd O(N*log(N)) for all tied patterns, early termination for all tied patterns)"	,"symmetric quicksort with asymmetric partitioning and early terminating on ties with search for non-pivot in pre-loop (random pivot), 'zack' is a German word with double meaning, it's 'zag' as in zigzag and it means 'quick'"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZacksortB"	,"ZackB"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: like Zacksort but tuned to reduce branch-misprediction"	,"like Zacksort but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zucksort"	,"Zuck"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: a variation of Zacksort"	,"lazy zig-zag , like Zacksort but only switching to other pivot-asymmetry  for the branch at risk, i.e. the branch potentially containing the dangerous pivot-ties, 'zuck' is a german word meaning 'twitch'"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZucksortA"	,"ZuckA"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: like Zucksort but simple-tuned to early-terminate on presorting before entering recursion"	,"like Zucksort but tuned with a single POET-loop to early-terminate on presorting before the recursion"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZucksortB"	,"ZuckB"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: like Zucksort but tuned to reduce branch-misprediction"	,"like Zucksort but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"ZucksortD"	,"ZuckD"	,"z"	,"z"	,"blue"	,"a-partitioning"	,"greeNsort: Zucksort with deterministic pivots"	,"lazy zig-zag , like Zucksort but with deterministic pivots"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"symmetric"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Ducksort"	,"Duck"	,"d"	,"d"	,"blue"	,"a-partitioning"	,"greeNsort: modification of Zucksort with Presorted Order Early Termination (POET)"	,"greeNsort: modification of Zucksort with Presorted Order Early Termination (POET)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DucksortB"	,"DuckB"	,"d"	,"d"	,"blue"	,"a-partitioning"	,"greeNsort: like Ducksort but tuned to reduce branch-misprediction"	,"greeNsort: like Ducksort but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"tuned"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Zicksort"	,"Zick"	,"Z"	,"Z"	,"blue"	,"a-partitioning"	,"greeNsort: like Zacksort but deterministic (with approxMedian pivot)"	,"Zacksort with approxMedian as pivot, O(N*log(N)) worst-case but slower"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Copysort"	,"Copy"	,"Y"	,"Y"	,"black"	,"c-merging"	,"like Knuthsort but without the 'nocopy' optimization (2 move passes, N elements buffer)"	,"mergesort coyping forth and merging back with Knuths merge (single loop check)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"back and forth"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Nocosort"	,"Noco"	,"y"	,"y"	,"black"	,"p-merging"	,"Sedgewicks 'nocopy' merge sort, like Knuthsort but with 3 loop checks (N elements buffer)"	,"ping-pong mergesort (avoiding copy-pass per merge by switching roles of data and buffer memory, but with triple loop check, as teached by Sedgewick)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Simplsort"	,"Simpl"	,"l"	,"l"	,"black"	,"p-merging"	,"simple mergesort with 1 move pass and 2 loop checks (N elements buffer)"	,"Simple ping-pong mergesort with double loop check and inline-comparison, note that in spite of the double loop check this can be faster than Knuthsort because it is easier compiled to instructions that reduce branch-misprediction"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"tuned"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Bimesort"	,"Bime"	,"B"	,"b"	,"black"	,"i-merging"	,"Sedgewicks 'bitonic' mergesort with single move pass, sentinel and single loop check (N elements buffer) but stable"	,"Sedgewicks 'bitonic' mergesort (left branch ascending, right branch descending) modified to be stable (left branch ascending, right branch reverse-ascending)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"symmetric"	,"'flip-recursion' over 'order asymmetry'"	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"BimesortB"	,"BimeB"	,"B"	,"B"	,"black"	,"i-merging"	,"like Bimesort but tuned to reduce branch-misprediction (N elements buffer)"	,"Bimesort with tuning against branch-misprediction (harms adaptivity)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"tuned"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"symmetric"	,"'flip-recursion' over 'order asymmetry'"	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Knuthsort"	,"Knuth"	,"M"	,"2"	,"black"	,"p-merging"	,"reference mergesort with single move pass and single loop check (N elements buffer) like Nocosort but with Knuths merge"	,"ping-pong mergesort with single loop check (Knuths merge)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"KnuthsortA"	,"KnuthA"	,"M"	,"2"	,"black"	,"p-merging"	,"like Knuthsort but tuned to adapt for presorted data (N elements buffer)"	,"parallel Knuthsort tuned for non-overlap  (the non-overlap check skips for presorted data over comparing and directly copies)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Katasort"	,"Kata"	,"K"	,"2"	,"brown"	,"p-merging"	,"mergesort tuned for half loop check (N elements buffer)"	,"ping-pong mergesort tuned for half loop check (Katajainens merge)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"KatasortA"	,"KataA"	,"K"	,"2"	,"brown"	,"p-merging"	,"like Katasort but tuned to adapt for presorted data (N elements buffer)"	,"Katasort tuned for non-overlap  (the non-overlap check skips for presorted data over comparing and directly copies)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"KatasortB"	,"KataB"	,"K"	,"2"	,"brown"	,"p-merging"	,"like Katasort but tuned to reduce branch-misprediction (N elements buffer)"	,"Katasort tuned to reduce branch-misprediction (harms adaptivity)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"tuned"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Knuth3sort"	,"Knuth3"	,"M"	,"3"	,"black"	,"p-merging"	,"3-ary mergesort using 1 move pass and Knuths merge with 1 loop check (N elements buffer)"	,"3-ary ping-pong mergesort using Knuths merge with one loop check"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,3	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"Jens Oehlschl\u00e4gel, trivial combinations of prior art"	,"undefined"	,"(c) Katajainen 1997 with Modifications by Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Knuth4sort"	,"Knuth4"	,"M"	,"4"	,"black"	,"p-merging"	,"4-ary mergesort using 1 move pass and Knuths merge with 1 loop check (N elements buffer)"	,"4-ary ping-pong mergesort using Knuths merge with one loop check"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,4	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"Jens Oehlschl\u00e4gel, trivial combinations of prior art"	,"undefined"	,"(c) Katajainen 1997 with Modifications by Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Kata3sort"	,"Kata3"	,"K"	,"3"	,"brown"	,"p-merging"	,"3-ary mergesort using 1 move pass and Katajainens tuned merge with a half loop check (N elements buffer)"	,"3-ary ping-pong mergesort using Katajainens tuned merge with a half loop check"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,3	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"Katajainen (1997) A meticulous analysis of mergesort programs"	,"undefined"	,"(c) Katajainen 1997 with Modifications by Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Kata4sort"	,"Kata4"	,"K"	,"4"	,"brown"	,"p-merging"	,"4-ary mergesort using 1 move pass and Katajainens tuned merge with a half loop check (N elements buffer)"	,"4-ary ping-pong mergesort using Katajainens tuned merge with a half loop check"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,4	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"Katajainen (1997) A meticulous analysis of mergesort programs"	,"undefined"	,"(c) Katajainen 1997 with Modifications by Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"GKnuthsort"	,"GKnuth"	,"M"	,"g"	,"grey70"	,"g-merging"	,"greeNsort: like Knuthsort but reducing distance with g-merging (only equally sized elements, N elements buffer)"	,"gapped ping-pong mergesort with single loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"gapped"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"TKnuthsort"	,"TKnuth"	,"K"	,"K"	,"black"	,"r-merging"	,"greeNsort: like Knuthsort but reducing distance with t-partitioning (N elements buffer)"	,"distance-reducing mergesort with relocation of both input streams and Knuths merge with single loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Crocosort"	,"Croco"	,"C"	,"2"	,"darkolivegreen4"	,"r-merging"	,"greeNsort: like Knuthsort but reducing distance with r-partitioning (less moves, N elements buffer)"	,"distance-reducing mergesort with relocation of one input streams, semi-inplace merge and Knuths single loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Croco3sort"	,"Croco3"	,"C"	,"3"	,"darkolivegreen4"	,"r-merging"	,"greeNsort: 3-ary Crocosort"	,"3-ary crocosort using partial inplace-merge and Knuths single loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,3	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Croco4sort"	,"Croco4"	,"C"	,"4"	,"darkolivegreen4"	,"r-merging"	,"greeNsort: 4-ary Crocosort"	,"4-ary crocosort using partial inplace-merge and Knuths single loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,4	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Kroco3sort"	,"Kroco3"	,"c"	,"3"	,"dodgerblue1"	,"r-merging"	,"greeNsort: like Croco3sort but tuned with Katajainens merge"	,"3-ary crocosort using partial inplace-merge and Katajainens tuned half loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,3	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Kroco4sort"	,"Kroco4"	,"c"	,"4"	,"dodgerblue1"	,"r-merging"	,"greeNsort: like Croco4sort but tuned with Katajainens merge"	,"4-ary crocosort using partial inplace-merge and Katajainens tuned half loop check"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,4	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"standard"	,NA	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Ininsort"	,"Inin"	,"N"	,"I"	,"grey90"	,"a-merging"	,"Langs merge sort with one move pass and 2 loop checks (N/2 elements buffer)"	,"asymmetric semi-inplace ping-pong mergesort' using only N/2 elements buffer with double loop check (Lang)"	,"S&M"	,"A&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"asymmetric"	,NA	,"prior art"	," Thomas Kloecker (2008) according to H.W. Lang"	,"undefined"	,"(c) 2003-2010 Hans-Werner Lang"	, "www.iti.fh-flensburg.de/lang/algorithmen/sortieren/merge/mergef.htm"	,"greeNsort"
  ,"Ninisort"	,"Nini"	,"N"	,"N"	,"grey90"	,"a-merging"	,"like Ininsort but with 1 loop check like Knuth (N/2 elements buffer)"	,"Langs Ininsort but rather with Knuths merge and hence one instead of two loop checks"	,"S&M"	,"A&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"asymmetric"	,NA	,"prior art"	," Thomas Kloecker (2008) according to H.W. Lang"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort0"	,"Frog0"	,"F"	,"0"	,"springgreen4"	,"s-merging"	,"greeNsort: asymmetric root version of Frogsort (adaptive to ascending (N/2 elements buffer)"	,"Frogsort (asymmetrically adaptive) initializing and then splitting triplets of two data and one buffer element"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Geckosort0"	,"Gecko0"	,"G"	,"0"	,"green"	,"s-merging"	,"greeNsort: symmetric root version of Geckosort (adaptive to ascending and descending, (N/2 elements buffer)"	,"Geckosort (almost symmetrically adaptive:  to ordered and strictly reverse-ordered) initializing and then splitting triplets of two data and one buffer element"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry' and 'flip-recursion' over 'order asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Geckosort1"	,"Gecko1"	,"G"	,"1"	,"green"	,"s-merging"	,"greeNsort: symmetric general version of Geckosort (adaptive to ascending and descending, (N/2 elements buffer)"	,"Geckosort (almost symmetrically adaptive:  to ordered and strictly reverse-ordered) with balanced F&M of data and N/2 buffer"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry' and 'flip-recursion' over 'order asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort1"	,"Frog1"	,"F"	,"1"	,"springgreen4"	,"s-merging"	,"greeNsort: asymmetric general version of Frogsort (adaptive to ascending, (N/2 elements buffer)"	,"Frogsort (asymmetrically adaptive) with balanced F&M of data and N/2 buffer"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort1A"	,"Frog1A"	,"F"	,"1"	,"springgreen4"	,"s-merging"	,"greeNsort: like Frogsort1 but tuned to adapt for presorted data (N/2 elements buffer)"	,"Frogsort with balanced F&M of data and N/2 buffer tuned with non-overlap-check for adaptivity"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort1B"	,"Frog1B"	,"F"	,"1"	,"springgreen4"	,"s-merging"	,"greeNsort: like Frogsort1 but tuned to reduce branch-misprediction (N/2 elements buffer)"	,"Frogsort with balanced F&M of data and N/2 buffer tuned to reduce branch-misprediction"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"tuned"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort2"	,"Frog2"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"greeNsort: p2-parametrized Frogsort with p2*N buffer (difficult branch-parallel, default N/8 elements buffer)"	,"Frogsort with imbalanced F&M of data and less than N/2 buffer (proportional split of buffer reduces memory and branch-misprediction, switch to Frogsort1 as soon as possible)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort2A"	,"Frog2A"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"greeNsort: like Frogsort2 but tuned to adapt for presorted data (N/2 elements buffer)"	,"Frogsort with imbalanced F&M of data and less than N/2 buffer tuned with non-overlap-check for adaptivity"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"tuned"	,"no"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort2B"	,"Frog2B"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"greeNsort: like Frogsort2 but tuned to reduce branch-mispredictions (difficult branch-parallel, default N/7 elements buffer)"	,"Frogsort with imbalanced F&M of data and less than N/2 buffer explicitely tuned to reduce branch-misprediction although Frogsort2 already reduces them (proportional split of buffer, switch to Frogsort1 as soon as possible)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft+tuned"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort3"	,"Frog3"	,"F"	,"3"	,"springgreen4"	,"s-merging"	,"greeNsort: p3-parametrized Frogsort with p3*N buffer (reduced branch-parallel, default N/8 elements buffer)"	,"Frogsort with imbalanced A&M of data and less than N/2 buffer (serially alternating buffer, switch to Frogsort1 as soon as possible)"	,"S&M"	,"A&M then F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort3B"	,"Frog3B"	,"F"	,"3"	,"springgreen4"	,"s-merging"	,"greeNsort: like Frogsort3 but tuned to reduce branch-mispredictions (reduced branch-parallel, default N/8 elements buffer)"	,"Frogsort with imbalanced A&M of data and less than N/2 buffer explicitely tuned to reduce branch-misprediction (serially alternating buffer, switch to Frogsort1 as soon as possible)"	,"S&M"	,"A&M then F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft+tuned"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort6"	,"Frog6"	,"F"	,"6"	,"springgreen4"	,"s-merging"	,"greeNsort: doubly parametrized Frogsort covering Frogsort1,2,3"	,"Starts S&M like Frogsort3 but switches to Frogsort2 as soon as possible"	,"S&M"	,"A&M then F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort4"	,"Frog4"	,"F"	,"4"	,"springgreen4"	,"s-merging"	,"greeNsort: bottom-up Frogsort (initially sqrt(N) elements buffer, then N/2 elements buffer)"	,"Frogsort balanced bottom-up sorting blocks of size (for example sqrt(N) with sqrt(N)/2 buffer) and then symmetrically merging them with N/2 buffer"	,"S&M"	,"A then F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"a*sqrt(N)+(1-a)*(N/2)"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"scan, then symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Frogsort5"	,"Frog5"	,"F"	,"5"	,"springgreen4"	,"s-merging"	,"greeNsort: bottom-up Frogsort (initially optimal buffer, then N/2 elements buffer)"	,"Frogsort balanced bottom-up sorting (bigger) blocks of optimal size B with B/2 buffer and then symmetrically merging them (delayed) with N/2 buffer"	,"S&M"	,"A then F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"a*sqrt(N)+(1-a)*(N/2)"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"scan, then symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Omitsort"	,"Omit"	,"O"	,"1"	,"grey70"	,"o-merging"	,"greeNsort: ping-pong mergesort' with single loop check tuned for less initial copying and O(N) adaptiveness to presorting"	,"like Knuthsort but tuned for less initial copying and for non-overlap (no initial copying upfront, the non-overlap check skips for presorted data over merging and hence then has zero copy passes)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Octosort"	,"Octo"	,"M"	,"g"	,"grey70"	,"o-merging"	,"greeNsort: ping-pong mergesort' with single loop check tuned for less initial copying and O(N) adaptiveness to presorting and reverse-presorting"	,"like Omitsort but symmetrically adapting to ascending and reverse-ascending by lazy direction enforcement (data-driven sorting)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Squidsort1"	,"Squid1"	,"O"	,"1"	,"green"	,"s-merging"	,"greeNsort: like Frogsort1 but tuned to adapt for presorted and reverse-sorted data (N/2 elements buffer)"	,"like Frogsort1 but with non-overlap-check for adaptivity and lazy direction enforcement (data-driven sorting)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry' and 'flip-recursion' over 'order asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Squidsort2"	,"Squid2"	,"O"	,"1"	,"green"	,"s-merging"	,"greeNsort: like Frogsort2 but tuned to adapt for presorted and reverse-sorted data (N/2 elements buffer)"	,"like Frogsort2 but with non-overlap-check for adaptivity and lazy direction enforcement (data-driven sorting)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry' and 'flip-recursion' over 'order asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"IMergesort"	,"IMerge"	,"i"	,"i"	,"palevioletred1"	,NA	,"classic inplace-mergesort (no buffer)"	,"classic inplace-mergesort (not competitive)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"1"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"??"	,"??"	,"N*log(N)^2"	,"inplace NOS"	,"standard?"	,NA	,"prior art"	,"unknown"	,"MIT"	,"(c) 2013 Andrey Astrelin (with modifications of Jens Oehlschl\u00e4gel)"	,"https://github.com/Mrrl/GrailSort"	,"greeNsort"
  ,"Grailsort"	,"Grail"	,"h"	,"h"	,"lightskyblue1"	,NA	,"Astrelins inplace Grailsort (no buffer)"	,"the holy grail of sorting: mergesort with no buffer (Astrelin)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"??"	,"??"	,"N*log(N)"	,"inplace NOS"	,"standard?"	,NA	,"prior art"	,"B-C. Huang and M. A. Langston, Fast Stable Merging and Sorting in Constant Extra Space (1989-1992)"	,"MIT"	,"(c) 2013 Andrey Astrelin (with modifications of Jens Oehlschl\u00e4gel)"	,"https://github.com/Mrrl/GrailSort"	,"greeNsort"
  ,"Grailsqrt"	,"Grailsqrt"	,"H"	,"H"	,"lightskyblue2"	,NA	,"Astrelins quasi-inplace Grailsort (sqrt(N) elements buffer)"	,"a faster version of Grailsort with sqrt(N) elements buffer (Astrelin)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"sqrt(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"??"	,"??"	,"N*log(N)"	,"inplace NOS"	,"standard?"	,NA	,"prior art"	,"B-C. Huang and M. A. Langston, Fast Stable Merging and Sorting in Constant Extra Space (1989-1992)"	,"MIT"	,"(c) 2013 Andrey Astrelin (with modifications of Jens Oehlschl\u00e4gel)"	,"https://github.com/Mrrl/GrailSort"	,"greeNsort"
  ,"Sqrtsort"	,"Sqrt"	,"S"	,"S"	,"lightskyblue3"	,NA	,"Astrelins quasi-inplace Sqrtsort (sqrt(N) elements buffer)"	,"yet another mergesort with sqrt(N) elements buffer (Astrelin)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"sqrt(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"??"	,"??"	,"N*log(N)"	,"inplace NOS"	,"standard?"	,NA	,"prior art"	,"B-C. Huang and M. A. Langston, Fast Stable Merging and Sorting in Constant Extra Space (1989-1992)"	,"MIT"	,"(c) 2014 Andrey Astrelin (with modifications of Jens Oehlschl\u00e4gel)"	,"https://github.com/Mrrl/SqrtSort"	,"greeNsort"
  ,"Walksort"	,"Walk"	,"w"	,"w"	,"cyan"	,"w-merging"	,"greeNsort: quasi-inplace mergesort with sqrt(N) elements buffer"	,"block-organized mergesort using only two blocks of sqrt(N) buffer per thread, data are virtually divided into sqrt(N) blocks of size sqrt(N), 2*p blocks are allocated for buffer, an array of sqrt(N)+2p pointers to buffers are allocated and initialized, two ping-pong arrays of sqrt(N)+2p integer indices into the pointer array are allocated and initialized, then S&M proceeds reading from ocupied blocks and writing to empty blocks; once S&M is completed the data are sorted but not in contiguous blocks, a final brings blocks into the correct order, which is already known by index information"	,"S&M"	,"(multi) A&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"2*B | B <= sqrt(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"block"	,"standard"	,NA	,"not claimed"	,"Denham Coates-Evelyn (2000) Optimum In-Place Merging"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Jumpsort"	,"Jump"	,"j"	,"j"	,"cyan"	,"w-merging"	,"greeNsort: distance reducing quasi-inplace mergesort with sqrt(N) elements buffer"	,"like Walksort but with extra relocation moves during S&M to reduce distance between merged data and buffer blocks"	,"S&M"	,"(multi) A&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"2*B | B <= sqrt(N)"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"block"	,"standard"	,NA	,"not claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietACPsort"	,"ACP"	,"O"	,"P"	,"black"	,"a-partitioning"	,"greeNsort: stable deterministic partitioning with approxMedian pivot and DIET in 3 phases: (A)pprox, (C)ounting, (P)artitioning (N elements buffer)"	,"stable binary ping-pong partition&pool symmetric DIET sorting using approxMedian as pivot,Counting pass,Partitioning Pass alternating between two distant memory regions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietCPsort"	,"CP"	,"O"	,"p"	,"black"	,"a-partitioning"	,"greeNsort: stable deterministic partitioning with random pivotand and DIET in 2 phases: (C)ounting, (P)artitioning (N elements buffer)"	,"like DietACPsort but with random pivot (sacrifycing O(N*log(N))) worst-case for greater speed"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"ping-pong"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietCPsortB"	,"CPB"	,"O"	,"B"	,"black"	,"a-partitioning"	,"greeNsort: like DietCPsort but tuned to reduce branch-misprediction (N elements buffer)"	,"like DietCPsort but tuned for avoiding branch-(mis)predictions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"relocation"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietcP2sort"	,"cP"	,"O"	,"p"	,"black"	,"a-partitioning"	,"greeNsort: like DietCPsort but with the enhanced call-structure of DietPcsort (N elements buffer)"	,"like DietCPsort but passes pivots and counts into the recursive call (preparation for DietPc)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"ping-pong"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietPcsort"	,"Pc"	,"O"	,"c"	,"black"	,"a-partitioning"	,"greeNsort: stable deterministic partitioning with random pivotand and DIET in 1 phase: (P)artitioning with (c)ounting of next partitioining (slower, N elements buffer)"	,"like DietcP2sort but tries to count the next partition size already during the previous partitioning"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"ping-pong"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietACPTsort"	,"ACPT"	,"O"	,"R"	,"black"	,"a-partitioning"	,"greeNsort: like DietACPsort but distance-reducing with t-partitioning (N elements buffer)"	,"stable binary distance-reducing partition&pool symmetric DIET sorting using Approx-median as pivot,Counting pass,Partitioning Pass and transport moves for buffer partition&pool within one memory region"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"relocation"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"DietCPTsort"	,"CPT"	,"O"	,"r"	,"black"	,"a-partitioning"	,"greeNsort: like DietCPsort but distance-reducing with t-partitioning (N elements buffer)"	,"like DietACPTsort but with random pivot (sacrifycing O(N*log(N))) worst-case but faster"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"relocation"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Kiwisort"	,"Kiwi"	,"K"	,"K"	,"grey50"	,"w-partitioning"	,"greeNsort: stable deterministic partitioning with random pivotand and DIET in 1 phase: (P)artitioning without counting (fastest, N elements buffer)"	,"stable binary ping-pong partition&pool symmetric DIET sorting using random pivots, wing-partitioning avoiding counting and avoiding pass transport moves alternating between two distant memory regions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"ping-pong"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"KiwisortA"	,"KiwiA"	,"K"	,"K"	,"grey50"	,"w-partitioning"	,"greeNsort: like Kiwisort but with tuned for presorted data with POET loop instead of DIET loop (N elements buffer)"	,"like Kiwisort but tuned for avoiding branch-(mis)predictions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"KiwisortB"	,"KiwiB"	,"K"	,"K"	,"grey50"	,"w-partitioning"	,"greeNsort: like Kiwisort but tuned to reduce branch-misprediction (N elements buffer)"	,"like Kiwisort but tuned for avoiding branch-(mis)predictions"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Swansort"	,"Swan"	,"S"	,"S"	,"grey30"	,"w-partitioning"	,"greeNsort: distance-reducing stable deterministic partitioning with random pivot and DIET: (P)artitioning without counting and less moves (N elements buffer)"	,"stable binary distance-reducing partition&pool symmetric DIET sorting using random pivots, wing-partitioning avoiding counting and relocation moves for buffer partition&pool within one memory region (insitu costs of one approx-median avoided by top-level merging)"	,"P&P"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry' and 'flip-recursion' over 'order-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"SwansortB"	,"SwanB"	,"S"	,"S"	,"grey30"	,"w-partitioning"	,"greeNsort: like Swansort but tuned to reduce branch-misprediction (N elements buffer)"	,"like Swansort but tuned for avoiding branch-(mis)predictions"	,"P&P"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry' and 'flip-recursion' over 'order-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Storksort"	,"Stork"	,"S"	,"S"	,"grey30"	,"w-partitioning"	,"greeNsort: distance-reducing stable deterministic partitioning with random pivotand and DIET: (P)artitioning with counting and less moves (only exsitu, N/2 elements buffer)"	,"like Swansort but with buffer reduced from N to N/2, an extra counting pass allows the smaller partition to the other side of wing-partitioning. Storksort differs from the other sorting algorithms in beeing a pure 'exsitu' sort: it does not complete sorting in the allocated RAM but 'sends' the elements at the leaf of recursion into the appropriate locations. The 'insitu' method here partitions the input vector into several stable leafs, but skips the final sending (didactic purpose only)."	,"P&P"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"hard"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry' and 'flip-recursion' over 'order-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"StorksortB"	,"StorkB"	,"S"	,"S"	,"grey30"	,"w-partitioning"	,"greeNsort: like Storksort but tuned to reduce branch-misprediction (N elements buffer)"	,"like Storksort but tuned for avoiding branch-(mis)predictions"	,"P&P"	,"F&M"	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"symmetric"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry' and 'flip-recursion' over 'order-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"UKnuthsort"	,"UKnuth"	,"z"	,"u"	,"red"	,"p-merging"	,"greeNsort: stable indirect Knuthsort for size-varying elements (2*N pointers buffer)"	,"indirect Knuthsort using pointers to size-varying elements (strings, stable)"	,"P&P"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"16N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"M*log(M)"	,"M*log(M)"	,"M*log(M)"	,"ping-pong"	,"standard"	,NA	,"not claimed"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"UZacksort"	,"UZack"	,"z"	,"u"	,"red"	,"a-partitioning"	,"greeNsort: unstable indirect Zacksort for size-varying elements (N pointers buffer)"	,"indirect Zacksort using pointers to size-varying elements (strings, not stable)"	,"P&P"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,FALSE	,FALSE	,"8N"	,"hard"	,"soft"	,"no"	,2	,"random"	,"M*log(M)"	,"M*log(M)"	,"N\u00b2"	,"pointer zoom with random access"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"UZacksortB"	,"UZackB"	,"z"	,"U"	,"red"	,"a-partitioning"	,"greeNsort: like UZacksort but tuned to reduce branch-misprediction"	,"indirect Zacksort using pointers to size-varying elements (strings,not stable) with tuning to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,FALSE	,FALSE	,"8N"	,"hard"	,"soft"	,"tuned"	,2	,"random"	,"M*log(M)"	,"M*log(M)"	,"N\u00b2"	,"pointer zoom with random access"	,"symmetric"	,"'zig-zag-recursion' over 'pivot-asymmetry' and 'toggle-recursion' over 'tie-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"WQuicksort2"	,"WQuick2"	,"w"	,"w"	,"violett"	,"s-partitioning"	,"stabilized indirect Quicksort for size-varying elements (N pointers buffer)"	,"indirect stabilized quicksort using pointers to size-varying elements (strings, no ties by definition)"	,"P&P"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"8N"	,"soft"	,"soft"	,"no"	,2	,"random"	,"M*log(M)"	,"M*log(M)"	,"N\u00b2"	,"pointer zoom with random access"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"WQuicksort2B"	,"WQuick2B"	,"w"	,"W"	,"violett"	,"s-partitioning"	,"greeNsort: like WQuicksort2 but tuned to reduce branch-misprediction"	,"indirect stabilized quicksort using pointers to size-varying elements (strings,no ties by definition) with tuning to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"8N"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"M*log(M)"	,"M*log(M)"	,"N\u00b2"	,"pointer zoom with random access"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"VKnuthsort"	,"VKnuth"	,"K"	,"k"	,"black"	,"p-merging"	,"greeNsort: direct Knuthsort for size-varying elements (N elements buffer)"	,"direct mergesort of size-varying elements (strings) using Knuths merge with one loop check"	,"S&M"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"M"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"M*log(M)"	,"M*log(M)"	,"M*log(M)"	,"ping-pong"	,"standard"	,NA	,"not claimed"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"VKnuthsortA"	,"VKnuthA"	,"K"	,"K"	,"black"	,"p-merging"	,"greeNsort: like VKnuthsort but tuned for adaptivity"	,"direct mergesort of size-varying elements (strings) using Knuths merge with one loop check and tuned for non-overlap"	,"S&M"	,NA	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"M"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"M*log(M)"	,"M*log(M)"	,"M*log(M)"	,"ping-pong"	,"standard"	,NA	,"not claimed"	,"unknown, trivial combinations of prior art"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"VFrogsort1"	,"VFrog1"	,"F"	,"f"	,"springgreen4"	,"s-merging"	,"greeNsort: direct Frogsort for size-varying elements (N/2 elements buffer)"	,"direct frogsort of size-varying elements (strings)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"M/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"M*log(M)"	,"M*log(M)"	,"M*log(M)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"VFrogsort1A"	,"VFrog1A"	,"F"	,"F"	,"springgreen4"	,"s-merging"	,"greeNsort: like VFrogsort1 but tuned for adaptivity"	,"direct frogsort of size-varying elements (strings) and tuned for non-overlap"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,TRUE	,FALSE	,TRUE	,TRUE	,"M/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"M*log(M)"	,"M*log(M)"	,"M*log(M)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"MKnuthsort"	,"MKnuth"	,"M"	,"2"	,"black"	,"p-merging"	,"like Knuthsort but for sorting of the colums of an 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"like Knuthsort but for sorting of the colums of an 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"NKnuthsort"	,"NKnuth"	,"M"	,"2"	,"black"	,"p-merging"	,"like MKnuthsort but sorting key+reference separately from the records, reordering the records in a final pass"	,"like MKnuthsort but sorting key+reference separately from the records, reordering the records in a final pass"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"MFrogsort2"	,"MFrog2"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"like Frogsort2 but for sorting of the colums of a 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"like Frogsort2 but for sorting of the colums of a 32bit-integer matrix by the values in the first row (sorting records with key and payload)"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"NFrogsort2"	,"NFrog2"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"like MFrogsort2 but sorting key+reference separately from the records, reordering the records in a final pass"	,"like MFrogsort2 but sorting key+reference separately from the records, reordering the records in a final pass"	,"S&M"	,"F&M"	,"SORT"	,FALSE	,FALSE	,TRUE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PQuicksort2"	,"PQuick2"	,"q"	,"q"	,"red"	,"s-partitioning"	,"branch-parallel Binary probabilistic quicksort with pointer movment by Hoare 1961, tie-handling by  Singleton 1969 as recommended by Sedgewick 1977 (random pivot, swapping ties, no early terminating on ties)"	,"branch-parallel Binary probabilistic quicksort with pointer movment vy Hoare 1961, tie-handling by  Singleton 1969 as recommended by Sedgewick 1977 (random pivot, swapping ties, no early terminating on ties)"	,"P&P"	,NA	,"SORT"	,TRUE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"no"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Hoare (1961) Algorithm 64: Quicksort"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PQuicksort2B"	,"PQuick2B"	,"q"	,"q"	,"red"	,"s-partitioning"	,"branch-parallel Block-quicksort, like Quicksort2 but tuned to reduce branch-misprediction and without their primitive early termination"	,"branch-parallel Block-quicksort following Edelkamp&Wei\u00df  to reduce branch-misprediction but without their primitive early termination"	,"P&P"	,NA	,"SORT"	,TRUE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"soft"	,"soft"	,"tuned"	,2	,"random"	,"N*log(N)"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,NA	,"prior art"	, "Stefan Edelkampand Armin Wei\u00df, 2016"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PDucksort"	,"PDuck"	,"d"	,"d"	,"blue"	,"a-partitioning"	,"greeNsort: branch-parallel modification of Zucksort with Presorted Order Early Termination (POET)"	,"greeNsort: branch-parallel modification of Zucksort with Presorted Order Early Termination (POET)"	,"P&P"	,NA	,"SORT"	,TRUE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PDucksortB"	,"PDuckB"	,"d"	,"d"	,"blue"	,"a-partitioning"	,"greeNsort: branch-parallel like Ducksort but tuned to reduce branch-misprediction"	,"greeNsort: branch-parallel like Ducksort but tuned to reduce branch-misprediction"	,"P&P"	,NA	,"SORT"	,TRUE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"hard"	,"tuned"	,"tuned"	,2	,"random"	,"N"	,"N*log(N)"	,"N\u00b2"	,"inplace zoom"	,"standard"	,"'zig-zag-recursion' over 'pivot-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PKnuthsort"	,"PKnuth"	,"M"	,"2"	,"black"	,"p-merging"	,"parallel reference mergesort like Knuthsort but parallel"	,"parallel ping-pong mergesort with single loop check (Knuths merge)"	,"S&M"	,NA	,"SORT"	,TRUE	,FALSE	,FALSE	,TRUE	,TRUE	,"N"	,"soft"	,"soft"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, trivial combinations of prior art"	,"undefined"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PFrogsort0"	,"PFrog0"	,"F"	,"1"	,"springgreen4"	,"s-merging"	,"greeNsort: parallel asymmetric root version of Frogsort (adaptive to ascending, (N/2 elements buffer)"	,"parallel Frogsort (asymmetrically adaptive) initializing and then splitting triplets of two data and one buffer element"	,"S&M"	,"F&M"	,"SORT"	,TRUE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PFrogsort1"	,"PFrog1"	,"F"	,"1"	,"springgreen4"	,"s-merging"	,"greeNsort: parallel asymmetric general version of Frogsort (adaptive to ascending and descending, (N/2 elements buffer)"	,"parallel Frogsort (asymmetrically adaptive) with balanced F&M of data and N/2 buffer"	,"S&M"	,"F&M"	,"SORT"	,TRUE	,FALSE	,FALSE	,TRUE	,TRUE	,"N/2"	,"soft"	,"hard"	,"no"	,2	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PFrogsort2"	,"PFrog2"	,"F"	,"2"	,"springgreen4"	,"s-merging"	,"greeNsort: parallel p2-parametrized Frogsort with p2*N buffer (difficult branch-parallel, default N/8 elements buffer)"	,"parallel Frogsort with imbalanced F&M of data and less than N/2 buffer (proportional split of buffer reduces memory and branch-misprediction, switch to Frogsort1 as soon as possible)"	,"S&M"	,"F&M"	,"SORT"	,TRUE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"PFrogsort3"	,"PFrog3"	,"F"	,"3"	,"springgreen4"	,"s-merging"	,"greeNsort: parallel p3-parametrized Frogsort with p3*N buffer (reduced branch-parallel, default N/8 elements buffer)"	,"parallel Frogsort with imbalanced A&M of data and less than N/2 buffer (serially alternating buffer, switch to Frogsort1 as soon as possible)"	,"S&M"	,"A&M then F&M"	,"SORT"	,TRUE	,FALSE	,FALSE	,TRUE	,TRUE	,"p*N | p <= 0.5"	,"soft"	,"hard"	,"soft"	,2	,"skewed"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"symmetric"	,"symmetric"	,"'flip-recursion' over 'buffer-asymmetry'"	,"claimed"	,"Jens Oehlschl\u00e4gel"	,"greeNsort"	,"(c) 2010 - 2020 Dr. Jens Oehlschl\u00e4gel"	,"greeNsort"	,"greeNsort"
  ,"Skasort"	,"Ska"	,"X"	,"x"	,"red"	,NA	,"stable generalized Radixsort by Malte Skarupke"	,"Skasort is a stable generalized Radixsort with 100\\% buffer, the user interface requries a key-extraction function, this supports a variety of data/object types, however, Skasort is less general than a comparision sort."	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,FALSE	,"N"	,"hard"	,"none"	,"hard"	,256	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, intelligent interfacing to well-known prior art"	,"Boost"	,"(c) 2016 Malte Skarupke"	,"https://github.com/skarupke/ska_sort"	,"greeNsort.Rcpp"
  ,"ISkasort"	,"ISka"	,"X"	,"x"	,"red"	,NA	,"unstable generalized Inplace Radixsort by Malte Skarupke"	,"LSkasort is a unstable generalized Inplace Radixsort (with 0\\% buffer), the user interface requries a key-extraction function, this supports a variety of data/object types, however, unlike Grailsort , Skasort is not stable and less general than a comparision sort."	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"N"	,"hard"	,"none"	,"hard"	,256	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"ping-pong"	,"standard"	,NA	,"prior art"	,"unknown, intelligent interfacing to well-known prior art"	,"Boost"	,"(c) 2016 Malte Skarupke"	,"https://github.com/skarupke/ska_sort"	,"greeNsort.Rcpp"
  ,"Pdqsort"	,"Pdq"	,"t"	,"t"	,"red"	,NA	,"Pdqsort by Orson Peters (not branchless)"	,"Pdqsort by Orson Peters (not branchless)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"tuned"	,"tuned"	,"no"	,2	,"random"	,"N"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Orson Peters, 2015, Pdqsort algorithm"	,"zlib"	,"(c) 2015 Orson Peters"	,"https://github.com/orlp/pdqsort"	,"greeNsort.Rcpp"
  ,"PdqsortB"	,"PdqB"	,"t"	,"t"	,"red"	,NA	,"Pdqsort by Orson Peters (branchless)"	,"Pdqsort by Orson Peters (branchless)"	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"log(N)"	,"tuned"	,"tuned"	,"tuned"	,2	,"random"	,"N"	,"N*log(N)"	,"N*log(N)"	,"inplace zoom"	,"standard"	,NA	,"prior art"	,"Orson Peters, 2015, Pdqsort algorithm"	,"zlib"	,"(c) 2015 Orson Peters"	,"https://github.com/orlp/pdqsort"	,"greeNsort.Rcpp"
  ,"IPS4o"	,"IPS4o"	,"X"	,"x"	,"red"	,NA	,"In-place Parallel Super Scalar Samplesort by Axtmann, Witt, Ferizovic, and Sanders"	,"A sorting algorithm that works in-place, executes in parallel, is cache-efficient, avoids branch-mispredictions, and performs work O(n log n) for arbitrary inputs with high probability. It is, however, NOT stable."	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"2*B | B <= sqrt(N)"	,"hard"	,NA	,"tuned"	,256	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"block"	,"standard"	,NA	,"prior art"	,"Michael Axtmann, Daniel Ferizovic, Sascha Witt, Peter Sanders"	,"BSD 2-Clause 'Simplified' License"	,"(c) 2017 Michael Axtmann, Daniel Ferizovic, Sascha Witt"	,"https://github.com/SaschaWitt/ips4o"	,"greeNsort.Rcpp"
  ,"IS4o"	,"IS4o"	,"X"	,"x"	,"red"	,NA	,"In-place (non-Parallel) Super Scalar Samplesort by Axtmann, Witt, Ferizovic, and Sanders"	,"A sorting algorithm that works in-place, here the single-threaded version, is cache-efficient, avoids branch-mispredictions, and performs work O(n log n) for arbitrary inputs with high probability. It is, however, NOT stable.  The authors claim that IS4o is up to 1.5 times faster than the closest sequential competitor, BlockQuicksort, however, I found PdqBsort to be faster."	,"P&P"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"2*B | B <= sqrt(N)"	,"hard"	,NA	,"tuned"	,256	,"balanced"	,"N*log(N)"	,"N*log(N)"	,"N*log(N)"	,"block"	,"standard"	,NA	,"prior art"	,"Michael Axtmann, Daniel Ferizovic, Sascha Witt, Peter Sanders"	,"BSD 2-Clause 'Simplified' License"	,"(c) 2017 Michael Axtmann, Daniel Ferizovic, Sascha Witt"	,"https://github.com/SaschaWitt/ips4o"	,"greeNsort.Rcpp"
  ,"Learnsort"	,"Learn"	,"X"	,"x"	,"red"	,NA	,"Learned Sort, a model-enhanced sorting algorithm that was published in 'The Case for a Learned Sorting Algorithm'"	,"Learned Sort (Learnsort) is a novel specific sorting algorithm that exploits the scale of the keys by 2-stage-projecting a learned (linear-spline) CDF-function. In theory it is stable, but this implementation is not: a) sorting of the spit-bucket was done with std::sort, b) in the final leafes it seems to use a (synthetic) counting sort."	,NA	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,FALSE	,FALSE	,"N"	,"soft"	,"soft"	,"tuned"	,1000	,"balanced"	,"N*4+S*log(S)"	,"N*4+S*log(S)"	,"N*4+S*log(S)"	,"block"	,"standard"	,NA	,"prior art"	,"Kristo, Vaidya, \u00c7etintemel, Misra, Kraska"	,"GPL-3 License"	,"(c) 2020 Kristo, Vaidya, \u00c7etintemel, Misra, Kraska"	,"https://github.com/learnedsystems/LearnedSort"	,"greeNsort.Rcpp"
  ,"Timsort"	,"Tim"	,"T"	,"T"	,"grey90"	,"c-merging"	,"Tim Peter's Timsort as implemented in C++ by Timothy Van Slyke"	,"Timsort is a Natural Mergesort heavily tuned to be (ON(N)) adaptive to (perfectly) presorted data, however, Timsort is based on an inefficient c-merging which moves data twice per merge, and the heavy tuning makes it slower when it comes to sorting truly unsorted data."	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,FALSE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"back and forth"	,"data-driven"	,NA	,"prior art"	,"Tim Peters (2002)"	,"MIT"	,"(c) 2018 Timothy Van Slyke"	,"https://github.com/tvanslyke/timsort-cpp"	,"greeNsort.Rcpp"
  ,"Peeksort"	,"Peek"	,"X"	,"x"	,"red"	,"c-merging"	,"Peeksort, top-down adaptive splitting in half by finding the run boundary closest to the middle of the array'"	,"Peeksort, top-down adaptive splitting in half by finding the run boundary closest to the middle of the array'"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,FALSE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"back and forth"	,"data-driven"	,NA	,"prior art"	,"Munro & Wild (2018)"	,"MIT"	,"(c) 2022 Sebastian Wild"	,"https://github.com/sebawild/powersort"	,"greeNsort.Rcpp"
  ,"Powersort"	,"Power"	,"X"	,"x"	,"red"	,"c-merging"	,"Powersort, bottom-up adaptive merging that for each new run decides to do some merges now, or to delay them and keep the runs on a to-do-stack'"	,"Powersort, bottom-up adaptive merging that for each new run decides to do some merges now, or to delay them and keep the runs on a to-do-stack'"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,FALSE	,"N/2"	,"soft"	,"tuned"	,"no"	,2	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"back and forth"	,"data-driven"	,NA	,"prior art"	,"Munro & Wild (2018)"	,"MIT"	,"(c) 2022 Sebastian Wild"	,"https://github.com/sebawild/powersort"	,"greeNsort.Rcpp"
  ,"Powersort4"	,"Power4"	,"X"	,"x"	,"red"	,"c-merging"	,"4-ary Powersort, bottom-up adaptive merging that for each new run decides to do some merges now, or to delay them and keep the runs on a to-do-stack'"	,"4-ary Powersort, bottom-up adaptive merging that for each new run decides to do some merges now, or to delay them and keep the runs on a to-do-stack'"	,"S&M"	,NA	,"SORT"	,FALSE	,FALSE	,FALSE	,TRUE	,FALSE	,"N"	,"soft"	,"tuned"	,"no"	,4	,"balanced"	,"N"	,"N*log(N)"	,"N*log(N)"	,"back and forth"	,"data-driven"	,NA	,"prior art"	,"Gelling, Nebel, Smith & Wild (2023)"	,"MIT"	,"(c) 2022 Sebastian Wild"	,"https://github.com/sebawild/powersort"	,"greeNsort.Rcpp"
)


algodb <- as.data.frame(algodb)
row.names(algodb) <- algodb$label

#evalOnLoad( parse(text = "
#algodb$func <- sapply(algodb$name, get)
algodb$func <- eval(parse(text = paste('c(', paste(algodb$name, collapse = ', '), ')', sep = '')))
names(algodb$func) <- algodb$name
algodb$situation_exsitu   = sapply(algodb$func, function(x)'exsitu' %in% unlist(as.list(as.list(x)$situation)[-1]))
algodb$situation_insitu   = sapply(algodb$func, function(x)'insitu' %in% unlist(as.list(as.list(x)$situation)[-1]))
algodb$situation_default = I(unlist(lapply(algodb$func, function(x)as.list(as.list(x)$situation)[-1][[1]])))
algodb$method_pointer  = sapply(algodb$func, function(x)'pointer' %in% unlist(as.list(as.list(x)$method)[-1]))
algodb$method_index  = sapply(algodb$func, function(x)'index' %in% unlist(as.list(as.list(x)$method)[-1]))
algodb$method_default   = I(unlist(lapply(algodb$func, function(x)as.list(as.list(x)$method)[-1][[1]])))
#") )


#dput(ls(2, pattern="Median"))
#dput(ls(2, pattern="select"))
#dput(ls(2, pattern="sort"))
#dput(ls(2, pattern="sqrt"))
