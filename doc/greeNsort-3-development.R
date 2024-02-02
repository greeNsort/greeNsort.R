## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE
, comment = "#>"
, fig.width = 7.5
, fig.height = 5 
, out.width = "100%"
)

## ----setup--------------------------------------------------------------------
library(greeNsort)

## -----------------------------------------------------------------------------
cran_compatible()

## -----------------------------------------------------------------------------
current_issorted(c(+0.5, -0.5))

## -----------------------------------------------------------------------------
current_issorted(c('+0.5', '-0.5'))

## -----------------------------------------------------------------------------
x <- c('ä','z')
print(sort(x))  # UTF-8 interpretation
print(sort(x, method="radix"))  # naive byte interpretation

## -----------------------------------------------------------------------------
x <- c('ä','z')
current_issorted(x)

## -----------------------------------------------------------------------------
insertionsort_limit()

## -----------------------------------------------------------------------------
x <- runif(9)
y <- sort(x)
c(x=is.unsorted(x), y=is.unsorted(y))

## -----------------------------------------------------------------------------
x <- seq(-2, 2, by=0.5)
data.frame(number_original=x, number_stable=stable_keys(x), string_original=num2str(x), string_stable=stable_keys(num2str(x)))

## -----------------------------------------------------------------------------
x <- c(+0.5, 0, -0.5)
current_keys(x)

## -----------------------------------------------------------------------------
current_compare(x, 0)

## -----------------------------------------------------------------------------
x <- c(9.1, 8.9, 1.9, 1.1)
y <- x[]; invisible(Frogsort1(y)) # sort to be verified
current_issorted(y)

## -----------------------------------------------------------------------------
o <- order(current_keys(x))       # order information
x <- x[o]
identical(x,y)

## -----------------------------------------------------------------------------
testsort(
  R = 0:2                               # random-number seeds for repetitions
, N = c(99:101)                         # sample sizes
, M = c("pointer", "index")             # methods (if available)
, S = c("insitu", "exsitu")             # situations (if available)
, A = c('Knuth','Frog1')                # algodb labels
, D = c('ascall', 'permut', 'descall')  # testdb labels
, T = 1                                 # thread counts
, out = ''
#, verbose = TRUE
)

## -----------------------------------------------------------------------------
algodb[algodb$task %in% c("select","SELECT"), c("label","task")]

## -----------------------------------------------------------------------------
testselect(
  R = 0
, N = c(99:101)                         # sample sizes
, S = "insitu"                          # situations (if available)
, A = c("quick","zack")                 # algodb labels
, D = c("tielog2","tiesqrt")            # testdb labels
, out = ""
#, verbose = TRUE
)

## -----------------------------------------------------------------------------
algodb[algodb$task %in% c("partial","PARTIAL"), c("label","task")]

## -----------------------------------------------------------------------------
testpart(
  R= 0:1
, N = c(99:101)                         # sample sizes
, S = "insitu"                          # situations (if available)
, A = c("quickpt","quickpl","quickpr","zackpt", "zackpl","zackpr")  # algodb labels
, D = c("tielog2","tiesqrt")            # testdb labels
, out = ""
#, verbose = TRUE
)

## -----------------------------------------------------------------------------
algodb[algodb$task == 'SORT' & algodb$varying , c("label","task")]

## -----------------------------------------------------------------------------
vtestsort(
  R = 0:1
, A = c("VFrog1","WQuick2","UZack")
, D = c("KW","KWa","KWp")
, out = ''
, verbose = TRUE
)

