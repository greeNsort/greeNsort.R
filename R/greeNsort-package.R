# greeNsort package documentation
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

#' greeNsort - a companion package for the greeNsort publications
#'
#' This package packages contains single threaded demo implementations for various sorting algorithms.
#' The algorithms are implemented in C-code and measure their runtime and memory consumption.
#' The sorting algorithms are not tuned for maximal performance but designed for fair comparision.
#' With a few exceptions the algorithms allocate all needed RAM before execution.
#' The 'exsitu' option emulates sorting from storage that cannot or shall not be used for sorting, hence fresh RAM is allocated for data and buffer,
#' the memory where the data resides is treated as immutable, the data is copied to the fresh RAM, then sorted and finally copied back;
#' one notable exception is \code{\link{Storksort}}, which does not complete sorting in the allocated RAM but 'sends' the elements at the leaf of recursion into the appropriate locations.
#' The 'insitu' method assumes that the data reside in 100\% RAM and allocate only the buffer memory required for the respective algorithm;
#' the notable exception is \code{\link{Storksort}} which cannot sort insitu (here the 'insitu' option returns the partitioned data \emph{before} it is 'sent' into final positions).
#' Note that the sorting functions do not follow R's convention to let their input unchanged and return sorted data; the sorting functions in this package do modify their input vector and return performance data:
#'
#' @import tibble
#' @importFrom RhpcBLASctl get_num_cores
#' @importFrom utils installed.packages read.table
#' @importFrom stats cor median quantile rnorm runif approx lm predict
#' @importFrom abind abind
#' @importFrom graphics plot abline matplot
#' @importFrom gplots heatmap.2 boxplot2
#' @importFrom rstudioapi askForPassword
#' @import greeNsort.Rcpp
#'
#' @return The algorithms return a performance measurement \code{\link{rawperf}} or something with an attribute \code{perf} containing a a performance measurement \code{\link{rawperf}}.
#'
#' @section Compiling:
#'
#' This package can be compiled with several choices.
#' \describe{
#' \item{CRAN_COMPATIBLE}{This flag activates CRAN compatibility: function \code{\link{cran_compatible}} returns \code{TRUE} if the package has beed compiled under R's memory management (Calloc,Free) for CRAN conpatibility,
#' and returns \code{FALSE} if the package has beed compiled for with the standard C memory management (malloc,free).}
#' \item{STABLE_TEST}{This flag activates rounding of key values before comparision for testing of sorting stability: the sorting comparator does not see decimals and the sporting algorithm must not change the order of decimals within ties. The function \code{\link{current_keys}} provides an API into the rounding (or not rounding) of the key values.}
#' \item{INSERTIONSORT_LIMIT}{This flag and related flags determine insertionsort-tuning. function \code{\link{insertionsort_limit}} returns the max value for which recursion is stopped and sorting is delegated to \code{\link{Insertionsort}}.}
#' }
#'
#' Check \code{\link{perfcheck}} and follow instructions if not OK.
#'
#' @section Metadata:
#'
#' This package comes with three databases:
#' \describe{
#' \item{Algorithms database}{see \code{\link{algodb}}}
#' \item{Test scenario database (double data)}{see \code{\link{testdb}}}
#' \item{Test scenario database for elements with varying length (string)}{see \code{\link{vtestdb}}}
#' }
#'
#'
#' @section Philosophy:
#' \tabular{ll}{
#' "Aber der Maulwurf der Vernunft ist nur in dem Sinne blind, dass er den Widerstand eines ungelösten Problems erkennen kann, ohne zu wissen, ob es eine Lösung geben wird. Dabei ist er hartnäckig genug, um sich trotzdem in seinen Gängen voranzubuddeln."
#' \tab
#' "But the mole of reason is blind only in the sense that it can recognize the resistance of an unsolved problem without knowing if there will be a solution. He is stubborn enough to dig his way through his corridors anyway."
#' \cr
#' (Jürgen Habermas)
#' \tab
#' (Jürgen Habermas)
#' \cr
#' }
#'
#'
#'@name greeNsort-package
NULL

# usethis::use_vignette("algorithms")
# usethis::use_vignette("empirics")
# usethis::use_vignette("test")

# usethis::use_vignette("greeNsort-1-quickstart")
# usethis::use_vignette("greeNsort-2-development")
# usethis::use_vignette("greeNsort-3-performance")
# devtools::build_vignettes()

globalVariables(c(".perfbackground"))
