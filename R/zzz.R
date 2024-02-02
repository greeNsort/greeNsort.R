# greeNsort package pre- and post actions
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

# @useDynLib greeNsort, .registration = TRUE
#' @useDynLib greeNsort, .registration = TRUE, .fixes = "C_"
#' @importFrom utils packageDescription
#' @importFrom Rcpp  evalCpp
# @exportPattern "^[^\\.]"

#.onLoad <- function(lib, pkg) {
  ##library.dynam("greeNsort", pkg, lib) use useDynLib(greeNsort, .registration = TRUE, .fixes = "C_") in NAMESPACE instead
#}


.onAttach <- function(libname, pkgname){
  options(
    greensort_example_size = getOption("greensort_example_size", as.integer(2^8))
  , greensort_perf_calc = getOption("greensort_perf_calc", 'raw')
  )
#  packageStartupMessage(
#     if (cran_compatible())
#       paste(
# "
# Attention: package ", pkgname, packageDescription(pkgname, fields = "Version"), " has been compiled for CRAN compatibility,
# for fair comparisons between algorithms you *must* compile the package
# with CRAN_COMPATIBILTY undefined. CRAN compatibility  uses R's random number
# generator and memory management. These settings can cause replicable
# performance biases *not* related to the algorithm. Furthermore you should
# compile the package without optimization, i.e. with CFLAGS=-O0. Note that
# submitting -O0 in Makevars resp. Makevars.win might be overwritten by
# R CMD INSTALL. We edited Makeconf to get rid of that.
# "
#       )
#     else
#       paste("Attaching package",  pkgname, packageDescription(pkgname, fields = "Version"), "\n")
#   )
}

.onDetach <- function(libpath) {
   packageStartupMessage("Detaching package greeNsort\n")
}

.onUnload <- function(libpath){
   library.dynam.unload("greeNsort", libpath)
}
