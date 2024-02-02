# greeNsort parallel random numbers
# Copyright (C) 2010 - 2024 Dr. Jens Oehlschlaegel
# GPL-3 clause
# Provided 'as is', use at your own risk

#' prunif
#'
#' Parallel random number generation
#'
#' Using \code{\link[rTRNG]{runif_trng}}
#'
#' @param n number of values to generate
#' @param min lower bound
#' @param max upper bound
#' @param pgrain parallelGrain paramter, default \code{\link{perfcores}}
#'
#' @return a random vector of doubles between min an max
#' @seealso \code{\link{runif}}, \code{\link[rTRNG]{runif_trng}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' require(RcppParallel)
#' require(rTRNG)
#' RcppParallel::setThreadOptions(numThreads = perfcores());
#' prunif(2^28)
#' }
#'
prunif <- function(n, min=0, max=.Machine$integer.max, pgrain=perfcores()){
  #requireNamespace(rTRNG)
  rTRNG::TRNGseed(runif(1, max=max));
  rTRNG::runif_trng(n, min=min, max=max, parallelGrain = pgrain)
}
