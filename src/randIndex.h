/*
# greeNsort random numbers
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

// CRAN disallows rand and insists on using R's random number generators
// if using unif_rand we need initialize GetRNGstate() first and terminate PutRNGstate() thereafter
// however, calling unif_rand across dll boundary causes performance penalties
// that have been observerd to depend on the location of the caller in the dll
// in other words: if you want fair comparisons between algorithms,
// you MUST compile without CRAN_COMPATIBLE

// returns uniform random index in range 0..(n-1)

#ifndef ALREADY_DEFINED_randIndex_h
#define ALREADY_DEFINED_randIndex_h

#include "algo.h"

#ifdef CRAN_COMPATIBLE
#define RANDINDEX(funcnam) IndexT funcnam (IndexT n) \
{ \
  IndexT r;          \
	while((r = ((IndexT)(unif_rand()*n))) >= n); \
  return r; \
}
#else
#define RANDINDEX(funcnam) IndexT funcnam (IndexT n) \
{ \
  IndexT r;          \
	while(n <= (r=(((ValueT)rand())*n) /RAND_MAX)); \
  return r; \
}
#endif

static RANDINDEX(randIndex)

#endif


/*
static IndexT randIndex (IndexT n)
{
  IndexT r;
#ifdef CRAN_COMPATIBLE
	while((r = ((IndexT)(unif_rand()*n))) >= n);
#else
	while(n <= (r=(((ValueT)rand())*n) /RAND_MAX));
#endif
  return r;
}
*/
