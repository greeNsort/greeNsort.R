/*
# greeNsort Frog merging Matrix-Columns by movig separated first row keys 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_NFrogmergeP_h
#define ALREADY_DEFINED_NFrogmergeP_h

#include "algo.h"

static void NFrogmergeP_asc_right(
  IntIndT *x
, IntIndT *ll
, IntIndT *lr
, IntIndT *rl
, IntIndT *rr
){
  // Rprintf("NFrogmergeP_asc_right n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (IntIndT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (IntIndT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  IntIndT u=*ll, v=*rl;
  for(;;){
    if (GT(u, v)){
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      *(x++) = u; ll++;
  	  if (ll>lr)
        return;
  	  u = *ll;
    }
  }
  while(ll<=lr){
    *(x++) = *ll++;
  }
  return;
}

static void NFrogmergeP_asc_left(
    IntIndT *x
  , IntIndT *ll
  , IntIndT *lr
  , IntIndT *rl
  , IntIndT *rr
){
  // Rprintf("NFrogmergeP_asc_left n=%d nl=%d nr=%d\n", rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (IntIndT *i=ll;i<=lr;i++)
  //   Rprintf(" l=%f\n", *i);
  // for (IntIndT *i=rl;i<=rr;i++)
  //   Rprintf(" r=%f\n", *i);
  IntIndT u=*lr, v=*rr;
  for(;;){
    if (GT(u, v)){
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }else{
      *(x--) = v; rr--;
      if (rl>rr)
        return;
      v = *rr;
    }
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}

/*
static void NFrogmergeP_asc_right(
    IntIndT *x
  , IntIndT *ll
  , IntIndT *lr
  , IntIndT *rl
  , IntIndT *rr
){
  while(ll<=lr && rl<=rr){
    *(x++) = GT(*ll, *rl) ? *(rl++) : *(ll++);
  }
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

static void NFrogmergeP_asc_left(
    IntIndT *x
  , IntIndT *ll
  , IntIndT *lr
  , IntIndT *rl
  , IntIndT *rr
){
  while(ll<=lr && rl<=rr){
    *(x--) = GT(*lr, *rr) ? *(lr--) : *(rr--);
  }
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}
*/

#endif
