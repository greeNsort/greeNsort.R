/*
# greeNsort Frog merge moving Matrix-Columns by first row keys (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_MFrogmergeP_h
#define ALREADY_DEFINED_MFrogmergeP_h

#include "algo.h"

static void MFrogmergeP_asc_right(
  IntValueT *x
, IntValueT *ll
, IntValueT *lr
, IntValueT *rl
, IntValueT *rr
, IndexT m
){
  // int d = (rr-rl+lr-ll+m);
  // Rprintf("MFrogmergeP_asc_right n=%d nl=%d nr=%d\n", (rr-rl+lr-ll)/m+2, (lr-ll)/m+1, (rr-rl)/m+1);
  // for (IntValueT *i=ll;i<=lr;i+=m)
  //    Rprintf(" l=%d\n", *i);
  // for (IntValueT *i=rl;i<=rr;i+=m)
  //    Rprintf(" r=%d\n", *i);
  IntValueT u=*ll, v=*rl;
  for(;;){
    if (GT(u, v)){
      MMOVE(x, rl, m); x+=m; rl+=m;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      MMOVE(x, ll, m); x+=m; ll+=m;
  	  if (ll>lr)
  	    return;
  	  u = *ll;
    }
  }
  while(ll<=lr){
    MMOVE(x, ll, m); x+=m; ll+=m;
  }
  // for (IntValueT *i=rr-d;i<=rr;i+=m)
  //   Rprintf(" m=%d\n", *i);
  return;
}

static void MFrogmergeP_asc_left(
    IntValueT *x
  , IntValueT *ll
  , IntValueT *lr
  , IntValueT *rl
  , IntValueT *rr
, IndexT m
){
  // Rprintf("MFrogmergeP_asc_left n=%d nl=%d nr=%d\n", (rr-rl+lr-ll)/m+2, (lr-ll)/m+1, (rr-rl)/m+1);
  // for (IntValueT *i=ll;i<=lr;i+=m)
  //   Rprintf(" l=%d\n", *i);
  // for (IntValueT *i=rl;i<=rr;i+=m)
  //   Rprintf(" r=%d\n", *i);
  IntValueT u=*lr, v=*rr;
  for(;;){
    if (GT(u, v)){
      MMOVE(x, lr, m); x-=m; lr-=m;
      if (ll>lr)
        break;
      u = *lr;
    }else{
      MMOVE(x, rr, m); x-=m; rr-=m;
      if (rl>rr)
        return;
      v = *rr;
    }
  }
  while(rl<=rr){
    MMOVE(x, rr, m); x-=m; rr-=m;
  }
  // for (IntValueT *i=ll;i<=ll+d;i+=m)
  //   Rprintf(" m=%d\n", *i);
  return;
}

#endif
