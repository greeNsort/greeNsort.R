/*
# greeNsort common Gecko merging (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_GeckomergeP_h
#define ALREADY_DEFINED_GeckomergeP_h

#include "algo.h"
/*  -- \/ -->  */
static void GeckomergeP_asc_right_stable(
  ValueT *x
, ValueT *ll
, ValueT *lr
, ValueT *rl
, ValueT *rr
){
  // Rprintf("\nGeckomergeP_asc_right_stable x=%l n=%d nl=%d nr=%d", ((long long)x)/8, rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf("\n l=%d %f", ((long long)i)/8, *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf("\n r=%d %f", ((long long)i)/8, *i);
  ValueT u=*lr, v=*rl;
  //if (LT(v,*ll))
  for(;;){
    if (LT(v,u)){
      // Rprintf("\nr: %f", v);
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      // Rprintf("\nl: %f", u);
      *(x++) = u; lr--;
  	  if (ll>lr)
  	    return;
  	  u = *lr;
    }
  }
  while(ll<=lr){
    // Rprintf("\nl: %f", *lr);
    *(x++) = *lr--;
  }
  return;
}

/*  <-- \/ --  */
static void GeckomergeP_asc_left_reverse(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  // Rprintf("\nGeckomergeP_asc_left_reverse x=%d n=%d nl=%d nr=%d", ((long long)x)/8, rr-rl+lr-ll+2, lr-ll+1, rr-rl+1);
  // for (ValueT *i=ll;i<=lr;i++)
  //   Rprintf("\n l=%d %f",((long long)i)/8, *i);
  // for (ValueT *i=rl;i<=rr;i++)
  //   Rprintf("\n r=%d %f",((long long)i)/8, *i);
  ValueT u=*lr, v=*rl;
  //if (GE(*rr,u))
  for(;;){
    if (LT(v,u)){
      // Rprintf("\nr: %f", v);
      *(x--) = v; rl++;
      if (rl>rr)
        return;
      v = *rl;
    }else{
      // Rprintf("\nl: %f", u);
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }
  }
  while(rl<=rr){
    // Rprintf("\nr: %f", *rl);
    *(x--) = *rl++;
  }
  return;
}

#endif
