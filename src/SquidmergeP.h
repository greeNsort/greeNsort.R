/*
# greeNsort Squid merging (pointered implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_SquidmergeP_h
#define ALREADY_DEFINED_SquidmergeP_h

#include "algo.h"

#define SKIP_FULL_PRESORTING
//no benefit of finegrained skipping
//#define SKIP_PARTIAL_PRESORTING

/*  -- // -->  */
static void SquidmergeP_asc_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){

  ValueT v=*rl;
#ifdef  SKIP_FULL_PRESORTING
  if (LT(v,*lr)){
#endif
#ifdef  SKIP_PARTIAL_PRESORTING
    ValueT *i=ll+(lr-ll)/2;
    while (i<lr && GE(v,*i)){
      while (ll<=i)
        *(x++) = *(ll++);
      i=ll+(lr-ll)/2;
    }
#endif
    ValueT u=*ll;
    for(;;){
      if (LT(v,u)){
        *(x++) = v; rl++;
        if (rl>rr)
          break;
        v = *rl;
      }else{
        *(x++) =u; ll++;
        if (ll>lr)
          return;
      }
      u = *ll;
    }
#ifdef  SKIP_FULL_PRESORTING
  }
#endif
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

/*  <-- // --  */
static void SquidmergeP_asc_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr;
#ifdef  SKIP_FULL_PRESORTING
  if (LT(*rl,u)){
#endif
#ifdef SKIP_PARTIAL_PRESORTING
    ValueT *i=rr-(rr-rl)/2;
    while (i>rl && GE(*i,u)){
      while (rr>=i)
        *(x--) = *(rr--);
      i=rr-(rr-rl)/2;
    }
#endif
    ValueT v=*rr;
    for(;;){
      if (GE(v,u)){
        *(x--) = v; rr--;
        if (rl>rr)
          return;
        v = *rr;
      }else{
        *(x--) = u; lr--;
        if (ll>lr)
          break;
        u = *lr;
      }
    }
#ifdef  SKIP_FULL_PRESORTING
  }
#endif
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}


/* -- \\ -->  */
static void SquidmergeP_des_des_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT v=*rl;
#ifdef  SKIP_FULL_PRESORTING
  if (GE(v,*lr)){
#endif
#ifdef  SKIP_PARTIAL_PRESORTING
    ValueT *i=ll+(lr-ll)/2;
    while (i<lr && LT(v,*i)){
      while (ll<=i)
        *(x++) = *(ll++);
      i=ll+(lr-ll)/2;
    }
#endif
    ValueT u=*ll;
    for(;;){
      if (GE(v,u)){
        *(x++) = v; rl++;
        if (rl>rr)
          break;
        v = *rl;
      }else{
        *(x++) =u; ll++;
        if (ll>lr)
          return;
      }
      u = *ll;
    }
#ifdef  SKIP_FULL_PRESORTING
  }
#endif
  while(ll<=lr){
    *(x++) = *(ll++);
  }
  return;
}

/*  <-- \\ --  */
static void SquidmergeP_des_des_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr;
#ifdef  SKIP_FULL_PRESORTING
  if (LE(u, *rl)){
#endif
#ifdef SKIP_PARTIAL_PRESORTING
    ValueT *i=rr-(rr-rl)/2;
    while (i>rl && LT(*i,u)){
      while (rr>=i)
        *(x--) = *(rr--);
      i=rr-(rr-rl)/2;
    }
#endif
    ValueT v=*rr;
    for(;;){
      if (LT(v,u)){
        *(x--) = v; rr--;
        if (rl>rr)
          return;
        v = *rr;
      }else{
        *(x--) = u; lr--;
        if (ll>lr)
          break;
        u = *lr;
      }
    }
#ifdef  SKIP_FULL_PRESORTING
  }
#endif
  while(rl<=rr){
    *(x--) = *rr--;
  }
  return;
}


/*  -- \/ -->  */
static void SquidmergeP_des_asc_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr, v=*rl;
  //if (LT(v,*ll))
  for(;;){
    if (LT(v,u)){
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      *(x++) = u; lr--;
      if (ll>lr)
        return;
      u = *lr;
    }
  }
  while(ll<=lr){
    *(x++) = *(lr--);
  }
  return;
}

/*  <-- \/ --  */
static void SquidmergeP_des_asc_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr, v=*rl;
  //if (GE(*rr,u))
  for(;;){
    if (LT(v,u)){
      *(x--) = v; rl++;
      if (rl>rr)
        return;
      v = *rl;
    }else{
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }
  }
  while(rl<=rr){
    *(x--) = *rl++;
  }
  return;
}

/* -- /\ -->  */
static void SquidmergeP_asc_des_right(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr, v=*rl;
  //if (GE(v,*ll))
  for(;;){
    if (GE(v,u)){
      *(x++) = v; rl++;
      if (rl>rr)
        break;
      v = *rl;
    }else{
      *(x++) = u; lr--;
      if (ll>lr)
        return;
      u = *lr;
    }
  }
  while(ll<=lr){
    *(x++) = *(lr--);
  }
  return;
}

/* <-- /\ --  */
static void SquidmergeP_asc_des_left(
    ValueT *x
  , ValueT *ll
  , ValueT *lr
  , ValueT *rl
  , ValueT *rr
){
  ValueT u=*lr, v=*rl;
  //if (LT(*rr,u))
  for(;;){
    if (GE(v,u)){
      *(x--) = v; rl++;
      if (rl>rr)
        return;
      v = *rl;
    }else{
      *(x--) = u; lr--;
      if (ll>lr)
        break;
      u = *lr;
    }
  }
  while(rl<=rr){
    *(x--) = *rl++;
  }
  return;
}

#endif
