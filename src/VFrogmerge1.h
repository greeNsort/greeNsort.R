/*
# greeNsort direct Frogsort1 for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_VFrogmerge1_h
#define ALREADY_DEFINED_VFrogmerge1_h

#include "algo.h"
#include "strtools.h"
#include "VInsertionsort_l2r.h"

static void VFrogsort1P_merge_asc_left_full(char *z, char *ll, char *lr, char *rl, char *rr){
  IndexT s;
  *(ll-1) = 0;
  *(rl-1) = 0;
  char *LR = lr;
  char *RR = rr;
  while(*--lr);
  while(*--rr);
  //if (GT(lr+1, rl))  // simple tuning for perfectly presorted
  for (;;){
    if (GT(lr+1, rr+1)){
      s = LR-lr;
      strcpy_rl(z, LR, s);
      z -= s;
      LR = lr;
      if (lr < ll)
        break;
      while(*--lr);
    }else{
      s = RR-rr;
      strcpy_rl(z, RR, s);
      z -= s;
      RR = rr;
      if (rr < rl)
        break;
      while(*--rr);
    }
  }
#if VINSERTIONSORT_LIMIT > 0
  while(rl <= RR)
    *z-- = *RR--;
  while(ll <= LR)
    *z-- = *LR--;
#else
  while(rl <= RR){
    s = strcpylen_rl(z, RR);
    z -= s;
    RR -= s;
  }
  while(ll <= LR){
    s = strcpylen_rl(z, LR);
    z -= s;
    LR -= s;
  }
#endif
}
static void VFrogsort1P_merge_asc_right_full(char *z, char *ll, char *lr, char *rl, char *rr){
  IndexT s;
  //*(ll-1) = NULL_CHAR; char *LR = lr; while(*--LR){}; if (GT(LR+1, rl))  // simple tuning for perfectly presorted
  for (;;){
    if (LT(rl, ll)){
      s = strcpylen_lr(z, rl);
      z += s;
      rl += s;
      if (rl > rr)
        break;
    }else{
      s = strcpylen_lr(z, ll);
      z += s;
      ll += s;
      if (ll > lr)
        break;
    }
  }
#if VINSERTIONSORT_LIMIT > 0
  while(ll <= lr)
    *z++ = *ll++;
  while(rl <= rr)
    *z++ = *rl++;
#else
  while(ll <= lr){
    s = strcpylen_lr(z, ll);
    z += s;
    ll += s;
  }
  while(rl <= rr){
    s = strcpylen_lr(z, rl);
    z += s;
    rl += s;
  }
#endif
}


static void VFrogsort1P_merge_asc_left_stable(char *z, char *ll, char *lr, char *rl, char *rr){
  IndexT s;
  *(ll-1) = 0;
  *(rl-1) = 0;
  char *LR = lr;
  char *RR = rr;
  while(*--lr);
  while(*--rr);
  //if (GT(lr+1, rl))  // simple tuning for perfectly presorted
  for (;;){
    if (GT(lr+1, rr+1)){
      s = LR-lr;
      strcpy_rl(z, LR, s);
      z -= s;
      LR = lr;
      if (lr < ll)
        break;
      while(*--lr);
    }else{
      s = RR-rr;
      strcpy_rl(z, RR, s);
      z -= s;
      RR = rr;
      if (rr < rl)
        break;
      while(*--rr);
    }
  }
#if VINSERTIONSORT_LIMIT > 0
  while(rl <= RR)
    *z-- = *RR--;
#else
  while(rl <= RR){
    s = strcpylen_rl(z, RR);
    z -= s;
    RR -= s;
  }
#endif
}
static void VFrogsort1P_merge_asc_right_stable(char *z, char *ll, char *lr, char *rl, char *rr){
  IndexT s;
  //*(ll-1) = NULL_CHAR; char *LR = lr; while(*--LR){}; if (GT(LR+1, rl))  // simple tuning for perfectly presorted
  for (;;){
    if (LT(rl, ll)){
      s = strcpylen_lr(z, rl);
      z += s;
      rl += s;
      if (rl > rr)
        break;
    }else{
      s = strcpylen_lr(z, ll);
      z += s;
      ll += s;
      if (ll > lr)
        break;
    }
  }
#if VINSERTIONSORT_LIMIT > 0
  while(ll <= lr)
    *z++ = *ll++;
#else
  while(ll <= lr){
    s = strcpylen_lr(z, ll);
    z += s;
    ll += s;
  }
#endif
}

#endif
