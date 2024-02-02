/*
# greeNsort ntile struct header
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_ntile2_h
#define ALREADY_DEFINED_ntile2_h

#include "algo.h"

typedef struct Ntile2Struct{
  IndexT l;
  IndexT r;
} Ntile2Struct;

/* /b// */
static Ntile2Struct Ntile2_asc_asc_left_to_right(ValueT *L, ValueT *R, IndexT Ln, IndexT Rn, IndexT k){
  assert( k <= Ln+Rn );
  Ntile2Struct ret;
  IndexT lmax = MIN(k, Ln);
  IndexT rmax = MIN(k, Rn);
  IndexT lmin = k - rmax;
  IndexT rmin = k - lmax;
  //Rprintf("Init: Ln=%d  Rn=%d  k=%d  lmin=%d  lmax=%d  rmin=%d  rmax=%d\n", Ln, Rn, k, lmin, lmax, rmin, rmax); R_FlushConsole();
  if (lmax == 0 || lmin == Ln){
    ret.l = lmin;
    ret.r = rmax;
    return ret;
  }
  if (rmax == 0 || rmin == Rn){
    ret.l = lmax;
    ret.r = rmin;
    return ret;
  }
  IndexT l = lmin;
  while((lmin+1) < lmax){
    l = lmin + (lmax-lmin) / 2;
    //Rprintf("loop: lmin=%d l=%d lmax=%d\n", lmin, l, lmax); R_FlushConsole();
    if (LE(L[l-1], R[k - l])){
      lmin = l;
    }else{
      lmax = l;
    }
  }
  assert((lmin+1) == lmax);
  //Rprintf("converged: lmin=%d l=%d lmax=%d\n", lmin, l, lmax); R_FlushConsole();
  if (l==lmax){
    // last step was narrowing from right: try the left remaining border
    if (GT(L[l - 1], R[k - l - 1])){
      l = lmin;
    }
  }else{
    // last step was narrowing from left: try the right remaining border
    if (LE(L[lmax - 1], R[k - l - 1])){
      l = lmax;
    }
  }
  ret.l = l;
  ret.r = k - l;
  //Rprintf("final: l=%d r=%d\n", ret.l, ret.r); R_FlushConsole();
  return ret;
}

/* /b\\ */
static Ntile2Struct Ntile2_asc_revasc_left_to_right(ValueT *L, ValueT *R, IndexT Ln, IndexT Rn, IndexT k){
  assert( k <= Ln+Rn );
  Ntile2Struct ret;
  IndexT lmax = MIN(k, Ln);
  IndexT rmax = MIN(k, Rn);
  IndexT lmin = k - rmax;
  IndexT rmin = k - lmax;
  //Rprintf("Init: Ln=%d  Rn=%d  k=%d  lmin=%d  lmax=%d  rmin=%d  rmax=%d\n", Ln, Rn, k, lmin, lmax, rmin, rmax); R_FlushConsole();
  if (lmax == 0 || lmin == Ln){
    ret.l = lmin;
    ret.r = rmax;
    return ret;
  }
  if (rmax == 0 || rmin == Rn){
    ret.l = lmax;
    ret.r = rmin;
    return ret;
  }
  IndexT l = lmin;
  /* Ln--; */ Rn--;
  while((lmin+1) < lmax){
    l = lmin + (lmax-lmin) / 2;
    //Rprintf("loop: lmin=%d l=%d lmax=%d\n", lmin, l, lmax); R_FlushConsole();
    if (LE(L[l-1], R[Rn - (k - l)])){
      lmin = l;
    }else{
      lmax = l;
    }
  }
  assert((lmin+1) == lmax);
  //Rprintf("converged: lmin=%d l=%d lmax=%d\n", lmin, l, lmax); R_FlushConsole();
  if (l==lmax){
    // last step was narrowing from right: try the left remaining border
    if (GT(L[l - 1], R[Rn - (k - l - 1)])){
      l = lmin;
    }
  }else{
    // last step was narrowing from left: try the right remaining border
    if (LE(L[lmax - 1], R[Rn - (k - l - 1)])){
      l = lmax;
    }
  }
  ret.l = l;
  ret.r = k - l;
  //Rprintf("final: l=%d r=%d\n", ret.l, ret.r); R_FlushConsole();
  return ret;
}


/* //b/ */
static Ntile2Struct Ntile2_asc_asc_right_to_left(ValueT *L, ValueT *R, IndexT Ln, IndexT Rn, IndexT k){
  assert( k <= Ln+Rn );
  Ntile2Struct ret;
  IndexT lmax = MIN(k, Ln);
  IndexT rmax = MIN(k, Rn);
  IndexT lmin = k - rmax;
  IndexT rmin = k - lmax;
  //Rprintf("Init: Ln=%d  Rn=%d  k=%d  lmin=%d  lmax=%d  rmin=%d  rmax=%d\n", Ln, Rn, k, lmin, lmax, rmin, rmax); R_FlushConsole();
  if (lmax == 0 || lmin == Ln){
    ret.l = lmin;
    ret.r = rmax;
    return ret;
  }
  if (rmax == 0 || rmin == Rn){
    ret.l = lmax;
    ret.r = rmin;
    return ret;
  }
  IndexT r = rmin;
  Ln--; Rn--;
  while((rmin+1) < rmax){
    r = rmin + (rmax-rmin) / 2;
    //Rprintf("loop: rmin=%d r=%d rmax=%d\n", rmin, r, rmax); R_FlushConsole();
    if (LE(L[Ln - (k - r)], R[Rn - (r - 1)])){
      rmin = r;
    }else{
      rmax = r;
    }
  }
  assert((rmin+1) == rmax);
  //Rprintf("converged: rmin=%d r=%d rmax=%d\n", rmin, r, rmax); R_FlushConsole();
  if (r==rmax){
    // last step was narrowing from right: try the left remaining border
    if (GT(L[Ln - (k - r - 1)], R[Rn - (r - 1)])){
      r = rmin;
    }
  }else{
    // last step was narrowing from left: try the right remaining border
    if (LE(L[Ln - (k - r - 1)], R[Rn - (rmax - 1)])){
      r = rmax;
    }
  }
  ret.l = k - r;
  ret.r = r;
  //Rprintf("final: l=%d r=%d\n", ret.l, ret.r); R_FlushConsole();
  return ret;
}


/* //b\ */
static Ntile2Struct Ntile2_asc_revasc_right_to_left(ValueT *L, ValueT *R, IndexT Ln, IndexT Rn, IndexT k){
  assert( k <= Ln+Rn );
  Ntile2Struct ret;
  IndexT lmax = MIN(k, Ln);
  IndexT rmax = MIN(k, Rn);
  IndexT lmin = k - rmax;
  IndexT rmin = k - lmax;
  //Rprintf("Init: Ln=%d  Rn=%d  k=%d  lmin=%d  lmax=%d  rmin=%d  rmax=%d\n", Ln, Rn, k, lmin, lmax, rmin, rmax); R_FlushConsole();
  if (lmax == 0 || lmin == Ln){
    ret.l = lmin;
    ret.r = rmax;
    return ret;
  }
  if (rmax == 0 || rmin == Rn){
    ret.l = lmax;
    ret.r = rmin;
    return ret;
  }
  IndexT r = rmin;
  Ln--; /* Rn--; */
  while((rmin+1) < rmax){
    r = rmin + (rmax-rmin) / 2;
    //Rprintf("loop: rmin=%d r=%d rmax=%d\n", rmin, r, rmax); R_FlushConsole();
    if (LE(L[Ln - (k - r)], R[r - 1])){
      rmin = r;
    }else{
      rmax = r;
    }
  }
  assert((rmin+1) == rmax);
  //Rprintf("converged: rmin=%d r=%d rmax=%d\n", rmin, r, rmax); R_FlushConsole();
  if (r==rmax){
    // last step was narrowing from right: try the left remaining border
    if (GT(L[Ln - (k - r - 1)], R[r - 1])){
      r = rmin;
    }
  }else{
    // last step was narrowing from left: try the right remaining border
    if (LE(L[Ln - (k - r - 1)], R[rmax - 1])){
      r = rmax;
    }
  }
  ret.l = k - r;
  ret.r = r;
  //Rprintf("final: l=%d r=%d\n", ret.l, ret.r); R_FlushConsole();
  return ret;
}

#endif
