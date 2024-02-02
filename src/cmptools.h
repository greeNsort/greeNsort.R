/*
# greeNsort comparison headers
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_cmptools_h
#define ALREADY_DEFINED_cmptools_h

#include "config.h"

inline static int dblcmp(double A ,double B){
  if (A<B)
    return(-1);
  else if (B<A)
    return(1);
  else
    return(0);
}

inline static double stable_dblkey(double x){
  if (x<0)
    return(ceil(x));
  else if (x>0)
    return(floor(x));
  else
    return 0;
}

inline static void stable_strkey(char *trg, char *src){
  int i;
  i=0;
  while(src[i] && src[i] != '.'){
    trg[i] = src[i];
    i++;
  }
  trg[i] = '\0';
}

inline static int stable_dblcmp(double A ,double B){
  double a,b;
  a = stable_dblkey(A);
  b = stable_dblkey(B);
  return dblcmp(a,b);
}

inline static int stable_strcmp(char *A ,char *B){
  char a[21];
  char b[21];
  if (strlen(A)>20)
    error("stable_strcmp A is longer than 20: %d", strlen(A));
  if (strlen(B)>20)
    error("stable_strcmp B is longer than 20: %d", strlen(B));
  stable_strkey(a,A);
  stable_strkey(b,B);
#ifdef USE_LOCALE
  return strcoll(a,b);
#else
  return strcmp(a,b);
#endif
}

// for stable test we do all comparisons ignoring the decimals
// With KEY the sorting algorithm does not know anything about the decimals
#ifdef STABLE_TEST
//#define KEY(x)( (x < 0.0) ? ceil(x) : floor(x))
#define KEY(x)( stable_dblkey(x) )
#else
#define KEY(x)(x)
#endif

// we do not use the inefficient 3-way CMP
#define LT(A,B) (KEY(A) < KEY(B))
#define LE(A,B) (KEY(A) <= KEY(B))
#define GT(A, B) LT((B), (A))
#define GE(A, B) LE((B), (A))
#define EQ(A,B) (KEY(A) == KEY(B))
#define NE(A,B) (KEY(A) != KEY(B))

#ifdef STABLE_TEST
#define CMP(x,y)( stable_dblcmp(x,y) )
//  inline int stable_strcmp(char *A ,char *B);
#define STRCMP(A,B)stable_strcmp(A,B)
#else
#define CMP(x,y)( LT(x,y) ? -1 : (GT(x,y) ? 1 : 0) )
#ifdef USE_LOCALE
#define STRCMP(A,B)strcoll(A,B)
#else
#define STRCMP(A,B)strcmp(A,B)
#endif
#endif




#define MIN(A,B) (A<B ? A : B)
#define MAX(A,B) (A>B ? A : B)

// MOVE is not systematically used so far, just where SWAP is needed
#define MOVE(TO,FROM) TO=FROM
#define MMOVE(TO,FROM,M) memcpy(TO, FROM, sizeof(IntValueT)*M)
//#define SWAP(A,B,t) {Rprintf("%f <-> %f\n", A, B); MOVE(t,A); MOVE(A,B); MOVE(B,t);}
#define SWAP(A,B,t) {MOVE(t,A); MOVE(A,B); MOVE(B,t);}
// not used
#define COMPSWAP(A,B,t) if (LT(B,A)) SWAP(A,B,t)
#define COMPSWAPi(A,B,t,Ai,Bi,ti) if (LT(B,A)) {SWAP(A,B,t) SWAP(Ai,Bi,ti)}

#endif
