/*
# greeNsort direct Knuthsort for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "VInsertionsort_l2r.h"

static void VKnuthsortP_merge(char *z, char *l, char *m, char *r){
  char *j=m+strnext(m, 0);
  IndexT s;
  //char *LR=m; while(*--LR); if (GT(LR+1,j)) // simple tuning for perfectly presorted
  for (;;){
    if (LT(j, l)){
      s = strcpylen_lr(z, j);
      z += s;
      j += s;
      if (j > r)
        break;
    }else{
      s = strcpylen_lr(z, l);
      z += s;
      l += s;
      if (l > m)
        break;
    }
  }
#if VINSERTIONSORT_LIMIT > 0
  while(l <= m)
    *z++=*l++;
  while(j <= r)
    *z++=*j++;
#else
  while(l <= m){
    s = strcpylen_lr(z, l);
    z += s;
    l += s;
  }
  while(j <= r){
    s = strcpylen_lr(z, j);
    z += s;
    j += s;
  }
#endif
}


// merges the two sorted sequences [l,m] [m+1,r]
static void VKnuthsort_merge(char *tar, char *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l, j=strnext(src, m), k=l;
  IndexT s;
  char *u=src+i, *v=src+j;
  for (;;){
    if (LT(v, u)){
      s = strcpylen_lr(tar+k, v);
      k += s;
      j += s;
      if (j > r)
        break;
      v = src+j;
    }else{
      s = strcpylen_lr(tar+k, u);
      k += s;
      i += s;
      if (i > m)
        break;
      u = src+i;
    }
  }
  while(i <= m){
    s = strcpylen_lr(tar+k, src+i);
    k += s;
    i += s;
  }
  while(j <= r){
    s = strcpylen_lr(tar+k, src+j);
    k += s;
    j += s;
  }
}

static void VKnuthsort_merge_noregister(char *tar, char *src, IndexT l, IndexT m, IndexT r){
  IndexT i=l, j=strnext(src, m), k=l;
  IndexT s;
  for (;;){
    if (LT(src+j, src+i)){
      s = strcpylen_lr(tar+k, src+j);
      k += s;
      j += s;
      if (j > r)
        break;
    }else{
      s = strcpylen_lr(tar+k, src+i);
      k += s;
      i += s;
      if (i > m)
        break;
    }
  }
  while(i <= m){
    s = strcpylen_lr(tar+k, src+i);
    k += s;
    i += s;
  }
  while(j <= r){
    s = strcpylen_lr(tar+k, src+j);
    k += s;
    j += s;
  }
}


static void VKnuthsort_recurse(char *a, char *b, char *c, IndexT l, IndexT r){
  if (l<r){
    IndexT m,i;
#if VINSERTIONSORT_LIMIT > 0
    if ((r-l) < VINSERTIONSORT_LIMIT){
      VInsertionsort_l2r(a, c, l, r);
      return ;
    }
#endif
    //strprint("VKnuthsort_recurse", a+l, r-l);
    m = l + (r-l)/2;
    // first search upwards where we can rely on a null-terminator sentinel
    i = nullGE(b,m);
    if (i<r){ // still strings on both sides of split position
        m = i;
    }else{
      // now search downwards (no sentinel)
      i = nullLT(b,m);
      if (i>=l){ // still strings on both sides of split position
        m = i;
      }else{ // we arrived at a single string
        // Rprintf("leaf=%s\n", b+l);
        return;
      }
    }
    VKnuthsort_recurse(b, a, c, l  , m);
    VKnuthsort_recurse(b, a, c, strnext(a, m), r);
    // almost no difference between pointer and index implementation
    //VKnuthsort_merge_noregister(a, b, l, m, r);
    //VKnuthsort_merge(a, b, l, m, r);
    VKnuthsortP_merge(a+l, b+l, b+m, b+r);
  }
}


void VKnuthsort_insitu(
    char *x   // we have leading sentinel \0 at x[0]
  , IndexT n  // n strings, the first at x+1
  , IndexT m  // m chars including trailing \0 and leading \0
  , IndexT b  // buffer size (of longest string incl. trailing \0)
  , PerfT *p
)
{
  IndexT i;
  char *y = (char *) MALLOC(m, typeof(char));
  char *z = (char *) MALLOC(b, typeof(char));
  // copies also the leading \0 in x[0]
  for (i=0;i<m;i++){
    y[i] = x[i];
  }
  VKnuthsort_recurse(x, y, z, 1, m-1); // excluding leading and including trailing \0
  FREE(z);
  FREE(y);
  p->secs = getNewSecs();
  p->size = ((2*m+b)*sizeof(char)) / (m*(double)sizeof(char));
}

void VKnuthsort_exsitu(
    char *x   // we have leading sentinel \0 at x[0]
  , IndexT n  // n strings, the first at x+1
  , IndexT m  // m chars including trailing \0 and leading \0
  , IndexT b  // buffer size (of longest string incl. trailing \0)
  , PerfT *p
)
{
  IndexT i;
  char *y = (char *) MALLOC(m, typeof(char));
  char *y2 = (char *) MALLOC(m, typeof(char));
  char *z = (char *) MALLOC(b, typeof(char));
  // copies also the leading \0 in x[0]
  for (i=0;i<m;i++){
    y2[i] = y[i] = x[i];
  }
  VKnuthsort_recurse(y, y2, z, 1, m-1); // excluding leading and including trailing \0
  for (i=1;i<m;i++){
    x[i] = y[i];
  }
  FREE(z);
  FREE(y);
  FREE(y2);
  p->secs = getNewSecs();
  p->size = ((2*m+b)*sizeof(char)) / (m*(double)sizeof(char));
}
