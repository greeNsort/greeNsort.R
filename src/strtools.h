/*
# greeNsort string tools
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_strtools_h
#define ALREADY_DEFINED_strtools_h

#include "algo.h"

#define NULL_CHAR '\0'
#define BUFFER_CHAR '\1'

// -- symmetric search --
// find null LE of i
inline static IndexT nullLE(char *x, IndexT i){
  while(x[i])
    --i;
  return i;
}
// find null GE of i
inline static IndexT nullGE(char *x, IndexT i){
  while(x[i])
    ++i;
  return i;
}
// find null LT i
inline static IndexT nullLT(char *x, IndexT i){
  while(x[--i]);
  return i;
}
// find null GT i
inline static IndexT nullGT(char *x, IndexT i){
  while(x[++i]);
  return i;
}


// -- asymmetric search NULL is treated as belonging to its string on the left  --
// return first pos of that string
inline static IndexT strbegin(char *x, IndexT i){
  while(((unsigned char)x[--i]) > ((unsigned char)BUFFER_CHAR));
  return i + 1;
}
// return pos of null-terminator that string
inline static IndexT strterm(char *x, IndexT i){
  while(x[i])
    i++;
  return i;
}
// return first pos of previous string
inline static IndexT strprev(char *x, IndexT i){
  while(((unsigned char)x[--i]) >  ((unsigned char)BUFFER_CHAR));
  --i;
  while(((unsigned char)x[--i]) >  ((unsigned char)BUFFER_CHAR));
  return i + 1;
}
// return first pos of next string
inline static IndexT strnext(char *x, IndexT i){
  while(x[i++]);
  return i;
}


// -- asymmetric copy and length  --
// return length of that string *including* null-terminator
inline static IndexT strlen_lr(char *x){
  char *ori = x;
  while(*x++)
    ;
  return x - ori;
}
// return length of that string *including* null-terminator
inline static IndexT strlen_rl(char *x){
  char *ori = x;
  while(((unsigned char)(*--x))  > ((unsigned char)BUFFER_CHAR))
    ;
  return ori - x;
}

// compress multiple {string|buffer} chunks into {strings|buffers}
inline static void strdebuf_lr(char *dst, char *l, char *r){
  for(;l<=r;l++){
    if (*l != BUFFER_CHAR)
      *dst++ = *l;
  }
  // for(;dst<=r;dst++)
  //   *dst = BUFFER_CHAR;
}

// compress multiple {string|buffer} chunks into {buffers|strings}
inline static void strdebuf_rl(char *dst, char *l, char *r){
  for(;l<=r;r--){
    if (*r != BUFFER_CHAR)
      *dst-- = *r;
  }
  // for(;l<=dst;dst--)
  //   *dst = BUFFER_CHAR;
  if (l<=dst)
    *dst = BUFFER_CHAR;
}



// copy from left *and* return the length (for merging)
inline static IndexT strcpylen_lr(char *dst, char *src){
  char *ori = src;
  while((*dst++ = *src++))
    ;
  return (IndexT)(src - ori);
}
// copy from right *and* return the length (for merging)
// dst and src are right (nullterminator) of string
inline static IndexT strcpylen_rl(char *dst, char *src){
  char *ori = src;
  while(((unsigned char)(*dst-- = *src--)) > ((unsigned char)BUFFER_CHAR))
    ;
  return (IndexT)(ori - src);
}

// tuned copy from left using the length *including* null-terminator
inline static void strcpy_lr(char *dst, char *src, IndexT len){
  IndexT i;
  for(i=0;i<len;++i){
    *(dst+i) = *(src+i);
  }
}
// tuned copy from right using the length *including* null-terminator
inline static void strcpy_rl(char *dst, char *src, IndexT len){
  IndexT i;
  for(i=0;i<len;++i){
    *(dst-i) = *(src-i);
  }
}


// copy rightmost to left
inline static void chrcpy_left(char *x, IndexT n, IndexT L, IndexT R){
  char* y = x + L;
  x += (R - n + 1);
  //y[-1] = 0;
  for(L=0;L<n;L++)
    y[L] = x[L];
}
// copy leftmost to right
inline static void chrcpy_right(char *x, IndexT n, IndexT L, IndexT R){
  char* y = x + R - n;
  x += (L - 1);
  for(;n>0;n--)
    y[n] = x[n];
  //y[0] = 0;
}

static int strprint(char* txt, char *x, IndexT n){
  IndexT s;
  IndexT i = 0;
  while(i<n){
    s = strlen_lr(x+i);
    Rprintf("%s i=%d s=%d %s\n", txt, i, s, x+i);
    i +=s;
  }
  return i;
}


static IndexT strcheck(char *x, IndexT n){
  IndexT s;
  IndexT i = 0, j;
  while(i<n){
    s = strlen_lr(x+i);
    i +=s;
    for (j = i - s; j < i; j++)
      if (x[j] == BUFFER_CHAR)
        return j;
  }
  return -1;
}

static void chrprint(char* txt, char *x, IndexT l, IndexT r){
  IndexT i;
  for (i=l-1;i<=r;i++){
    if (x[i]==0)
      Rprintf("%s x[%d]=\\0 ----\n", txt, i);
    else if (x[i]==1)
      Rprintf("%s x[%d]=\\1\n", txt, i);
    else if (x[i]==2)
      Rprintf("%s x[%d]=\\2\n", txt, i);
    else if (x[i]==3)
      Rprintf("%s x[%d]=\\3\n", txt, i);
    else if (x[i]==4)
      Rprintf("%s x[%d]=\\4\n", txt, i);
    else if (x[i]==7)
      Rprintf("%s x[%d]=\\7\n", txt, i);
    else
      Rprintf("%s x[%d]='%c' (%u)\n", txt, i, x[i], (short)x[i]);
  }
}

#endif
