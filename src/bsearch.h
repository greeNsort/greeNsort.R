/*
# greeNsort binary search
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_bsearch_h
#define ALREADY_DEFINED_bsearch_h

#include "algo.h"

// TORIGHT is TOLEFT: manually mirrored
// DESC is ASC: replacing LT with GT

// --- standard (wikipedia) binary and exponential search ---

/* Wikipedias Pseudocode version
function binary_search_leftmost(A, n, T):
  L := 0
  R := n
  while L < R:
    m := floor((L + R) / 2)
    if A[m] < T:
      L := m + 1
    else:
      R := m
      return L

 function binary_search_rightmost(A, n, T):
   L := 0
 R := n
   while L < R:
     m := floor((L + R) / 2)
     if A[m] > T:
       R := m
       else:
         L := m + 1
       return R - 1
 */


// binary search leftmost
#define BSEARCH_ASC_WIKILEFT(data, l, r, value)                      \
{                                                                    \
  IndexT m;                                                          \
  r++;                                                               \
  while (l<r){                                                       \
    m = (l + r) / 2;                                                 \
    if (LT(data[m], value))                                          \
      l = m + 1;                                                     \
    else                                                             \
      r = m;                                                         \
  }                                                                  \
}                                                                    \

// binary search rightmost
#define BSEARCH_ASC_WIKIRIGHT(data, l, r, value)                              \
{                                                                             \
  IndexT m;                                                                   \
  r++;                                                                        \
  while (l<r){                                                                \
    m = (l + r) / 2;                                                          \
    if (LT(value, data[m]))                                                   \
      r = m;                                                                  \
    else                                                                      \
      l = m + 1;                                                              \
  }                                                                           \
}                                                                             \

// binary search leftmost
#define BSEARCH_DESC_WIKILEFT(data, l, r, value)                     \
{                                                                    \
  IndexT m;                                                          \
  r++;                                                               \
  while (l<r){                                                       \
    m = (l + r) / 2;                                                 \
    if (GT(data[m], value))                                          \
      l = m + 1;                                                     \
    else                                                             \
      r = m;                                                         \
  }                                                                  \
}                                                                    \

// binary search rightmost
#define BSEARCH_DESC_WIKIRIGHT(data, l, r, value)                              \
{                                                                              \
  IndexT m;                                                                    \
  r++;                                                                         \
  while (l<r){                                                                 \
    m = (l + r) / 2;                                                           \
    if (GT(value, data[m]))                                                    \
      r = m;                                                                   \
    else                                                                       \
      l = m + 1;                                                               \
  }                                                                            \
}                                                                              \


// --- greeNsort (mirrored) binary and exponential search ---

// binary search leftmost data GE value in l..r
// if all data LT value returns r
#define BSEARCH_ASC_TOLEFT(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (LT(data[m], value))     \
      l = m + 1; \
    else \
      r = m; \
  } \
} \

// binary search rightmost data LE value in l..r
// if all data GT value returns l
#define BSEARCH_ASC_TORIGHT(data, l, r, value)                              \
{                                                                           \
  IndexT m;                                                                 \
  while (l<r){                                                              \
    m = r - ((r - l) / 2);                                                  \
    if (LT(value, data[m]))                                                 \
      r = m - 1;                                                            \
    else                                                                    \
      l = m;                                                                \
  }                                                                         \
}                                                                           \


// binary search leftmost data LE value in l..r
// if all data GT value returns r
#define BSEARCH_DESC_TOLEFT(data, l, r, value) \
{ \
IndexT m; \
  while (l<r){ \
    m = l + ((r - l) / 2); \
    if (GT(data[m], value)) \
      l = m + 1; \
    else \
      r = m; \
  } \
}
// binary search rightmost data GE value in l..r
// if all data LT value returns l
#define BSEARCH_DESC_TORIGHT(data, l, r, value)                              \
{                                                                           \
  IndexT m;                                                                 \
  while (l<r){                                                              \
    m = r - ((r - l) / 2);                                                  \
    if (GT(value, data[m]))                                                 \
      r = m - 1;                                                            \
    else                                                                    \
      l = m;                                                                \
  }                                                                         \
}                                                                           \



#define BSEARCH_ASC_WL(data, l, r, value, ret)   \
BSEARCH_ASC_WIKILEFT(data, l, r, value)          \
  ret l;                                         \

#define BSEARCH_ASC_WR(data, l, r, value, ret)   \
BSEARCH_ASC_WIKIRIGHT(data, l, r, value)         \
  ret r-1;                                       \

#define BSEARCH_ASC_L(data, l, r, value, ret)    \
BSEARCH_ASC_TOLEFT(data, l, r, value)            \
  ret l;                                         \

#define BSEARCH_ASC_R(data, l, r, value, ret)    \
BSEARCH_ASC_TORIGHT(data, l, r, value)           \
  ret l;                                         \



#define BSEARCH_DESC_WL(data, l, r, value, ret)   \
BSEARCH_DESC_WIKILEFT(data, l, r, value)          \
  ret l;                                          \

#define BSEARCH_DESC_WR(data, l, r, value, ret)   \
BSEARCH_DESC_WIKIRIGHT(data, l, r, value)         \
  ret r-1;                                        \

#define BSEARCH_DESC_L(data, l, r, value, ret)    \
BSEARCH_DESC_TOLEFT(data, l, r, value)            \
  ret l;                                          \

#define BSEARCH_DESC_R(data, l, r, value, ret)    \
BSEARCH_DESC_TORIGHT(data, l, r, value)           \
  ret l;                                          \



// binary search for leftmost EQ
// if none is equal returns -1
#define BSEARCH_ASC_EQL(data, l, r, value, ret) \
BSEARCH_ASC_TOLEFT(data, l, r, value)          \
  if (EQ(value, data[l]))                      \
    ret l;                                     \
  else                                         \
    ret -1;                                    \

// binary search for rightmost EQ
// if none is equal returns -1
#define BSEARCH_ASC_EQR(data, l, r, value, ret) \
  BSEARCH_ASC_TORIGHT(data, l, r, value)       \
  if (EQ(value, data[l]))                      \
    ret l;                                     \
  else                                         \
    ret -1;                                    \

// binary search leftmost data GE value in l..r
// if all data LT value returns r+1
#define BSEARCH_ASC_GE(data, l, r, value, ret) \
  BSEARCH_ASC_TOLEFT(data, l, r, value) \
  if (LT(data[l], value)) \
    ret r+1; \
  else \
    ret l;   \

// binary search rightmost data LE value in l..r
// if all data GT value returns l-1
#define BSEARCH_ASC_GT(data, l, r, value, ret) \
  BSEARCH_ASC_TORIGHT(data, l, r, value) \
  if (LT(value, data[l])) \
    ret l;   \
  else \
    ret r+1; \


// binary search rightmost data LE value in l..r
// if all data GT value returns l-1
#define BSEARCH_ASC_LE(data, l, r, value, ret) \
  BSEARCH_ASC_TORIGHT(data, l, r, value) \
  if (LT(value, data[l])) \
    ret l-1; \
  else \
    ret r;   \

#define BSEARCH_ASC_LT(data, l, r, value, ret) \
  BSEARCH_ASC_TOLEFT(data, l, r, value) \
  if (LT(data[l], value)) \
    ret r;   \
  else \
    ret l-1; \






#define BSEARCH_DESC_EQL(data, l, r, value, ret) \
  BSEARCH_DESC_TOLEFT(data, l, r, value)        \
    if (LT(value, data[l]))                     \
      ret -1;                                   \
    else if (LT(data[l], value))                \
      ret -1;                                   \
    else                                        \
      ret l;                                    \

#define BSEARCH_DESC_EQR(data, l, r, value, ret) \
    BSEARCH_DESC_TORIGHT(data, l, r, value) \
      if (LT(value, data[l]))              \
        ret -1;                            \
      else if (LT(data[l], value))         \
        ret -1;                            \
      else                                 \
        ret l;                             \

#define BSEARCH_DESC_GE(data, l, r, value, ret) \
  BSEARCH_DESC_TORIGHT(data, l, r, value) \
  if (LT(data[l], value)) \
    ret l-1; \
  else \
    ret l;   \

#define BSEARCH_DESC_GT(data, l, r, value, ret) \
  BSEARCH_DESC_TOLEFT(data, l, r, value) \
  if (LT(value, data[l])) \
    ret l;   \
  else \
    ret l-1; \


#define BSEARCH_DESC_LE(data, l, r, value, ret) \
  BSEARCH_DESC_TOLEFT(data, l, r, value) \
 if (LT(value, data[l])) \
    ret r+1; \
  else \
    ret l;   \


#define BSEARCH_DESC_LT(data, l, r, value, ret) \
  BSEARCH_DESC_TORIGHT(data, l, r, value) \
  if (LT(data[l], value)) \
    ret l;   \
  else \
    ret r+1; \


/*
IndexT bsearch_asc_WL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_WR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_L(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_R(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT bsearch_desc_WL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_WR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_L(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_R(ValueT *data, IndexT l, IndexT r, ValueT value);


IndexT bsearch_asc_EQL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_EQR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT bsearch_desc_EQL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_EQR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);
*/

#endif
