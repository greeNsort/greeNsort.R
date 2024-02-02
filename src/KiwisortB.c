/*
# greeNsort Kiwisort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "randIndex.h"
#include <stdbool.h>
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif


// make sure the data dat is in location ori and undo reversals if needed
static void KiwisortB_unwind(ValueT *ori, ValueT *dat, IndexT reversals, IndexT l, IndexT r){
  ValueT t;
  if (dat==ori){
    if (reversals % 2){
      while (l<r){
        SWAP(ori[l], ori[r], t);
        l++;
        r--;
      }
    }
  }else{ // dat!=ori
    if (reversals % 2){
      for (reversals=r; l <= reversals; l++,r--) // misusing reversals as stopper
        ori[l] = dat[r];
    }else{
      for (;l<=r;l++)
        ori[l] = dat[l];
    }
  }
}


// ascending partitioning of data between l and r around pivot
// two partitions are written from wingtips to body of bird
// returns 1 if partitioning is done (due to all ties)


static IndexT KiwisortB_partition_TieRight(
    ValueT *dat  // pointer to memory with current source data now
  , ValueT *buf  // pointer to memory for buffer (receives target data)
  , IndexT l     // leftmost position to be partitioned
  , IndexT r     // rightmost position to be partitioned
  , ValueT v     // pivot value
  , IndexT *c    // pointer to K counters
)
{
  IndexT done;
  IndexT i;             // source position
  IndexT w[2] = {l, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  // DIET loop
  for (i=l; i<=r; i++){
    if (EQ(dat[i], v))
      buf[(w[1])--] = dat[i]; // ties to other side
    else
      break;
  }
  if (i<=r){
    // MAIN loop
    for (; i<=r; i++){
      b = GE(dat[i], v);
      buf[(w[b])] = dat[i];
      w[b] += d[b];
    }
    done = 0;
  }else{
    done = 1;
  }
  // determine counts afterwards as a side-product of partitioning
  c[0] = (w[0]) - l;
  c[1] = r - (w[1]);
  return(done);
}
static IndexT KiwisortB_partition_TieLeft(
    ValueT *dat  // pointer to memory with current source data now
  , ValueT *buf  // pointer to memory for buffer (receives target data)
  , IndexT l     // leftmost position to be partitioned
  , IndexT r     // rightmost position to be partitioned
  , ValueT v     // pivot value
  , IndexT *c    // pointer to K counters
)
{
  IndexT done;
  IndexT i;           // source position
  IndexT w[2] = {l, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  // DIET loop
  for (i=l; i<=r; i++){
    if (EQ(dat[i], v))
      buf[(w[0])++] = dat[i];
    else
      break;
  }
  if (i<=r){
    // MAIN loop
    for (; i<=r; i++){
      b = GT(dat[i], v);
      buf[(w[b])] = dat[i];
      w[b] += d[b];
    }
    done = 0;
  }else{
    done = 1;
  }
  // determine counts afterwards as a side-product of partitioning
  c[0] = (w[0]) - l;
  c[1] = r - (w[1]);
  return(done);
}



static void KiwisortB_rec_TieLeft(
  ValueT *ori
, ValueT *dat
, ValueT *buf
, IndexT l
, IndexT r
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
);



static void KiwisortB_rec_TieRight(
    ValueT *ori   // pointer to original (equals dat or buf)
  , ValueT *dat   // pointer to memory with current source data now
  , ValueT *buf   // pointer to memory for buffer (receives target data)
  , IndexT l         // leftmost position to be sorted
  , IndexT r         // rightmost position to be sorted
  , IndexT reversals // number of direction reversals (needed to recover stable ties)
){
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    KiwisortB_unwind(ori, dat, reversals, l, r);
    Insertionsort_l2r(ori, l, r);
    return;
  }
#endif
  ValueT v;
  IndexT c[2];
  IndexT done;
  v = dat[l + randIndex(r-l+1)];
  // counts generated during partitioning:  DIET_count_TieRight(aux, l, r, v, c);
  done = KiwisortB_partition_TieRight(dat, buf, l, r, v, c);
  if (done || c[0]==1)
    KiwisortB_unwind(ori, buf, reversals, l, r-c[1]);
  else if (c[0]>1)
    KiwisortB_rec_TieLeft(ori, buf, dat, l, r-c[1], reversals);
  reversals++;
  if (done || c[1]==1)
    KiwisortB_unwind(ori, buf, reversals, l+c[0], r);
  else if (c[1]>1)
    KiwisortB_rec_TieLeft(ori, buf, dat, l+c[0], r, reversals);
}


static void KiwisortB_rec_TieLeft(
    ValueT *ori   // pointer to original (equals dat or buf)
  , ValueT *dat   // pointer to memory with current source data now
  , ValueT *buf   // pointer to memory for buffer (receives target data)
  , IndexT l         // leftmost position to be sorted
  , IndexT r         // rightmost position to be sorted
  , IndexT reversals // number of direction reversals (needed to recover stable ties)
){
#if INSERTIONSORT_LIMIT > 0
  if ((r-l) < INSERTIONSORT_LIMIT){
    KiwisortB_unwind(ori, dat, reversals, l, r);
    Insertionsort_l2r(ori, l, r);
    return;
  }
#endif
  ValueT v;
  IndexT c[2];
  IndexT done;
  v = dat[r - randIndex(r-l+1)];
  // counts generated during partitioning:  DIET_count_TieLeft(aux, l, r, v, c);
  done = KiwisortB_partition_TieLeft(dat, buf, l, r, v, c);
  if (done || c[0]==1)
    KiwisortB_unwind(ori, buf, reversals, l, r-c[1]);
  else if (c[0]>1)
    KiwisortB_rec_TieRight(ori, buf, dat, l, r-c[1], reversals);
  reversals++;
  if (done || c[1]==1)
    KiwisortB_unwind(ori, buf, reversals, l+c[0], r);
  else if (c[1]>1)
    KiwisortB_rec_TieRight(ori, buf, dat, l+c[0], r, reversals);
}


void KiwisortB_insitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT *buf;
    buf = (ValueT *) MALLOC(n, ValueT);
    KiwisortB_rec_TieRight(
      x   // ValueT *ori   // pointer to original (equals dat or buf)
    , x   // ValueT *dat   // pointer to memory with current source data now
    , buf // ValueT *buf   // pointer to memory for buffer (receives target data)
    , 0   // IndexT l         // leftmost position to be sorted
    , n-1 // IndexT r         // rightmost position to be sorted
    , 0   // IndexT reversals // number of direction reversals (needed to recover stable ties)
    );
    FREE(buf);
  }
}


void KiwisortB_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    IndexT i;
    ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
    ValueT *aux2 = aux + n;
    for (i=0; i<n; i++)
        aux[i] = x[i];
    KiwisortB_rec_TieRight(
      aux    // ValueT *ori   // pointer to original (equals dat or buf)
    , aux    // ValueT *dat   // pointer to memory with current source data now
    , aux2   // ValueT *buf   // pointer to memory for buffer (receives target data)
    , 0      // IndexT l         // leftmost position to be sorted
    , n-1    // IndexT r         // rightmost position to be sorted
    , 0      // IndexT reversals // number of direction reversals (needed to recover stable ties)
    );
    for (i=0; i<n; i++)
        x[i] = aux[i];
    FREE(aux);
  }
}
