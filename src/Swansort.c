/*
# greeNsort Swansort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "SZackPart2Split.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// Trick: ties always to other side

// ascending partitioning of data between l and r around pivot
// returns 1 if all values are the same (tie)
static IndexT Swansort_partition_LeftTieRight(
ValueT *x    // pointer to data
, IndexT l      // leftmost position to be sorted
, IndexT n      // number of elements
, ValueT v   // pivot value
, IndexT *c     // pointer to K counters: OUTPUT, these do not come initialized but are returned
)
{
  IndexT r = l + n + n - 1;  // rightmost buffer position
  IndexT il=l, ir=r;  // wing positions to write to
  IndexT i, is, done;
  // DIET loop
  for (i=l, is=l+n; i<is; i++){
    if (EQ(x[i], v)){
      x[ir--] = x[i];  // ties to other side
    }else{
      break;
    }
  }
  if (i<is){
    // MAIN loop
    for (; i<is; i++){
      if (LT(x[i], v)){
        x[il++] = x[i];
      }else{
        x[ir--] = x[i];  // ties to other side
      }
    }
    done = 0;
  }else{
    done = 1;
  }
  // determine counts afterwards as a side-product of partitioning
  c[0] = il - l;
  c[1] = r - ir;
  return(done);
}

// ascending partitioning of data between l and r around pivot
// returns 1 if all values are the same (tie)
static IndexT Swansort_partition_RightTieLeft(
ValueT *x    // pointer to data
, IndexT r      // rightmost position to be sorted
, IndexT n      // number of elements
, ValueT v   // pivot value
, IndexT *c     // pointer to K counters: OUTPUT, these do not come initialized but are returned
)
{
  IndexT l = r - n - n + 1;  // rightmost buffer position
  IndexT il=l, ir=r;  // wing positions to write to
  IndexT i, is, done;
  // DIET loop
  for (i=r, is = r-n; i>is; i--){
    if (EQ(x[i], v)){
      x[il++] = x[i];  // ties to other side
    }else{
      break;
    }
  }
  if (i>is){
    // main LOOP
    for (; i>is; i--){
      if (LE(x[i], v)){
        x[il++] = x[i];  // ties to other side
      }else{
        x[ir--] = x[i];
      }
    }
    done = 0;
  }else{
    done = 1;
  }
    // determine counts afterwards as a side-product of partitioning
  c[0] = il-l;
  c[1] = r-ir;
  return(done);
}


static void Swansort_unwind_RightTieLeft(
  ValueT *x      // data + buffer
, IndexT ll         // offset relativ to x
, IndexT r          // rightmost value in x for unwinding
, IndexT n          // number of values for unwinding
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
){
    if (reversals % 2){
      n += ll; // misuse n as stopper
      while(ll < n){
        x[ll++] = x[r--];
      }
    }else{
      r -= n;  // misuse r as l-1
      n += ll; // misuse n as stopper
      while(ll < n){
        x[ll++] = x[++r];
      }
    }
}

static void Swansort_unwind_LeftTieRight(
  ValueT *x      // data + buffer
, IndexT ll         // offset relativ to x
, IndexT l          // leftmost value in x for unwinding
, IndexT n          // number of values for unwinding
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
){
    if (reversals % 2){
      l += n;  // misuse l as r+1
      n += ll; // misuse n as stopper
      while(ll < n){
        x[ll++] = x[--l];
      }
    }else{
      n += ll; // misuse n as stopper
      while(ll < n){
        x[ll++] = x[l++];
      }
    }
}


static void Swansort_rec_RightTieLeft(
  ValueT *x      // data + buffer
, IndexT ll         // offset relativ to x
, IndexT r          // rightmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
);

static void Swansort_rec_LeftTieRight(
  ValueT *x      // data + buffer
, IndexT ll         // offset relativ to x
, IndexT l          // leftmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Swansort_unwind_LeftTieRight(x, ll, l, n, reversals);
    Insertionsort_l2r(x, ll, ll+n-1);
    return;
  }
#endif
  IndexT r = l + n + n - 1;
  ValueT v;
  IndexT c[2];
  IndexT done;
  v = x[l + randIndex(n)];
  // counts generated during partitioning:  DIET_count_LeftTieRight(x, l, r, v, c);
  done = Swansort_partition_LeftTieRight(x, l, n, v, c);  // reverses right wing
  // left wing
  if (done || c[0]==1)
    Swansort_unwind_LeftTieRight(x, ll, l, c[0], reversals);
  else if(c[0]>1)
    Swansort_rec_LeftTieRight(x, ll, l, c[0], reversals);
  // reverse
  reversals++;
  // right wing
  if (done || c[1]==1)
    Swansort_unwind_RightTieLeft(x, ll+c[0], r, c[1], reversals);
  else if (c[1]>1)
    Swansort_rec_RightTieLeft(x, ll+c[0], r, c[1], reversals);
}

static void Swansort_rec_RightTieLeft(
  ValueT *x      // data + buffer
, IndexT ll         // offset relativ to x
, IndexT r          // rightmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
){
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    Swansort_unwind_RightTieLeft(x, ll, r, n, reversals);
    Insertionsort_l2r(x, ll, ll+n-1);
    return;
  }
#endif
  IndexT l = r - n - n + 1;
  ValueT v;
  IndexT c[2];
  IndexT done;
  v = x[r - randIndex(n)];
  // counts generated during partitioning:  DIET_count_RightTieLeft(x, l, r, v, c);
  done = Swansort_partition_RightTieLeft(x, r, n, v, c); // reverses left wing
  // reverse
  reversals++;
  // left wing
  if (done || c[0]==1)
    Swansort_unwind_LeftTieRight(x, ll, l, c[0], reversals);
  else if(c[0]>1 && !done)
    Swansort_rec_LeftTieRight(x, ll, l, c[0], reversals);
  reversals--;
  // right wing
  if (done || c[1]==1)
    Swansort_unwind_RightTieLeft(x, ll+c[0], r, c[1], reversals);
  else if (c[1]>1)
    Swansort_rec_RightTieLeft(x, ll+c[0], r, c[1], reversals);
}



void Swansort_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT *aux;
    aux = (ValueT *) MALLOC(2*n, ValueT);
    for (IndexT i=0;i<n;i++)
      aux[i] = x[i];
    Swansort_rec_LeftTieRight(
        aux    // ValueT *x      // data + buffer
      , 0    // IndexT ll         // offset relativ to x
      , 0    // IndexT l          // leftmost value to be sorted
      , n    // IndexT n          // number of values to be sorted
      , 0    // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
    );
    for (IndexT i=0;i<n;i++)
      x[i] = aux[i];
    FREE(aux);
  }
}

// Median-Finding for pure partitioning even at the top-split us too expensive
void Swansort_insitu_strict_partition(
    ValueT *x
  , IndexT n
){
  if (n>1){
    IndexT nl = n/2;
    IndexT nr = n-nl;
    ValueT *aux;
    aux = (ValueT *) MALLOC(2*nr, ValueT);
    // put lower nl left in x and the upper nr left in aux
    SZackPart2Split(x, aux, 0, n-1, nl-1);
    // TODO further tuning opportunity: use SZackPart3Split, exploit return value of SPart and place median ties directly into final position
    // move ties from left partition into final position
    Swansort_rec_LeftTieRight(
      x      // ValueT *x      // data + buffer
      , 0    // IndexT ll         // offset relativ to x
      , 0    // IndexT l          // leftmost value to be sorted
      , nl   // IndexT n          // number of values to be sorted
      , 0    // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
    );
    Swansort_rec_LeftTieRight(
      aux    // ValueT *aux    // data + buffer
      , 0    // IndexT ll         // offset relativ to x
      , 0    // IndexT l          // leftmost value to be sorted
      , nr   // IndexT n          // number of values to be sorted
      , 0    // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
    );
    x+= nl;
    for (IndexT i=0;i<nr;i++)
      x[i] = aux[i];
    x-=nl;
    FREE(aux);
  }
}

static void Frogmerge_asc_left_final(
    ValueT *x
  , ValueT *ldat  // pointer to left element of target stream and left stream
  , IndexT nl     // number of values in left stream
  , ValueT *rdat  // pointer to left element of right stream
  , IndexT nr     // number of values in right stream
)
{
  const IndexT ll = 0;
  IndexT lr = nl - 1;
  const IndexT rl = 0;
  IndexT rr = nr - 1;
  nr = nl+nr-1; // re-using nr as write-index
  ValueT u=ldat[lr], v=rdat[rr];
  for(;;){
    if (GT(u, v)){
      x[nr--] = u; lr--;
      if (lr<ll)
        break;
      u=ldat[lr];
    }else{
      x[nr--] = v; rr--;
      if (rr<rl)
        return;
      v=rdat[rr];
    }
  }
  while(rl<=rr){
    x[nr--] = rdat[rr--];
  }
  return;
}


// hence at the top we use semi-inplace merge
void Swansort_insitu(
    ValueT *x
  , IndexT n
){
  if (n>1){
    IndexT nl = n/2;
    IndexT nr = n-nl;
    IndexT i;
    ValueT *aux;
    aux = (ValueT *) MALLOC(2*nr, ValueT);
    // put lower nl left in x and the upper nr left in aux
    x+= nl;
    for (i=0;i<nr;i++)
      aux[i] = x[i];
    x-=nl;
    Swansort_rec_LeftTieRight(
        x    // ValueT *x      // data + buffer
      , 0    // IndexT ll         // offset relativ to x
      , 0    // IndexT l          // leftmost value to be sorted
      , nl   // IndexT n          // number of values to be sorted
      , 0    // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
    );
    Swansort_rec_LeftTieRight(
        aux  // ValueT *aux    // data + buffer
      , 0    // IndexT ll         // offset relativ to x
      , 0    // IndexT l          // leftmost value to be sorted
      , nr   // IndexT n          // number of values to be sorted
      , 0    // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
    );
    Frogmerge_asc_left_final(
        x       // pointer left element of target
      , x       // pointer to left element of left stream
      , nl      // number of values in left stream
      , aux     // pointer to left element of right stream
      , nr      // number of values in right stream
    );
    // x+= nl;
    // for (i=0;i<nr;i++)
    //   x[i] = aux[i];
    // x-=nl;
    FREE(aux);
  }
}
