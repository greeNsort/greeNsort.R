/*
# greeNsort Storksort B-tuned
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "DIET_count.h"
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

static IndexT StorksortB_initial_partition_Left_LE(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT l      // leftmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v   // pivot value
  , IndexT *c     // pointer to K counters: OUTPUT, these do not come initialized but are returned
)
{
  IndexT r = l + n + n/2 - 1;  // rightmost buffer position
  IndexT i, is, done;
  IndexT w[2] = {l, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  // DIET LOOP
  for (i=l, is=l+n; i<is; i++){
    if (EQ(s[i], v)){
      t[(w[0])++] = s[i];  // ties to other side
    }else{
      break;
    }
  }
  if (i<is){
    // MAIN loop
    for (; i<is; i++){
      b = GT(s[i], v);
      t[(w[b])] = s[i];
      (w[b]) += d[b];
    }
    done = 0;
  }else{
    done = 1;
  }
  // determine counts afterwards as a side-product of partitioning
  c[0] = w[0] - l;
  c[1] = r - w[1];
  return(done);
}


static void StorksortB_partition_Left_LE(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT l      // leftmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {l, l + n + n/2 - 1}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n += l;
  for (i=l; i<n; i++){
    b = GT(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Left_LT(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT l      // leftmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {l, l + n + n/2 - 1}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n += l;
  for (i=l; i<n; i++){
    b = GE(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Left_GE(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT l      // leftmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {l, l + n + n/2 - 1}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n += l;
  for (i=l; i<n; i++){
    b = LT(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Left_GT(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT l      // leftmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {l, l + n + n/2 - 1}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n += l;
  for (i=l; i<n; i++){
    b = LE(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}

static void StorksortB_partition_Right_LE(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT r      // rightmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {r - n - n/2 + 1, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n = r - n;
  for (i=r; i>n; i--){
    b = LE(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Right_LT(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT r      // rightmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {r - n - n/2 + 1, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n = r - n;
  for (i=r; i>n; i--){
    b = LT(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Right_GE(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT r      // rightmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {r - n - n/2 + 1, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n = r - n;
  for (i=r; i>n; i--){
    b = GE(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}
static void StorksortB_partition_Right_GT(
    ValueT *t     // pointer to target
  , ValueT *s     // pointer to source
  , IndexT r      // rightmost position to be sorted
  , IndexT n      // number of elements
  , ValueT v      // pivot value
)
{
  IndexT w[2] = {r - n - n/2 + 1, r}; // wing positions to write to
  IndexT d[2] = {1,-1};
  bool b;
  IndexT i;
  n = r - n;
  for (i=r; i>n; i--){
    b = GT(s[i], v);
    t[(w[b])] = s[i];
    (w[b]) += d[b];
  }
}


static void StorksortB_rec_Left_LE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT l          // leftmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
);
static void StorksortB_rec_Right_GE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT r          // rightmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
);
static void StorksortB_rec_Right_LE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT r          // rightmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
);
static void StorksortB_rec_Left_GE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT l          // leftmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
);

static void StorksortB_rec_Left_LE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT l          // leftmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals  // number of direction reversals (needed to recover stable ties)
){
  IndexT i,r;
  ValueT v;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    if (reversals%2){
      for(i=l,r=l+n-1;i<r;i++,r--){
        v=y[i];
        y[i] = y[r];
        y[r] = v;
      }
    }
    x += ll;
    y += l;
    Insertionsort_l2r(y, 0, n-1);
    for (i=0; i<n; i++){
      x[i] = y[i];
    }
    return;
  }
#endif
  r = l + n + n/2 - 1;
  v = y[l + randIndex(n)];
  IndexT c[2];
  IndexT done = DIET_count_TieLeft(
      y      // pointer to data
    , l      // leftmost position
    , l+n-1  // rightmost position
    , v      // pivot value
    , c     // RETURNED: pointer to 2 counters LE,GT
  );
  //Rprintf("StorksortB_rec_Left_LE l=%d c0=%d v=%f c1=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    x += ll;
    y += l;
    if (reversals%2){
      for (i=0; i<n; i++){
        x[i] = y[n-1-i];
      }
    }else{
      for (i=0; i<n; i++){
        x[i] = y[i];
      }
    }
  }else{
    if (c[0] >= c[1]){
      // LE is bigger, remains left
      StorksortB_partition_Left_LE(y, y, l, n, v);  // reverses right wing
      if (c[0])
        StorksortB_rec_Left_GE(
          x          // target data
        , y          // data + buffer
        , ll         // leftmost target
        , l          // leftmost value to be sorted
        , c[0]       // number of values to be sorted
      , reversals  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Right_GE(
          x            // target data
        , y            // data + buffer
        , ll+c[0]      // leftmost target
        , r            // rightmost value to be sorted
        , c[1]         // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
    }else{
      // GT is bigger, remains left
      StorksortB_partition_Left_GT(y, y, l, n, v);  // reverses right wing
      if (c[0])
        StorksortB_rec_Right_GE(
          x            // target data
        , y            // data + buffer
        , ll           // leftmost target
        , r            // rightmost value to be sorted
        , c[0]         // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Left_GE(
          x          // target data
        , y          // data + buffer
        , ll+c[0]      // leftmost target
        , l          // leftmost value to be sorted
        , c[1]       // number of values to be sorted
        , reversals  // number of direction reversals (needed to recover stable ties)
        );
    }
  }
}

static void StorksortB_rec_Left_GE(
    ValueT *x         // target data
  , ValueT *y         // data + buffer
  , IndexT ll         // leftmost target
  , IndexT l          // leftmost value to be sorted
  , IndexT n          // number of values to be sorted
  , IndexT reversals  // number of direction reversals (needed to recover stable ties)
){
  IndexT r,i;
  ValueT v;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    if (reversals%2){
      for(i=l,r=l+n-1;i<r;i++,r--){
        v=y[i];
        y[i] = y[r];
        y[r] = v;
      }
    }
    x += ll;
    y += l;
    Insertionsort_l2r(y, 0, n-1);
    for (i=0; i<n; i++){
      x[i] = y[i];
    }
    return;
  }
#endif
  r = l + n + n/2 - 1;
  v = y[l + randIndex(n)];
  IndexT c[2];
  IndexT done = DIET_count_TieRight(
    y      // pointer to data
  , l      // leftmost position
  , l+n-1  // rightmost position
  , v      // pivot value
  , c     // RETURNED: pointer to 2 counters LT,GE
  );
  //Rprintf("StorksortB_rec_Left_GE l=%d c0=%d v=%f c1=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    x += ll;
    y += l;
    if (reversals%2){
      for (i=0; i<n; i++){
        x[i] = y[n-1-i];
      }
    }else{
      for (i=0; i<n; i++){
        x[i] = y[i];
      }
    }
  }else{
    if (c[1] >= c[0]){
      // GE is bigger, remains left
      StorksortB_partition_Left_GE(y, y, l, n, v);  // reverses right wing
      if (c[0])
        StorksortB_rec_Right_LE(
          x            // target data
        , y            // data + buffer
        , ll           // leftmost target
        , r            // rightmost value to be sorted
        , c[0]         // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Left_LE(
          x          // target data
        , y          // data + buffer
        , ll+c[0]      // leftmost target
        , l          // leftmost value to be sorted
        , c[1]       // number of values to be sorted
        , reversals  // number of direction reversals (needed to recover stable ties)
        );
    }else{
      // LT is bigger, remains left
      StorksortB_partition_Left_LT(y, y, l, n, v);  // reverses right wing
      if (c[0])
        StorksortB_rec_Left_LE(
          x          // target data
        , y          // data + buffer
        , ll         // leftmost target
        , l          // leftmost value to be sorted
        , c[0]       // number of values to be sorted
      , reversals  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Right_LE(
          x            // target data
        , y            // data + buffer
        , ll+c[0]      // leftmost target
        , r            // rightmost value to be sorted
        , c[1]         // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
    }
  }
}




static void StorksortB_rec_Right_GE(
  ValueT *x         // target data
, ValueT *y         // data + buffer
, IndexT ll         // leftmost target
, IndexT r          // rightmost value to be sorted
, IndexT n          // number of values to be sorted
, IndexT reversals     // number of direction reversals (needed to recover stable ties)
){
  IndexT l,i;
  ValueT v;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    if (reversals%2){
      for(l=r-n+1,i=r;l<i;l++,i--){
        v=y[i];
        y[i] = y[l];
        y[l] = v;
      }
    }
    l = r-n+1;
    x += ll;
    y += l;
    Insertionsort_l2r(y, 0, n-1);
    for (i=0; i<n; i++){
      x[i] = y[i];
    }
    return;
  }
#endif
  l = r - n - n/2 + 1;
  v = y[r - randIndex(n)];
  IndexT c[2];
  IndexT done = DIET_count_TieRight(
    y      // pointer to data
  , r-n+1  // leftmost position
  , r      // rightmost position
  , v      // pivot value
  , c      // RETURNED: pointer to 2 counters LT,GE
  );
  //Rprintf("StorksortB_rec_Right_GE l=%d c0=%d v=%f c1=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    x += ll;
    y += (r - n + 1);
    if (reversals%2){
      for (i=0; i<n; i++){
        x[i] = y[n-1-i];
      }
    }else{
      for (i=0; i<n; i++){
        x[i] = y[i];
      }
    }
  }else{
    if (c[1] >= c[0]){
      // GE is bigger, remains right
      StorksortB_partition_Right_GE(y, y, r, n, v);  // reverses left wing
      if (c[0])
        StorksortB_rec_Left_LE(
          x            // target data
        , y            // data + buffer
        , ll           // leftmost target
        , l            // leftmost value to be sorted
      , c[0]         // number of values to be sorted
      , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Right_LE(
          x            // target data
        , y            // data + buffer
        , ll+c[0]      // leftmost target
        , r            // rightmost value to be sorted
        , c[1]         // number of values to be sorted
      , reversals    // number of direction reversals (needed to recover stable ties)
        );
    }else{
      // LT is bigger, remains right
      StorksortB_partition_Right_LT(y, y, r, n, v);  // reverses left wing
      if (c[0])
        StorksortB_rec_Right_LE(
          x            // target data
        , y            // data + buffer
        , ll           // leftmost target
        , r            // rightmost value to be sorted
        , c[0]         // number of values to be sorted
      , reversals      // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Left_LE(
          x          // target data
        , y          // data + buffer
        , ll+c[0]      // leftmost target
        , l          // leftmost value to be sorted
        , c[1]       // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
    }
  }
}


static void StorksortB_rec_Right_LE(
    ValueT *x         // target data
  , ValueT *y         // data + buffer
  , IndexT ll         // leftmost target
  , IndexT r          // rightmost value to be sorted
  , IndexT n          // number of values to be sorted
  , IndexT reversals     // number of direction reversals (needed to recover stable ties)
){
  IndexT l,i;
  ValueT v;
#if INSERTIONSORT_LIMIT > 0
  if (n <= INSERTIONSORT_LIMIT){
    if (reversals%2){
      for(l=r-n+1,i=r;l<i;l++,i--){
        v=y[i];
        y[i] = y[l];
        y[l] = v;
      }
    }
    l = r-n+1;
    x += ll;
    y += l;
    Insertionsort_l2r(y, 0, n-1);
    for (i=0; i<n; i++){
      x[i] = y[i];
    }
    return;
  }
#endif
  l = r - n - n/2 + 1;
  v = y[r - randIndex(n)];
  IndexT c[2];
  IndexT done = DIET_count_TieLeft(
    y      // pointer to data
  , r-n+1  // leftmost position
  , r      // rightmost position
  , v      // pivot value
  , c      // RETURNED: pointer to 2 counters LE,GT
  );
  //Rprintf("StorksortB_rec_Right_LE l=%d c0=%d v=%f c1=%d r=%d done=%d\n", l, c[0], v, c[1], r, done);
  if (done){
    x += ll;
    y += (r - n + 1);
    if (reversals%2){
      for (i=0; i<n; i++){
        x[i] = y[n-1-i];
      }
    }else{
      for (i=0; i<n; i++){
        x[i] = y[i];
      }
    }
  }else{
    if (c[0] >= c[1]){
      // LE is bigger, remains right
      StorksortB_partition_Right_LE(y, y, r, n, v);  // reverses left wing
      if (c[0])
        StorksortB_rec_Right_GE(
          x            // target data
        , y            // data + buffer
        , ll           // leftmost target
        , r            // rightmost value to be sorted
        , c[0]         // number of values to be sorted
      , reversals    // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Left_GE(
          x            // target data
        , y            // data + buffer
        , ll+c[0]      // leftmost target
        , l            // leftmost value to be sorted
        , c[1]         // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
    }else{
      // GT is bigger, remains right
      StorksortB_partition_Right_GT(y, y, r, n, v);  // reverses left wing
      if (c[0])
        StorksortB_rec_Left_GE(
          x          // target data
        , y          // data + buffer
        , ll           // leftmost target
        , l          // leftmost value to be sorted
        , c[0]       // number of values to be sorted
        , reversals+1  // number of direction reversals (needed to recover stable ties)
        );
      if (c[1])
        StorksortB_rec_Right_GE(
          x            // target data
        , y            // data + buffer
        , ll+c[0]      // leftmost target
        , r            // rightmost value to be sorted
        , c[1]         // number of values to be sorted
        , reversals      // number of direction reversals (needed to recover stable ties)
        );
    }
  }
}




void StorksortB_exsitu(
ValueT *x
, IndexT n
){
  if (n>1){
    ValueT v;
    IndexT c[2];
    //IndexT done;
    IndexT naux = n+n/2;
    ValueT *aux;
    aux = (ValueT *) MALLOC(naux, ValueT);

    // once the data is received in aux partitioning is done in aux
    // however, sorting is NOT completed within aux,
    // instead the  values are sent back to x in sorted order DURING sorting
    // this is only a sensible method if the memory cost of x canbe ignored
    // for example for a microservice that receives unsorted data
    // and sends sorted data over a wire
    // (if the memory cost of x cannot be ignored: note that aux+x is 250% RAM during sorting !!)

    v = x[randIndex(n)];
    // partition from source to working area, reverses right wing
    /*done =*/ StorksortB_initial_partition_Left_LE(
      aux  // pointer to target
    , x    // pointer to source
    , 0    // leftmost position to be sorted
    , n    // number of elements
    , v    // pivot value
    , c    // two uninitialized counters LE,GT
    );
    if (c[0]){
      StorksortB_rec_Left_GE(
        x       // ValueT *x      // original data
      , aux     // ValueT *aux    // working space (data + buffer)
      , 0       // IndexT ll         // leftmost target
      , 0       // IndexT l          // leftmost value to be sorted
      , c[0]    // IndexT n          // number of values to be sorted
      , 0       // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
      );
    }
    if (c[1]){
      StorksortB_rec_Right_GE(
        x       // ValueT *x      // original data
      , aux     // ValueT *aux    // working space (data + buffer)
      , c[0]    // IndexT ll         // leftmost target
      , naux-1  // IndexT r          // rightmost value to be sorted
      , c[1]    // IndexT n          // number of values to be sorted
      , 1       // IndexT reversals  // number of left direction reversals (needed to recover stable ties)
      );
    }
    FREE(aux);
  }
}
