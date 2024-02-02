/*
# greeNsort Jumpsort (same code like Walksort)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#include "Insertionsort_l2r.h"
#endif

#undef K

// setting this to 0 gives Walksort
// setting this to 1 gives Jumpsort
#define JUMPSORT_TRANSFER 1

static void Jumpsort_Mergesort_merge(ValueT *t, ValueT *s, IndexT l, IndexT m, IndexT r){
  IndexT i=l,j=m+1,k=l;
  ValueT u=s[i], v=s[j];
  for (;;){
    if (LT(v, u)){
      t[k++] = v; j++;
      if (j > r)
        break;
      v = s[j];
    }else{
      t[k++] = u; i++;
      if (i > m)
        break;
      u = s[i];
    }
  }
  while(i <= m)
    t[k++] = s[i++];
  while(j <= r)
    t[k++] = s[j++];
}

static void Jumpsort_Mergesort_recurse(ValueT *t, ValueT *s, IndexT l, IndexT r){
  if (l<r){
    IndexT m;
#if INSERTIONSORT_LIMIT > 0
    if ((r-l) < INSERTIONSORT_LIMIT){
      for (m=l;m<=r; m++)
        t[m] = s[m];
      Insertionsort_l2r(t, l, r);
      return ;
    }
#endif
    m = l + (r-l)/2;
    Jumpsort_Mergesort_recurse(s, t, l  , m);
    Jumpsort_Mergesort_recurse(s, t, m+1, r);
    Jumpsort_Mergesort_merge(t, s, l, m, r);
  }
}

static void Jumpsort_copyblock(ValueT *from, ValueT *to, IndexT b){
  IndexT i;
  if (from != to){
    for (i=0;i<b;i++)
      to[i] = from[i];
  }
}


static void Jumpsort_final(
    ValueT **p  // pointer to pointers to blocks (data and buffer)
  , IndexT *o   // order of blocks (data and buffer)
  , ValueT *v   // pointer to (at least) one buffer block
  , IndexT K    // number of data blocks
  , IndexT b    // blocksize in elements per block
)
{
  ValueT *d1, *d2;
  IndexT  i, j, k, l;
  for (i = 0; i < K; i++)
    if (o[i] != i){
      // free position i by copying to buffer
      d1 = p[i]; k = i;
      for (l=0;l<b;l++){
        v[l] = d1[l];
      }
      // now fill position until we arrive at the order that points to i
      do{
        j = k;
        d2 = p[o[j]];
        for (l=0;l<b;l++){
          d1[l] = d2[l];
        }
        d1 = d2;
        k = o[j]; o[j] = j;
      }while (k != i);
      d1 = p[j];
      for (l=0;l<b;l++){
        d1[l] = v[l];
      }
    }
}


#if Jumpsort_TRANSFER > 0
// starts with n data blocks left and 2 buffer blocks right
// finish with n data blocks right and 2 buffer blocks left
// moves from target back to source !!
static void Jumpsort_copyright(
    ValueT **p  // pointer to pointers to blocks (data and buffer)
  , IndexT *os   // source (and final target) order of blocks (data and buffer)
  , IndexT *ot   // temporary tareget order of blocks (data and buffer)
  , IndexT l    // left data block (because of buffer left, add 2)
  , IndexT r   // rightmost of right data blocks (because of buffer left, add 2)
  , IndexT b    // buffer size
)
{
  IndexT i, t, o, R = r + 2;

  // move right
  os[l] = ot[(r+2)-1];
  os[l+1] = ot[(r+2)];
  for (i=l;i<=r;i++)
    os[i+2] = ot[i];

  // buffer are left at l,l+1 but may point to data between l+2 and R
  // determine which buffers do so
  t = 0;
  if (os[l] > l+1){
    ot[t] = l;
    t++;
  }
  if (os[l+1] > l+1){
    ot[t] = l+1;
    t++;
  }
  // sort them
  if (t==2 && os[ot[1]]>os[ot[0]]){
    o = os[ot[1]]; os[ot[1]]=os[ot[0]]; os[ot[0]]=o;
  }
  if (t>0){
    // search for blocks that are located right: o[i] > r and copy them to left l <= o[i] <= r
    for (i=l+2;i<=R;i++){
      if (os[i] <= l+1){
        o = os[ot[--t]];
        Jumpsort_copyblock(p[os[i]], p[o], b);
        // Rprintf("copy %d[%d] to %d[%d]\n", os[i], i, o, ot[t]);
        os[ot[t]] = os[i];
        os[i] = o;
        if (t<=0)
          break;
      }
    }
  }
}
#else
// starts with n data blocks left and 2 buffer blocks right
// finish with n data blocks right and 2 buffer blocks left
// moves from target back to source !!
static void Jumpsort_moveright(
    IndexT *os   // source (and final target) order of blocks (data and buffer)
  , IndexT *ot   // temporary tareget order of blocks (data and buffer)
  , IndexT l    // left data block (because of buffer left, add 2)
  , IndexT r   // rightmost of right data blocks (because of buffer left, add 2)
)
{
  os[l] = ot[(r+2)-1];
  os[l+1] = ot[(r+2)];
  for (;l<=r;l++)
    os[l+2] = ot[l];
}
#endif


// starts with n data blocks left and 2 buffer blocks right
// finish with n data blocks left and 2 buffer blocks right
// (where the data are physically left and the buffers are physically right)
static void Jumpsort_copyleft(
    ValueT **p  // pointer to pointers to blocks (data and buffer)
  , IndexT *os   // source (and final target) order of blocks (data and buffer)
  , IndexT *ot   // temporary tareget order of blocks (data and buffer)
  , IndexT l    // left data block (because of buffer left, add 2)
  , IndexT r   // rightmost of right data blocks (because of buffer left, add 2)
  , IndexT b    // buffer size
){
  IndexT i, t, o, R = r + 2;

  // update source order
  for (i=l;i<=R;i++)
    os[i] = ot[i];

  // buffer are right at R-1,R but may point to data between l and r
  // determine which buffers do so
  t = 0;
  if (os[R-1] <= r){
    ot[t] = R-1;
    t++;
  }
  if (os[R] <= r){
    ot[t] = R;
    t++;
  }
  // sort them
  if (t==2 && os[ot[1]]<os[ot[0]]){
    o = os[ot[1]]; os[ot[1]]=os[ot[0]]; os[ot[0]]=o;
  }
  if (t>0){
    // search for blocks that are located right: o[i] > r and copy them to left l <= o[i] <= r
    for (i=r;i>=l;i--){
      if (os[i] > r){
        o = os[ot[--t]];
        Jumpsort_copyblock(p[os[i]], p[o], b);
        // Rprintf("copy %d[%d] to %d[%d]\n", os[i], i, o, ot[t]);
        os[ot[t]] = os[i];
        os[i] = o;
        if (t<=0)
          break;
      }
    }
  }
}


// starts with n data blocks right and 2 buffer blocks left
// finish with n data blocks left and 2 buffer blocks right
static void Jumpsort_mergeleft(
    ValueT **p  // pointer to pointers to blocks (data and buffer)
  , IndexT *os   // source (and final target) order of blocks (data and buffer)
  , IndexT *ot   // temporary target order of blocks (data and buffer)
  , IndexT l    // left data block (because of buffer left, add 2)
  , IndexT m   // rightmost of left data blocks (because of buffer left, add 2)
  , IndexT r   // rightmost of right data blocks (because of buffer left, add 2)
  , IndexT b    // buffer size
)
{
  // Rprintf("Walksort_mergeleft l=%d m=%d r=%d b=%d\n", l,m,r,b);
  IndexT t, i, il, ir, j, jl=0, jr=0;
  ValueT *bt, *bl = p[os[l+2]], *br = p[os[m+3]];
  ValueT u=bl[jl], v=br[jr];
  // for (i=l;i<=r+2;i++)
  // Rprintf("  os[%d]=%d\n", i, os[i]);
  // for (i=l+2; i<=r+2; i++){
  // for (j=0; j<b; j++)
  // Rprintf("  i=%d os=%d j=%d x=%f\n", i, os[i], i*b+j, p[os[i]][j]);
  // }
  IndexT M = m+2;
  IndexT R = r+2;
  // initialize target with the two buffer
  ot[l] = os[l];
  t=l+1;
  ot[t] = os[t];
  il=l+2,ir=M+1,i=l;
  while(i<=r){
    bt = p[ot[i++]];
    // Rprintf("i++ ot[%d]=%d\n", i, ot[i]);
    j = 0;
    while(j<b){
      //bt[j] = (LT(br[jr],bl[jl])) ? br[jr++] : bl[jl++];
      if (LT(v,u)){
        bt[j++] = v; ++jr;
        if (jr < b){
          v = br[jr];
        }else{
          if (j==b){
            if (i>r)
              return;
            j = 0;
            bt = p[ot[i++]];
          }
          ot[++t] = os[ir];
          // Rprintf("t++ ot[%d]=%d\n", t, ot[t]);
          if (ir<R){
            br=p[os[++ir]];
            v = br[jr=0];
            // Rprintf("ir++ os[%d]=%d\n", ir, os[ir]);
          }else{
            for (;j<b;j++){
              // Rprintf("ir exhausted ot[i][j] = %d[%d][%d] <- ol[il][jl] = %d[%d][%d] = %f\n", ot[i], i, j, os[il], il, jl, bl[jl]);
              bt[j] = bl[jl++];
            }
            ot[R-1] = ot[t];
            ot[R] = os[il++];
            // Rprintf("ir exhausted buffer assign ot[%d]=%d\n", R-1, ot[R-1]);
            // Rprintf("ir exhausted buffer assign ot[%d]=%d\n", R, ot[R]);
            for(;i<=r;i++,il++){
              ot[i] = os[il];
              // Rprintf("ir exhausted ot[i=%d] = os[il=%d] = %d\n", i, il, ot[i]);
            }
            // goto fin;
            return;
          }
        }
      }else{
        bt[j++] = u; ++jl;
        if (jl < b){
          u = bl[jl];
        }else{
          if (j==b){
            if (i>r)
              return;
            j = 0;
            bt = p[ot[i++]];
          }
          ot[++t] = os[il];
          // Rprintf("t++ ot[%d]=%d\n", t, ot[t]);
          if (il<M){
            bl=p[os[++il]];
            u = bl[jl=0];
            // Rprintf("il++ os[%d]=%d\n", il, os[il]);
          }else{
            for (;j<b;j++){
              // Rprintf("i++ exhausted ot[i][j] = %d[%d][%d] <- os[ir][jr] = %d[%d][%d] = %f\n", ot[i], i, j, os[ir], ir, jr, br[jr]);
              bt[j] = br[jr++];
            }
            ot[R-1] = ot[t];
            ot[R] = os[ir++];
            // Rprintf("il exhausted buffer assign ot[%d]=%d\n", R-1, ot[R-1]);
            // Rprintf("il exhausted buffer assign ot[%d]=%d\n", R, ot[R]);
            for(;i<=r;i++,ir++){
              ot[i] = os[ir];
              // Rprintf("il exhausted ot[i=%d] = o[ir=%d] = %d\n", i, ir, ot[i]);
            }
            // goto fin;
            return;
          }
        }
      }
    }
  }
  // fin:
  // Rprintf("post last i %d[%d]  il %d[%d]  ir %d[%d]\n", ot[i-1], i-1, ot[il], il, ot[ir], ir);
  // for (i=l;i<=r+2;i++)
  // Rprintf("  ot[%d]=%d\n", i, ot[i]);
  // for (i=l; i<=r; i++){
  // for (j=0; j<b; j++)
  // Rprintf("  i=%d o=%d j=%d x=%f\n", i, ot[i], i*b+j, p[ot[i]][j]);
  // }
}



// starts with buffer at the right end
// finish with buffer at the left end
void Jumpsort_sortright(
    ValueT **p  // pointer to pointers to blocks (data and buffer)
  , IndexT *o   // input and output order of blocks (data and buffer)
  , IndexT *o2  // temporay order of blocks (data and buffer)
  , IndexT l    // left data block
  , IndexT r    // right data block
  , IndexT b    // buffer size
)
{
  IndexT m;
  // Rprintf("Jumpsort_sortright l=%d r=%d b=%d\n", l, r, b);
  if (r <= l){
    ValueT *lp = p[o[l]], *rp = p[o[l+2]];
    // Rprintf("Mergesort_sortright l=%d l+2=%d  o=%d o+2=%d\n", l, l+2, o[l], o[l+2]);
    for (m = 0; m < b; m++){
      rp[m] = lp[m];
    }
    Jumpsort_Mergesort_recurse(rp, lp, 0, b-1);  // sort to right
    return;
  }
  m = (l+r)/2;
  Jumpsort_sortright(p, o, o2, m+1, r, b);
  Jumpsort_sortright(p, o, o2, l, m, b);
  Jumpsort_mergeleft(p, o, o2, l, m, r, b);
#if Jumpsort_TRANSFER > 0
  Jumpsort_copyright(p, o, o2, l, r, b); // better for linear distance cost
#else
  Jumpsort_moveright(o, o2, l, r);       // better for constant or log distance cost
#endif
}

static void Jumpsort_Mergesort_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *buf = (ValueT *) MALLOC(n, ValueT);
  for (i = 0; i < n; i++){
    buf[i] = x[i];
  }
  Jumpsort_Mergesort_recurse(x, buf, 0, n-1);
  FREE(buf);
}

static void Jumpsort_Mergesort_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *buf = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = buf + n;
  for (i = 0; i < n; i++){
    buf[i] = x[i];
    aux2[i] = x[i];
  }
  Jumpsort_Mergesort_recurse(buf, aux2, 0, n-1);
  for (i=0; i<n; i++)
    x[i] = buf[i];
  FREE(buf);
}


void Jumpsort_insitu(ValueT *x, IndexT n, IndexT b, PerfT *perf)
{
  // Special case: up to n == b we switch to merge sort
  if (n<=b){
    Jumpsort_Mergesort_insitu(x, n);
    perf->secs = getNewSecs();
    perf->size = 2;
    return;
  }
  IndexT K,k,R,r,m,i;
  PerfT p1,p2;
  // adjust blocksize if necessary
  K = n/2 + (n%2 > 0);
  if (b>K)
    b = K;
  // calculate sizing
  K = n/b;
  R = n%b;
  r = R>0 ? 1 : 0;

  IndexT naux = b*(2+r);
  IndexT np = K+2+r;
  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  ValueT **p = (ValueT **) MALLOC(np, ValueT*);
  IndexT *o = (IndexT *) MALLOC(np, IndexT);
  IndexT *o2 = (IndexT *) MALLOC(np, IndexT);

  o[0] = 0;
  p[0] = x;
  for (k=1;k<K;k++){
    o[k] = k;
    p[k] = p[k-1] + b;
  }

  // handle incomplete block
  if (r){
    m = b*K;
    o[k] = k;
    p[k] = aux;
    for (i=0;i<R;i++){
      p[k][i] = x[m+i];
    }
    for (;i<b;i++){
      p[k][i] = R_PosInf;
    }
    k++;
    // first buffer
    o[k] = k;
    p[k] = p[k-1]+b;
  }else{
    // first buffer
    o[k] = k;
    p[k] = aux;
  }
  k++;
  // second buffer
  o[k] = k;
  p[k] = p[k-1]+b;

  k = K+r;
  m =  k / 2;
  // Rprintf("Jumpsort_exsitu n=%d b=%d K=%d r=%d R=%d\n", n, b, K, r, R);
  Jumpsort_sortright(p, o, o2, m , k-1, b);
  Jumpsort_sortright(p, o, o2, 0 , m-1, b);
  Jumpsort_mergeleft(p, o, o2, 0, m-1, k-1, b);
  Jumpsort_copyleft(p, o, o2, 0, k-1, b);

  FREE(o2);

  p1.size = (np*(sizeof(ValueT*)+2*sizeof(IndexT)) + (n+naux)*sizeof(ValueT)) / ((double)n*sizeof(ValueT));
  p1.secs = getNewSecs();

  Jumpsort_final(
    p  // pointer to pointers to blocks (data and buffer)
    , o   // order of blocks (data and buffer)
    , p[k+1]  // pointer to (at least) one buffer block
  , k       // number of data blocks
  , b       // blocksize in elements per block
  );

  // copy back the incomplete block
  if (r){
    x += K*b;
    for (i=0; i<R; i++)
      x[i] = aux[i];
  }

  FREE(o);
  FREE(p);
  FREE(aux);

  p2.size = (np*(sizeof(ValueT*)+sizeof(IndexT)) + (n+naux)*sizeof(ValueT)) / ((double)n*sizeof(ValueT));
  p2.secs = getNewSecs() - p1.secs;

  perf->secs = p1.secs + p2.secs;
  perf->size= (p1.secs*p1.size + p2.secs*p2.size) / perf->secs;
  // Rprintf("sizeof(ValueT*)=%d sizeof(ValueT)=%d sizeof(Index)=%d b=%d K=%d np=%d naux=%d n=%d \n", (sizeof(ValueT*)), (sizeof(ValueT)), (sizeof(IndexT)), b, K, np, naux, n);
  // Rprintf("p1.secs=%f size=%f sizesecs=%f\n", p1.secs, p1.size);
  // Rprintf("p2.secs=%f size=%f sizesecs=%f\n", p2.secs, p2.size);
  // Rprintf("perf->secs=%f size=%f\n", perf->secs, perf->size);
  return;
}


void Jumpsort_exsitu(ValueT *x, IndexT n, IndexT b, PerfT *perf)
{
  // Special case: up to n == b we switch to merge sort
  if (n<=b){
    Jumpsort_Mergesort_exsitu(x, n);
    perf->secs = getNewSecs();
    perf->size = 2;
    return;
  }

  PerfT p1,p2,p3,p4;
  IndexT K,k,R,r,m,i;
  //IndexT j;
  // adjust blocksize if necessary
  K = n/2 + (n%2 > 0);
  if (b>K)
    b = K;
  // calculate sizing
  K = n/b;
  R = n%b;
  r = R>0 ? 1 : 0;

  IndexT np = K+2+r;
  IndexT naux = np*b;

  ValueT *aux = (ValueT *) MALLOC(naux, ValueT);
  // copy complete blocks
  m = b*K;
  for (i = 0; i < m; i++){
    aux[i] = x[i];
  }
  p1.secs = getNewSecs();
  p1.size = naux / ((double) n);
  perf->secs = p1.secs;

  ValueT **p = (ValueT **) MALLOC(np, ValueT*);
  IndexT *o = (IndexT *) MALLOC(np, IndexT);
  IndexT *o2 = (IndexT *) MALLOC(np, IndexT);
  o[0] = 0;
  p[0] = aux;
  for (k=1;k<K;k++){
    o[k] = k;
    p[k] = p[k-1] + b;
  }
  // handle incomplete block
  if (r){
    m = b*K;
    o[k] = k;
    p[k] = p[k-1] + b;
    for (i=0;i<R;i++){
      p[k][i] = x[m+i];
    }
    for (;i<b;i++){
      p[k][i] = R_PosInf;
    }
    k++;
  }
  // handle buffer blocks
  m = k + 2;
  for (;k<m;k++){
    o[k] = k;
    p[k] = p[k-1] + b;
    // mark buffer for testing
    // for (i=0;i<b;i++){
    //   p[k][i] = -1;
    // }
  }
  // Rprintf("Jumpsort_exsitu n=%d b=%d K=%d r=%d R=%d\n", n, b, K, r, R);
  // for (j=0; j<np; j++)
  //   for (i=0; i<b; i++)
  //     Rprintf("pre sort j=%d i=%d v=%f\n", j, i, p[o[j]][i]);

  k = K+r;
  m =  k / 2;
  // Rprintf("Jumpsort_exsitu n=%d b=%d K=%d r=%d R=%d\n", n, b, K, r, R);
  Jumpsort_sortright(p, o, o2, m , k-1, b);
  // for (j=0; j<np; j++)
  //   for (i=0; i<b; i++)
  //     Rprintf("right half j=%d i=%d v=%f\n", j, i, p[o[j]][i]);

  Jumpsort_sortright(p, o, o2, 0 , m-1, b);
  // for (j=0; j<np; j++)
  //   for (i=0; i<b; i++)
  //     Rprintf("left half j=%d i=%d v=%f\n", j, i, p[o[j]][i]);

  Jumpsort_mergeleft(p, o, o2, 0, m-1, k-1, b);
  Jumpsort_copyleft(p, o, o2, 0, k-1, b);

  FREE(o2);

  p2.size = (np*(sizeof(ValueT*)+2*sizeof(IndexT)) + naux*sizeof(ValueT)) / ((double)n*sizeof(ValueT));
  p2.secs = getNewSecs() - perf->secs;
  perf->secs += p2.secs;
  Jumpsort_final(
    p  // pointer to pointers to blocks (data and buffer)
    , o   // order of blocks (data and buffer)
    , p[k+1]  // pointer to (at least) one buffer block
  , k       // number of data blocks
  , b       // blocksize in elements per block
  );

  FREE(o);
  FREE(p);

  p3.size = (np*(sizeof(ValueT*)+sizeof(IndexT)) + naux*sizeof(ValueT)) / ((double)n*sizeof(ValueT));
  p3.secs = getNewSecs() - perf->secs;
  perf->secs += p3.secs;

  // emulate exsitu back
  for (i=0; i<n; i++)
    x[i] = aux[i];

  FREE(aux);

  p4.size = naux / ((double) n);
  p4.secs = getNewSecs() - perf->secs;
  perf->secs += p4.secs;

  perf->size= (p1.secs*p1.size + p2.secs*p2.size + p3.secs*p3.size + p4.secs*p4.size) / perf->secs;
  //   Rprintf("sizeof(ValueT*)=%d sizeof(ValueT)=%d sizeof(Index)=%d b=%d K=%d np=%d naux=%d n=%d \n", (sizeof(ValueT*)), (sizeof(ValueT)), (sizeof(IndexT)), b, K, np, naux, n);
  //   Rprintf("p1.secs=%f size=%f\n", p1.secs, p1.size);
  //   Rprintf("p2.secs=%f size=%f\n", p2.secs, p2.size);
  //   Rprintf("p3.secs=%f size=%f\n", p3.secs, p3.size);
  //   Rprintf("p4.secs=%f size=%f\n", p4.secs, p4.size);
  //   Rprintf("perf->secs=%f size=%f\n", perf->secs, perf->size);
  return;
}

