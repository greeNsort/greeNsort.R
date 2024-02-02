/*
# greeNsort branch parallel Quicksort
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include <pthread.h>
#include "randIndex.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#endif

// do not change this unless you know what you do
//#define PRELOCATE_SIZE 3
#define USE_MEDIAN_OF_3_PIVOT 0


static void copy(ValueT *t, ValueT *s, IndexT n){
  IndexT i;
  for (i=0; i < n; i++){
    t[i] = s[i];
  }
}

typedef struct Pcopy_t{
  ValueT *t;
  ValueT *s;
  IndexT n;
} Pcopy_t;

static void *Pcopy(void *arg){
  Pcopy_t *parg = (Pcopy_t*)arg;
  ValueT *t = parg->t;
  ValueT *s = parg->s;
  IndexT i,n=parg->n;
  for (i=0; i < n; i++){
    t[i] = s[i];
  }
  return NULL;
}

static void Scopy(ValueT *target, ValueT *source, IndexT n, double maxt){
  IndexT t = ceilf(maxt);
  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  Pcopy_t *args = (Pcopy_t *) MALLOC(t, Pcopy_t);
  double d = n/((double)t);
  double c = 0;
  IndexT i, k=0, l=0;
  for (i=0; i<t; i++){
    c += d;
    k = roundf(c);
    args[i].t = target+l;
    args[i].s = source+l;
    args[i].n = k - l;
    pthread_create(threads+i, NULL, Pcopy, args+i);
    l = k;
  }
  for (i=0; i<t; i++){
    pthread_join(threads[i], NULL);
  }
  FREE(args);
  FREE(threads);
}


static void Quicksort2(ValueT *x, IndexT l, IndexT r)
{
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    Insertionsort_l2r(x, l, r);
    return;
  }
#else
  if (l >= r)
    return;
#endif
  IndexT j, i;
#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  i = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#else
  i = l+randIndex(r-l+1);
#endif
  ValueT t, v;
  SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = l-1; j = r;
  for (;;){
    while (LT(x[++i], v)); // sentinel stop of for loop
    while (LT(v, x[--j])){
      if (j <= i)       // explicit stop of for loop
        break;
    }
    if (j <= i)
      break;
    SWAP(x[i], x[j], t);
  }
  SWAP(x[i], x[r], t);
  Quicksort2(x, l, i-1);
  Quicksort2(x, i+1, r);
}


typedef struct PQuicksort2_t{
  ValueT *x;
  IndexT l;
  IndexT r;
  double threads;
} PQuicksort2_t;



static void * PQuicksort2(void *arg)
{
  PQuicksort2_t *parg = (PQuicksort2_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    Quicksort2(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PQuicksort2_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;

  IndexT j, i;
#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c,n=r-l+1;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  i = LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a);
#else
  i = l+randIndex(r-l+1);
#endif
    ValueT t, v;
    SWAP(x[i], x[r], v); // first argument pivot now in v and x[r]
    i = l-1; j = r;
    for (;;){
      while (LT(x[++i], v)); // sentinel stop of for loop
      while (LT(v, x[--j])){
        if (j <= i)       // explicit stop of for loop
          break;
      }
      if (j <= i)
        break;
      SWAP(x[i], x[j], t);
    }
    SWAP(x[i], x[r], t);

    //Quicksort2(x, l, k-1);
    //Quicksort2(x, k+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = i-1;
    Larg.threads = (i-l)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = i+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;

    // PQuicksort2(&Larg);
    // PQuicksort2(&Rarg);

    pthread_create(&thread0, NULL, PQuicksort2, &Larg);
    pthread_create(&thread1, NULL, PQuicksort2, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}



/* --- from here experiments -- parallel partitioning is too complicated ------------------------------------------------------------------------------- //

// this requires the pivot in r
// static IndexT Quicksort2_partition(ValueT *x, IndexT l, IndexT r, ValueT v)
// {
//   IndexT j, i;
//   ValueT t;
//   i = l-1; j = r;
//   for (;;){
//     while (LT(x[++i], v)); // sentinel stop of for loop
//     while (LT(v, x[--j]))
//       if (j <= i)       // explicit stop of for loop
//         break;
//       if (j <= i)
//         break;
//       SWAP(x[i], x[j], t);
//   }
//   return i;
// }


// this requires no pivot anywhere
// static IndexT PQuicksort2_partition2(ValueT *x, IndexT l, IndexT r, ValueT v)
// {
//   IndexT j, i;
//   ValueT t;
//   i = l-1; j = r;
//
//   // first loop traversal is different: j is on an unchecked position allowing i to march through the whole partition
//   // and we have no sentinels
//   while (LT(x[++i], v)){
//     if (j <= i){         // explicit stop of for loop
//       return i;          // rightmost of low partition (high partition may be empty)
//     }
//   }
//   j++; // == j = r  + 1;
//   while (LT(v, x[--j])){
//     if (j <= i){       // explicit stop of for loop
//       return i-1;         // rightmost of low partition (low partition may be empty)
//     }
//   }
//   if (j <= i){
//     return i-1; // rightmost of low partition (low partition may be empty)
//   }
//   SWAP(x[i], x[j], t);
//
//   // now we have sentinels on both sides
//   // in the main loop j is on the leftmost checked of the high partition
//   for (;;){
//     while (LT(x[++i], v));
//     if (j<=i)
//       return i-1;
//     while (LT(v, x[--j]));
//     if (j<=i)
//       return i-1;
//     SWAP(x[i], x[j], t);
//   }
// }


typedef struct PQuicksort2_partition4_t{
  ValueT *x;
  ValueT pivot;
  IndexT ll;
  IndexT lr;
  IndexT rl;
  IndexT rr;
  IndexT result;
  IndexT ln;
  IndexT rn;
} PQuicksort2_partition4_t;


static void * PQuicksort2_partition4(void *arg)
{
  PQuicksort2_partition4_t *parg = (PQuicksort2_partition4_t*)arg;
  ValueT *x = parg->x;
  IndexT ll = parg->ll;
  IndexT lr = parg->lr;
  IndexT rl = parg->rl;
  IndexT rr = parg->rr;
  ValueT v = parg->pivot;
  ValueT t;
  IndexT i = ll-1, j = rr+1;

  // first loop traversal is different: j is on an unchecked position allowing i to march through the whole partition
  // and we have no sentinels
  while (LT(x[++i], v)){
    if (i >= lr){
      i = rl - 1;
      while (LT(x[++i], v)){
        if (i >= rr){
          parg->result = i;
          return NULL;
        }
      }
      break;
    }
  }

  if (i <= lr){  // segmented search
    while (LT(v, x[--j])){
      if (j <= rl){
        j = lr + 1;
        while (LT(v, x[--j])){
          if (j <= i){
            parg->result = i - 1;
            return NULL;
          }
        }
        break;
      }
    }
  }else{ // i >= rl // simple search
    while (LT(v, x[--j])){
      if (j <= i){       // explicit stop of for loop
        if (i==rl){
          parg->result = lr;
          return NULL;
        }else{
          parg->result = i - 1;
          return NULL;
        }
      }
    }
  }
  if (j <= i){
    parg->result = i - 1;
    return NULL;
  }
  SWAP(x[i], x[j], t);

  // but now we can be exhausted
  if (i >= lr){    // left id exhausted
    if (j <= rl){  // both are exhausted
      parg->result = lr;
      return NULL;
    }
    goto rightsearch_fromleft;
  }
  if (j <= rl){   // right is exhausted
    goto leftsearch_fromleft;
  }

  // for now we ignore sentinels and check explicitely
  // in the main loop j is on the leftmost checked of the high partition
  bothsearch:
  for (;;){
    while (LT(x[++i], v))
      if (i >= lr){
        goto rightsearch_fromleft;
      }
    while (LT(v, x[--j]))
      if (j <= rl){
        goto leftsearch_fromright;
      }
    // cannot be crossed already here, hence unconditional SWAP
    SWAP(x[i], x[j], t);
    // but now we can be exhausted
    if (i >= lr){    // left id exhausted
      if (j <= rl){  // both are exhausted
        parg->result = lr;
        return NULL;
      }
      goto rightsearch_fromleft;
    }
    if (j <= rl){   // right is exhausted
      goto leftsearch_fromleft;
    }
  }

  leftsearch_fromleft:
    j = lr + 1;
    goto leftsearch_loop;

  leftsearch_fromright:
    j = lr + 1;
    while (LT(v, x[--j]))
      if (j<=i){
        parg->result = i - 1;
        return NULL;
      }
    if (j<=i){
      parg->result = i - 1;
      return NULL;
    }
    SWAP(x[i], x[j], t);

  leftsearch_loop:
  for (;;){
    while (LT(x[++i], v))
      if (i >= j){
        parg->result = i - 1;
        return NULL;
      }
    while (LT(v, x[--j]))
      if (j<=i){
        parg->result = i - 1;
        return NULL;
      }
    if (j<=i){
      parg->result = i - 1;
      return NULL;
    }
    SWAP(x[i], x[j], t);
  }


  rightsearch_fromleft:
    i = rl - 1;
  rightsearch_loop:
    for (;;){
      while (LT(x[++i], v))
        if (j<=i){
          parg->result = i - 1;
          return NULL;
        }
      while (LT(v, x[--j]))
        if (j<=i){
          parg->result = i - 1;
          return NULL;
        }
      if (j<=i){
        parg->result = i - 1;
        return NULL;
      }
      SWAP(x[i], x[j], t);
    }
}



typedef struct PRelocate_t{
  ValueT *x;
  IndexT *lllrrlrr;
  int origin;
  int count;
} PRelocate_t;


static void * PRelocate(void *arg)
{
  PRelocate_t *parg = (PRelocate_t*)arg;
  ValueT *x = parg->x;
  IndexT *lllrrlrr  = parg->lllrrlrr + PRELOCATE_SIZE*parg->origin;
  IndexT count  = parg->count;
  ValueT *ll,*lr,*rl,*rr;
  ValueT t;
  for (int r=0; r < count; r++){
    //Rprintf("relocating #%d ll=%d lr=%d rl=%d rr=%d\n", parg->origin + r, lllrrlrr[0], lllrrlrr[1], lllrrlrr[2], lllrrlrr[3]);
    ll = x + lllrrlrr[0];
    lr = x + lllrrlrr[1];
    rl = x + lllrrlrr[2];
    //rr = x + lllrrlrr[3];
    while(ll<=lr){
      SWAP(*ll,*rl,t);
      ll++;
      rl++;
    }
    lllrrlrr += PRELOCATE_SIZE;
  }
}


static IndexT SQuicksort2_partition4(ValueT *x, IndexT l, IndexT r, double maxt)
{
  // begin - segmented partitioning
  IndexT n = r - l + 1;
#if USE_MEDIAN_OF_3_PIVOT
  IndexT a,b,c;
  a = l+randIndex(n);
  b = l+randIndex(n);
  c = l+randIndex(n);
  ValueT v = x[LT(x[a], x[b]) ? (LT(x[b], x[c]) ? b : LT(x[a], x[c]) ? c : a)  : (LT(x[c], x[b]) ? b : LT(x[c], x[a]) ? c : a)];
#else
  ValueT v = x[l+randIndex(n)];
#endif
  int i, t = MIN(ceilf(maxt), n / THREAD_LIMIT_MERGE);
  IndexT d = n / (2 * t);
  //Rprintf("l=%d r=%d v=%f t=%d d=%d\n", l, r, v, t, d);
  IndexT k,lastk;

  pthread_t *threads = (pthread_t *) MALLOC(t, pthread_t);
  PQuicksort2_partition4_t *args = (PQuicksort2_partition4_t *) MALLOC(t, PQuicksort2_partition4_t);
  k = d; lastk = 0;
  t--; // threads - 1
  for (i=0; i<=t; i++){
    args[i].x = x;
    args[i].ll = l + lastk;
    args[i].lr = l + (k - 1);
    if (i==t) // middle section
      args[i].rl = l +  k;
    else
      args[i].rl = r - (k - 1);
    args[i].rr = r - lastk;
    args[i].pivot = v;
    //PQuicksort2_partition4(args+i);
    pthread_create(threads+i, NULL, PQuicksort2_partition4, args+i);
    lastk = k;
    k += d;
  }
  IndexT Lsplit,Rsplit=l;
  IndexT s=0;
  for (i=0; i<=t; i++){
    pthread_join(threads[i], NULL);
    if (args[i].result <= args[i].lr){
      args[i].ln = args[i].result - args[i].ll + 1;  // results included
      args[i].rn = (args[i].lr - args[i].result) + (args[i].rr - args[i].rl) + 1; // results excluded
    }else{
      args[i].ln = (args[i].lr - args[i].ll) + (args[i].result - args[i].rl) + 2;  // results included
      args[i].rn = args[i].rr - args[i].result; // results excluded
    }
    Rsplit += args[i].ln;
    s += (args[i].lr - args[i].ll + 1) + (args[i].rr - args[i].rl + 1);
  }
  Lsplit = Rsplit - 1;
  // end - segmented partitioning

  // Rprintf("v=%f LN=%d RN=%d s=%d n=%d\n", v, Rsplit-l, r-Lsplit, s, r-l+1); R_FlushConsole();
  // for (i=0; i<=t; i++)
  //   Rprintf("i=%d k=%d ln=%d rn=%d ll=%d lr=%d rl=%d rr=%d  result=%d\n", i, args[i].ln+args[i].rn, args[i].ln, args[i].rn, args[i].ll, args[i].lr, args[i].rl, args[i].rr, args[i].result);  R_FlushConsole();

  // begin - determine number of exchanges needed for redistribution with first and second left search loop //
  IndexT e = 0;
  IndexT L = l;
  IndexT ll,lr,ln;
  i = 0;
  while(L <= Lsplit && i <= t){
    ll = MAX(L, args[i].ll+args[i].ln);
    lr = MIN(Lsplit, args[i].lr);
    ln = lr - ll + 1;
    if (ln>0)
      e = e + ln;
    L = args[i].lr + 1;
    i++;
  }
  i = t;
  while (L <= Lsplit && i >= 0){
    ll = MAX(L, args[i].rl + (args[i].ln - (args[i].lr - args[i].ll + 1)));
    lr = MIN(Lsplit, args[i].rr);
    ln = lr - ll + 1;
    if (ln>0)
      e = e + ln;
    L = args[i].rr + 1;
    i--;
  }
  IndexT LE = e;
  IndexT rw, lw = e / (t+1) + (e % (t+1) ? 1 : 0); // recall that t=threads-1
  //Rprintf("l=%d Lsplit=%d Rsplit=%d r=%d  LE=%d lw=%d\n", l, Lsplit, Rsplit, r, LE, lw); R_FlushConsole();
  // end - determine number of exchanges needed for redistribution with with first and second left search loop //
  // initialisation left search loop
  i = 0;
  L = l;
  // initialisation right search loop
  int loop_status = 0;
  IndexT j = 0;
  IndexT R = r;
  IndexT rl,rr,rn;
  // initialisation output loop
  PRelocate_t *rargs = (PRelocate_t *) MALLOC(3*(t+1), PRelocate_t); // recall that t=threads-1
  IndexT *lllrrlrr = (IndexT *) MALLOC(3*(t+1)*PRELOCATE_SIZE, IndexT); // max number of relocations is 3xthreads(xPRELOCATE_SIZE positions)
  IndexT *reloc_writer = lllrrlrr;
  int reloc_origin = 0;
  int reloc_count = 0;
  int reloc_index;
  IndexT th = 0;
  IndexT RE = 0;
  e = 0;
  // begin - first end second left search loop // for output
  while(L <= Lsplit && i <= t){
    ll = MAX(L, args[i].ll+args[i].ln);
    lr = MIN(Lsplit, args[i].lr);
    ln = lr - ll + 1;
    if (ln>0){
      if (ln<=(lw-e))
        rw = ln;
      else
        rw = (lw-e);
      // begin - first end second right search loop // first instance //
      if (loop_status < 2){
        loop_status = 0;
        while(R >= Rsplit && j <= t){
          rr = MIN(R, args[j].rr-args[j].rn);
          rl = MAX(Rsplit, args[j].rl);
          rn = rr - rl + 1;
          if (rn > 0){
            if (rn <= rw)
              k = rn;
            else
              k = rw;
              //Rprintf("th=%d ls=%d, i=%d j=%d rs=%d k=%d ll=%d lr=%d rl=%d rr=%d\n", th, 0, i, j, 0, k, ll, ll+(k-1), rr-(k-1), rr); R_FlushConsole();
              *reloc_writer++ = ll;
              *reloc_writer++ = ll+(k-1);
              *reloc_writer++ = rr-(k-1);
              // *reloc_writer++ = rr;
              reloc_count++;
              if (k < rn){
                R = rr - k;
              }else{
                R = args[j].rl - 1;
                j++;
              }
              loop_status = 1;
              break;
          }else{
            R = args[j].rl - 1;
            j++;
          }
        }
        if (loop_status < 1){
          loop_status = 2;
          j = t;
        }
      }
      if (loop_status > 1){
        while (R >= Rsplit && j >= 0){
          rr = MIN(R, args[j].lr - (args[j].rn - (args[j].rr - args[j].rl + 1)));
          rl = MAX(Rsplit, args[j].ll);
          rn = rr - rl + 1;
          if (rn > 0){
            if (rn <= rw)
              k = rn;
            else
              k = rw;
              //Rprintf("th=%d ls=%d, i=%d j=%d rs=%d k=%d ll=%d lr=%d rl=%d rr=%d\n", th, 0, i, j, 1, k, ll, ll+(k-1), rr-(k-1), rr); R_FlushConsole();
              *reloc_writer++ = ll;
              *reloc_writer++ = ll+(k-1);
              *reloc_writer++ = rr-(k-1);
              // *reloc_writer++ = rr;
              reloc_count++;
              if (k < rn){
                R = rr - k;
              }else{
                R = args[j].ll - 1;
                j--;
              }
              break;
          }else{
            R = args[j].ll - 1;
            j--;
          }
        }
      }
// end - first end second right search loop // first instance //
      e += k;
      if (e >= lw){
        //Rprintf("closing %d\n", th);
        rargs[th].x = x;
        rargs[th].lllrrlrr = lllrrlrr;
        rargs[th].origin = reloc_origin;
        rargs[th].count = reloc_count;
        //PRelocate(rargs + th);
        pthread_create(threads+th, NULL, PRelocate, rargs+th);
        reloc_origin += reloc_count;
        reloc_count = 0;
        th++;
        RE += e; e = 0;
      }
      if (k < ln){
        L = ll + k;
        continue;
      }
    }
    L = args[i].lr + 1;
    i++;
  }
  i = t;
  while (L <= Lsplit && i >= 0){
    ll = MAX(L, args[i].rl + (args[i].ln - (args[i].lr - args[i].ll + 1)));
    lr = MIN(Lsplit, args[i].rr);
    ln = lr - ll + 1;
    if (ln>0){
      if (ln<=(lw-e))
        rw = ln;
      else
        rw = (lw-e);
        // begin - first end second right search loop // second instance //
        if (loop_status < 2){
          loop_status = 0;
          while(R >= Rsplit && j <= t){
            rr = MIN(R, args[j].rr-args[j].rn);
            rl = MAX(Rsplit, args[j].rl);
            rn = rr - rl + 1;
            if (rn > 0){
              if (rn <= rw)
                k = rn;
              else
                k = rw;
              //Rprintf("th=%d ls=%d, i=%d j=%d rs=%d k=%d ll=%d lr=%d rl=%d rr=%d\n", th, 1, i, j, 0, k, ll, ll+(k-1), rr-(k-1), rr); R_FlushConsole();
              *reloc_writer++ = ll;
              *reloc_writer++ = ll+(k-1);
              *reloc_writer++ = rr-(k-1);
              // *reloc_writer++ = rr;
              reloc_count++;
              if (k < rn){
                R = rr - k;
              }else{
                R = args[j].rl - 1;
                j++;
              }
              loop_status = 1;
              break;
            }else{
              R = args[j].rl - 1;
              j++;
            }
          }
          if (loop_status < 1){
            loop_status = 2;
            j = t;
          }
        }
        if (loop_status > 1){
          while (R >= Rsplit && j >= 0){
            rr = MIN(R, args[j].lr - (args[j].rn - (args[j].rr - args[j].rl + 1)));
            rl = MAX(Rsplit, args[j].ll);
            rn = rr - rl + 1;
            if (rn > 0){
              if (rn <= rw)
                k = rn;
              else
                k = rw;
              //Rprintf("th=%d ls=%d, i=%d j=%d rs=%d k=%d ll=%d lr=%d rl=%d rr=%d\n", th, 1, i, j, 1, k, ll, ll+(k-1), rr-(k-1), rr); R_FlushConsole();
              *reloc_writer++ = ll;
              *reloc_writer++ = lr;
              *reloc_writer++ = rl;
              // *reloc_writer++ = rr;
              reloc_count++;
              if (k < rn){
                R = rr - k;
              }else{
                R = args[j].ll - 1;
                j--;
              }
              break;
            }else{
              R = args[j].ll - 1;
              j--;
            }
          }
        }
        // end - first end second right search loop // first instance //
        e += k;
        if (e >= lw){
          //Rprintf("closing %d\n", th);
          rargs[th].x = x;
          rargs[th].lllrrlrr = lllrrlrr;
          rargs[th].origin = reloc_origin;
          rargs[th].count = reloc_count;
          //PRelocate(rargs + th);
          pthread_create(threads+th, NULL, PRelocate, rargs+th);
          reloc_origin += reloc_count;
          reloc_count = 0;
          th++;
          RE += e; e = 0;
        }
        if (k < ln){
          L = ll + k;
          continue;
        }
    }
    L = args[i].rr + 1;
    i--;
  }
  if (e){
    //Rprintf("closing %d\n", th);
    rargs[th].x = x;
    rargs[th].lllrrlrr = lllrrlrr;
    rargs[th].origin = reloc_origin;
    rargs[th].count = reloc_count;
    //PRelocate(rargs + th);
    pthread_create(threads+th, NULL, PRelocate, rargs+th);
    th++;
    RE += e;
  }

  for (i=0; i<th; i++){
    pthread_join(threads[i], NULL);
  }
  // end - first end second left search loop // for output

  FREE(rargs);
  FREE(lllrrlrr);
  FREE(threads);
  FREE(args);

  if (LE != RE)
    error("LE != RE : %d != %d\n", LE, RE);

  return Lsplit;
}




static void * SQuicksort2(void *arg)
{
  PQuicksort2_t *parg = (PQuicksort2_t*)arg;
  if (parg->threads <= 1 || ((parg->r - parg->l + 1) <= THREAD_LIMIT_SORT)){
    Quicksort2(parg->x, parg->l, parg->r);
  }else{
    pthread_t thread0, thread1;
    PQuicksort2_t Larg,Rarg;

    ValueT *x = parg->x;
    IndexT l = parg->l;
    IndexT r = parg->r;
    double t = parg->threads;

    IndexT i = SQuicksort2_partition4(x, l, r, t);

    //Quicksort2(x, l, k-1);
    //Quicksort2(x, k+1, r);
    Larg.x = x;
    Larg.l = l;
    Larg.r = i;
    Larg.threads = (i-l+1)/((double)(r-l+1));
    Rarg.x = x;
    Rarg.l = i+1;
    Rarg.r = r;
    Rarg.threads = parg->threads - Larg.threads;
    // SQuicksort2(&Larg);
    // SQuicksort2(&Rarg);
    pthread_create(&thread0, NULL, SQuicksort2, &Larg);
    pthread_create(&thread1, NULL, SQuicksort2, &Rarg);
    pthread_join(thread0, NULL);
    pthread_join(thread1, NULL);
  }
  return NULL;
}

*/

void PQuicksort2_insitu(ValueT *x, IndexT n, double t)
{
  //Quicksort2(x, 0, n-1, t);
  PQuicksort2_t arg;
  arg.x = x;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PQuicksort2(&arg);
}

void PQuicksort2_exsitu(ValueT *x, IndexT n, double t)
{
  ValueT * aux = (ValueT *) MALLOC(n, ValueT);
  Scopy(aux, x, n, t);

  //Quicksort2(aux, 0, n-1, t);
  PQuicksort2_t arg;
  arg.x = aux;
  arg.l = 0;
  arg.r = n-1;
  arg.threads = t;
  PQuicksort2(&arg);

  Scopy(x, aux, n, t);
  FREE(aux);
}

//#undef PRELOCATE_SIZE
#undef USE_MEDIAN_OF_3_PIVOT
