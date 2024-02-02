/*
# greeNsort Octosort (pointer implementation)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#if INSERTIONSORT_LIMIT > 0
#  include "Insertionsort_l2r.h"
#  include "Insertionsort_r2l.h"
#endif
#include <stdbool.h>


// Attention: naming of full-merges was aligned with half-merges
// this is NOT identical to OctosortP.c.digital

// 0..3 activating this costs one extra comparison per eligible merge (this is what makes it O(N) bestcase)
// = 0 no adaptive tuning
// > 0 tries omitting correlated full merges (1 = DEFAULT)
// > 1 tries skipping comparisons in correlated half merges
// > 2 tries skipping comparisons in conflicted merges
#define OMIT_PRESORTING 1

// 0 = write correlated to any target (0 = DEFAULT)
// 1 = write correlated to original
#define FORCE_ORIGINAL 0

//#define VERBOSE_FINAL_ACTION
//#define DEBUG_PRINT 1

// fullmerge is NOT called IF OMIT_PRESORTING > 0
// halfmerge is ALWAYS called for any OMIT_PRESORTING

// ========== Summary of Merge functions ==========

// ---------- asc asc asc -- asc asc l2r -- asc asc written from left ------------
// OctosortP_fullmerge_asc_asc_l2r
// OctosortP_halfmerge_asc_asc_l2r (FORCE_ORIGINAL: OctosortP_halfmerge_asc_asc_r2l)

// ---------- desc desc desc -- des des r2l -- desc desc written from right --------------
// OctosortP_fullmerge_des_des_r2l
// OctosortP_halfmerge_des_des_r2l (FORCE_ORIGINAL: OctosortP_halfmerge_des_des_l2r)

// ---------- asc desc asc -- asc des r2l -- asc desc written from right, ties preferred from right --------------
// OctosortP_fullmerge_asc_des_r2l
// OctosortP_halfmerge_asc_des_r2l

// ---------- desc asc asc -- des asc l2r -- desc asc written from left, ties preferred from left --------------
// OctosortP_fullmerge_des_asc_l2r
// OctosortP_halfmerge_des_asc_l2r

// ---------- asc desc desc (NEW) -- asc dec l2r -- asc desc written from left, ties preferred from right --------------
// OctosortP_fullmerge_asc_des_l2r
// OctosortP_halfmerge_asc_des_l2r

// ---------- desc asc desc (NEW) -- des asc r2l -- desc asc written from right, ties preferred from left --------------
// OctosortP_fullmerge_des_asc_r2l
// OctosortP_halfmerge_des_asc_r2l


// {---------- correlated ------------

// {---------- asc asc asc -- asc asc l2r -- asc asc written from left ------------
// ties preferred from left
// hence ties are stable

static void OctosortP_fullmerge_asc_asc_l2r(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT v=*rl;
  // OMIT_PRESORTING handled outside
  ValueT u=*ll;
  for (;;){
    if (LT(v, u)){
      *(k++) = v;
      ++rl;
      if (rl > rr)  // check either here
        break;
      v = *rl;
    }else{
      *(k++) = u;
      ++ll;
      if (ll > lr)  // or check here
        break;
      u = *ll;
    }
  }
  while(ll <= lr)
    *(k++) = *(ll++);
  while(rl <= rr)
    *(k++) = *(rl++);
}

// here the right half is already stored in rl..rr
static void OctosortP_halfmerge_asc_asc_l2r(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = rl - (lr-ll+1);
  ValueT v=*rl;
#if OMIT_PRESORTING > 1
  if (LT(v, *lr)){  // if we are not presorted we compare and merge
#endif
    ValueT u=*ll;
    for (;;){
      if (LT(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        ++ll;
        if (ll > lr)  // or check here
          break;
        u = *ll;
      }
    }
#if OMIT_PRESORTING > 1
  }
#endif
  // move the rest of the left half to right
  while(ll <= lr)
    *(k++) = *(ll++);
}

#if FORCE_ORIGINAL > 0
// here the left half is already stored in ll..lr
static void OctosortP_halfmerge_asc_asc_r2l(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = lr + (rr-rl+1);
  ValueT u=*lr, v=*rr;
#if OMIT_PRESORTING > 1
  if (GT(u, *rl)){  // if we are not presorted we compare and merge
#endif
    for (;;){
      if (GE(v, u)){
        *(k--) = v;
        --rr;
        if (rl > rr)  // check either here
          break;
        v = *rr;
      }else{
        *(k--) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 1
  }
#endif
  // move the rest of the right half to left
  while(rl <= rr)
    *(k--) = *(rr--);
}
#endif

// ---------- asc asc asc --------------}



// {---------- desc desc desc -- des des r2l -- desc desc written from right --------------
// ties preferred from LEFT
// ties were already reversed and hence remain reversed (exception: ties crossing the border are reversed now)

static void OctosortP_fullmerge_des_des_r2l(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  k += (lr-ll) + (rr-rl) + 1;
  ValueT u=*lr;
  // OMIT_PRESORTING handled outside
  ValueT v=*rr;
  for (;;){
    if (LT(v, u)){
      *(k--) = v;
      --rr;
      if (rl > rr)  // check either here
        break;
      v = *rr;
    }else{
      *(k--) = u;
      --lr;
      if (ll > lr)  // or check here
        break;
      u = *lr;
    }
  }
  while(rl <= rr)
    *(k--) = *(rr--);
  while(ll <= lr)
    *(k--) = *(lr--);
}

// here the left half is already stored in ll..lr
static void OctosortP_halfmerge_des_des_r2l(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = lr + (rr-rl+1);
  ValueT u=*lr;
#if OMIT_PRESORTING > 1
  if (LE(u, *rl)){  // if we are not presorted we compare and merge
#endif
    ValueT v=*rr;
    for (;;){
      if (LT(v, u)){
        *(k--) = v;
        --rr;
        if (rl > rr)  // check either here
          break;
        v = *rr;
      }else{
        *(k--) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 1
  }
#endif
  // move the rest of the right half to left
  while(rl <= rr)
    *(k--) = *(rr--);
}

#if FORCE_ORIGINAL > 0
// here the right half is already stored in rl..rr
static void OctosortP_halfmerge_des_des_l2r(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = rl - (lr-ll+1);
  ValueT u=*ll, v=*rl;
#if OMIT_PRESORTING > 1
  if (GE(v, *lr)){  // if we are not presorted we compare and merge
#endif
    for (;;){
      if (GE(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        ++ll;
        if (ll > lr)  // or check here
          break;
        u = *ll;
      }
    }
#if OMIT_PRESORTING > 1
  }
#endif
  // move the rest of the left half to right
  while(ll <= lr)
    *(k++) = *(ll++);
}
#endif

// ---------- desc desc desc --------------}


// ---------- correlated ------------ }




// {---------- conflicted ------------


// {---------- asc desc asc -- asc des r2l -- asc desc written from right, ties preferred from right --------------

// ties on right were reversed and are stable now

// digital had OctosortP_fullmerge_asc_des_l2r
static void OctosortP_fullmerge_asc_des_r2l(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  k += (lr-ll) + (rr-rl) + 1;
  ValueT u=*lr;
#if OMIT_PRESORTING > 2
  if (LT(*rr,u)){  // if we are not presorted we compare and merge
#endif
    ValueT v=*rl;
    for (;;){
      if (GE(v, u)){
        *(k--) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k--) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  while(rl <= rr)
    *(k--) = *(rl++);
  while(ll <= lr)
    *(k--) = *(lr--);
}

// here the left half is already stored in ll..lr
static void OctosortP_halfmerge_asc_des_r2l(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = lr + (rr-rl+1);
  ValueT u=*lr;
#if OMIT_PRESORTING > 2
  if (LT(*rr,u)){  // if we are not presorted we compare and merge
#endif
    ValueT v=*rl;
    for (;;){
      if (GE(v, u)){
        *(k--) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k--) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  // move the rest of the right half to left
  while(rl <= rr)
    *(k--) = *(rl++);
}

// ---------- asc desc asc --------------}

// {---------- desc asc asc -- des asc l2r -- desc asc written from left, ties preferred from left --------------

// ties on left were reversed and are stable now
static void OctosortP_fullmerge_des_asc_l2r(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT v=*rl;
#if OMIT_PRESORTING > 2
  if (LT(v, *ll)){  // if we are not presorted we compare and merge
#endif
    ValueT u=*lr;
    for (;;){
      if (LT(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  while(ll <= lr)
    *(k++) = *(lr--);
  while(rl <= rr)
    *(k++) = *(rl++);
}

// here the right half is already stored in rl..rr
static void OctosortP_halfmerge_des_asc_l2r(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = rl - (lr-ll+1);
  ValueT v=*rl;
#if OMIT_PRESORTING > 2
  if (LT(v, *ll)){  // if we are not presorted we compare and merge
#endif
    ValueT u=*lr;
    for (;;){
      if (LT(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  // move the rest of the left half to right
  while(ll <= lr)
    *(k++) = *(lr--);
}

// ---------- desc asc asc --------------}

// {---------- asc desc desc (NEW) -- asc dec l2r -- asc desc written from left, ties preferred from right --------------

// ties on right were reversed, now both are reversed

// digital had OctosortP_fullmerge_asc_des_r2l
static void OctosortP_fullmerge_asc_des_l2r(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  //Rprintf("/\\\n");
  ValueT v=*rl;
#if OMIT_PRESORTING > 2
  if (GE(v, *ll)){  // if we are not presorted we compare and merge
#endif
    ValueT u=*lr;
    for (;;){
      if (GE(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  while(rl <= rr)
    *(k++) = *(rl++);
  while(ll <= lr)
    *(k++) = *(lr--);

}

// here the right half is already stored in rl..rr
static void OctosortP_halfmerge_asc_des_l2r(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = rl - (lr-ll+1);
  ValueT v=*rl;
#if OMIT_PRESORTING > 2
  if (GE(v, *ll)){  // if we are not presorted we compare and merge
#endif
    ValueT u=*lr;
    for (;;){
      if (GE(v, u)){
        *(k++) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k++) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  // move the rest of the left half to right
  while(ll <= lr)
    *(k++) = *(lr--);
}

// ---------- asc desc desc (NEW) --------------}

// {---------- desc asc desc (NEW) -- des asc r2l -- desc asc written from right, ties preferred from left --------------

// ties on left were reversed, now both are reversed
static void OctosortP_fullmerge_des_asc_r2l(ValueT *k, ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  //Rprintf("\\/\n");
  k += (lr-ll) + (rr-rl) + 1;
  ValueT u=*lr;
#if OMIT_PRESORTING > 2
  if (GE(*rr,u)){  // if we are not presorted we compare and merge
#endif
    ValueT v=*rl;
    for (;;){
    if (LT(v, u)){
      *(k--) = v;
      ++rl;
      if (rl > rr)  // check either here
        break;
      v = *rl;
    }else{
      *(k--) = u;
      --lr;
      if (ll > lr)  // or check here
        break;
      u = *lr;
    }
  }
#if OMIT_PRESORTING > 2
  }
#endif
  while(ll <= lr)
    *(k--) = *(lr--);
  while(rl <= rr)
    *(k--) = *(rl++);
}


// here the left half is already stored in ll..lr
static void OctosortP_halfmerge_des_asc_r2l(ValueT *ll, ValueT *lr, ValueT *rl, ValueT *rr){
  ValueT *k = lr + (rr-rl+1);
  ValueT u=*lr;
#if OMIT_PRESORTING > 2
  if (GE(*rr,u)){  // if we are not presorted we compare and merge
#endif
    ValueT v=*rl;
    for (;;){
      if (LT(v, u)){
        *(k--) = v;
        ++rl;
        if (rl > rr)  // check either here
          break;
        v = *rl;
      }else{
        *(k--) = u;
        --lr;
        if (ll > lr)  // or check here
          break;
        u = *lr;
      }
    }
#if OMIT_PRESORTING > 2
  }
#endif
  // move the rest of the right half to left
  while(rl <= rr)
    *(k--) = *(rl++);
}

// ---------- desc asc desc (NEW) --------------}


// ---------- conflicted ------------}




// dir >= 0 presorted
// dir < 0 reverse-sorted

static ValueT * OctosortP_recurse(ValueT *ori, ValueT *tar, ValueT *src, IndexT n, IndexT *dir){
  IndexT r=n-1;
#if INSERTIONSORT_LIMIT > 0
  if ((n-1) < INSERTIONSORT_LIMIT){
    if ((LT(*(ori+r), *ori))){
      *dir = -n;
      Insertionsort_r2l(ori, 0, r);
    }else{
      *dir = +n;
      Insertionsort_l2r(ori, 0, r);
    }
    return ori;
  }
#else
  if (n<2){
    *dir = +n;
    return ori;
  }
  if (n == 2){
    *dir= LT(*(ori+r), *ori) ? -n : +n ; // we do nothing other than returning the info about order
    return ori;
  }
#endif
  ValueT *L, *R;
  IndexT ldir, rdir;
  IndexT m = n/2;
  L = OctosortP_recurse(ori, src, tar, m, &ldir);
  R = OctosortP_recurse(ori+m, src+m, tar+m, n-m, &rdir) - m;
  *dir = ldir + rdir;      // let the majority decide the new direction
#ifdef DEBUG_PRINT
  Rprintf("\nn=%d m=%d ldir=%d rdir=%d dir=%d L=ori=%d R==ori=%d\n", n, m, ldir, rdir, *dir, L==ori, R==ori);
  int i;
  for (i=0;i<m;i++)
    Rprintf("L[%d]=%f\n", i, L[i]);
  for (;i<n;i++)
    Rprintf("R[%d]=%f\n", i, R[i]);
#endif
  if (*dir < 0){
    if ((ldir < 0)) {
      if ((rdir < 0)) {
        // des des => des
        if (L == R){
#if OMIT_PRESORTING > 0
          if (LT(L[m], L[m-1])) return L;
#endif
          if (L==src){
            OctosortP_fullmerge_des_des_r2l(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_des_des_r2l(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        } else {
#if FORCE_ORIGINAL > 0
          if (L == ori){
#endif
            OctosortP_halfmerge_des_des_r2l(L, L+m-1, R+m, R+r); return L;
#if FORCE_ORIGINAL > 0
          }else{
            OctosortP_halfmerge_des_des_l2r(L, L+m-1, R+m, R+r); return R;
          }
#endif
        }
      } else {
        // des asc => des
        if (L == R){
          if (L==src){
            OctosortP_fullmerge_des_asc_r2l(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_des_asc_r2l(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        } else {
            OctosortP_halfmerge_des_asc_r2l(L, L+m-1, R+m, R+r); return L;
        }
      }
    } else {
      //  (rdir < 0) because dir < 0 & ldir >= 0
        // asc des => des
        //NEW
        if (L == R){
          if (L==src){
            OctosortP_fullmerge_asc_des_l2r(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_asc_des_l2r(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        } else {
            OctosortP_halfmerge_asc_des_l2r(L, L+m-1, R+m, R+r); return R;
        }
    }
  } else {
    if ((ldir < 0)) {
      // (rdir >= 0) because dir >= 0 & ldir < 0
        // des asc => asc
        if (L == R){
          if (L==src){
            OctosortP_fullmerge_des_asc_l2r(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_des_asc_l2r(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        } else {
            OctosortP_halfmerge_des_asc_l2r(L, L+m-1, R+m, R+r); return R;
        }
    } else {
      if ((rdir < 0)) {
        // asc des => asc
        if (L == R){
          if (L==src){
            OctosortP_fullmerge_asc_des_r2l(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_asc_des_r2l(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        } else {
            OctosortP_halfmerge_asc_des_r2l(L, L+m-1, R+m, R+r); return L;
        }
      } else {
        // asc asc => asc
        if (L==R){
#if OMIT_PRESORTING > 0
          if (LE(R[m-1], R[m])) return R;
#endif
          if (L==src){
            OctosortP_fullmerge_asc_asc_l2r(tar, src, src+m-1, src+m, src+r); return tar;
          }else{
            OctosortP_fullmerge_asc_asc_l2r(src, tar, tar+m-1, tar+m, tar+r); return src;
          }
        }else{
#if FORCE_ORIGINAL > 0
          if (R == ori){
#endif
            OctosortP_halfmerge_asc_asc_l2r(L, L+m-1, R+m, R+r); return R;
#if FORCE_ORIGINAL > 0
          }else{
            OctosortP_halfmerge_asc_asc_r2l(L, L+m-1, R+m, R+r); return L;
          }
#endif
        }
      }
    }
  }

}



void OctosortP_insitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n, ValueT);
  IndexT dir;
  ret = OctosortP_recurse(x, x, aux, n, &dir);
  if (ret==aux){
    if ((dir<0)){
#ifdef VERBOSE_FINAL_ACTION
      Rprintf("OctosortP_insitu auxrev\n");
#endif
      for (int j=n-1,i=0; i<n; i++,j--)
        x[i] = aux[j];
    }else{
#ifdef VERBOSE_FINAL_ACTION
      Rprintf("OctosortP_insitu aux\n");
#endif
      for (i=0; i<n; i++)
        x[i] = aux[i];
    }
    FREE(aux);
  }else{
    FREE(aux);
    if ((dir<0)){
#ifdef VERBOSE_FINAL_ACTION
      Rprintf("OctosortP_insitu xrev\n");
#endif
      ValueT t;
      for (int j=n-1,i=0;i<j;i++,j--){
        t = x[i];
        x[i] = x[j];
        x[j] = t;
      }
    }
  }
}

void OctosortP_exsitu(ValueT *x, IndexT n)
{
  IndexT i;
  ValueT *ret;
  ValueT *aux = (ValueT *) MALLOC(n+n, ValueT);
  ValueT *aux2 = aux + n;
  for (i=0; i<n; i++)
    aux[i] = x[i];
  IndexT dir;
  ret = OctosortP_recurse(aux, aux, aux2, n, &dir);
#ifdef DEBUG_PRINT
  Rprintf("OctosortP_exsitu dir=%d\n", dir);
#endif
  if ((dir<0)){
    for (int j=n-1,i=0; i<n; i++,j--)
      x[i] = ret[j];
  }else{
    for (i=0; i<n; i++)
      x[i] = ret[i];
  }
  FREE(aux);
}
