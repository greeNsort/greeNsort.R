/*
# greeNsort direct Frogsort1 for variable-sized strings
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "VInsertionsort_l2r.h"
#include "VFrogmerge1.h"

/*

 let n be size of the data measured in some units
 let b be the size of the buffer measured in the same units
 let N = n+b be the size of data and buffer

 A generic (sizevarying) balanced splitting is given by (VFrogsort)

 initialize  150% memory (N = n+n/2) with chunks {data left|buffer right} of size N = n + b = n + n/2 + c} where c is 0 for even n (n%%2==0) and c is 1 for every second odd n ()

 Search in data&buffer for a valid split such that
 N = Ni + No where Ni <= No
 no = No + 2*No/3 + (No%%3 > 0 ? 1 : 0)
 ni = n - no
 b = N - n

 For equally sized elements a simpler balanced splitting is given by (Frogsort1)
 assign 150% memory (N = n+b = n + n/2)
 split data by pure calculation (no searching) such that
 ni = n/2, no = n-ni hence ni <= no
 bi = ni/2, bo = no/2
 br = b - bi - bo
 in the left leaves of the initial split place data {no|bo|br|bi|ni}
 in the right leaves of the initial split place data {ni|bi|br|bo|no}

 For an even number of equaly sized elements we have exactly

 n = 2*b hence
 N = 3*b

 and a balanced splitting is given (Frogsort0)

 initialize  150% memory with m=n/2 triplets {n=1|b=1|n=1}
 the split the m triplets such that
 mi = m/2
 mo = m - mi


 */


static IndexT VFrogsort1_init(
    char *x
  , char *y
  , IndexT m
)
{
  IndexT r=0,s,b;
  char *X=x+m, *y0=y, *Y;
  while (x<X){
    s = strcpylen_lr(y, x);
    R_FlushConsole();
    b = s/2;
    x+=s;
    y+=s;
    Y = y + b;
    for(;y<Y;y++)
      *y = BUFFER_CHAR;
    // count the number of odd strings in r
    r += s % 2;
    // for every second odd string add an extra buffer element
    if (r==2){
      *(y++) = BUFFER_CHAR;
      r = 0;
    }
    // hence an even string sum n == 2*b and an odd string sum n == 2*b + 1
    // hence always b == n/2
    R_FlushConsole();
  }
  return y - y0;
}



static void VFrogsort1_sort_right(
    char *y
  , char *z
  , IndexT n
  , IndexT N
  , IndexT R
);

static void VFrogsort1_sort_left(
    char *y
  , char *z
  , IndexT n
  , IndexT N
  , IndexT L
){
  if (n >= 1){
    IndexT R,LR,RL,LN,RN,ln,rn;
    R = L+N-1;
#if VINSERTIONSORT_LIMIT > 0
    if (n <= VINSERTIONSORT_LIMIT){
      //Rprintf("VFrogsort1_sort_left VINSERTIONSORT_LIMIT n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
      strdebuf_lr(y+L, y+L, y+R);
      VInsertionsort_l2r(y, z, L, L+n-1);
      return;
    }
#endif
    RL = L + N/2 + N%2;
    LR = RL-1;
    // search for split to right
    if (((unsigned char)y[LR]) > ((unsigned char)BUFFER_CHAR)){
      // because character is not terminator string has at least length two hence at least one buffer following hence at least another null terminator thereafter
      while(((unsigned char)y[RL]) > ((unsigned char)BUFFER_CHAR))
        RL++;
      // now first \0
      RL++;
    }
    while (y[RL] == BUFFER_CHAR) // terminated by string or string terminator
      RL++;
    LR = RL-1;

    LN = LR - L + 1;
    RN = N - LN;
    ln = 2*(LN / 3) + (LN % 3 > 0 ? 1 : 0);
    rn = n - ln;
    if (RN<=0){
      //Rprintf("VFrogsort1_sort_left no feasible split n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
      strdebuf_lr(y+L, y+L, y+R);
      VInsertionsort_l2r(y, z, L, L+n-1);
      return;
    }
    //Rprintf("VFrogsort1_sort_left splitting n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
    VFrogsort1_sort_left (y, z, ln, LN, L);
    VFrogsort1_sort_right(y, z, rn, RN, R);
    VFrogsort1P_merge_asc_left_stable(y+L+n-1, y+L, y+L+ln-1, y+R-rn+1, y+R);
  }
}

static void VFrogsort1_sort_right(
    char *y
  , char *z
  , IndexT n
  , IndexT N
  , IndexT R
)
{
  if (n >= 1){
    IndexT L,LR,RL,LN,RN,ln,rn;
    L = R-N+1;
#if VINSERTIONSORT_LIMIT > 0
    if (n <= VINSERTIONSORT_LIMIT){
      //Rprintf("VFrogsort1_sort_right VINSERTIONSORT_LIMIT n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
      strdebuf_rl(y+R, y+L, y+R);
      VInsertionsort_l2r(y, z, R-n+1, R);
      return;
    }
#endif
    LR = R - N/2 - N%2;
    RL = LR + 1;
    // search for split to left
    if (y[RL] == BUFFER_CHAR){
      while(y[LR] != NULL_CHAR){
        LR--;
      }
      // now \0 of previous string
      LR--;
      // now one left of \0 of previous string
    }
    while (((unsigned char)y[LR]) > ((unsigned char)BUFFER_CHAR))
      LR--;

    RL = LR + 1;

    RN = R - RL + 1;
    LN = N - RN;
    rn = 2*(RN / 3) + (RN % 3 > 0 ? 1 : 0);
    ln = n - rn;
    //chrprint("VFrogsort1_sort_right", y, L, R);
    if (LN<=0){
      //Rprintf("VFrogsort1_sort_right no feasible split n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
      strdebuf_rl(y+R, y+L, y+R);
      VInsertionsort_l2r(y, z, R-n+1, R);
      return;
    }
    //Rprintf("VFrogsort1_sort_right splitting n=%d ln=%d rn=%d l=%d r=%d N=%d LN=%d RN=%d L=%d LR=%d RL=%d R=%d\n", n, ln, rn, l, r, N, LN, RN, L, LR, RL, R);
    VFrogsort1_sort_left (y, z, ln, LN, L);
    VFrogsort1_sort_right(y, z, rn, RN, R);
    VFrogsort1P_merge_asc_right_stable(y+R-n+1, y+L, y+L+ln-1, y+R-rn+1, y+R);
  }
}


void VFrogsort1_exsitu(
    char *x   // we have leading sentinel \0 at x[0]
, IndexT n  // n strings, the first at x+1
, IndexT m  // m chars including trailing \0 and leading \0
, IndexT b  // buffer size (of longest string incl. trailing \0)
, PerfT *p
)
{
  IndexT M,i, mb = (m-1) / 2;
  IndexT maux = (m-1) + mb;
  char *aux = (char *) MALLOC(maux+1, char);
  char *buf = (char *) MALLOC(b, char);
  aux[0] = NULL_CHAR;
  M = VFrogsort1_init(x+1, aux+1, (m-1));
  if (M != maux)
    error("maux=%d M=%d\n", maux, M);
  VFrogsort1_sort_left(aux, buf, (m-1), M, 1);
  for (i=1;i<m;i++){
    x[i] = aux[i];
  }
  FREE(buf);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = ((maux+1+b)*sizeof(char)) / (m*(double)sizeof(char));
}


void VFrogsort1_insitu(
  char *x  // we have sentinel \0 at x[0], n is the number of character-bytes incl. null-terminators (excl. init sentinel)
, IndexT n  // n strings
, IndexT m  // m chars including leading and trailing \0
, IndexT b  // buffer size (of longest string incl. \0)
, PerfT *p
)
{
  IndexT mr,ml;

  // minimum size to be sorted in buffer
  ml = (m-1)/3 + (m-1)%3;
  // search for terminator to right
  ml = nullGE(x, ml);

  if (ml <= 1 || ml >= (m-1)){
    VFrogsort1_exsitu(x, n, m, b, p);
    return;
  }
  mr = (m-1) - ml;
  IndexT bl = ml / 2;
  IndexT br = mr / 2;
  IndexT maux = ml + bl;
  char *aux = (char *) MALLOC(maux+1, char);
  char *buf = (char *) MALLOC(b, char);

  //Rprintf("maux=%d m=%d ml+bl=%d ml=%d bl=%d  mr+br=%d mr=%d br=%d\n", maux, m, ml+bl, ml, bl, mr+br, mr, br);

  //chrprint("initial", x, 1, m-1);
  // ML = VFrogsort1_init(x+1, aux+1, ml);
  // assert(ML == (ml+bl));
  VFrogsort1_init(x+1, aux+1, ml);
  aux[0] = NULL_CHAR;
  //chrprint("left init", aux, 1, ML);
  // MR = VFrogsort1_init(x+1+ml, x+m-mr-br, mr);
  // assert(MR == (mr+br));
  VFrogsort1_init(x+1+ml, x+m-mr-br, mr);
  x[m-mr-br-1] = NULL_CHAR;
  //chrprint("rigt init", x, m-mr-br, m-1);

  //Rprintf("maux=%d m=%d ML=%d ml+bl=%d ml=%d bl=%d  MR=%d mr+br=%d mr=%d br=%d\n", maux, m, ML, ml+bl, ml, bl, MR, mr+br, mr, br);

  VFrogsort1_sort_left (aux, buf, ml, ml+bl,   1);
  //chrprint("left sort", aux, 1, ML);
  VFrogsort1_sort_right(  x, buf, mr, mr+br, m-1);
  //chrprint("rigt sort", x, 1, m-1);

  VFrogsort1P_merge_asc_right_stable(x+1, aux+1, aux+ml, x+m-mr, x+m-1);

  //VFrogsort1P_merge_asc_right_full(x+1, aux+1, aux+ML, x+m-MR+1, x+m);

  //VFrogsort1_initsort_left (x+1     , aux+1              , buf, ml, 0     , 0          );
  //VFrogsort1_initsort_right(x+1 + ml, x+1 + (m - mr - br), buf, mr, mr - 1, mr + br - 1);
  // VFrogsort1_init_left (x+1     , aux+1              , buf, ml, 0     , 0          );
  // VFrogsort1_init_right(x+1 + ml, x+1 + (m - mr - br), buf, mr, mr - 1, mr + br - 1);
  // VFrogsort1_sort_left (aux+1              , buf, ml, 0     );
  // VFrogsort1_sort_right(x+1 + (m - mr - br), buf, mr, mr + br - 1);
  //VFrogsort1_merge_asc_right_final(x+1, aux+1, ml, x+1 + (m - mr), mr);
  FREE(buf);
  FREE(aux);
  p->secs = getNewSecs();
  p->size = ((m+maux+1+b)*sizeof(char)) / (m*(double)sizeof(char));
}

