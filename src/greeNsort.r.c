/*
# greeNsort package gluecode (and some implemetations)
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "greeNsort.r.h"
#include <Rdefines.h>  // needed here for definition of R_PosInf R_NegInf and R_alloc GetRNGstate PutRNGstate
#include "lib_timing.h"
#include "lib_energy.h"
#include "cmptools.h"
#include "strtools.h"  // for BUFFER_CHAR

//#include "unistd.h"

//static INSERTIONSORT_L2R(Insertionsort_l2r)
#ifdef SCANNED_RANGE
  double scanned_range;
#endif


SEXP r_cran_compatible()
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP, 1) );
#ifdef CRAN_COMPATIBLE
  LOGICAL(ret_)[0] = TRUE;
#else
  LOGICAL(ret_)[0] = FALSE;
#endif
  UNPROTECT(1);
  return ret_;
}


SEXP r_insertionsort_limit()
{
  SEXP ret_;
  if (INSERTIONSORT_LIMIT == 1)
    error("INSERTIONSORT_LIMIT == 1");
  if (INSERTIONSORT_LIMIT != TRIPLET_LIMIT*2)
    error("INSERTIONSORT_LIMIT != TRIPLET_LIMIT*2");
  PROTECT( ret_ = allocVector(INTSXP, 1) );
#if INSERTIONSORT_LIMIT>0
  INTEGER(ret_)[0] = INSERTIONSORT_LIMIT;
#else
  ret_ = R_NilValue;
#endif
  UNPROTECT(1);
  return ret_;
}


SEXP r_string_check(
    SEXP x_
)
{
  SEXP ret_;
  int nx = LENGTH(x_);
  PROTECT( ret_ = allocVector(INTSXP, nx) );
  int* ret = INTEGER(ret_);
  char *sx;
  int ns, i, j;
  for (i=0; i<nx; i++){
    ret[i] = 0;
    sx = (char *) CHAR(STRING_ELT(x_, i));
    ns = strlen(sx);
    for (j=0; j<ns; j++){
      if (sx[j] == BUFFER_CHAR){
        ret[i] = j;
        break;
      }
    }
  }
  UNPROTECT(1);
  return ret_;
}


SEXP r_stable_keys(
    SEXP x_
)
{
  SEXP ret_;
  int n = LENGTH(x_);
  switch (TYPEOF(x_)){
  //case LGLSXP:
  //case INTSXP:
  case REALSXP:
  {
    PROTECT( ret_ = allocVector(REALSXP, n) );
    double *x,*ret;
    x = REAL(x_);
    ret = REAL(ret_);
    for (int i=0; i<n; i++)
      ret[i] = stable_dblkey(x[i]);
    break;
  }
  case STRSXP:
  {
    PROTECT( ret_ = allocVector(STRSXP, n) );
    char c[21];
    for (int i=0; i<n; i++){
      stable_strkey(c, (char *) CHAR(STRING_ELT(x_, i)));
      SET_STRING_ELT(ret_, i, mkChar(c));
    }
    break;
  }
  default:
    error("unimplemented type in stable_keys");
  }
  UNPROTECT(1);
  return ret_;
}


SEXP r_current_keys(
    SEXP x_
)
{
#ifdef STABLE_TEST
  SEXP ret_;
  int n = LENGTH(x_);
  switch (TYPEOF(x_)){
  //case LGLSXP:
  //case INTSXP:
  case REALSXP:
  {
    PROTECT( ret_ = allocVector(REALSXP, n) );
    double *x,*ret;
    x = REAL(x_);
    ret = REAL(ret_);
    for (int i=0; i<n; i++)
      ret[i] = stable_dblkey(x[i]);
    break;
  }
  case STRSXP:
  {
    PROTECT( ret_ = allocVector(STRSXP, n) );
    char c[21];
    for (int i=0; i<n; i++){
      stable_strkey(c, (char *) CHAR(STRING_ELT(x_, i)));
      SET_STRING_ELT(ret_, i, mkChar(c));
    }
    break;
  }
  default:
    error("unimplemented type in current_keys");
  }
  UNPROTECT(1);
  return ret_;
#else
  return x_;
#endif
}


SEXP r_current_compare(
  SEXP x_
, SEXP y_
)
{
  SEXP ret_;
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  if (nx != ny)
    error("lengths differ");
  PROTECT( ret_ = allocVector(INTSXP, nx) );
  int* ret = INTEGER(ret_);

  switch (TYPEOF(x_)){
  //case LGLSXP:
  //case INTSXP:
  case REALSXP:
  {
    double *x,*y;
    x = REAL(x_);
    y = REAL(y_);
    for (int i=0; i<nx; i++){
      ret[i] = CMP(x[i], y[i]);
    }
    break;
  }
  case STRSXP:
  {
    char *sx, *sy;
    for (int i=0; i<nx; i++){
      sx = (char *) CHAR(STRING_ELT(x_, i));
      sy = (char *) CHAR(STRING_ELT(y_, i));
      ret[i] = STRCMP(sx, sy);
    }
    break;
  }
  default:
    error("unimplemented type in current_compare");
  }

  UNPROTECT(1);
  return ret_;
}


SEXP r_current_issorted(
    SEXP x_
)
{
  SEXP ret_;
  int nx = LENGTH(x_);
  PROTECT( ret_ = allocVector(LGLSXP, 1) );
  LOGICAL(ret_)[0] = TRUE;

  switch (TYPEOF(x_)){
  //case LGLSXP:
  //case INTSXP:
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    for (int i=1; i<nx; i++){
      if (GT(x[i-1], x[i])){
        LOGICAL(ret_)[0] = FALSE;
        break;
      }
    }
    break;
  }
  case STRSXP:
  {
    char *sx, *sy;
    for (int i=1; i<nx; i++){
      sx = (char *) CHAR(STRING_ELT(x_, i-1));
      sy = (char *) CHAR(STRING_ELT(x_, i));
      //if (STRCMP((char *) CHAR(STRING_ELT(x_, i-1)), (char *) CHAR(STRING_ELT(x_, i))) > 0){
      if (STRCMP(sx,sy) > 0){
          LOGICAL(ret_)[0] = FALSE;
        break;
      }
    }
    break;
  }
  default:
    error("unimplemented type in current_issorted");
  }

  UNPROTECT(1);
  return ret_;
}




SEXP r_perfpaths(
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(STRSXP, 4) );

#ifdef PATH_POWERCAP_PACKAGE
  SET_STRING_ELT(ret_, 0, mkChar(PATH_POWERCAP_PACKAGE));
#else
  SET_STRING_ELT(ret_, 0, NA_STRING);
#endif
  #ifdef PATH_POWERCAP_CORE
  SET_STRING_ELT(ret_, 1, mkChar(PATH_POWERCAP_CORE));
#else
  SET_STRING_ELT(ret_, 1, NA_STRING);
#endif
#ifdef PATH_POWERCAP_UNCORE
  SET_STRING_ELT(ret_, 2, mkChar(PATH_POWERCAP_UNCORE));
#else
  SET_STRING_ELT(ret_, 2, NA_STRING);
#endif
#ifdef PATH_POWERCAP_DRAM
  SET_STRING_ELT(ret_, 3, mkChar(PATH_POWERCAP_DRAM));
#else
  SET_STRING_ELT(ret_, 3, NA_STRING);
#endif

  UNPROTECT(1);
  return ret_;
}

SEXP r_perfnow(
)
{
  SEXP ret_;
  double *ret;
  PCapEnergyT now;
  PROTECT( ret_ = allocVector(REALSXP, 5) );
  ret = REAL(ret_);
  now = GreensortEnergyNow();
  ret[1] = now.package / 1000000;
  ret[2] = now.core / 1000000;
  ret[3] = now.uncore / 1000000;
  ret[4] = now.dram / 1000000;
  UNPROTECT(1);
  return ret_;
}

SEXP r_perfmax(
)
{
  SEXP ret_;
  double *ret;
  PCapEnergyT now;
  PROTECT( ret_ = allocVector(REALSXP, 5) );
  ret = REAL(ret_);
  now = GreensortEnergyMax();
  ret[0] = R_PosInf;
  ret[1] = now.package / 1000000;
  ret[2] = now.core / 1000000;
  ret[3] = now.uncore / 1000000;
  ret[4] = now.dram / 1000000;
  UNPROTECT(1);
  return ret_;
}

SEXP r_perfsleep(
    SEXP x_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  double n = asReal(x_);

  eLast = GreensortEnergyNow();
  initTicks();
  usleep((useconds_t) n*1000000);
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast);
  doneTicks();
  perf->n = n;
  perf->b = 0;
  perf->p = 0;
  perf->t = 0;
  perf->size=0;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}




SEXP r_symsearch(
    SEXP x_
  , SEXP v_          // value to be searched
  , SEXP cmp_        // // -3=LT -2=LE -1=EQL 1=EQR 2=GE 3=GT
  , SEXP decreasing_ // FALSE=ASC TRUE=DESC
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(INTSXP, 1) );
  int l = 0;
  int r = LENGTH(x_) - 1;
  Rboolean decreasing = asLogical(decreasing_);
  int cmp = asInteger(cmp_);

  initTicks();
  switch (TYPEOF(x_)){
  //case LGLSXP:
  //case INTSXP:
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    double v = asReal(v_);
    if (decreasing){
        switch (cmp){
        case -5: {
      INTEGER(ret_)[0] = bsearch_desc_WL(x, l, r, v) + 1;
      break;
    }
        case -4: {
          INTEGER(ret_)[0] = bsearch_desc_L(x, l, r, v) + 1;
          break;
        }
        case -3: {
          INTEGER(ret_)[0] = bsearch_desc_LT(x, l, r, v) + 1;
          break;
        }
        case -2: {
          INTEGER(ret_)[0] = bsearch_desc_LE(x, l, r, v) + 1;
          break;
        }
        case -1: {
        INTEGER(ret_)[0] = bsearch_desc_EQL(x, l, r, v) + 1;
        break;
      }
      case +1: {
        INTEGER(ret_)[0] = bsearch_desc_EQR(x, l, r, v) + 1;
        break;
      }
        case +2: {
        INTEGER(ret_)[0] = bsearch_desc_GE(x, l, r, v) + 1;
        break;
      }
        case +3: {
          INTEGER(ret_)[0] = bsearch_desc_GT(x, l, r, v) + 1;
          break;
        }
        case +4: {
          INTEGER(ret_)[0] = bsearch_desc_R(x, l, r, v) + 1;
          break;
        }
        case +5: {
          INTEGER(ret_)[0] = bsearch_desc_WR(x, l, r, v) + 1;
          break;
        }
        default:
        error("undefined cmp");
      }
    }else{
        switch (cmp){
        case -5: {
      INTEGER(ret_)[0] = bsearch_asc_WL(x, l, r, v) + 1;
      break;
    }
        case -4: {
          INTEGER(ret_)[0] = bsearch_asc_L(x, l, r, v) + 1;
          break;
        }
        case -3: {
          INTEGER(ret_)[0] = bsearch_asc_LT(x, l, r, v) + 1;
          break;
        }
        case -2: {
          INTEGER(ret_)[0] = bsearch_asc_LE(x, l, r, v) + 1;
          break;
        }
        case -1: {
        INTEGER(ret_)[0] = bsearch_asc_EQL(x, l, r, v) + 1;
        break;
      }
      case +1: {
        INTEGER(ret_)[0] = bsearch_asc_EQR(x, l, r, v) + 1;
        break;
      }
        case +2: {
        INTEGER(ret_)[0] = bsearch_asc_GE(x, l, r, v) + 1;
        break;
      }
        case +3: {
          INTEGER(ret_)[0] = bsearch_asc_GT(x, l, r, v) + 1;
          break;
        }
        case +4: {
          INTEGER(ret_)[0] = bsearch_asc_R(x, l, r, v) + 1;
          break;
        }
        case +5: {
          INTEGER(ret_)[0] = bsearch_asc_WR(x, l, r, v) + 1;
          break;
        }
        default:
        error("undefined cmp");
      }
    }
    break;
  }
  default:
    error("unimplemented type in symsearch");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = r + 1;
  perf->b = 8;
  perf->p = 1;
  perf->t = 1;
  perf->size = 1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}


#define SERIAL_SORT_INTINT_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (    \
SEXP x_                                                                     \
)                                                                           \
{                                                                           \
  SEXP ret_, perf_; PerfT *perf;                                            \
  PCapEnergyT eLast;                                                        \
  GreensortEnergyT eNext;                                                   \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_); \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                 \
  int n = INTEGER(GET_DIM(x_))[1];                                          \
  int m = INTEGER(GET_DIM(x_))[0];                                          \
  GetRNGstate();                                                            \
  eLast = GreensortEnergyNow(); initTicks();                                \
  switch (TYPEOF(x_)){                                                      \
  case INTSXP:                                                              \
  {                                                                         \
    int *x;                                                                 \
    x = INTEGER(x_);                                                        \
    SORTNAME(x, (IndexT) n, (IndexT) m);                                    \
    break;                                                                  \
  }                                                                         \
  default:                                                                  \
    error("unimplemented type in SORTNAME");                                \
  }                                                                         \
  perf->secs = getNewSecs();                                                \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                        \
  perf->n = n;                                                              \
  perf->b = sizeof(IntValueT)*m;                                            \
  perf->p = 1;                                                              \
  perf->t = 1;                                                              \
  perf->size=SORTSIZE;                                                      \
  perf->base = eNext.base;                                                  \
  perf->core = eNext.core;                                                  \
  perf->unco = eNext.unco;                                                  \
  perf->dram = eNext.dram;                                                  \
  setAttrib(ret_, install("perf"), perf_);                                  \
  PutRNGstate();                                                            \
  UNPROTECT(2);                                                             \
  return ret_;                                                              \
}                                                                           \



#define SERIAL_SORT_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (   \
SEXP x_                                                                    \
)                                                                          \
{                                                                          \
  SEXP ret_, perf_; PerfT *perf;                                           \
  PCapEnergyT eLast;                                                       \
  GreensortEnergyT eNext;                                                  \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_); \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                \
  int n = LENGTH(x_);                                                      \
  GetRNGstate();                                                           \
  eLast = GreensortEnergyNow(); initTicks();                               \
  switch (TYPEOF(x_)){                                                     \
  case REALSXP:                                                            \
  {                                                                        \
    double *x;                                                             \
    x = REAL(x_);                                                          \
    SORTNAME(x, (IndexT) n);                                               \
    break;                                                                 \
  }                                                                        \
  default:                                                                 \
    error("unimplemented type in SORTNAME");                               \
  }                                                                        \
  perf->secs = getNewSecs();                                               \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                       \
  perf->n = n;                                                             \
  perf->b = 8;                                                             \
  perf->p = 1;                                                             \
  perf->t = 1;                                                             \
  perf->size=SORTSIZE;                                                     \
  perf->base = eNext.base;                                                 \
  perf->core = eNext.core;                                                 \
  perf->unco = eNext.unco;                                                 \
  perf->dram = eNext.dram;                                                 \
  setAttrib(ret_, install("perf"), perf_);                                 \
  PutRNGstate();                                                           \
  UNPROTECT(2);                                                            \
  return ret_;                                                             \
}                                                                          \



#define SERIAL_SORTPART_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (           \
SEXP x_                                                                                \
  , SEXP partial_                                                                      \
)                                                                                      \
{                                                                                      \
  SEXP ret_, perf_; PerfT *perf;                                                       \
  PCapEnergyT eLast;                                                                   \
  GreensortEnergyT eNext;                                                              \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);             \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                            \
  int n = LENGTH(x_);                                                                  \
  GetRNGstate();                                                                       \
  eLast = GreensortEnergyNow(); initTicks();                                           \
  switch (TYPEOF(x_)){                                                                 \
  case REALSXP:                                                                        \
  {                                                                                    \
    double *x;                                                                         \
    x = REAL(x_);                                                                      \
    int *partial;                                                                      \
    partial = INTEGER(partial_);                                                       \
    SORTNAME(x, (IndexT) n, partial);                                                  \
    break;                                                                             \
  }                                                                                    \
  default:                                                                             \
    error("unimplemented type in SORTNAME");                                           \
  }                                                                                    \
  perf->secs = getNewSecs();                                                           \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                                   \
  perf->n = n;                                                                         \
  perf->b = 8;                                                                         \
  perf->p = 1;                                                                         \
  perf->t = 1;                                                                         \
  perf->size=SORTSIZE;                                                                 \
  perf->base = eNext.base;                                                             \
  perf->core = eNext.core;                                                             \
  perf->unco = eNext.unco;                                                             \
  perf->dram = eNext.dram;                                                             \
  setAttrib(ret_, install("perf"), perf_);                                             \
  PutRNGstate();                                                                       \
  UNPROTECT(2);                                                                        \
  return ret_;                                                                         \
}                                                                                      \


#define SERIAL_SORTREORDER_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME  (                              \
SEXP x_                                                                                              \
  , SEXP reorder_inplace_                                                                            \
)                                                                                                    \
{                                                                                                    \
  SEXP ret_, perf_; PerfT *perf;                                                                     \
  PCapEnergyT eLast;                                                                                 \
  GreensortEnergyT eNext;                                                                            \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);                           \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                                          \
  int n = LENGTH(x_);                                                                                \
  int reorder_inplace = LOGICAL(reorder_inplace_)[0];                                                \
  GetRNGstate();                                                                                     \
  eLast = GreensortEnergyNow(); initTicks();                                                         \
  switch (TYPEOF(x_)){                                                                               \
  case REALSXP:                                                                                      \
  {                                                                                                  \
    double *x;                                                                                       \
    x = REAL(x_);                                                                                    \
    SORTNAME(x, (IndexT) n, reorder_inplace, perf);                                                  \
    break;                                                                                           \
  }                                                                                                  \
  default:                                                                                           \
    error("unimplemented type in SORTNAME");                                                         \
  }                                                                                                  \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                                                 \
  perf->n = n;                                                                                       \
  perf->b = 8;                                                                                       \
  perf->p = 1;                                                                                       \
  perf->t = 1;                                                                                       \
  perf->base = eNext.base;                                                                           \
  perf->core = eNext.core;                                                                           \
  perf->unco = eNext.unco;                                                                           \
  perf->dram = eNext.dram;                                                                           \
  setAttrib(ret_, install("perf"), perf_);                                                           \
  PutRNGstate();                                                                                     \
  UNPROTECT(2);                                                                                      \
  return ret_;                                                                                       \
}                                                                                                    \





#define SERIAL_SELECT_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (     \
SEXP x_                                                                        \
)                                                                              \
{                                                                              \
  SEXP ret_, perf_; PerfT *perf;                                               \
  PCapEnergyT eLast;                                                           \
  GreensortEnergyT eNext;                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);     \
  PROTECT( ret_ = allocVector(REALSXP, 1) );                                   \
  int n = LENGTH(x_);                                                          \
  GetRNGstate();                                                               \
  eLast = GreensortEnergyNow(); initTicks();                                   \
  switch (TYPEOF(x_)){                                                         \
  case REALSXP:                                                                \
  {                                                                            \
    double *x;                                                                 \
    x = REAL(x_);                                                              \
    REAL(ret_)[0]  = SORTNAME(x, (IndexT) n);                                  \
    break;                                                                     \
  }                                                                            \
  default:                                                                     \
    error("unimplemented type in SORTNAME");                                   \
  }                                                                            \
  perf->secs = getNewSecs();                                                   \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                           \
  perf->n = n;                                                                 \
  perf->b = 8;                                                                 \
  perf->p = 1;                                                                 \
  perf->t = 1;                                                                 \
  perf->size=SORTSIZE;                                                         \
  perf->base = eNext.base;                                                     \
  perf->core = eNext.core;                                                     \
  perf->unco = eNext.unco;                                                     \
  perf->dram = eNext.dram;                                                     \
  setAttrib(ret_, install("perf"), perf_);                                     \
  PutRNGstate();                                                               \
  UNPROTECT(2);                                                                \
  return ret_;                                                                 \
}                                                                              \


#define SERIAL_SELECTK_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (    \
SEXP x_                                                                        \
  , SEXP k_                                                                    \
)                                                                              \
{                                                                              \
  SEXP ret_, perf_; PerfT *perf;                                               \
  PCapEnergyT eLast;                                                           \
  GreensortEnergyT eNext;                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);     \
  PROTECT( ret_ = allocVector(REALSXP, 1) );                                   \
  int n = LENGTH(x_);                                                          \
  int k = asInteger(k_);                                                       \
  GetRNGstate();                                                               \
  eLast = GreensortEnergyNow(); initTicks();                                   \
  switch (TYPEOF(x_)){                                                         \
  case REALSXP:                                                                \
  {                                                                            \
    double *x;                                                                 \
    x = REAL(x_);                                                              \
    REAL(ret_)[0]  = SORTNAME(x, (IndexT) n, k - 1);                           \
    break;                                                                     \
  }                                                                            \
  default:                                                                     \
    error("unimplemented type in SORTNAME");                                   \
  }                                                                            \
  perf->secs = getNewSecs();                                                   \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                           \
  perf->n = n;                                                                 \
  perf->b = 8;                                                                 \
  perf->p = 1;                                                                 \
  perf->t = 1;                                                                 \
  perf->size=SORTSIZE;                                                         \
  perf->base = eNext.base;                                                     \
  perf->core = eNext.core;                                                     \
  perf->unco = eNext.unco;                                                     \
  perf->dram = eNext.dram;                                                     \
  setAttrib(ret_, install("perf"), perf_);                                     \
  PutRNGstate();                                                               \
  UNPROTECT(2);                                                                \
  return ret_;                                                                 \
}                                                                              \


// k in [0, n-1]
#define SERIAL_SELECTK2_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME  (    \
SEXP x_                                                                         \
  , SEXP k_                                                                     \
)                                                                               \
{                                                                               \
  SEXP ret_, perf_; PerfT *perf; RangeIndexT *ret;                              \
  PCapEnergyT eLast;                                                            \
  GreensortEnergyT eNext;                                                       \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);      \
  PROTECT( ret_ = allocVector(INTSXP, 2) ); ret = (RangeIndexT *)INTEGER(ret_); \
  int n = LENGTH(x_);                                                           \
  int k = asInteger(k_);                                                        \
  GetRNGstate();                                                                \
  eLast = GreensortEnergyNow(); initTicks();                                    \
  switch (TYPEOF(x_)){                                                          \
  case REALSXP:                                                                 \
  {                                                                             \
    double *x;                                                                  \
    x = REAL(x_);                                                               \
    *ret  = SORTNAME(x, (IndexT) n, k - 1);                                     \
    ret->min++;                                                                 \
    ret->max++;                                                                 \
    break;                                                                      \
  }                                                                             \
  default:                                                                      \
    error("unimplemented type in SORTNAME");                                    \
  }                                                                             \
  perf->secs = getNewSecs();                                                    \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                            \
  perf->n = n;                                                                  \
  perf->b = 8;                                                                  \
  perf->p = 1;                                                                  \
  perf->t = 1;                                                                  \
  perf->size=SORTSIZE;                                                          \
  perf->base = eNext.base;                                                      \
  perf->core = eNext.core;                                                      \
  perf->unco = eNext.unco;                                                      \
  perf->dram = eNext.dram;                                                      \
  setAttrib(ret_, install("perf"), perf_);                                      \
  PutRNGstate();                                                                \
  UNPROTECT(2);                                                                 \
  return ret_;                                                                  \
}                                                                               \


#define SERIAL_SORT_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME(                \
SEXP x_                                                                       \
)                                                                             \
{                                                                             \
  SEXP ret_, perf_; PerfT *perf;                                              \
  PCapEnergyT eLast;                                                          \
  GreensortEnergyT eNext;                                                     \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                   \
  int n = LENGTH(x_);                                                         \
  GetRNGstate();                                                              \
  eLast = GreensortEnergyNow(); initTicks();                                  \
  switch (TYPEOF(x_)){                                                        \
  case REALSXP:                                                               \
  {                                                                           \
    double *x;                                                                \
    x = REAL(x_);                                                             \
    SORTNAME(x, (IndexT) n, perf);                                            \
    break;                                                                    \
  }                                                                           \
  default:                                                                    \
    error("unimplemented type in SORTNAME");                                  \
  }                                                                           \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
  perf->n = n;                                                                \
  perf->b = 8;                                                                \
  perf->p = 1;                                                                \
  perf->t = 1;                                                                \
  perf->base = eNext.base;                                                    \
  perf->core = eNext.core;                                                    \
  perf->unco = eNext.unco;                                                    \
  perf->dram = eNext.dram;                                                    \
  setAttrib(ret_, install("perf"), perf_);                                    \
  PutRNGstate();                                                              \
  UNPROTECT(2);                                                               \
  return ret_;                                                                \
}                                                                             \


#define SERIAL_SORTB_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME(               \
SEXP x_                                                                       \
  , SEXP b_                                                                   \
)                                                                             \
{                                                                             \
  SEXP ret_, perf_; PerfT *perf;                                              \
  PCapEnergyT eLast;                                                          \
  GreensortEnergyT eNext;                                                     \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                   \
  int n = LENGTH(x_);                                                         \
  int b = asInteger(b_);                                                      \
  GetRNGstate();                                                              \
  eLast = GreensortEnergyNow(); initTicks();                                  \
  switch (TYPEOF(x_)){                                                        \
  case REALSXP:                                                               \
  {                                                                           \
    double *x;                                                                \
    x = REAL(x_);                                                             \
    SORTNAME(x, (IndexT) n, b, perf);                                         \
    break;                                                                    \
  }                                                                           \
  default:                                                                    \
    error("unimplemented type in SORTNAME");                                  \
  }                                                                           \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
  perf->n = n;                                                                \
  perf->b = 8;                                                                \
  perf->p = 1;                                                                \
  perf->t = 1;                                                                \
  perf->base = eNext.base;                                                    \
  perf->core = eNext.core;                                                    \
  perf->unco = eNext.unco;                                                    \
  perf->dram = eNext.dram;                                                    \
  setAttrib(ret_, install("perf"), perf_);                                    \
  PutRNGstate();                                                              \
  UNPROTECT(2);                                                               \
  return ret_;                                                                \
}                                                                             \


#define SERIAL_SORTP_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME(               \
SEXP x_                                                                       \
  , SEXP p_                                                                   \
)                                                                             \
{                                                                             \
  SEXP ret_, perf_; PerfT *perf;                                              \
  PCapEnergyT eLast;                                                          \
  GreensortEnergyT eNext;                                                     \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                   \
  int n = LENGTH(x_);                                                         \
  double p = asReal(p_);                                                      \
  GetRNGstate();                                                              \
  eLast = GreensortEnergyNow(); initTicks();                                  \
  switch (TYPEOF(x_)){                                                        \
  case REALSXP:                                                               \
  {                                                                           \
    double *x;                                                                \
    x = REAL(x_);                                                             \
    SORTNAME(x, (IndexT) n, p, perf);                                         \
    break;                                                                    \
  }                                                                           \
  default:                                                                    \
    error("unimplemented type in SORTNAME");                                  \
  }                                                                           \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
  perf->n = n;                                                                \
  perf->b = 8;                                                                \
  perf->p = 1;                                                                \
  perf->t = 1;                                                                \
  perf->base = eNext.base;                                                    \
  perf->core = eNext.core;                                                    \
  perf->unco = eNext.unco;                                                    \
  perf->dram = eNext.dram;                                                    \
  setAttrib(ret_, install("perf"), perf_);                                    \
  PutRNGstate();                                                              \
  UNPROTECT(2);                                                               \
  return ret_;                                                                \
}                                                                             \




#define SERIAL_SORTP_INTINT_NOSIZE(SORTNAME) SEXP r_##SORTNAME(                \
SEXP x_                                                                        \
, SEXP p_                                                                      \
)                                                                              \
{                                                                              \
  SEXP ret_, perf_; PerfT *perf;                                               \
  PCapEnergyT eLast;                                                           \
  GreensortEnergyT eNext;                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                    \
  int n = INTEGER(GET_DIM(x_))[1];                                             \
  int m = INTEGER(GET_DIM(x_))[0];                                             \
  double p = asReal(p_);                                                       \
  GetRNGstate();                                                               \
  eLast = GreensortEnergyNow(); initTicks();                                   \
  switch (TYPEOF(x_)){                                                         \
  case INTSXP:                                                                 \
  {                                                                            \
    int *x;                                                                    \
    x = INTEGER(x_);                                                           \
    SORTNAME(x, (IndexT) n, (IndexT) m, p, perf);                              \
    break;                                                                     \
  }                                                                            \
  default:                                                                     \
    error("unimplemented type in SORTNAME");                                   \
  }                                                                            \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                           \
  perf->n = n;                                                                 \
  perf->b = sizeof(IntValueT)*m;                                               \
  perf->p = 1;                                                                 \
  perf->t = 1;                                                                 \
  perf->base = eNext.base;                                                     \
  perf->core = eNext.core;                                                     \
  perf->unco = eNext.unco;                                                     \
  perf->dram = eNext.dram;                                                     \
  setAttrib(ret_, install("perf"), perf_);                                     \
  PutRNGstate();                                                               \
  UNPROTECT(2);                                                                \
  return ret_;                                                                 \
}                                                                              \

#define SERIAL_SORT_INTINT_NOSIZE(SORTNAME) SEXP r_##SORTNAME(                \
SEXP x_                                                                        \
)                                                                              \
{                                                                              \
  SEXP ret_, perf_; PerfT *perf;                                               \
  PCapEnergyT eLast;                                                           \
  GreensortEnergyT eNext;                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                    \
  int n = INTEGER(GET_DIM(x_))[1];                                             \
  int m = INTEGER(GET_DIM(x_))[0];                                             \
  GetRNGstate();                                                               \
  eLast = GreensortEnergyNow(); initTicks();                                   \
  switch (TYPEOF(x_)){                                                         \
  case INTSXP:                                                                 \
  {                                                                            \
    int *x;                                                                    \
    x = INTEGER(x_);                                                           \
    SORTNAME(x, (IndexT) n, (IndexT) m, perf);                                 \
    break;                                                                     \
  }                                                                            \
  default:                                                                     \
    error("unimplemented type in SORTNAME");                                   \
  }                                                                            \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                           \
  perf->n = n;                                                                 \
  perf->b = sizeof(IntValueT)*m;                                               \
  perf->p = 1;                                                                 \
  perf->t = 1;                                                                 \
  perf->base = eNext.base;                                                     \
  perf->core = eNext.core;                                                     \
  perf->unco = eNext.unco;                                                     \
  perf->dram = eNext.dram;                                                     \
  setAttrib(ret_, install("perf"), perf_);                                     \
  PutRNGstate();                                                               \
  UNPROTECT(2);                                                                \
  return ret_;                                                                 \
}                                                                              \



#define SERIAL_SORTPP_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME(              \
SEXP x_                                                                       \
  , SEXP p_                                                                   \
  , SEXP p2_                                                                  \
)                                                                             \
{                                                                             \
  SEXP ret_, perf_; PerfT *perf;                                              \
  PCapEnergyT eLast;                                                          \
  GreensortEnergyT eNext;                                                     \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);    \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                   \
  int n = LENGTH(x_);                                                         \
  double p = asReal(p_);                                                      \
  double p2 = asReal(p2_);                                                    \
  GetRNGstate();                                                              \
  eLast = GreensortEnergyNow(); initTicks();                                  \
  switch (TYPEOF(x_)){                                                        \
  case REALSXP:                                                               \
  {                                                                           \
    double *x;                                                                \
    x = REAL(x_);                                                             \
    SORTNAME(x, (IndexT) n, p, p2, perf);                                     \
    break;                                                                    \
  }                                                                           \
  default:                                                                    \
    error("unimplemented type in SORTNAME");                                  \
  }                                                                           \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
  perf->n = n;                                                                \
  perf->b = 8;                                                                \
  perf->p = 1;                                                                \
  perf->t = 1;                                                                \
  perf->base = eNext.base;                                                    \
  perf->core = eNext.core;                                                    \
  perf->unco = eNext.unco;                                                    \
  perf->dram = eNext.dram;                                                    \
  setAttrib(ret_, install("perf"), perf_);                                    \
  PutRNGstate();                                                              \
  UNPROTECT(2);                                                               \
  return ret_;                                                                \
}                                                                             \


#define PARALLEL_SORT_DOUBLE_SIZE(SORTNAME, SORTSIZE) SEXP r_##SORTNAME(     \
SEXP x_                                                                      \
  , SEXP t_                                                                  \
)                                                                            \
{                                                                            \
  SEXP ret_, perf_; PerfT *perf;                                             \
  PCapEnergyT eLast;                                                         \
  GreensortEnergyT eNext;                                                    \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);   \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                  \
  int n = LENGTH(x_);                                                        \
  double t = asReal(t_);                                                     \
  GetRNGstate();                                                             \
  eLast = GreensortEnergyNow(); initTicks();                                 \
  switch (TYPEOF(x_)){                                                       \
  case REALSXP:                                                              \
  {                                                                          \
    double *x;                                                               \
    x = REAL(x_);                                                            \
    SORTNAME(x, (IndexT) n, t);                                              \
    break;                                                                   \
  }                                                                          \
  default:                                                                   \
    error("unimplemented type in SORTNAME");                                 \
  }                                                                          \
  perf->secs = getNewSecs();                                                 \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                         \
  perf->n = n;                                                               \
  perf->b = 8;                                                               \
  perf->p = 1;                                                               \
  perf->t = t;                                                               \
  perf->size=SORTSIZE;                                                       \
  perf->base = eNext.base;                                                   \
  perf->core = eNext.core;                                                   \
  perf->unco = eNext.unco;                                                   \
  perf->dram = eNext.dram;                                                   \
  setAttrib(ret_, install("perf"), perf_);                                   \
  PutRNGstate();                                                             \
  UNPROTECT(2);                                                              \
  return ret_;                                                               \
}                                                                            \


#define PARALLEL_SORTP_DOUBLE_NOSIZE(SORTNAME) SEXP r_##SORTNAME(      \
SEXP x_                                                                        \
  , SEXP p_                                                                    \
  , SEXP t_                                                                    \
)                                                                              \
{                                                                              \
  SEXP ret_, perf_; PerfT *perf;                                               \
  PCapEnergyT eLast;                                                           \
  GreensortEnergyT eNext;                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);     \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                    \
  int n = LENGTH(x_);                                                          \
  double p = asReal(p_);                                                       \
  double t = asReal(t_);                                                       \
  GetRNGstate();                                                               \
  eLast = GreensortEnergyNow(); initTicks();                                   \
  switch (TYPEOF(x_)){                                                         \
  case REALSXP:                                                                \
  {                                                                            \
    double *x;                                                                 \
    x = REAL(x_);                                                              \
    SORTNAME(x, (IndexT) n, p, t, perf);                                       \
    break;                                                                     \
  }                                                                            \
  default:                                                                     \
    error("unimplemented type in SORTNAME");                                   \
  }                                                                            \
  eNext = GreensortEnergyDelta(&eLast); doneTicks();                           \
  perf->n = n;                                                                 \
  perf->b = 8;                                                                 \
  perf->p = 1;                                                                 \
  perf->t = t;                                                                 \
  perf->base = eNext.base;                                                     \
  perf->core = eNext.core;                                                     \
  perf->unco = eNext.unco;                                                     \
  perf->dram = eNext.dram;                                                     \
  setAttrib(ret_, install("perf"), perf_);                                     \
  PutRNGstate();                                                               \
  UNPROTECT(2);                                                                \
  return ret_;                                                                 \
}                                                                              \


#define PARALLEL_SORTB_VARCHAR_NOSIZE(SORTNAME) SEXP r_##SORTNAME(               \
SEXP x_                                                                         \
  ,  SEXP t_                                                                    \
)                                                                               \
{                                                                               \
  SEXP ret_, perf_; PerfT *perf;                                                \
  PCapEnergyT eLast;                                                            \
  GreensortEnergyT eNext;                                                       \
  int b,i,s,m,n = LENGTH(x_);                                                   \
  char *c;                                                                      \
  double t = asReal(t_);                                                        \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);      \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                     \
  /*PROTECT( ret_ = allocVector(STRSXP, n) );  */                               \
  GetRNGstate();                                                                \
  switch (TYPEOF(x_)){                                                          \
  case STRSXP:                                                                  \
  {                                                                             \
    /* now count the string size */                                             \
    m = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      s = strlen(CHAR(STRING_ELT(x_, i)));                                      \
      m += s;                                                                   \
    }                                                                           \
    /* m characters + n null terminators + initial sentinel */                  \
    char *y = (char *) MALLOC(m+n+1, typeof(char));                             \
    /*Rprintf("now copy to character array n=%d m=%d\n", n, m);*/               \
    y[0] = '\0';                                                                \
    m = 1;                                                                      \
    b = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = (char *) CHAR(STRING_ELT(x_, i));                                     \
      s = strlen(c) + 1;                                                        \
      strcpy(y+m, c);                                                           \
      m += s;                                                                   \
      if (b<s)                                                                  \
        b = s;                                                                  \
    }                                                                           \
    /*Rprintf("now sort character array n=%d m=%d b=%d\n", n, m, b);*/          \
    eLast = GreensortEnergyNow(); initTicks();                                  \
    SORTNAME(y, n, m, b, t, perf);                                              \
    eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
    /* now copy back */                                                         \
    m = 1;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = y+m;                                                                  \
      s = strlen(c) + 1;                                                        \
      SET_STRING_ELT(x_, i, mkChar(c));                                         \
      m += s;                                                                   \
    }                                                                           \
    FREE(y);                                                                    \
    break;                                                                      \
  }                                                                             \
  default:                                                                      \
    error("unimplemented type in SORTNAME");                                    \
  }                                                                             \
  perf->n = n;                                                                  \
  perf->b = (m+n)/((double)n);                                                  \
  perf->p = 1;                                                                  \
  perf->t = t;                                                                  \
  perf->base = eNext.base;                                                      \
  perf->core = eNext.core;                                                      \
  perf->unco = eNext.unco;                                                      \
  perf->dram = eNext.dram;                                                      \
  setAttrib(ret_, install("perf"), perf_);                                      \
  PutRNGstate();                                                                \
  UNPROTECT(2);                                                                 \
  return ret_;                                                                  \
}                                                                               \


#define SERIAL_SORTB_VARCHAR_NOSIZE(SORTNAME) SEXP r_##SORTNAME(                 \
SEXP x_                                                                         \
)                                                                               \
{                                                                               \
  SEXP ret_, perf_; PerfT *perf;                                                \
  PCapEnergyT eLast;                                                            \
  GreensortEnergyT eNext;                                                       \
  int b,i,s,m,n = LENGTH(x_);                                                   \
  char *c;                                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);      \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                     \
  /*PROTECT( ret_ = allocVector(STRSXP, n) );  */                               \
  GetRNGstate();                                                                \
  switch (TYPEOF(x_)){                                                          \
  case STRSXP:                                                                  \
  {                                                                             \
    /* now count the string size */                                             \
    m = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      s = strlen(CHAR(STRING_ELT(x_, i)));                                      \
      m += s;                                                                   \
    }                                                                           \
    /* m characters + n null terminators + initial sentinel */                  \
    char *y = (char *) MALLOC(m+n+1, typeof(char));                             \
    /*Rprintf("now copy to character array n=%d m=%d\n", n, m);*/               \
    y[0] = '\0';                                                                \
    m = 1;                                                                      \
    b = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = (char *) CHAR(STRING_ELT(x_, i));                                     \
      s = strlen(c) + 1;                                                        \
      strcpy(y+m, c);                                                           \
      m += s;                                                                   \
      if (b<s)                                                                  \
        b = s;                                                                  \
    }                                                                           \
    /*Rprintf("now sort character array n=%d m=%d b=%d\n", n, m, b);*/          \
    eLast = GreensortEnergyNow(); initTicks();                                  \
    SORTNAME(y, n, m, b, perf);                                                 \
    eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
    /* now copy back */                                                         \
    m = 1;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = y+m;                                                                  \
      s = strlen(c) + 1;                                                        \
      SET_STRING_ELT(x_, i, mkChar(c));                                         \
      m += s;                                                                   \
    }                                                                           \
    FREE(y);                                                                    \
    break;                                                                      \
  }                                                                             \
  default:                                                                      \
    error("unimplemented type in SORTNAME");                                    \
  }                                                                             \
  perf->n = n;                                                                  \
  perf->b = (m+n)/((double)n);                                                  \
  perf->p = 1;                                                                  \
  perf->t = 1;                                                                  \
  perf->base = eNext.base;                                                      \
  perf->core = eNext.core;                                                      \
  perf->unco = eNext.unco;                                                      \
  perf->dram = eNext.dram;                                                      \
  setAttrib(ret_, install("perf"), perf_);                                      \
  PutRNGstate();                                                                \
  UNPROTECT(2);                                                                 \
  return ret_;                                                                  \
}                                                                               \

#define PARALLEL_SORT_VARCHAR_NOSIZE(SORTNAME) SEXP r_##SORTNAME(               \
SEXP x_                                                                         \
  ,  SEXP t_                                                                    \
)                                                                               \
{                                                                               \
  SEXP ret_, perf_; PerfT *perf;                                                \
  PCapEnergyT eLast;                                                            \
  GreensortEnergyT eNext;                                                       \
  int b,i,s,m,n = LENGTH(x_);                                                   \
  char *c;                                                                      \
  double t = asReal(t_);                                                        \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);      \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                     \
  /*PROTECT( ret_ = allocVector(STRSXP, n) );  */                               \
  GetRNGstate();                                                                \
  switch (TYPEOF(x_)){                                                          \
  case STRSXP:                                                                  \
  {                                                                             \
    /* now count the string size */                                             \
    m = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      s = strlen(CHAR(STRING_ELT(x_, i)));                                      \
      m += s;                                                                   \
    }                                                                           \
    /* m characters + n null terminators + initial sentinel */                  \
    char *y = (char *) MALLOC(m+n+1, typeof(char));                             \
    /*Rprintf("now copy to character array n=%d m=%d\n", n, m);*/               \
    y[0] = '\0';                                                                \
    m = 1;                                                                      \
    b = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = (char *) CHAR(STRING_ELT(x_, i));                                     \
      s = strlen(c) + 1;                                                        \
      strcpy(y+m, c);                                                           \
      m += s;                                                                   \
      if (b<s)                                                                  \
        b = s;                                                                  \
    }                                                                           \
    /*Rprintf("now sort character array n=%d m=%d b=%d\n", n, m, b);*/          \
    eLast = GreensortEnergyNow(); initTicks();                                  \
    SORTNAME(y, n, m, t, perf);                                              \
    eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
    /* now copy back */                                                         \
    m = 1;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = y+m;                                                                  \
      s = strlen(c) + 1;                                                        \
      SET_STRING_ELT(x_, i, mkChar(c));                                         \
      m += s;                                                                   \
    }                                                                           \
    FREE(y);                                                                    \
    break;                                                                      \
  }                                                                             \
  default:                                                                      \
    error("unimplemented type in SORTNAME");                                    \
  }                                                                             \
  perf->n = n;                                                                  \
  perf->b = (m+n)/((double)n);                                                  \
  perf->p = 1;                                                                  \
  perf->t = t;                                                                  \
  perf->base = eNext.base;                                                      \
  perf->core = eNext.core;                                                      \
  perf->unco = eNext.unco;                                                      \
  perf->dram = eNext.dram;                                                      \
  setAttrib(ret_, install("perf"), perf_);                                      \
  PutRNGstate();                                                                \
  UNPROTECT(2);                                                                 \
  return ret_;                                                                  \
}                                                                               \


#define SERIAL_SORT_VARCHAR_NOSIZE(SORTNAME) SEXP r_##SORTNAME(                 \
SEXP x_                                                                         \
)                                                                               \
{                                                                               \
  SEXP ret_, perf_; PerfT *perf;                                                \
  PCapEnergyT eLast;                                                            \
  GreensortEnergyT eNext;                                                       \
  int b,i,s,m,n = LENGTH(x_);                                                   \
  char *c;                                                                      \
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);      \
  PROTECT( ret_ = allocVector(LGLSXP, 0) );                                     \
  /*PROTECT( ret_ = allocVector(STRSXP, n) );  */                               \
  GetRNGstate();                                                                \
  switch (TYPEOF(x_)){                                                          \
  case STRSXP:                                                                  \
  {                                                                             \
    /* now count the string size */                                             \
    m = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      s = strlen(CHAR(STRING_ELT(x_, i)));                                      \
      m += s;                                                                   \
    }                                                                           \
    /* m characters + n null terminators + initial sentinel */                  \
    char *y = (char *) MALLOC(m+n+1, typeof(char));                             \
    /*Rprintf("now copy to character array n=%d m=%d\n", n, m);*/               \
    y[0] = '\0';                                                                \
    m = 1;                                                                      \
    b = 0;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = (char *) CHAR(STRING_ELT(x_, i));                                     \
      s = strlen(c) + 1;                                                        \
      strcpy(y+m, c);                                                           \
      m += s;                                                                   \
      if (b<s)                                                                  \
        b = s;                                                                  \
    }                                                                           \
    /*Rprintf("now sort character array n=%d m=%d b=%d\n", n, m, b);*/          \
    eLast = GreensortEnergyNow(); initTicks();                                  \
    SORTNAME(y, n, m, perf);                                                 \
    eNext = GreensortEnergyDelta(&eLast); doneTicks();                          \
    /* now copy back */                                                         \
    m = 1;                                                                      \
    for (i=0;i<n;i++){                                                          \
      c = y+m;                                                                  \
      s = strlen(c) + 1;                                                        \
      SET_STRING_ELT(x_, i, mkChar(c));                                         \
      m += s;                                                                   \
    }                                                                           \
    FREE(y);                                                                    \
    break;                                                                      \
  }                                                                             \
  default:                                                                      \
    error("unimplemented type in SORTNAME");                                    \
  }                                                                             \
  perf->n = n;                                                                  \
  perf->b = (m+n)/((double)n);                                                  \
  perf->p = 1;                                                                  \
  perf->t = 1;                                                                  \
  perf->base = eNext.base;                                                      \
  perf->core = eNext.core;                                                      \
  perf->unco = eNext.unco;                                                      \
  perf->dram = eNext.dram;                                                      \
  setAttrib(ret_, install("perf"), perf_);                                      \
  PutRNGstate();                                                                \
  UNPROTECT(2);                                                                 \
  return ret_;                                                                  \
}                                                                               \


SEXP r_UVWoverhead(
    SEXP x_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  int b,i,s,m,n = LENGTH(x_);
  char *c;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  /*PROTECT( ret_ = allocVector(STRSXP, n) );  */
  GetRNGstate();
  switch (TYPEOF(x_)){
  case STRSXP:
  {
    eLast = GreensortEnergyNow(); initTicks();
    /* now count the string size */
    m = 0;
    for (i=0;i<n;i++){
      s = strlen(CHAR(STRING_ELT(x_, i)));
      m += s;
    }
    /* m characters + n null terminators + initial sentinel */
    char *y = (char *) MALLOC(m+n+1, typeof(char));
    /*Rprintf("now copy to character array n=%d m=%dn", n, m);*/
    y[0] = '\0';
    m = 1;
    b = 0;
    for (i=0;i<n;i++){
      c = (char *) CHAR(STRING_ELT(x_, i));
      s = strlen(c) + 1;
      strcpy(y+m, c);
      m += s;
      if (b<s)
        b = s;
    }
    /*Rprintf("now sort character array n=%d m=%d b=%d\n", n, m, b);*/
    //SORTNAME(y, n, m, b, perf);
    /* now copy back */
    m = 1;
    for (i=0;i<n;i++){
      c = y+m;
      s = strlen(c) + 1;
      SET_STRING_ELT(x_, i, mkChar(c));
      m += s;
    }
    FREE(y);
    perf->secs = getDeltaSecs();
    eNext = GreensortEnergyDelta(&eLast); doneTicks();
    break;
  }
  default:
    error("unimplemented type in SORTNAME");
  }
  perf->n = n;
  perf->b = (m+n)/((double)n);
  perf->p = 1;
  perf->t = 1;
  perf->size = 2; //NA_REAL;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  PutRNGstate();
  UNPROTECT(2);
  return ret_;
}

SERIAL_SORT_DOUBLE_SIZE(Selectionsort_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Selectionsort_exsitu, 1)
SERIAL_SORT_DOUBLE_NOSIZE(Selectionsort2_insitu)
SERIAL_SORT_DOUBLE_NOSIZE(Selectionsort2_exsitu)


SERIAL_SORT_INTINT_SIZE(MInsertionsort_l2r_insitu, 1)
SERIAL_SORT_INTINT_SIZE(MInsertionsort_l2r_exsitu, 1)
SERIAL_SORT_INTINT_SIZE(NInsertionsort_l2r_insitu, 1)
SERIAL_SORT_INTINT_SIZE(NInsertionsort_l2r_exsitu, 1)

SERIAL_SORT_DOUBLE_SIZE(Insertionsort_l2r_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_r2l_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_desc_l2r_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_desc_r2l_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_l2r_exsitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_r2l_exsitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_desc_l2r_exsitu, 1)
SERIAL_SORT_DOUBLE_SIZE(Insertionsort_desc_r2l_exsitu, 1)



SEXP r_Insertionorder_l2r_insitu(
    SEXP x_
  , SEXP o_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    int *o;
    o = INTEGER(o_);
    int i;
    for (i=0; i<n; i++)
      o[i] = o[i] - 1;
    Insertionorder_l2r_insitu(x, o, n);
    for (i=0; i<n; i++)
      o[i] = o[i] + 1;
    break;
  }
  default:
    error("unimplemented type in Insertionorder_l2r_insitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 12;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}

SEXP r_Insertionorder_l2r_exsitu(
    SEXP x_
  , SEXP o_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    int *o;
    o = INTEGER(o_);
    int i;
    for (i=0; i<n; i++)
      o[i] = o[i] - 1;
    Insertionorder_l2r_exsitu(x, o, n);
    for (i=0; i<n; i++)
      o[i] = o[i] + 1;
    break;
  }
  default:
    error("unimplemented type in Insertionorder_l2r_exsitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 12;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}


SEXP r_Insertionorder_r2l_insitu(
    SEXP x_
  , SEXP o_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    int *o;
    o = INTEGER(o_);
    int i;
    for (i=0; i<n; i++)
      o[i] = o[i] - 1;
    Insertionorder_r2l_insitu(x, o, n);
    for (i=0; i<n; i++)
      o[i] = o[i] + 1;
    break;
  }
  default:
    error("unimplemented type in Insertionorder_r2l_insitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 12;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}


SEXP r_Insertionorder_r2l_exsitu(
    SEXP x_
  , SEXP o_
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    int *o;
    o = INTEGER(o_);
    int i;
    for (i=0; i<n; i++)
      o[i] = o[i] - 1;
    Insertionorder_r2l_exsitu(x, o, n);
    for (i=0; i<n; i++)
      o[i] = o[i] + 1;
    break;
  }
  default:
    error("unimplemented type in Insertionorder_r2l_exsitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 12;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  UNPROTECT(2);
  return ret_;
}


SEXP r_Chunksort_noperf(
    SEXP x_            // data vector
  ,   SEXP b_            // chunks size
  ,   SEXP d_            // direction
)
{
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  int b = asInteger(b_);
  int d = asInteger(d_);
  GetRNGstate();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    Chunksort_insitu(x, n, b, d);
    break;
  }
  default:
    error("unimplemented type in Chunksort_noperf");
  }
  PutRNGstate();
  UNPROTECT(1);
  return ret_;
}


SEXP r_Chunksort_insitu(
    SEXP x_            // data vector
  ,   SEXP b_            // chunks size
  ,   SEXP d_            // direction
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  int b = asInteger(b_);
  int d = asInteger(d_);
  GetRNGstate();
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    Chunksort_insitu(x, n, b, d);
    break;
  }
  default:
    error("unimplemented type in Chunksort_insitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 8;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  PutRNGstate();
  UNPROTECT(2);
  return ret_;
}

SEXP r_Chunksort_exsitu(
    SEXP x_            // data vector
  ,   SEXP b_            // chunks size
  ,   SEXP d_            // direction
)
{
  SEXP ret_, perf_; PerfT *perf;
  PCapEnergyT eLast;
  GreensortEnergyT eNext;
  PROTECT( perf_ = allocVector(REALSXP, 10) ); perf = (PerfT *)REAL(perf_);
  PROTECT( ret_ = allocVector(LGLSXP, 0) );
  int n = LENGTH(x_);
  int b = asInteger(b_);
  int d = asInteger(d_);
  GetRNGstate();
  eLast = GreensortEnergyNow(); initTicks();
  switch (TYPEOF(x_)){
  case REALSXP:
  {
    double *x;
    x = REAL(x_);
    Chunksort_exsitu(x, n, b, d);
    break;
  }
  default:
    error("unimplemented type in Chunksort_exsitu");
  }
  perf->secs = getNewSecs();
  eNext = GreensortEnergyDelta(&eLast); doneTicks();
  perf->n = n;
  perf->b = 8;
  perf->p = 1;
  perf->t = 1;
  perf->size=1;
  perf->base = eNext.base;
  perf->core = eNext.core;
  perf->unco = eNext.unco;
  perf->dram = eNext.dram;
  setAttrib(ret_, install("perf"), perf_);
  PutRNGstate();
  UNPROTECT(2);
  return ret_;
}

SERIAL_SORT_DOUBLE_SIZE(InplaceMergesortP_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(InplaceMergesortP_exsitu, 1)

SERIAL_SORT_DOUBLE_SIZE(GrailsortP_insitu, 1)
SERIAL_SORT_DOUBLE_SIZE(GrailsortP_exsitu, 1)
SERIAL_SORT_DOUBLE_SIZE(GrailsqrtP_insitu, (n+2*sqrt(n))/((double)n))
SERIAL_SORT_DOUBLE_SIZE(GrailsqrtP_exsitu, (n+2*sqrt(n))/((double)n))
SERIAL_SORT_DOUBLE_NOSIZE(SqrtsortP_insitu)
SERIAL_SORT_DOUBLE_NOSIZE(SqrtsortP_exsitu)

SERIAL_SORTB_DOUBLE_NOSIZE(Jumpsort_insitu)
SERIAL_SORTB_DOUBLE_NOSIZE(Jumpsort_exsitu)
SERIAL_SORTB_DOUBLE_NOSIZE(Walksort_insitu)
SERIAL_SORTB_DOUBLE_NOSIZE(Walksort_exsitu)


SERIAL_SORT_DOUBLE_SIZE(Copysort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Copysort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(CopysortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(CopysortP_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(Nocosort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Nocosort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(NocosortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(NocosortP_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(Simplsort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Simplsort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(SimplsortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(SimplsortP_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Bimesort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Bimesort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortP_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortP_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortB_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortB_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortBP_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(BimesortBP_exsitu, 2)

  SERIAL_SORT_INTINT_SIZE(MKnuthsortP_insitu, 2)
  SERIAL_SORT_INTINT_SIZE(MKnuthsortP_exsitu, 2)
  SERIAL_SORT_INTINT_NOSIZE(NKnuthsortP_insitu)
  SERIAL_SORT_INTINT_NOSIZE(NKnuthsortP_exsitu)

SERIAL_SORT_DOUBLE_SIZE(Knuthsort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Knuthsort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortP_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortA_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortA_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortAP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KnuthsortAP_exsitu, 2)


SERIAL_SORT_DOUBLE_SIZE(Katasort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Katasort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortP_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortA_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortA_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortAP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortAP_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortB_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortB_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortBP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(KatasortBP_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(Omitsort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(Omitsort_exsitu, 2)
SERIAL_SORT_DOUBLE_SIZE(OmitsortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(OmitsortP_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(OctosortP_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(OctosortP_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(Ininsort_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Ininsort_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Ninisort_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Ninisort_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(GKnuthsort_insitu, 2)
SERIAL_SORT_DOUBLE_SIZE(GKnuthsort_exsitu, 2)

SERIAL_SORT_DOUBLE_SIZE(Frogsort0_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort0_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort0P_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort0P_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Geckosort0_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Geckosort0_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Frogsort1_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1P_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1P_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Geckosort1_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Geckosort1_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Geckosort1P_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Geckosort1P_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Frogsort1A_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1A_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1AP_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1AP_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Frogsort1B_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1B_exsitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1BP_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Frogsort1BP_exsitu, 1.5)

SERIAL_SORT_DOUBLE_SIZE(Squidsort1P_insitu, 1.5)
SERIAL_SORT_DOUBLE_SIZE(Squidsort1P_exsitu, 1.5)

SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2P_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2P_exsitu)

SERIAL_SORTP_INTINT_NOSIZE(MFrogsort2P_insitu)
SERIAL_SORTP_INTINT_NOSIZE(MFrogsort2P_exsitu)
SERIAL_SORTP_INTINT_NOSIZE(NFrogsort2P_insitu)
SERIAL_SORTP_INTINT_NOSIZE(NFrogsort2P_exsitu)

SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2A_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2A_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2AP_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2AP_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2B_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2B_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2BP_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort2BP_exsitu)

SERIAL_SORTP_DOUBLE_NOSIZE(Squidsort2P_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Squidsort2P_exsitu)

SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3P_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3P_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3B_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3B_exsitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3BP_insitu)
SERIAL_SORTP_DOUBLE_NOSIZE(Frogsort3BP_exsitu)

SERIAL_SORTPP_DOUBLE_NOSIZE(Frogsort6_insitu)
SERIAL_SORTPP_DOUBLE_NOSIZE(Frogsort6_exsitu)
SERIAL_SORTPP_DOUBLE_NOSIZE(Frogsort6P_insitu)
SERIAL_SORTPP_DOUBLE_NOSIZE(Frogsort6P_exsitu)

SERIAL_SORTB_DOUBLE_NOSIZE(Frogsort4_insitu)
SERIAL_SORTB_DOUBLE_NOSIZE(Frogsort4_exsitu)

  SERIAL_SORT_DOUBLE_SIZE(Knuth3sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Knuth3sort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Knuth4sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Knuth4sort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Kata3sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kata3sort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kata4sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kata4sort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Croco3sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Croco3sort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Croco4sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Croco4sort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Kroco3sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kroco3sort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kroco4sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kroco4sort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(TKnuthsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(TKnuthsort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Crocosort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Crocosort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(DietACPsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietACPsort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietACPTsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietACPTsort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPsort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPsortB_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPsortB_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietPcsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietPcsort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietcP2sort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietcP2sort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPTsort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(DietCPTsort_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Kiwisort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Kiwisort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(KiwisortA_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(KiwisortA_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(KiwisortB_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(KiwisortB_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Swansort_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(Swansort_exsitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(SwansortB_insitu, 2)
  SERIAL_SORT_DOUBLE_SIZE(SwansortB_exsitu, 2)

  SERIAL_SORT_DOUBLE_SIZE(Storksort_insitu, 1.5)
  SERIAL_SORT_DOUBLE_SIZE(Storksort_exsitu, 1.5)
  SERIAL_SORT_DOUBLE_SIZE(StorksortB_exsitu, 1.5)

  SERIAL_SORT_DOUBLE_SIZE(BFPRTsort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Picksort_insitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Plugsort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Pugsort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Slicksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Sicksort_insitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Zocksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zocksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zicksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zicksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zacksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zacksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZacksortB_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZacksortB_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zucksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Zucksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortA_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortA_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortB_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortB_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortD_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ZucksortD_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Ducksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Ducksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(DucksortB_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(DucksortB_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Chicksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Chicksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ChicksortP_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(ChicksortP_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Dupisort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Dupisort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(DupisortP_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(DupisortP_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Tricksort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Tricksort_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(TricksortP_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(TricksortP_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Quicksort1_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort1_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Lomutosort_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Lomutosort_exsitu, 1)

  SERIAL_SORT_DOUBLE_SIZE(Quicksort2_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort2_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort2P_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort2P_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort2B_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort2B_exsitu, 1)

 SERIAL_SORT_DOUBLE_SIZE(Quicksort3_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort3_exsitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort3P_insitu, 1)
  SERIAL_SORT_DOUBLE_SIZE(Quicksort3P_exsitu, 1)

  SERIAL_SORT_DOUBLE_NOSIZE(SInsertionsort_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(SInsertionsort_exsitu)
  SERIAL_SORT_DOUBLE_NOSIZE(RInsertionsort_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(RInsertionsort_exsitu)

  SERIAL_SORT_DOUBLE_NOSIZE(SQuicksort2_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(SQuicksort2_exsitu)
  SERIAL_SORT_DOUBLE_NOSIZE(SQuicksort2B_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(SQuicksort2B_exsitu)

  SERIAL_SORTREORDER_DOUBLE_NOSIZE(RQuicksort2_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(RQuicksort2_exsitu)
  SERIAL_SORTREORDER_DOUBLE_NOSIZE(RQuicksort2B_insitu)
  SERIAL_SORT_DOUBLE_NOSIZE(RQuicksort2B_exsitu)

  SERIAL_SELECT_DOUBLE_SIZE(approxMedian_insitu, 1)
  SERIAL_SELECT_DOUBLE_SIZE(approxMedian_exsitu, 1.2 )

  SERIAL_SORTPART_DOUBLE_SIZE(Dotnetpart_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Dotnetpart_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Quickpart2_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Quickpart2_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Zackpart_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Zackpart_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Zuckpart_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Zuckpart_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Duckpart_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(Duckpart_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(ZuckpartB_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(ZuckpartB_exsitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(DuckpartB_insitu, 1)
  SERIAL_SORTPART_DOUBLE_SIZE(DuckpartB_exsitu, 1)

  SERIAL_SELECTK_DOUBLE_SIZE(Quickselect2_insitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Quickselect2_exsitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Quickpartleft2_insitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Quickpartleft2_exsitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Quickpartright2_insitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Quickpartright2_exsitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(BFPRTselect_insitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(BFPRTselect_exsitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Pickselect_insitu, 1)
  SERIAL_SELECTK_DOUBLE_SIZE(Pickselect_exsitu, 1)

  SERIAL_SELECTK2_DOUBLE_SIZE(Zackpartleft_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zackpartleft_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zackpartright_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zackpartright_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zackselect_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zackselect_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zuckselect_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zuckselect_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(ZuckselectB_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(ZuckselectB_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Duckselect_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Duckselect_exsitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(DuckselectB_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(DuckselectB_exsitu, 1)

  SERIAL_SELECTK2_DOUBLE_SIZE(Zickselect_insitu, 1)
  SERIAL_SELECTK2_DOUBLE_SIZE(Zickselect_exsitu, 1)

  PARALLEL_SORT_DOUBLE_SIZE(PQuicksort2_insitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PQuicksort2_exsitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PQuicksort2B_insitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PQuicksort2B_exsitu, 1)

  PARALLEL_SORT_DOUBLE_SIZE(PDucksort_insitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PDucksort_exsitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PDucksortB_insitu, 1)
  PARALLEL_SORT_DOUBLE_SIZE(PDucksortB_exsitu, 1)

  PARALLEL_SORT_DOUBLE_SIZE(PKnuthsortP_insitu, 2)
  PARALLEL_SORT_DOUBLE_SIZE(PKnuthsortP_exsitu, 2)

  PARALLEL_SORT_DOUBLE_SIZE(PFrogsort0P_insitu, 1.5)
  PARALLEL_SORT_DOUBLE_SIZE(PFrogsort0P_exsitu, 1.5)
  PARALLEL_SORT_DOUBLE_SIZE(PFrogsort1P_insitu, 1.5)
  PARALLEL_SORT_DOUBLE_SIZE(PFrogsort1P_exsitu, 1.5)

  PARALLEL_SORTP_DOUBLE_NOSIZE(PFrogsort2P_insitu)
  PARALLEL_SORTP_DOUBLE_NOSIZE(PFrogsort2P_exsitu)
  PARALLEL_SORTP_DOUBLE_NOSIZE(PFrogsort3P_insitu)
  PARALLEL_SORTP_DOUBLE_NOSIZE(PFrogsort3P_exsitu)



  SERIAL_SORT_VARCHAR_NOSIZE(UInsertionsort_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UInsertionsort_exsitu)
  SERIAL_SORT_VARCHAR_NOSIZE(WInsertionsort_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(WInsertionsort_exsitu)

  SERIAL_SORT_VARCHAR_NOSIZE(WQuicksort2_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(WQuicksort2_exsitu)
  SERIAL_SORT_VARCHAR_NOSIZE(WQuicksort2B_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(WQuicksort2B_exsitu)

  SERIAL_SORT_VARCHAR_NOSIZE(UKnuthsortP_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UKnuthsortP_exsitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UZacksort_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UZacksort_exsitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UZacksortB_insitu)
  SERIAL_SORT_VARCHAR_NOSIZE(UZacksortB_exsitu)


  SERIAL_SORTB_VARCHAR_NOSIZE(VInsertionsort_insitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VInsertionsort_exsitu)

  SERIAL_SORTB_VARCHAR_NOSIZE(VKnuthsort_insitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VKnuthsort_exsitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VKnuthsortA_insitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VKnuthsortA_exsitu)

  SERIAL_SORTB_VARCHAR_NOSIZE(VFrogsort1_insitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VFrogsort1_exsitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VFrogsort1A_insitu)
  SERIAL_SORTB_VARCHAR_NOSIZE(VFrogsort1A_exsitu)

  PARALLEL_SORTB_VARCHAR_NOSIZE(PVKnuthsort_insitu)
  PARALLEL_SORTB_VARCHAR_NOSIZE(PVKnuthsort_exsitu)

  PARALLEL_SORTB_VARCHAR_NOSIZE(PVFrogsort1_insitu)
  PARALLEL_SORTB_VARCHAR_NOSIZE(PVFrogsort1_exsitu)


