/*
# greeNsort ntile calculation
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include "algo.h"
#include "ntile2.h"

SEXP r_Ntile2_asc_asc_left_to_right(
    SEXP L_
  , SEXP R_
  , SEXP K_
)
{
  SEXP ret_;
  int i;
  int Ln = LENGTH(L_);
  int Rn = LENGTH(R_);
  int Kn = LENGTH(K_);
  Ntile2Struct lr, cumlr;
  PROTECT( ret_ = allocVector(INTSXP, 2*Kn) );
  int *ret = INTEGER(ret_);

  if (TYPEOF(L_) != REALSXP)
    error("L_ is not double in r_Ntile2_asc_asc_left_to_right");
  if (TYPEOF(R_) != REALSXP)
    error("R_ is not double in r_Ntile2_asc_asc_left_to_right");

  double *L,*R;
  L = REAL(L_);
  R = REAL(R_);
  int *K2 = INTEGER(K_);

  cumlr.l = 0;
  cumlr.r = 0;
  for (i=0; i<Kn; i++){
    //Rprintf("i=%d cumlr.l=%d cumlr.r=%d\n", i, cumlr.l, cumlr.r);
    lr = Ntile2_asc_asc_left_to_right(L+cumlr.l, R+cumlr.r, Ln-cumlr.l, Rn-cumlr.r, K2[i]-cumlr.l-cumlr.r);
    cumlr.l += lr.l;
    cumlr.r += lr.r;
    ret[2*i] = cumlr.l;
    ret[2*i+1] = cumlr.r;
  }

  UNPROTECT(1);
  return ret_;
}

SEXP r_Ntile2_asc_revasc_left_to_right(
    SEXP L_
  , SEXP R_
  , SEXP K_
)
{
  SEXP ret_;
  int i;
  int Ln = LENGTH(L_);
  int Rn = LENGTH(R_);
  int Kn = LENGTH(K_);
  Ntile2Struct lr, cumlr;
  PROTECT( ret_ = allocVector(INTSXP, 2*Kn) );
  int *ret = INTEGER(ret_);

  if (TYPEOF(L_) != REALSXP)
    error("L_ is not double in r_Ntile2_asc_asc_left_to_right");
  if (TYPEOF(R_) != REALSXP)
    error("R_ is not double in r_Ntile2_asc_asc_left_to_right");

  double *L,*R;
  L = REAL(L_);
  R = REAL(R_);
  int *K2 = INTEGER(K_);

  cumlr.l = 0;
  cumlr.r = 0;
  for (i=0; i<Kn; i++){
    //Rprintf("i=%d cumlr.l=%d cumlr.r=%d\n", i, cumlr.l, cumlr.r);
    lr = Ntile2_asc_revasc_left_to_right(L+cumlr.l, R, Ln-cumlr.l, Rn-cumlr.r, K2[i]-cumlr.l-cumlr.r);
    cumlr.l += lr.l;
    cumlr.r += lr.r;
    ret[2*i] = cumlr.l;
    ret[2*i+1] = cumlr.r;
  }

  UNPROTECT(1);
  return ret_;
}

SEXP r_Ntile2_asc_asc_right_to_left(
    SEXP L_
  , SEXP R_
  , SEXP K_
)
{
  SEXP ret_;
  int i;
  int Ln = LENGTH(L_);
  int Rn = LENGTH(R_);
  int Kn = LENGTH(K_);
  Ntile2Struct lr, cumlr;
  PROTECT( ret_ = allocVector(INTSXP, 2*Kn) );
  int *ret = INTEGER(ret_);

  if (TYPEOF(L_) != REALSXP)
    error("L_ is not double in r_Ntile2_asc_asc_left_to_right");
  if (TYPEOF(R_) != REALSXP)
    error("R_ is not double in r_Ntile2_asc_asc_left_to_right");

  double *L,*R;
  L = REAL(L_);
  R = REAL(R_);
  int *K2 = INTEGER(K_);

  cumlr.l = 0;
  cumlr.r = 0;
  for (i=0; i<Kn; i++){
    //Rprintf("i=%d cumlr.l=%d cumlr.r=%d\n", i, cumlr.l, cumlr.r);
    lr = Ntile2_asc_asc_right_to_left(L, R, Ln-cumlr.l, Rn-cumlr.r, K2[i]-cumlr.l-cumlr.r);
    cumlr.l += lr.l;
    cumlr.r += lr.r;
    ret[2*i] = cumlr.l;
    ret[2*i+1] = cumlr.r;
  }

  UNPROTECT(1);
  return ret_;
}

SEXP r_Ntile2_asc_revasc_right_to_left(
    SEXP L_
  , SEXP R_
  , SEXP K_
)
{
  SEXP ret_;
  int i;
  int Ln = LENGTH(L_);
  int Rn = LENGTH(R_);
  int Kn = LENGTH(K_);
  Ntile2Struct lr, cumlr;
  PROTECT( ret_ = allocVector(INTSXP, 2*Kn) );
  int *ret = INTEGER(ret_);

  if (TYPEOF(L_) != REALSXP)
    error("L_ is not double in r_Ntile2_asc_asc_left_to_right");
  if (TYPEOF(R_) != REALSXP)
    error("R_ is not double in r_Ntile2_asc_asc_left_to_right");

  double *L,*R;
  L = REAL(L_);
  R = REAL(R_);
  int *K2 = INTEGER(K_);

  cumlr.l = 0;
  cumlr.r = 0;
  for (i=0; i<Kn; i++){
    //Rprintf("i=%d cumlr.l=%d cumlr.r=%d\n", i, cumlr.l, cumlr.r);
    lr = Ntile2_asc_revasc_right_to_left(L, R+cumlr.r, Ln-cumlr.l, Rn-cumlr.r, K2[i]-cumlr.l-cumlr.r);
    cumlr.l += lr.l;
    cumlr.r += lr.r;
    ret[2*i] = cumlr.l;
    ret[2*i+1] = cumlr.r;
  }

  UNPROTECT(1);
  return ret_;
}

