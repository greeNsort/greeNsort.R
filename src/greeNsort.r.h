/*
# greeNsort package gluecode declarations
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_greeNsort_r_h
#define ALREADY_DEFINED_greeNsort_r_h

#include "greeNsort.h"

// for SEXP
#include <R.h>  // needed here for SEXP and definition of GetRNGstate PutRNGstate unif_rand

SEXP r_last_value();
SEXP r_last_range();

SEXP r_insertionsort_limit();

SEXP r_string_check(
    SEXP x_           // string vector
);

SEXP r_stable_keys(
    SEXP x_            // data vector
);

SEXP r_current_keys(
  SEXP x_            // data vector
);

SEXP r_current_compare(
    SEXP x_
    , SEXP y_
);

SEXP r_current_issorted(
    SEXP x_
);

SEXP r_perfpaths(
);

SEXP r_perfnow(
);
SEXP r_perfmax(
);
SEXP r_perfsleep(
    SEXP x_           // double scalar number of seconds to sleep
);

SEXP r_symsearch(
    SEXP x_
  , SEXP v_          // value to be searched
  , SEXP cmp_        // -2=LT -1=LE 0=EQ 1=GE 2=GT
  , SEXP decreasing_ // FALSE=ASC TRUE=DESC
);


SEXP r_Selectionsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Selectionsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Selectionsort2_insitu(
    SEXP x_            // data vector
);
SEXP r_Selectionsort2_exsitu(
    SEXP x_            // data vector
);


SEXP r_MInsertionsort_l2r_insitu(
    SEXP x_
);
SEXP r_MInsertionsort_l2r_exsitu(
    SEXP x_
);
SEXP r_NInsertionsort_l2r_insitu(
    SEXP x_
);
SEXP r_NInsertionsort_l2r_exsitu(
    SEXP x_
);

SEXP r_Insertionsort_l2r_insitu(
  SEXP x_
);
SEXP r_Insertionsort_r2l_insitu(
  SEXP x_
);
SEXP r_Insertionsort_desc_l2r_insitu(
  SEXP x_
);
SEXP r_Insertionsort_desc_r2l_insitu(
  SEXP x_
);
SEXP r_Insertionorder_l2r_insitu(
  SEXP x_
, SEXP o_
);
SEXP r_Insertionorder_r2l_insitu(
  SEXP x_
, SEXP o_
);

SEXP r_Insertionsort_l2r_exsitu(
    SEXP x_
);
SEXP r_Insertionsort_r2l_exsitu(
    SEXP x_
);
SEXP r_Insertionsort_desc_l2r_exsitu(
    SEXP x_
);
SEXP r_Insertionsort_desc_r2l_exsitu(
    SEXP x_
);
SEXP r_Insertionorder_l2r_exsitu(
    SEXP x_
  , SEXP o_
);
SEXP r_Insertionorder_r2l_exsitu(
    SEXP x_
  , SEXP o_
);

SEXP r_Jumpsort_insitu(
    SEXP x_            // data vector
  , SEXP b_            // size of one buffer (2 or 3 are used)
);
SEXP r_Jumpsort_exsitu(
    SEXP x_            // data vector
  , SEXP b_            // size of one buffer (2 or 3 are used)
);
SEXP r_Walksort_insitu(
  SEXP x_            // data vector
, SEXP b_            // size of one buffer (2 or 3 are used)
);
SEXP r_Walksort_exsitu(
  SEXP x_            // data vector
, SEXP b_            // size of one buffer (2 or 3 are used)
);
SEXP r_SqrtsortP_insitu(
  SEXP x_            // data vector
);
SEXP r_SqrtsortP_exsitu(
  SEXP x_            // data vector
);
SEXP r_GrailsqrtP_insitu(
  SEXP x_            // data vector
);
SEXP r_GrailsqrtP_exsitu(
  SEXP x_            // data vector
);
SEXP r_GrailsortP_insitu(
  SEXP x_            // data vector
);
SEXP r_GrailsortP_exsitu(
  SEXP x_            // data vector
);
SEXP r_InplaceMergesortP_insitu(
    SEXP x_            // data vector
);
SEXP r_InplaceMergesortP_exsitu(
    SEXP x_            // data vector
);


SEXP r_Copysort_insitu(
    SEXP x_            // data vector
);
SEXP r_Copysort_exsitu(
    SEXP x_            // data vector
);
SEXP r_CopysortP_insitu(
    SEXP x_            // data vector
);
SEXP r_CopysortP_exsitu(
    SEXP x_            // data vector
);


SEXP r_Nocosort_insitu(
  SEXP x_            // data vector
);
SEXP r_Nocosort_exsitu(
  SEXP x_            // data vector
);
SEXP r_NocosortP_insitu(
    SEXP x_            // data vector
);
SEXP r_NocosortP_exsitu(
    SEXP x_            // data vector
);

SEXP r_Simplsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Simplsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_SimplsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_SimplsortP_exsitu(
    SEXP x_            // data vector
);

SEXP r_Bimesort_insitu(
    SEXP x_            // data vector
);
SEXP r_Bimesort_exsitu(
    SEXP x_            // data vector
);
SEXP r_BimesortP_insitu(
    SEXP x_            // data vector
);
SEXP r_BimesortP_exsitu(
    SEXP x_            // data vector
);

SEXP r_MKnuthsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_MKnuthsortP_exsitu(
    SEXP x_            // data vector
);
SEXP r_NKnuthsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_NKnuthsortP_exsitu(
    SEXP x_            // data vector
);


SEXP r_Knuthsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Knuthsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_KnuthsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_KnuthsortP_exsitu(
    SEXP x_            // data vector
);
SEXP r_PKnuthsortP_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PKnuthsortP_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PFrogsort0P_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PFrogsort0P_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PFrogsort1P_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PFrogsort1P_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PFrogsort2P_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP t_            // threads
);
SEXP r_PFrogsort2P_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP t_            // threads
);
SEXP r_PFrogsort3P_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP t_            // threads
);
SEXP r_PFrogsort3P_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP t_            // threads
);


SEXP r_Katasort_insitu(
    SEXP x_            // data vector
);
SEXP r_Katasort_exsitu(
    SEXP x_            // data vector
);
SEXP r_KatasortP_insitu(
    SEXP x_            // data vector
);
SEXP r_KatasortP_exsitu(
    SEXP x_            // data vector
);

SEXP r_KnuthsortA_insitu(
    SEXP x_            // data vector
);
SEXP r_KnuthsortA_exsitu(
    SEXP x_            // data vector
);
SEXP r_KnuthsortAP_insitu(
    SEXP x_            // data vector
);
SEXP r_KnuthsortAP_exsitu(
    SEXP x_            // data vector
);
SEXP r_KatasortA_insitu(
    SEXP x_            // data vector
);
SEXP r_KatasortA_exsitu(
    SEXP x_            // data vector
);
SEXP r_KatasortAP_insitu(
    SEXP x_            // data vector
);
SEXP r_KatasortAP_exsitu(
    SEXP x_            // data vector
);
SEXP r_KatasortB_insitu(
    SEXP x_            // data vector
);
SEXP r_KatasortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_KatasortBP_insitu(
    SEXP x_            // data vector
);
SEXP r_KatasortBP_exsitu(
    SEXP x_            // data vector
);
SEXP r_BimesortB_insitu(
    SEXP x_            // data vector
);
SEXP r_BimesortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_BimesortBP_insitu(
    SEXP x_            // data vector
);
SEXP r_BimesortBP_exsitu(
    SEXP x_            // data vector
);


SEXP r_Omitsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Omitsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_OmitsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_OmitsortP_exsitu(
    SEXP x_            // data vector
);
SEXP r_OctosortP_insitu(
    SEXP x_            // data vector
);
SEXP r_OctosortP_exsitu(
    SEXP x_            // data vector
);


SEXP r_Ininsort_insitu(
  SEXP x_            // data vector
);
SEXP r_Ininsort_exsitu(
  SEXP x_            // data vector
);
SEXP r_Ninisort_insitu(
  SEXP x_            // data vector
);
SEXP r_Ninisort_exsitu(
  SEXP x_            // data vector
);

SEXP r_GKnuthsort_insitu(
    SEXP x_            // data vector
);
SEXP r_GKnuthsort_exsitu(
    SEXP x_            // data vector
);

SEXP r_Frogsort0_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort0_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort0P_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort0P_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1P_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1P_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1A_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1A_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1AP_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1AP_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1B_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1B_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1BP_insitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort1BP_exsitu(
    SEXP x_            // data vector
);
SEXP r_Frogsort2_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2P_insitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2P_exsitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_MFrogsort2P_insitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_MFrogsort2P_exsitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_NFrogsort2P_insitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_NFrogsort2P_exsitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2A_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2A_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2AP_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2AP_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2B_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2B_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2BP_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort2BP_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3P_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3P_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3B_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3B_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3BP_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort3BP_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Frogsort6_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP p2_           // fraction of buffer  0 < p < p2 <= 0.5
);
SEXP r_Frogsort6_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP p2_           // fraction of buffer  0 < p < p2 <= 0.5
);
SEXP r_Frogsort6P_insitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP p2_           // fraction of buffer  0 < p < p2 <= 0.5
);
SEXP r_Frogsort6P_exsitu(
    SEXP x_            // data vector
  , SEXP p_            // fraction of buffer  0 < p <= 0.5
  , SEXP p2_           // fraction of buffer  0 < p < p2 <= 0.5
);
SEXP r_Frogsort4_insitu(
    SEXP x_            // data vector
  , SEXP b_            // size of buffer for initial phase
);
SEXP r_Frogsort4_exsitu(
    SEXP x_            // data vector
  , SEXP b_            // size of buffer for initial phase
);


SEXP r_Geckosort0_insitu(
  SEXP x_            // data vector
);
SEXP r_Geckosort0_exsitu(
  SEXP x_            // data vector
);

SEXP r_Geckosort1_insitu(
    SEXP x_            // data vector
);
SEXP r_Geckosort1_exsitu(
    SEXP x_            // data vector
);
SEXP r_Geckosort1P_insitu(
    SEXP x_            // data vector
);
SEXP r_Geckosort1P_exsitu(
    SEXP x_            // data vector
);
SEXP r_Squidsort1P_insitu(
    SEXP x_            // data vector
);
SEXP r_Squidsort1P_exsitu(
    SEXP x_            // data vector
);
SEXP r_Squidsort2P_insitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);
SEXP r_Squidsort2P_exsitu(
    SEXP x_            // data vector
    , SEXP p_            // fraction of buffer  0 < p <= 0.5
);

SEXP r_TKnuthsort_insitu(
  SEXP x_            // data vector
);
SEXP r_TKnuthsort_exsitu(
  SEXP x_            // data vector
);

SEXP r_Crocosort_insitu(
  SEXP x_            // data vector
);
SEXP r_Crocosort_exsitu(
  SEXP x_            // data vector
);



SEXP r_Knuth4sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Knuth4sort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Knuth3sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Knuth3sort_exsitu(
    SEXP x_            // data vector
);

SEXP r_Kata4sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Kata4sort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Kata3sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Kata3sort_exsitu(
    SEXP x_            // data vector
);

SEXP r_Croco3sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Croco3sort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Croco4sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Croco4sort_exsitu(
    SEXP x_            // data vector
);

SEXP r_Kroco3sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Kroco3sort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Kroco4sort_insitu(
    SEXP x_            // data vector
);
SEXP r_Kroco4sort_exsitu(
    SEXP x_            // data vector
);


SEXP r_DietACPsort_insitu(
  SEXP x_            // data vector
);
SEXP r_DietACPsort_exsitu(
  SEXP x_            // data vector
);
SEXP r_DietACPTsort_insitu(
  SEXP x_            // data vector
);
SEXP r_DietACPTsort_exsitu(
  SEXP x_            // data vector
);

SEXP r_DietCPsort_insitu(
    SEXP x_            // data vector
);
SEXP r_DietCPsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_DietCPsortB_insitu(
    SEXP x_            // data vector
);
SEXP r_DietCPsortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_DietcP2sort_insitu(
    SEXP x_            // data vector
);
SEXP r_DietcP2sort_exsitu(
    SEXP x_            // data vector
);
SEXP r_DietPcsort_insitu(
    SEXP x_            // data vector
);
SEXP r_DietPcsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_DietCPTsort_insitu(
  SEXP x_            // data vector
);
SEXP r_DietCPTsort_exsitu(
  SEXP x_            // data vector
);

SEXP r_Kiwisort_insitu(
    SEXP x_            // data vector
);
SEXP r_Kiwisort_exsitu(
    SEXP x_            // data vector
);
SEXP r_KiwisortA_insitu(
    SEXP x_            // data vector
);
SEXP r_KiwisortA_exsitu(
    SEXP x_            // data vector
);
SEXP r_KiwisortB_insitu(
    SEXP x_            // data vector
);
SEXP r_KiwisortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_Swansort_insitu(
    SEXP x_            // data vector
);
SEXP r_Swansort_exsitu(
    SEXP x_            // data vector
);
SEXP r_SwansortB_insitu(
    SEXP x_            // data vector
);
SEXP r_SwansortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_Storksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Storksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_StorksortB_exsitu(
    SEXP x_            // data vector
);


SEXP r_BFPRTsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Picksort_insitu(
    SEXP x_            // data vector
);

SEXP r_Zocksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Zocksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Zacksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Zacksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_Zucksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Zucksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortD_insitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortD_exsitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortA_insitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortA_exsitu(
    SEXP x_            // data vector
);
SEXP r_ZacksortB_insitu(
    SEXP x_            // data vector
);
SEXP r_ZacksortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortB_insitu(
    SEXP x_            // data vector
);
SEXP r_ZucksortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_Ducksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Ducksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_DucksortB_insitu(
    SEXP x_            // data vector
);
SEXP r_DucksortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_Chicksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Chicksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_ChicksortP_insitu(
    SEXP x_            // data vector
);
SEXP r_ChicksortP_exsitu(
    SEXP x_            // data vector
);

SEXP r_Dupisort_insitu(
  SEXP x_            // data vector
);
SEXP r_Dupisort_exsitu(
  SEXP x_            // data vector
);
SEXP r_DupisortP_insitu(
  SEXP x_            // data vector
);
SEXP r_DupisortP_exsitu(
  SEXP x_            // data vector
);

SEXP r_Tricksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Tricksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_TricksortP_insitu(
  SEXP x_            // data vector
);
SEXP r_TricksortP_exsitu(
  SEXP x_            // data vector
);


SEXP r_Plugsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Pugsort_insitu(
    SEXP x_            // data vector
);
SEXP r_Slicksort_insitu(
    SEXP x_            // data vector
);
SEXP r_Sicksort_insitu(
    SEXP x_            // data vector
);

SEXP r_Chunksort_noperf(
    SEXP x_            // data vector
  , SEXP b_            // chunk size
  , SEXP d_            // direction
);
SEXP r_Chunksort_insitu(
    SEXP x_            // data vector
  , SEXP b_            // chunk size
  , SEXP d_            // direction
);
SEXP r_Chunksort_exsitu(
    SEXP x_            // data vector
  , SEXP b_            // chunk size
  , SEXP d_            // direction
);

SEXP r_Quicksort1_insitu(
    SEXP x_            // data vector
);
SEXP r_Quicksort1_exsitu(
    SEXP x_            // data vector
);
SEXP r_Lomutosort_insitu(
    SEXP x_            // data vector
);
SEXP r_Lomutosort_exsitu(
    SEXP x_            // data vector
);

SEXP r_Quicksort2_insitu(
    SEXP x_            // data vector
);
SEXP r_Quicksort2_exsitu(
    SEXP x_            // data vector
);
SEXP r_Quicksort2B_insitu(
    SEXP x_            // data vector
);
SEXP r_Quicksort2B_exsitu(
    SEXP x_            // data vector
);
SEXP r_PQuicksort2_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PQuicksort2_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PQuicksort2B_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PQuicksort2B_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PDucksort_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PDucksort_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PDucksortB_insitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_PDucksortB_exsitu(
    SEXP x_            // data vector
  , SEXP t_            // threads
);
SEXP r_Quicksort2P_insitu(
  SEXP x_            // data vector
);
SEXP r_Quicksort2P_exsitu(
  SEXP x_            // data vector
);
SEXP r_Quicksort3_insitu(
  SEXP x_            // data vector
);
SEXP r_Quicksort3_exsitu(
  SEXP x_            // data vector
);
SEXP r_Quicksort3P_insitu(
  SEXP x_            // data vector
);

SEXP r_SQuicksort2_insitu(
    SEXP x_            // data vector
);
SEXP r_SQuicksort2_exsitu(
    SEXP x_            // data vector
);
SEXP r_SQuicksort2B_insitu(
    SEXP x_            // data vector
);
SEXP r_SQuicksort2B_exsitu(
    SEXP x_            // data vector
);


SEXP r_Dotnetpart_insitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);
SEXP r_Dotnetpart_exsitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);
SEXP r_Quickpart2_insitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);
SEXP r_Quickpart2_exsitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);
SEXP r_Zackpart_insitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);
SEXP r_Zackpart_exsitu(
    SEXP x_            // data vector
  ,   SEXP partial_      // lower and upper limit for partial sorting
);


SEXP r_RQuicksort2_insitu(
    SEXP x_            // data vector
  , SEXP reorder_inplace_
);
SEXP r_RQuicksort2_exsitu(
    SEXP x_            // data vector
);
SEXP r_RQuicksort2B_insitu(
    SEXP x_            // data vector
  , SEXP reorder_inplace_
);
SEXP r_RQuicksort2B_exsitu(
    SEXP x_            // data vector
);



SEXP r_approxMedian_insitu(
    SEXP x_            // data vector of length n
);
SEXP r_approxMedian_exsitu(
    SEXP x_            // data vector of length n
);
SEXP r_BFPRTselect_insitu(
    SEXP x_            // data vector of length n
    , SEXP k_            // selector in [0, n-1]
);
SEXP r_BFPRTselect_exsitu(
    SEXP x_            // data vector of length n
    , SEXP k_            // selector in [0, n-1]
);


SEXP r_Quickpartleft2_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Quickpartleft2_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Quickpartright2_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Quickpartright2_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);

SEXP r_Quickselect2_insitu(
  SEXP x_            // data vector of length n
, SEXP k_            // selector in [0, n-1]
);
SEXP r_Quickselect2_exsitu(
  SEXP x_            // data vector of length n
, SEXP k_            // selector in [0, n-1]
);

SEXP r_BFPRTelect_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_BFPRTelect_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);

SEXP r_Pickselect_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Pickselect_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);

SEXP r_Zackpartleft_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zackpartleft_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zackpartright_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zackpartright_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);

SEXP r_Zackselect_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zackselect_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zuckselect_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_Zuckselect_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_ZuckselectB_insitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);
SEXP r_ZuckselectB_exsitu(
    SEXP x_            // data vector of length n
  , SEXP k_            // selector in [0, n-1]
);

SEXP r_Zickselect_insitu(
  SEXP x_            // data vector of length n
, SEXP k_            // selector in [0, n-1]
);
SEXP r_Zickselect_exsitu(
  SEXP x_            // data vector of length n
, SEXP k_            // selector in [0, n-1]
);


SEXP r_SInsertionsort_insitu(
    SEXP x_            // data vector
);
SEXP r_SInsertionsort_exsitu(
    SEXP x_            // data vector
);




SEXP r_UInsertionsort_insitu(
    SEXP x_            // data vector
);
SEXP r_UInsertionsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_WInsertionsort_insitu(
    SEXP x_            // data vector
);
SEXP r_WInsertionsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_VInsertionsort_insitu(
    SEXP x_            // data vector
);
SEXP r_VInsertionsort_exsitu(
    SEXP x_            // data vector
);

SEXP r_VKnuthsortA_insitu(
    SEXP x_            // data vector
);
SEXP r_VKnuthsortA_exsitu(
    SEXP x_            // data vector
);
SEXP r_VKnuthsort_insitu(
    SEXP x_            // data vector
);
SEXP r_VKnuthsort_exsitu(
    SEXP x_            // data vector
);
SEXP r_PVKnuthsort_insitu(
    SEXP x_            // data vector
  ,  SEXP t_            // threads
);
SEXP r_PVKnuthsort_exsitu(
    SEXP x_            // data vector
  ,  SEXP t_            // threads
);
SEXP r_VFrogsort1_insitu(
    SEXP x_            // data vector
);
SEXP r_VFrogsort1_exsitu(
    SEXP x_            // data vector
);
SEXP r_PVFrogsort1_insitu(
    SEXP x_            // data vector
  ,  SEXP t_            // threads
);
SEXP r_PVFrogsort1_exsitu(
    SEXP x_            // data vector
  ,  SEXP t_            // threads
);
SEXP r_VFrogsort1A_insitu(
    SEXP x_            // data vector
);
SEXP r_VFrogsort1A_exsitu(
    SEXP x_            // data vector
);
SEXP r_UKnuthsortP_insitu(
    SEXP x_            // data vector
);
SEXP r_UKnuthsortP_exsitu(
    SEXP x_            // data vector
);
SEXP r_UZacksort_insitu(
    SEXP x_            // data vector
);
SEXP r_UZacksort_exsitu(
    SEXP x_            // data vector
);
SEXP r_UZacksortB_insitu(
    SEXP x_            // data vector
);
SEXP r_UZacksortB_exsitu(
    SEXP x_            // data vector
);
SEXP r_WQuicksort2_insitu(
    SEXP x_            // data vector
);
SEXP r_WQuicksort2_exsitu(
    SEXP x_            // data vector
);
SEXP r_WQuicksort2B_insitu(
    SEXP x_            // data vector
);
SEXP r_WQuicksort2B_exsitu(
    SEXP x_            // data vector
);
SEXP r_UVWoverhead(
    SEXP x_            // data vector
);

#endif
