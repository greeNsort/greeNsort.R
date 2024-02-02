/*
# greeNsort package declarations for shared lib loading 
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP r_cran_compatible();
extern SEXP r_insertionsort_limit();

extern SEXP r_string_check(SEXP);

extern SEXP r_stable_keys(SEXP);
extern SEXP r_current_keys(SEXP);
extern SEXP r_current_compare(SEXP, SEXP);
extern SEXP r_current_issorted(SEXP);

extern SEXP r_Ntile2_asc_asc_left_to_right(SEXP, SEXP, SEXP);
extern SEXP r_Ntile2_asc_revasc_left_to_right(SEXP, SEXP, SEXP);
extern SEXP r_Ntile2_asc_asc_right_to_left(SEXP, SEXP, SEXP);
extern SEXP r_Ntile2_asc_revasc_right_to_left(SEXP, SEXP, SEXP);

extern SEXP r_perfpaths();
extern SEXP r_perfnow();
extern SEXP r_perfmax();
extern SEXP r_perfsleep(SEXP);

extern SEXP r_symsearch(SEXP, SEXP, SEXP, SEXP);

extern SEXP r_approxMedian_exsitu(SEXP);
extern SEXP r_approxMedian_insitu(SEXP);
extern SEXP r_Bimesort_exsitu(SEXP);
extern SEXP r_Bimesort_insitu(SEXP);
extern SEXP r_BimesortB_exsitu(SEXP);
extern SEXP r_BimesortB_insitu(SEXP);
extern SEXP r_BimesortBP_exsitu(SEXP);
extern SEXP r_BimesortBP_insitu(SEXP);
extern SEXP r_BimesortP_exsitu(SEXP);
extern SEXP r_BimesortP_insitu(SEXP);
extern SEXP r_Chicksort_insitu(SEXP);
extern SEXP r_Chicksort_exsitu(SEXP);
extern SEXP r_ChicksortP_insitu(SEXP);
extern SEXP r_ChicksortP_exsitu(SEXP);
extern SEXP r_Copysort_exsitu(SEXP);
extern SEXP r_Copysort_insitu(SEXP);
extern SEXP r_CopysortP_exsitu(SEXP);
extern SEXP r_CopysortP_insitu(SEXP);
extern SEXP r_Croco3sort_exsitu(SEXP);
extern SEXP r_Croco3sort_insitu(SEXP);
extern SEXP r_Croco4sort_exsitu(SEXP);
extern SEXP r_Croco4sort_insitu(SEXP);
extern SEXP r_Crocosort_exsitu(SEXP);
extern SEXP r_Crocosort_insitu(SEXP);
extern SEXP r_DietACPsort_exsitu(SEXP);
extern SEXP r_DietACPsort_insitu(SEXP);
extern SEXP r_DietACPTsort_exsitu(SEXP);
extern SEXP r_DietACPTsort_insitu(SEXP);
extern SEXP r_DietCPsort_exsitu(SEXP);
extern SEXP r_DietCPsort_insitu(SEXP);
extern SEXP r_DietCPsortB_exsitu(SEXP);
extern SEXP r_DietCPsortB_insitu(SEXP);
extern SEXP r_DietcP2sort_exsitu(SEXP);
extern SEXP r_DietcP2sort_insitu(SEXP);
extern SEXP r_DietCPTsort_exsitu(SEXP);
extern SEXP r_DietCPTsort_insitu(SEXP);
extern SEXP r_DietPcsort_exsitu(SEXP);
extern SEXP r_DietPcsort_insitu(SEXP);
extern SEXP r_Dupisort_exsitu(SEXP);
extern SEXP r_Dupisort_insitu(SEXP);
extern SEXP r_DupisortP_exsitu(SEXP);
extern SEXP r_DupisortP_insitu(SEXP);
extern SEXP r_Frogsort0_exsitu(SEXP);
extern SEXP r_Frogsort0_insitu(SEXP);
extern SEXP r_Frogsort0P_exsitu(SEXP);
extern SEXP r_Frogsort0P_insitu(SEXP);
extern SEXP r_Frogsort1_exsitu(SEXP);
extern SEXP r_Frogsort1_insitu(SEXP);
extern SEXP r_Frogsort1P_exsitu(SEXP);
extern SEXP r_Frogsort1P_insitu(SEXP);
extern SEXP r_Frogsort1A_exsitu(SEXP);
extern SEXP r_Frogsort1A_insitu(SEXP);
extern SEXP r_Frogsort1AP_exsitu(SEXP);
extern SEXP r_Frogsort1AP_insitu(SEXP);
extern SEXP r_Frogsort1B_exsitu(SEXP);
extern SEXP r_Frogsort1B_insitu(SEXP);
extern SEXP r_Frogsort1BP_exsitu(SEXP);
extern SEXP r_Frogsort1BP_insitu(SEXP);
extern SEXP r_Frogsort2_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2_insitu(SEXP, SEXP);
extern SEXP r_Frogsort2P_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2P_insitu(SEXP, SEXP);
extern SEXP r_MFrogsort2P_exsitu(SEXP, SEXP);
extern SEXP r_MFrogsort2P_insitu(SEXP, SEXP);
extern SEXP r_NFrogsort2P_exsitu(SEXP, SEXP);
extern SEXP r_NFrogsort2P_insitu(SEXP, SEXP);
extern SEXP r_Frogsort2A_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2A_insitu(SEXP, SEXP);
extern SEXP r_Frogsort2AP_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2AP_insitu(SEXP, SEXP);
extern SEXP r_Frogsort2B_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2B_insitu(SEXP, SEXP);
extern SEXP r_Frogsort2BP_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort2BP_insitu(SEXP, SEXP);
extern SEXP r_Frogsort3_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort3_insitu(SEXP, SEXP);
extern SEXP r_Frogsort3P_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort3P_insitu(SEXP, SEXP);
extern SEXP r_Frogsort3B_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort3B_insitu(SEXP, SEXP);
extern SEXP r_Frogsort3BP_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort3BP_insitu(SEXP, SEXP);
extern SEXP r_Frogsort4_exsitu(SEXP, SEXP);
extern SEXP r_Frogsort4_insitu(SEXP, SEXP);
extern SEXP r_Frogsort6_exsitu(SEXP, SEXP, SEXP);
extern SEXP r_Frogsort6_insitu(SEXP, SEXP, SEXP);
extern SEXP r_Frogsort6P_exsitu(SEXP, SEXP, SEXP);
extern SEXP r_Frogsort6P_insitu(SEXP, SEXP, SEXP);
extern SEXP r_Geckosort0_exsitu(SEXP);
extern SEXP r_Geckosort0_insitu(SEXP);
extern SEXP r_Geckosort1_exsitu(SEXP);
extern SEXP r_Geckosort1_insitu(SEXP);
extern SEXP r_Geckosort1P_exsitu(SEXP);
extern SEXP r_Geckosort1P_insitu(SEXP);
extern SEXP r_Squidsort1P_exsitu(SEXP);
extern SEXP r_Squidsort1P_insitu(SEXP);
extern SEXP r_Squidsort2P_exsitu(SEXP, SEXP);
extern SEXP r_Squidsort2P_insitu(SEXP, SEXP);
extern SEXP r_GKnuthsort_exsitu(SEXP);
extern SEXP r_GKnuthsort_insitu(SEXP);
extern SEXP r_GrailsortP_exsitu(SEXP);
extern SEXP r_GrailsortP_insitu(SEXP);
extern SEXP r_GrailsqrtP_exsitu(SEXP);
extern SEXP r_GrailsqrtP_insitu(SEXP);
extern SEXP r_Ininsort_exsitu(SEXP);
extern SEXP r_Ininsort_insitu(SEXP);
extern SEXP r_Selectionsort_exsitu(SEXP);
extern SEXP r_Selectionsort_insitu(SEXP);
extern SEXP r_Selectionsort2_exsitu(SEXP);
extern SEXP r_Selectionsort2_insitu(SEXP);
extern SEXP r_InplaceMergesortP_exsitu(SEXP);
extern SEXP r_InplaceMergesortP_insitu(SEXP);
extern SEXP r_Insertionorder_l2r_exsitu(SEXP, SEXP);
extern SEXP r_Insertionorder_l2r_insitu(SEXP, SEXP);
extern SEXP r_Insertionorder_r2l_exsitu(SEXP, SEXP);
extern SEXP r_Insertionorder_r2l_insitu(SEXP, SEXP);
extern SEXP r_Insertionsort_desc_l2r_exsitu(SEXP);
extern SEXP r_Insertionsort_desc_l2r_insitu(SEXP);
extern SEXP r_Insertionsort_desc_r2l_exsitu(SEXP);
extern SEXP r_Insertionsort_desc_r2l_insitu(SEXP);
extern SEXP r_MInsertionsort_l2r_exsitu(SEXP);
extern SEXP r_MInsertionsort_l2r_insitu(SEXP);
extern SEXP r_NInsertionsort_l2r_exsitu(SEXP);
extern SEXP r_NInsertionsort_l2r_insitu(SEXP);
extern SEXP r_Insertionsort_l2r_exsitu(SEXP);
extern SEXP r_Insertionsort_l2r_insitu(SEXP);
extern SEXP r_Insertionsort_r2l_exsitu(SEXP);
extern SEXP r_Insertionsort_r2l_insitu(SEXP);
extern SEXP r_Jumpsort_exsitu(SEXP, SEXP);
extern SEXP r_Jumpsort_insitu(SEXP, SEXP);
extern SEXP r_Kata3sort_exsitu(SEXP);
extern SEXP r_Kata3sort_insitu(SEXP);
extern SEXP r_Kata4sort_exsitu(SEXP);
extern SEXP r_Kata4sort_insitu(SEXP);
extern SEXP r_Katasort_exsitu(SEXP);
extern SEXP r_Katasort_insitu(SEXP);
extern SEXP r_KatasortA_exsitu(SEXP);
extern SEXP r_KatasortA_insitu(SEXP);
extern SEXP r_KatasortAP_exsitu(SEXP);
extern SEXP r_KatasortAP_insitu(SEXP);
extern SEXP r_KatasortB_exsitu(SEXP);
extern SEXP r_KatasortB_insitu(SEXP);
extern SEXP r_KatasortBP_exsitu(SEXP);
extern SEXP r_KatasortBP_insitu(SEXP);
extern SEXP r_KatasortP_exsitu(SEXP);
extern SEXP r_KatasortP_insitu(SEXP);
extern SEXP r_Kiwisort_exsitu(SEXP);
extern SEXP r_Kiwisort_insitu(SEXP);
extern SEXP r_KiwisortA_exsitu(SEXP);
extern SEXP r_KiwisortA_insitu(SEXP);
extern SEXP r_KiwisortB_exsitu(SEXP);
extern SEXP r_KiwisortB_insitu(SEXP);
extern SEXP r_Knuth3sort_exsitu(SEXP);
extern SEXP r_Knuth3sort_insitu(SEXP);
extern SEXP r_Knuth4sort_exsitu(SEXP);
extern SEXP r_Knuth4sort_insitu(SEXP);
extern SEXP r_Knuthsort_exsitu(SEXP);
extern SEXP r_Knuthsort_insitu(SEXP);
extern SEXP r_KnuthsortA_exsitu(SEXP);
extern SEXP r_KnuthsortA_insitu(SEXP);
extern SEXP r_KnuthsortAP_exsitu(SEXP);
extern SEXP r_KnuthsortAP_insitu(SEXP);
extern SEXP r_KnuthsortP_exsitu(SEXP);
extern SEXP r_KnuthsortP_insitu(SEXP);
extern SEXP r_MKnuthsortP_exsitu(SEXP);
extern SEXP r_MKnuthsortP_insitu(SEXP);
extern SEXP r_NKnuthsortP_exsitu(SEXP);
extern SEXP r_NKnuthsortP_insitu(SEXP);
extern SEXP r_PKnuthsortP_exsitu(SEXP, SEXP);
extern SEXP r_PKnuthsortP_insitu(SEXP, SEXP);
extern SEXP r_PFrogsort0P_exsitu(SEXP, SEXP);
extern SEXP r_PFrogsort0P_insitu(SEXP, SEXP);
extern SEXP r_PFrogsort1P_exsitu(SEXP, SEXP);
extern SEXP r_PFrogsort1P_insitu(SEXP, SEXP);
extern SEXP r_PFrogsort2P_exsitu(SEXP, SEXP, SEXP);
extern SEXP r_PFrogsort2P_insitu(SEXP, SEXP, SEXP);
extern SEXP r_PFrogsort3P_exsitu(SEXP, SEXP, SEXP);
extern SEXP r_PFrogsort3P_insitu(SEXP, SEXP, SEXP);

extern SEXP r_Omitsort_exsitu(SEXP);
extern SEXP r_Omitsort_insitu(SEXP);
extern SEXP r_OmitsortP_exsitu(SEXP);
extern SEXP r_OmitsortP_insitu(SEXP);
extern SEXP r_OctosortP_exsitu(SEXP);
extern SEXP r_OctosortP_insitu(SEXP);

extern SEXP r_Kroco3sort_exsitu(SEXP);
extern SEXP r_Kroco3sort_insitu(SEXP);
extern SEXP r_Kroco4sort_exsitu(SEXP);
extern SEXP r_Kroco4sort_insitu(SEXP);
extern SEXP r_Ninisort_exsitu(SEXP);
extern SEXP r_Ninisort_insitu(SEXP);
extern SEXP r_Nocosort_exsitu(SEXP);
extern SEXP r_Nocosort_insitu(SEXP);
extern SEXP r_NocosortP_exsitu(SEXP);
extern SEXP r_NocosortP_insitu(SEXP);
extern SEXP r_BFPRTselect_exsitu(SEXP, SEXP);
extern SEXP r_BFPRTselect_insitu(SEXP, SEXP);
extern SEXP r_Pickselect_exsitu(SEXP, SEXP);
extern SEXP r_Pickselect_insitu(SEXP, SEXP);
extern SEXP r_BFPRTsort_insitu(SEXP);
extern SEXP r_Picksort_insitu(SEXP);
extern SEXP r_Plugsort_insitu(SEXP);
extern SEXP r_Pugsort_insitu(SEXP);
extern SEXP r_Quickselect2_exsitu(SEXP, SEXP);
extern SEXP r_Quickselect2_insitu(SEXP, SEXP);
extern SEXP r_Quickpartleft2_exsitu(SEXP, SEXP);
extern SEXP r_Quickpartleft2_insitu(SEXP, SEXP);
extern SEXP r_Quickpartright2_exsitu(SEXP, SEXP);
extern SEXP r_Quickpartright2_insitu(SEXP, SEXP);

extern SEXP r_Chunksort_noperf(SEXP, SEXP, SEXP);
extern SEXP r_Chunksort_exsitu(SEXP, SEXP, SEXP);
extern SEXP r_Chunksort_insitu(SEXP, SEXP, SEXP);
extern SEXP r_Quicksort1_exsitu(SEXP);
extern SEXP r_Quicksort1_insitu(SEXP);
extern SEXP r_Lomutosort_exsitu(SEXP);
extern SEXP r_Lomutosort_insitu(SEXP);
extern SEXP r_Quicksort2_exsitu(SEXP);
extern SEXP r_Quicksort2_insitu(SEXP);
extern SEXP r_Quicksort2B_exsitu(SEXP);
extern SEXP r_Quicksort2B_insitu(SEXP);
extern SEXP r_PQuicksort2_exsitu(SEXP,SEXP);
extern SEXP r_PQuicksort2_insitu(SEXP,SEXP);
extern SEXP r_PQuicksort2B_exsitu(SEXP,SEXP);
extern SEXP r_PQuicksort2B_insitu(SEXP,SEXP);
extern SEXP r_PDucksort_exsitu(SEXP,SEXP);
extern SEXP r_PDucksort_insitu(SEXP,SEXP);
extern SEXP r_PDucksortB_exsitu(SEXP,SEXP);
extern SEXP r_PDucksortB_insitu(SEXP,SEXP);
extern SEXP r_Quicksort2P_exsitu(SEXP);
extern SEXP r_Quicksort2P_insitu(SEXP);
extern SEXP r_Quicksort3_exsitu(SEXP);
extern SEXP r_Quicksort3_insitu(SEXP);
extern SEXP r_Quicksort3P_exsitu(SEXP);
extern SEXP r_Quicksort3P_insitu(SEXP);
extern SEXP r_RInsertionsort_exsitu(SEXP);
extern SEXP r_RInsertionsort_insitu(SEXP);
extern SEXP r_RQuicksort2_exsitu(SEXP);
extern SEXP r_RQuicksort2_insitu(SEXP, SEXP);
extern SEXP r_RQuicksort2B_exsitu(SEXP);
extern SEXP r_RQuicksort2B_insitu(SEXP, SEXP);

extern SEXP r_Dotnetpart_exsitu(SEXP, SEXP);
extern SEXP r_Dotnetpart_insitu(SEXP, SEXP);
extern SEXP r_Quickpart2_exsitu(SEXP, SEXP);
extern SEXP r_Quickpart2_insitu(SEXP, SEXP);
extern SEXP r_Zackpart_exsitu(SEXP, SEXP);
extern SEXP r_Zackpart_insitu(SEXP, SEXP);
extern SEXP r_Zuckpart_exsitu(SEXP, SEXP);
extern SEXP r_Zuckpart_insitu(SEXP, SEXP);
extern SEXP r_Duckpart_exsitu(SEXP, SEXP);
extern SEXP r_Duckpart_insitu(SEXP, SEXP);
extern SEXP r_ZuckpartB_exsitu(SEXP, SEXP);
extern SEXP r_ZuckpartB_insitu(SEXP, SEXP);
extern SEXP r_DuckpartB_exsitu(SEXP, SEXP);
extern SEXP r_DuckpartB_insitu(SEXP, SEXP);

extern SEXP r_Sicksort_insitu(SEXP);
extern SEXP r_Simplsort_exsitu(SEXP);
extern SEXP r_Simplsort_insitu(SEXP);
extern SEXP r_SimplsortP_exsitu(SEXP);
extern SEXP r_SimplsortP_insitu(SEXP);
extern SEXP r_SInsertionsort_exsitu(SEXP);
extern SEXP r_SInsertionsort_insitu(SEXP);
extern SEXP r_Slicksort_insitu(SEXP);
extern SEXP r_SqrtsortP_exsitu(SEXP);
extern SEXP r_SqrtsortP_insitu(SEXP);
extern SEXP r_SQuicksort2_exsitu(SEXP);
extern SEXP r_SQuicksort2_insitu(SEXP);
extern SEXP r_SQuicksort2B_exsitu(SEXP);
extern SEXP r_SQuicksort2B_insitu(SEXP);
extern SEXP r_Storksort_exsitu(SEXP);
extern SEXP r_Storksort_insitu(SEXP);
extern SEXP r_StorksortB_exsitu(SEXP);
extern SEXP r_Swansort_exsitu(SEXP);
extern SEXP r_Swansort_insitu(SEXP);
extern SEXP r_SwansortB_exsitu(SEXP);
extern SEXP r_SwansortB_insitu(SEXP);
extern SEXP r_TKnuthsort_exsitu(SEXP);
extern SEXP r_TKnuthsort_insitu(SEXP);
extern SEXP r_Tricksort_exsitu(SEXP);
extern SEXP r_Tricksort_insitu(SEXP);
extern SEXP r_TricksortP_exsitu(SEXP);
extern SEXP r_TricksortP_insitu(SEXP);
extern SEXP r_UInsertionsort_exsitu(SEXP);
extern SEXP r_UInsertionsort_insitu(SEXP);
extern SEXP r_UKnuthsortP_exsitu(SEXP);
extern SEXP r_UKnuthsortP_insitu(SEXP);
extern SEXP r_UZacksort_exsitu(SEXP);
extern SEXP r_UZacksort_insitu(SEXP);
extern SEXP r_UZacksortB_exsitu(SEXP);
extern SEXP r_UZacksortB_insitu(SEXP);
extern SEXP r_VFrogsort1_exsitu(SEXP);
extern SEXP r_VFrogsort1_insitu(SEXP);
extern SEXP r_PVFrogsort1_exsitu(SEXP,SEXP);
extern SEXP r_PVFrogsort1_insitu(SEXP,SEXP);
extern SEXP r_VFrogsort1A_exsitu(SEXP);
extern SEXP r_VFrogsort1A_insitu(SEXP);
extern SEXP r_VInsertionsort_exsitu(SEXP);
extern SEXP r_VInsertionsort_insitu(SEXP);
extern SEXP r_VKnuthsort_exsitu(SEXP);
extern SEXP r_VKnuthsort_insitu(SEXP);
extern SEXP r_PVKnuthsort_exsitu(SEXP,SEXP);
extern SEXP r_PVKnuthsort_insitu(SEXP,SEXP);
extern SEXP r_VKnuthsortA_exsitu(SEXP);
extern SEXP r_VKnuthsortA_insitu(SEXP);
extern SEXP r_UVWoverhead(SEXP);
extern SEXP r_Walksort_exsitu(SEXP, SEXP);
extern SEXP r_Walksort_insitu(SEXP, SEXP);
extern SEXP r_WInsertionsort_exsitu(SEXP);
extern SEXP r_WInsertionsort_insitu(SEXP);
extern SEXP r_WQuicksort2_exsitu(SEXP);
extern SEXP r_WQuicksort2_insitu(SEXP);
extern SEXP r_WQuicksort2B_exsitu(SEXP);
extern SEXP r_WQuicksort2B_insitu(SEXP);
extern SEXP r_Zackselect_exsitu(SEXP, SEXP);
extern SEXP r_Zackselect_insitu(SEXP, SEXP);
extern SEXP r_Zuckselect_exsitu(SEXP, SEXP);
extern SEXP r_Zuckselect_insitu(SEXP, SEXP);
extern SEXP r_ZuckselectB_exsitu(SEXP, SEXP);
extern SEXP r_ZuckselectB_insitu(SEXP, SEXP);
extern SEXP r_Duckselect_exsitu(SEXP, SEXP);
extern SEXP r_Duckselect_insitu(SEXP, SEXP);
extern SEXP r_DuckselectB_exsitu(SEXP, SEXP);
extern SEXP r_DuckselectB_insitu(SEXP, SEXP);
extern SEXP r_Zackpartleft_exsitu(SEXP, SEXP);
extern SEXP r_Zackpartleft_insitu(SEXP, SEXP);
extern SEXP r_Zackpartright_exsitu(SEXP, SEXP);
extern SEXP r_Zackpartright_insitu(SEXP, SEXP);
extern SEXP r_Zacksort_exsitu(SEXP);
extern SEXP r_Zacksort_insitu(SEXP);
extern SEXP r_Zucksort_exsitu(SEXP);
extern SEXP r_Zucksort_insitu(SEXP);
extern SEXP r_ZucksortD_exsitu(SEXP);
extern SEXP r_ZucksortD_insitu(SEXP);
extern SEXP r_ZucksortA_exsitu(SEXP);
extern SEXP r_ZucksortA_insitu(SEXP);
extern SEXP r_ZacksortB_exsitu(SEXP);
extern SEXP r_ZacksortB_insitu(SEXP);
extern SEXP r_ZucksortB_exsitu(SEXP);
extern SEXP r_ZucksortB_insitu(SEXP);
extern SEXP r_Ducksort_insitu(SEXP);
extern SEXP r_Ducksort_exsitu(SEXP);
extern SEXP r_DucksortB_insitu(SEXP);
extern SEXP r_DucksortB_exsitu(SEXP);
extern SEXP r_Zickselect_exsitu(SEXP, SEXP);
extern SEXP r_Zickselect_insitu(SEXP, SEXP);
extern SEXP r_Zicksort_insitu(SEXP);
extern SEXP r_Zicksort_exsitu(SEXP);
extern SEXP r_Zocksort_exsitu(SEXP);
extern SEXP r_Zocksort_insitu(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"r_cran_compatible",               (DL_FUNC) &r_cran_compatible,               0},
    {"r_insertionsort_limit",           (DL_FUNC) &r_insertionsort_limit,           0},
    {"r_string_check",                  (DL_FUNC) &r_string_check,                  1},
    {"r_stable_keys",                  (DL_FUNC) &r_stable_keys,                  1},
    {"r_current_keys",                  (DL_FUNC) &r_current_keys,                  1},
    {"r_current_compare",                (DL_FUNC) &r_current_compare,                2},
    {"r_current_issorted",                (DL_FUNC) &r_current_issorted,                1},
    {"r_perfpaths",                     (DL_FUNC) &r_perfpaths,                     0},
    {"r_perfnow",                     (DL_FUNC) &r_perfnow,                         0},
    {"r_perfmax",                     (DL_FUNC) &r_perfmax,                         0},
    {"r_perfsleep",                     (DL_FUNC) &r_perfsleep,                     1},
    {"r_symsearch",                     (DL_FUNC) &r_symsearch,                     4},
    {"r_Ntile2_asc_asc_left_to_right",  (DL_FUNC)  &r_Ntile2_asc_asc_left_to_right,  3},
    {"r_Ntile2_asc_revasc_left_to_right",  (DL_FUNC)  &r_Ntile2_asc_revasc_left_to_right,  3},
    {"r_Ntile2_asc_asc_right_to_left",  (DL_FUNC)  &r_Ntile2_asc_asc_right_to_left,  3},
    {"r_Ntile2_asc_revasc_right_to_left",  (DL_FUNC)  &r_Ntile2_asc_revasc_right_to_left,  3},
    {"r_approxMedian_exsitu",           (DL_FUNC) &r_approxMedian_exsitu,           1},
    {"r_approxMedian_insitu",           (DL_FUNC) &r_approxMedian_insitu,           1},
    {"r_Bimesort_exsitu",               (DL_FUNC) &r_Bimesort_exsitu,               1},
    {"r_Bimesort_insitu",               (DL_FUNC) &r_Bimesort_insitu,               1},
    {"r_BimesortB_exsitu",              (DL_FUNC) &r_BimesortB_exsitu,              1},
    {"r_BimesortB_insitu",              (DL_FUNC) &r_BimesortB_insitu,              1},
    {"r_BimesortBP_exsitu",             (DL_FUNC) &r_BimesortBP_exsitu,             1},
    {"r_BimesortBP_insitu",             (DL_FUNC) &r_BimesortBP_insitu,             1},
    {"r_BimesortP_exsitu",              (DL_FUNC) &r_BimesortP_exsitu,              1},
    {"r_BimesortP_insitu",              (DL_FUNC) &r_BimesortP_insitu,              1},
    {"r_Chicksort_insitu",              (DL_FUNC) &r_Chicksort_insitu,              1},
    {"r_Chicksort_exsitu",              (DL_FUNC) &r_Chicksort_exsitu,              1},
    {"r_ChicksortP_insitu",             (DL_FUNC) &r_ChicksortP_insitu,             1},
    {"r_ChicksortP_exsitu",             (DL_FUNC) &r_ChicksortP_exsitu,             1},
    {"r_Copysort_exsitu",               (DL_FUNC) &r_Copysort_exsitu,               1},
    {"r_Copysort_insitu",               (DL_FUNC) &r_Copysort_insitu,               1},
    {"r_CopysortP_exsitu",              (DL_FUNC) &r_CopysortP_exsitu,              1},
    {"r_CopysortP_insitu",              (DL_FUNC) &r_CopysortP_insitu,              1},
    {"r_Croco3sort_exsitu",             (DL_FUNC) &r_Croco3sort_exsitu,             1},
    {"r_Croco3sort_insitu",             (DL_FUNC) &r_Croco3sort_insitu,             1},
    {"r_Croco4sort_exsitu",             (DL_FUNC) &r_Croco4sort_exsitu,             1},
    {"r_Croco4sort_insitu",             (DL_FUNC) &r_Croco4sort_insitu,             1},
    {"r_Crocosort_exsitu",              (DL_FUNC) &r_Crocosort_exsitu,              1},
    {"r_Crocosort_insitu",              (DL_FUNC) &r_Crocosort_insitu,              1},
    {"r_DietACPsort_exsitu",          (DL_FUNC) &r_DietACPsort_exsitu,          1},
    {"r_DietACPsort_insitu",          (DL_FUNC) &r_DietACPsort_insitu,          1},
    {"r_DietACPTsort_exsitu",         (DL_FUNC) &r_DietACPTsort_exsitu,         1},
    {"r_DietACPTsort_insitu",         (DL_FUNC) &r_DietACPTsort_insitu,         1},
    {"r_DietCPsort_exsitu",           (DL_FUNC) &r_DietCPsort_exsitu,           1},
    {"r_DietCPsort_insitu",           (DL_FUNC) &r_DietCPsort_insitu,           1},
    {"r_DietCPsortB_exsitu",          (DL_FUNC) &r_DietCPsortB_exsitu,          1},
    {"r_DietCPsortB_insitu",          (DL_FUNC) &r_DietCPsortB_insitu,          1},
    {"r_DietcP2sort_exsitu",          (DL_FUNC) &r_DietcP2sort_exsitu,          1},
    {"r_DietcP2sort_insitu",          (DL_FUNC) &r_DietcP2sort_insitu,          1},
    {"r_DietCPTsort_exsitu",          (DL_FUNC) &r_DietCPTsort_exsitu,          1},
    {"r_DietCPTsort_insitu",          (DL_FUNC) &r_DietCPTsort_insitu,          1},
    {"r_DietPcsort_exsitu",           (DL_FUNC) &r_DietPcsort_exsitu,           1},
    {"r_DietPcsort_insitu",           (DL_FUNC) &r_DietPcsort_insitu,           1},
    {"r_Dupisort_exsitu",               (DL_FUNC) &r_Dupisort_exsitu,               1},
    {"r_Dupisort_insitu",               (DL_FUNC) &r_Dupisort_insitu,               1},
    {"r_DupisortP_exsitu",              (DL_FUNC) &r_DupisortP_exsitu,              1},
    {"r_DupisortP_insitu",              (DL_FUNC) &r_DupisortP_insitu,              1},
    {"r_Frogsort0_exsitu",              (DL_FUNC) &r_Frogsort0_exsitu,              1},
    {"r_Frogsort0_insitu",              (DL_FUNC) &r_Frogsort0_insitu,              1},
    {"r_Frogsort0P_exsitu",             (DL_FUNC) &r_Frogsort0P_exsitu,             1},
    {"r_Frogsort0P_insitu",             (DL_FUNC) &r_Frogsort0P_insitu,             1},
    {"r_Frogsort1_exsitu",              (DL_FUNC) &r_Frogsort1_exsitu,              1},
    {"r_Frogsort1_insitu",              (DL_FUNC) &r_Frogsort1_insitu,              1},
    {"r_Frogsort1P_exsitu",             (DL_FUNC) &r_Frogsort1P_exsitu,             1},
    {"r_Frogsort1P_insitu",             (DL_FUNC) &r_Frogsort1P_insitu,             1},
    {"r_Frogsort1A_exsitu",             (DL_FUNC) &r_Frogsort1A_exsitu,             1},
    {"r_Frogsort1A_insitu",             (DL_FUNC) &r_Frogsort1A_insitu,             1},
    {"r_Frogsort1AP_exsitu",            (DL_FUNC) &r_Frogsort1AP_exsitu,            1},
    {"r_Frogsort1AP_insitu",            (DL_FUNC) &r_Frogsort1AP_insitu,            1},
    {"r_Frogsort1B_exsitu",             (DL_FUNC) &r_Frogsort1B_exsitu,             1},
    {"r_Frogsort1B_insitu",             (DL_FUNC) &r_Frogsort1B_insitu,             1},
    {"r_Frogsort1BP_exsitu",            (DL_FUNC) &r_Frogsort1BP_exsitu,            1},
    {"r_Frogsort1BP_insitu",            (DL_FUNC) &r_Frogsort1BP_insitu,            1},
    {"r_Frogsort2_exsitu",              (DL_FUNC) &r_Frogsort2_exsitu,              2},
    {"r_Frogsort2_insitu",              (DL_FUNC) &r_Frogsort2_insitu,              2},
    {"r_Frogsort2P_exsitu",             (DL_FUNC) &r_Frogsort2P_exsitu,             2},
    {"r_Frogsort2P_insitu",             (DL_FUNC) &r_Frogsort2P_insitu,             2},
    {"r_MFrogsort2P_exsitu",             (DL_FUNC) &r_MFrogsort2P_exsitu,           2},
    {"r_MFrogsort2P_insitu",             (DL_FUNC) &r_MFrogsort2P_insitu,           2},
    {"r_NFrogsort2P_exsitu",             (DL_FUNC) &r_NFrogsort2P_exsitu,           2},
    {"r_NFrogsort2P_insitu",             (DL_FUNC) &r_NFrogsort2P_insitu,           2},
    {"r_Frogsort2A_exsitu",             (DL_FUNC) &r_Frogsort2A_exsitu,             2},
    {"r_Frogsort2A_insitu",             (DL_FUNC) &r_Frogsort2A_insitu,             2},
    {"r_Frogsort2AP_exsitu",            (DL_FUNC) &r_Frogsort2AP_exsitu,            2},
    {"r_Frogsort2AP_insitu",            (DL_FUNC) &r_Frogsort2AP_insitu,            2},
    {"r_Frogsort2B_exsitu",             (DL_FUNC) &r_Frogsort2B_exsitu,             2},
    {"r_Frogsort2B_insitu",             (DL_FUNC) &r_Frogsort2B_insitu,             2},
    {"r_Frogsort2BP_exsitu",            (DL_FUNC) &r_Frogsort2BP_exsitu,            2},
    {"r_Frogsort2BP_insitu",            (DL_FUNC) &r_Frogsort2BP_insitu,            2},
    {"r_Frogsort3_exsitu",              (DL_FUNC) &r_Frogsort3_exsitu,              2},
    {"r_Frogsort3_insitu",              (DL_FUNC) &r_Frogsort3_insitu,              2},
    {"r_Frogsort3P_exsitu",             (DL_FUNC) &r_Frogsort3P_exsitu,             2},
    {"r_Frogsort3P_insitu",             (DL_FUNC) &r_Frogsort3P_insitu,             2},
    {"r_Frogsort3B_exsitu",             (DL_FUNC) &r_Frogsort3B_exsitu,             2},
    {"r_Frogsort3B_insitu",             (DL_FUNC) &r_Frogsort3B_insitu,             2},
    {"r_Frogsort3BP_exsitu",            (DL_FUNC) &r_Frogsort3BP_exsitu,            2},
    {"r_Frogsort3BP_insitu",            (DL_FUNC) &r_Frogsort3BP_insitu,            2},
    {"r_Frogsort4_exsitu",              (DL_FUNC) &r_Frogsort4_exsitu,              2},
    {"r_Frogsort4_insitu",              (DL_FUNC) &r_Frogsort4_insitu,              2},
    {"r_Frogsort6_exsitu",              (DL_FUNC) &r_Frogsort6_exsitu,              3},
    {"r_Frogsort6_insitu",              (DL_FUNC) &r_Frogsort6_insitu,              3},
    {"r_Frogsort6P_exsitu",             (DL_FUNC) &r_Frogsort6P_exsitu,             3},
    {"r_Frogsort6P_insitu",             (DL_FUNC) &r_Frogsort6P_insitu,             3},
    {"r_Geckosort0_exsitu",             (DL_FUNC) &r_Geckosort0_exsitu,             1},
    {"r_Geckosort0_insitu",             (DL_FUNC) &r_Geckosort0_insitu,             1},
    {"r_Geckosort1_exsitu",             (DL_FUNC) &r_Geckosort1_exsitu,             1},
    {"r_Geckosort1_insitu",             (DL_FUNC) &r_Geckosort1_insitu,             1},
    {"r_Geckosort1P_exsitu",            (DL_FUNC) &r_Geckosort1P_exsitu,            1},
    {"r_Geckosort1P_insitu",            (DL_FUNC) &r_Geckosort1P_insitu,            1},
    {"r_Squidsort1P_exsitu",            (DL_FUNC) &r_Squidsort1P_exsitu,            1},
    {"r_Squidsort1P_insitu",            (DL_FUNC) &r_Squidsort1P_insitu,            1},
    {"r_Squidsort2P_exsitu",            (DL_FUNC) &r_Squidsort2P_exsitu,            2},
    {"r_Squidsort2P_insitu",            (DL_FUNC) &r_Squidsort2P_insitu,            2},
    {"r_GKnuthsort_exsitu",             (DL_FUNC) &r_GKnuthsort_exsitu,             1},
    {"r_GKnuthsort_insitu",             (DL_FUNC) &r_GKnuthsort_insitu,             1},
    {"r_GrailsortP_exsitu",             (DL_FUNC) &r_GrailsortP_exsitu,             1},
    {"r_GrailsortP_insitu",             (DL_FUNC) &r_GrailsortP_insitu,             1},
    {"r_GrailsqrtP_exsitu",             (DL_FUNC) &r_GrailsqrtP_exsitu,             1},
    {"r_GrailsqrtP_insitu",             (DL_FUNC) &r_GrailsqrtP_insitu,             1},
    {"r_Ininsort_exsitu",               (DL_FUNC) &r_Ininsort_exsitu,               1},
    {"r_Ininsort_insitu",               (DL_FUNC) &r_Ininsort_insitu,               1},
    {"r_Selectionsort_exsitu",          (DL_FUNC) &r_Selectionsort_exsitu,          1},
    {"r_Selectionsort_insitu",          (DL_FUNC) &r_Selectionsort_insitu,          1},
    {"r_Selectionsort2_exsitu",         (DL_FUNC) &r_Selectionsort2_exsitu,          1},
    {"r_Selectionsort2_insitu",         (DL_FUNC) &r_Selectionsort2_insitu,          1},
    {"r_InplaceMergesortP_exsitu",      (DL_FUNC) &r_InplaceMergesortP_exsitu,      1},
    {"r_InplaceMergesortP_insitu",      (DL_FUNC) &r_InplaceMergesortP_insitu,      1},
    {"r_Insertionorder_l2r_exsitu",     (DL_FUNC) &r_Insertionorder_l2r_exsitu,     2},
    {"r_Insertionorder_l2r_insitu",     (DL_FUNC) &r_Insertionorder_l2r_insitu,     2},
    {"r_Insertionorder_r2l_exsitu",     (DL_FUNC) &r_Insertionorder_r2l_exsitu,     2},
    {"r_Insertionorder_r2l_insitu",     (DL_FUNC) &r_Insertionorder_r2l_insitu,     2},
    {"r_Insertionsort_desc_l2r_exsitu", (DL_FUNC) &r_Insertionsort_desc_l2r_exsitu, 1},
    {"r_Insertionsort_desc_l2r_insitu", (DL_FUNC) &r_Insertionsort_desc_l2r_insitu, 1},
    {"r_Insertionsort_desc_r2l_exsitu", (DL_FUNC) &r_Insertionsort_desc_r2l_exsitu, 1},
    {"r_Insertionsort_desc_r2l_insitu", (DL_FUNC) &r_Insertionsort_desc_r2l_insitu, 1},
    {"r_MInsertionsort_l2r_exsitu",      (DL_FUNC) &r_MInsertionsort_l2r_exsitu,    1},
    {"r_MInsertionsort_l2r_insitu",      (DL_FUNC) &r_MInsertionsort_l2r_insitu,    1},
    {"r_NInsertionsort_l2r_exsitu",      (DL_FUNC) &r_NInsertionsort_l2r_exsitu,    1},
    {"r_NInsertionsort_l2r_insitu",      (DL_FUNC) &r_NInsertionsort_l2r_insitu,    1},
    {"r_Insertionsort_l2r_exsitu",      (DL_FUNC) &r_Insertionsort_l2r_exsitu,      1},
    {"r_Insertionsort_l2r_insitu",      (DL_FUNC) &r_Insertionsort_l2r_insitu,      1},
    {"r_Insertionsort_r2l_exsitu",      (DL_FUNC) &r_Insertionsort_r2l_exsitu,      1},
    {"r_Insertionsort_r2l_insitu",      (DL_FUNC) &r_Insertionsort_r2l_insitu,      1},
    {"r_Jumpsort_exsitu",               (DL_FUNC) &r_Jumpsort_exsitu,               2},
    {"r_Jumpsort_insitu",               (DL_FUNC) &r_Jumpsort_insitu,               2},
    {"r_Kata3sort_exsitu",              (DL_FUNC) &r_Kata3sort_exsitu,              1},
    {"r_Kata3sort_insitu",              (DL_FUNC) &r_Kata3sort_insitu,              1},
    {"r_Kata4sort_exsitu",              (DL_FUNC) &r_Kata4sort_exsitu,              1},
    {"r_Kata4sort_insitu",              (DL_FUNC) &r_Kata4sort_insitu,              1},
    {"r_Katasort_exsitu",               (DL_FUNC) &r_Katasort_exsitu,               1},
    {"r_Katasort_insitu",               (DL_FUNC) &r_Katasort_insitu,               1},
    {"r_KatasortA_exsitu",              (DL_FUNC) &r_KatasortA_exsitu,              1},
    {"r_KatasortA_insitu",              (DL_FUNC) &r_KatasortA_insitu,              1},
    {"r_KatasortAP_exsitu",             (DL_FUNC) &r_KatasortAP_exsitu,             1},
    {"r_KatasortAP_insitu",             (DL_FUNC) &r_KatasortAP_insitu,             1},
    {"r_KatasortB_exsitu",              (DL_FUNC) &r_KatasortB_exsitu,              1},
    {"r_KatasortB_insitu",              (DL_FUNC) &r_KatasortB_insitu,              1},
    {"r_KatasortBP_exsitu",             (DL_FUNC) &r_KatasortBP_exsitu,             1},
    {"r_KatasortBP_insitu",             (DL_FUNC) &r_KatasortBP_insitu,             1},
    {"r_KatasortP_exsitu",              (DL_FUNC) &r_KatasortP_exsitu,              1},
    {"r_KatasortP_insitu",              (DL_FUNC) &r_KatasortP_insitu,              1},
    {"r_Kiwisort_exsitu",               (DL_FUNC) &r_Kiwisort_exsitu,               1},
    {"r_Kiwisort_insitu",               (DL_FUNC) &r_Kiwisort_insitu,               1},
    {"r_KiwisortA_exsitu",               (DL_FUNC) &r_KiwisortA_exsitu,             1},
    {"r_KiwisortA_insitu",               (DL_FUNC) &r_KiwisortA_insitu,             1},
    {"r_KiwisortB_exsitu",              (DL_FUNC) &r_KiwisortB_exsitu,              1},
    {"r_KiwisortB_insitu",              (DL_FUNC) &r_KiwisortB_insitu,              1},
    {"r_Knuth3sort_exsitu",             (DL_FUNC) &r_Knuth3sort_exsitu,             1},
    {"r_Knuth3sort_insitu",             (DL_FUNC) &r_Knuth3sort_insitu,             1},
    {"r_Knuth4sort_exsitu",             (DL_FUNC) &r_Knuth4sort_exsitu,             1},
    {"r_Knuth4sort_insitu",             (DL_FUNC) &r_Knuth4sort_insitu,             1},
    {"r_Knuthsort_exsitu",              (DL_FUNC) &r_Knuthsort_exsitu,              1},
    {"r_Knuthsort_insitu",              (DL_FUNC) &r_Knuthsort_insitu,              1},
    {"r_KnuthsortA_exsitu",             (DL_FUNC) &r_KnuthsortA_exsitu,             1},
    {"r_KnuthsortA_insitu",             (DL_FUNC) &r_KnuthsortA_insitu,             1},
    {"r_KnuthsortAP_exsitu",            (DL_FUNC) &r_KnuthsortAP_exsitu,            1},
    {"r_KnuthsortAP_insitu",            (DL_FUNC) &r_KnuthsortAP_insitu,            1},
    {"r_KnuthsortP_exsitu",             (DL_FUNC) &r_KnuthsortP_exsitu,             1},
    {"r_KnuthsortP_insitu",             (DL_FUNC) &r_KnuthsortP_insitu,             1},
    {"r_MKnuthsortP_exsitu",             (DL_FUNC) &r_MKnuthsortP_exsitu,             1},
    {"r_MKnuthsortP_insitu",             (DL_FUNC) &r_MKnuthsortP_insitu,             1},
    {"r_NKnuthsortP_exsitu",             (DL_FUNC) &r_NKnuthsortP_exsitu,             1},
    {"r_NKnuthsortP_insitu",             (DL_FUNC) &r_NKnuthsortP_insitu,             1},
    {"r_PKnuthsortP_exsitu",             (DL_FUNC) &r_PKnuthsortP_exsitu,             2},
    {"r_PKnuthsortP_insitu",             (DL_FUNC) &r_PKnuthsortP_insitu,             2},
    {"r_PFrogsort0P_exsitu",             (DL_FUNC) &r_PFrogsort0P_exsitu,             2},
    {"r_PFrogsort0P_insitu",             (DL_FUNC) &r_PFrogsort0P_insitu,             2},
    {"r_PFrogsort1P_exsitu",             (DL_FUNC) &r_PFrogsort1P_exsitu,             2},
    {"r_PFrogsort1P_insitu",             (DL_FUNC) &r_PFrogsort1P_insitu,             2},
    {"r_PFrogsort2P_exsitu",             (DL_FUNC) &r_PFrogsort2P_exsitu,             3},
    {"r_PFrogsort2P_insitu",             (DL_FUNC) &r_PFrogsort2P_insitu,             3},
    {"r_PFrogsort3P_exsitu",             (DL_FUNC) &r_PFrogsort3P_exsitu,             3},
    {"r_PFrogsort3P_insitu",             (DL_FUNC) &r_PFrogsort3P_insitu,             3},
    {"r_Omitsort_exsitu",               (DL_FUNC) &r_Omitsort_exsitu,               1},
    {"r_Omitsort_insitu",               (DL_FUNC) &r_Omitsort_insitu,               1},
    {"r_OmitsortP_exsitu",              (DL_FUNC) &r_OmitsortP_exsitu,              1},
    {"r_OmitsortP_insitu",              (DL_FUNC) &r_OmitsortP_insitu,              1},
    {"r_OctosortP_exsitu",              (DL_FUNC) &r_OctosortP_exsitu,              1},
    {"r_OctosortP_insitu",              (DL_FUNC) &r_OctosortP_insitu,              1},
    {"r_Kroco3sort_exsitu",             (DL_FUNC) &r_Kroco3sort_exsitu,             1},
    {"r_Kroco3sort_insitu",             (DL_FUNC) &r_Kroco3sort_insitu,             1},
    {"r_Kroco4sort_exsitu",             (DL_FUNC) &r_Kroco4sort_exsitu,             1},
    {"r_Kroco4sort_insitu",             (DL_FUNC) &r_Kroco4sort_insitu,             1},
    {"r_Ninisort_exsitu",               (DL_FUNC) &r_Ninisort_exsitu,               1},
    {"r_Ninisort_insitu",               (DL_FUNC) &r_Ninisort_insitu,               1},
    {"r_Nocosort_exsitu",               (DL_FUNC) &r_Nocosort_exsitu,               1},
    {"r_Nocosort_insitu",               (DL_FUNC) &r_Nocosort_insitu,               1},
    {"r_NocosortP_exsitu",              (DL_FUNC) &r_NocosortP_exsitu,              1},
    {"r_NocosortP_insitu",              (DL_FUNC) &r_NocosortP_insitu,              1},
    {"r_BFPRTselect_exsitu",             (DL_FUNC) &r_BFPRTselect_exsitu,           2},
    {"r_BFPRTselect_insitu",             (DL_FUNC) &r_BFPRTselect_insitu,           2},
    {"r_BFPRTsort_insitu",               (DL_FUNC) &r_BFPRTsort_insitu,             1},
    {"r_Pickselect_exsitu",             (DL_FUNC) &r_Pickselect_exsitu,             2},
    {"r_Pickselect_insitu",             (DL_FUNC) &r_Pickselect_insitu,             2},
    {"r_Picksort_insitu",               (DL_FUNC) &r_Picksort_insitu,               1},
    {"r_Plugsort_insitu",               (DL_FUNC) &r_Plugsort_insitu,               1},
    {"r_Pugsort_insitu",                (DL_FUNC) &r_Pugsort_insitu,                1},
    {"r_Quickselect2_exsitu",           (DL_FUNC) &r_Quickselect2_exsitu,           2},
    {"r_Quickselect2_insitu",           (DL_FUNC) &r_Quickselect2_insitu,           2},
    {"r_Chunksort_noperf",              (DL_FUNC) &r_Chunksort_noperf,              3},
    {"r_Chunksort_exsitu",              (DL_FUNC) &r_Chunksort_exsitu,              3},
    {"r_Chunksort_insitu",              (DL_FUNC) &r_Chunksort_insitu,              3},
    {"r_Quicksort1_exsitu",              (DL_FUNC) &r_Quicksort1_exsitu,              1},
    {"r_Quicksort1_insitu",              (DL_FUNC) &r_Quicksort1_insitu,              1},
    {"r_Lomutosort_exsitu",             (DL_FUNC) &r_Lomutosort_exsitu,             1},
    {"r_Lomutosort_insitu",             (DL_FUNC) &r_Lomutosort_insitu,             1},
    {"r_Quicksort2_exsitu",             (DL_FUNC) &r_Quicksort2_exsitu,             1},
    {"r_Quicksort2_insitu",             (DL_FUNC) &r_Quicksort2_insitu,             1},
    {"r_Quicksort2B_exsitu",            (DL_FUNC) &r_Quicksort2B_exsitu,            1},
    {"r_Quicksort2B_insitu",            (DL_FUNC) &r_Quicksort2B_insitu,            1},
    {"r_PQuicksort2_exsitu",            (DL_FUNC) &r_PQuicksort2_exsitu,            2},
    {"r_PQuicksort2_insitu",            (DL_FUNC) &r_PQuicksort2_insitu,            2},
    {"r_PQuicksort2B_exsitu",           (DL_FUNC) &r_PQuicksort2B_exsitu,           2},
    {"r_PQuicksort2B_insitu",           (DL_FUNC) &r_PQuicksort2B_insitu,           2},
    {"r_PDucksort_exsitu",              (DL_FUNC) &r_PDucksort_exsitu,              2},
    {"r_PDucksort_insitu",              (DL_FUNC) &r_PDucksort_insitu,              2},
    {"r_PDucksortB_exsitu",             (DL_FUNC) &r_PDucksortB_exsitu,             2},
    {"r_PDucksortB_insitu",             (DL_FUNC) &r_PDucksortB_insitu,             2},
    {"r_Quicksort2P_exsitu",            (DL_FUNC) &r_Quicksort2P_exsitu,            1},
    {"r_Quicksort2P_insitu",            (DL_FUNC) &r_Quicksort2P_insitu,            1},
    {"r_Quicksort3_exsitu",             (DL_FUNC) &r_Quicksort3_exsitu,             1},
    {"r_Quicksort3_insitu",             (DL_FUNC) &r_Quicksort3_insitu,             1},
    {"r_Quicksort3P_exsitu",            (DL_FUNC) &r_Quicksort3P_exsitu,            1},
    {"r_Quicksort3P_insitu",            (DL_FUNC) &r_Quicksort3P_insitu,            1},
    {"r_Dotnetpart_exsitu",             (DL_FUNC) &r_Dotnetpart_exsitu,             2},
    {"r_Dotnetpart_insitu",             (DL_FUNC) &r_Dotnetpart_insitu,             2},
    {"r_Quickpart2_exsitu",             (DL_FUNC) &r_Quickpart2_exsitu,             2},
    {"r_Quickpart2_insitu",             (DL_FUNC) &r_Quickpart2_insitu,             2},
    {"r_Zackpart_exsitu",               (DL_FUNC) &r_Zackpart_exsitu,               2},
    {"r_Zackpart_insitu",               (DL_FUNC) &r_Zackpart_insitu,               2},
    {"r_Zuckpart_exsitu",               (DL_FUNC) &r_Zuckpart_exsitu,               2},
    {"r_Zuckpart_insitu",               (DL_FUNC) &r_Zuckpart_insitu,               2},
    {"r_Duckpart_exsitu",               (DL_FUNC) &r_Duckpart_exsitu,               2},
    {"r_Duckpart_insitu",               (DL_FUNC) &r_Duckpart_insitu,               2},
    {"r_ZuckpartB_exsitu",               (DL_FUNC) &r_ZuckpartB_exsitu,               2},
    {"r_ZuckpartB_insitu",               (DL_FUNC) &r_ZuckpartB_insitu,               2},
    {"r_DuckpartB_exsitu",               (DL_FUNC) &r_DuckpartB_exsitu,               2},
    {"r_DuckpartB_insitu",               (DL_FUNC) &r_DuckpartB_insitu,               2},
    {"r_RInsertionsort_exsitu",         (DL_FUNC) &r_RInsertionsort_exsitu,         1},
    {"r_RInsertionsort_insitu",         (DL_FUNC) &r_RInsertionsort_insitu,         1},
    {"r_RQuicksort2_exsitu",            (DL_FUNC) &r_RQuicksort2_exsitu,            1},
    {"r_RQuicksort2_insitu",            (DL_FUNC) &r_RQuicksort2_insitu,            2},
    {"r_RQuicksort2B_exsitu",           (DL_FUNC) &r_RQuicksort2B_exsitu,           1},
    {"r_RQuicksort2B_insitu",           (DL_FUNC) &r_RQuicksort2B_insitu,           2},
    {"r_Sicksort_insitu",               (DL_FUNC) &r_Sicksort_insitu,               1},
    {"r_Simplsort_exsitu",              (DL_FUNC) &r_Simplsort_exsitu,              1},
    {"r_Simplsort_insitu",              (DL_FUNC) &r_Simplsort_insitu,              1},
    {"r_SimplsortP_exsitu",             (DL_FUNC) &r_SimplsortP_exsitu,             1},
    {"r_SimplsortP_insitu",             (DL_FUNC) &r_SimplsortP_insitu,             1},
    {"r_SInsertionsort_exsitu",         (DL_FUNC) &r_SInsertionsort_exsitu,         1},
    {"r_SInsertionsort_insitu",         (DL_FUNC) &r_SInsertionsort_insitu,         1},
    {"r_Slicksort_insitu",              (DL_FUNC) &r_Slicksort_insitu,              1},
    {"r_SqrtsortP_exsitu",              (DL_FUNC) &r_SqrtsortP_exsitu,              1},
    {"r_SqrtsortP_insitu",              (DL_FUNC) &r_SqrtsortP_insitu,              1},
    {"r_SQuicksort2_exsitu",            (DL_FUNC) &r_SQuicksort2_exsitu,            1},
    {"r_SQuicksort2_insitu",            (DL_FUNC) &r_SQuicksort2_insitu,            1},
    {"r_SQuicksort2B_exsitu",           (DL_FUNC) &r_SQuicksort2B_exsitu,           1},
    {"r_SQuicksort2B_insitu",           (DL_FUNC) &r_SQuicksort2B_insitu,           1},
    {"r_Storksort_exsitu",              (DL_FUNC) &r_Storksort_exsitu,              1},
    {"r_Storksort_insitu",              (DL_FUNC) &r_Storksort_insitu,              1},
    {"r_StorksortB_exsitu",             (DL_FUNC) &r_StorksortB_exsitu,             1},
    {"r_Swansort_exsitu",               (DL_FUNC) &r_Swansort_exsitu,               1},
    {"r_Swansort_insitu",               (DL_FUNC) &r_Swansort_insitu,               1},
    {"r_SwansortB_exsitu",              (DL_FUNC) &r_SwansortB_exsitu,              1},
    {"r_SwansortB_insitu",              (DL_FUNC) &r_SwansortB_insitu,              1},
    {"r_TKnuthsort_exsitu",             (DL_FUNC) &r_TKnuthsort_exsitu,             1},
    {"r_TKnuthsort_insitu",             (DL_FUNC) &r_TKnuthsort_insitu,             1},
    {"r_Tricksort_exsitu",              (DL_FUNC) &r_Tricksort_exsitu,              1},
    {"r_Tricksort_insitu",              (DL_FUNC) &r_Tricksort_insitu,              1},
    {"r_TricksortP_exsitu",             (DL_FUNC) &r_TricksortP_exsitu,             1},
    {"r_TricksortP_insitu",             (DL_FUNC) &r_TricksortP_insitu,             1},
    {"r_UInsertionsort_exsitu",         (DL_FUNC) &r_UInsertionsort_exsitu,         1},
    {"r_UInsertionsort_insitu",         (DL_FUNC) &r_UInsertionsort_insitu,         1},
    {"r_UKnuthsortP_exsitu",              (DL_FUNC) &r_UKnuthsortP_exsitu,              1},
    {"r_UKnuthsortP_insitu",              (DL_FUNC) &r_UKnuthsortP_insitu,              1},
    {"r_UZacksort_exsitu",              (DL_FUNC) &r_UZacksort_exsitu,              1},
    {"r_UZacksort_insitu",              (DL_FUNC) &r_UZacksort_insitu,              1},
    {"r_UZacksortB_exsitu",             (DL_FUNC) &r_UZacksortB_exsitu,             1},
    {"r_UZacksortB_insitu",             (DL_FUNC) &r_UZacksortB_insitu,             1},
    {"r_VFrogsort1_exsitu",             (DL_FUNC) &r_VFrogsort1_exsitu,             1},
    {"r_VFrogsort1_insitu",             (DL_FUNC) &r_VFrogsort1_insitu,             1},
    {"r_PVFrogsort1_exsitu",             (DL_FUNC) &r_PVFrogsort1_exsitu,             2},
    {"r_PVFrogsort1_insitu",             (DL_FUNC) &r_PVFrogsort1_insitu,             2},
    {"r_VFrogsort1A_exsitu",            (DL_FUNC) &r_VFrogsort1A_exsitu,            1},
    {"r_VFrogsort1A_insitu",            (DL_FUNC) &r_VFrogsort1A_insitu,            1},
    {"r_VInsertionsort_exsitu",         (DL_FUNC) &r_VInsertionsort_exsitu,         1},
    {"r_VInsertionsort_insitu",         (DL_FUNC) &r_VInsertionsort_insitu,         1},
    {"r_VKnuthsort_exsitu",             (DL_FUNC) &r_VKnuthsort_exsitu,             1},
    {"r_VKnuthsort_insitu",             (DL_FUNC) &r_VKnuthsort_insitu,             1},
    {"r_PVKnuthsort_exsitu",             (DL_FUNC) &r_PVKnuthsort_exsitu,             2},
    {"r_PVKnuthsort_insitu",             (DL_FUNC) &r_PVKnuthsort_insitu,             2},
    {"r_VKnuthsortA_exsitu",            (DL_FUNC) &r_VKnuthsortA_exsitu,            1},
    {"r_VKnuthsortA_insitu",            (DL_FUNC) &r_VKnuthsortA_insitu,            1},
    {"r_UVWoverhead",                   (DL_FUNC) &r_UVWoverhead,                   1},
    {"r_Walksort_exsitu",               (DL_FUNC) &r_Walksort_exsitu,               2},
    {"r_Walksort_insitu",               (DL_FUNC) &r_Walksort_insitu,               2},
    {"r_WInsertionsort_exsitu",         (DL_FUNC) &r_WInsertionsort_exsitu,         1},
    {"r_WInsertionsort_insitu",         (DL_FUNC) &r_WInsertionsort_insitu,         1},
    {"r_WQuicksort2_exsitu",            (DL_FUNC) &r_WQuicksort2_exsitu,            1},
    {"r_WQuicksort2_insitu",            (DL_FUNC) &r_WQuicksort2_insitu,            1},
    {"r_WQuicksort2B_exsitu",           (DL_FUNC) &r_WQuicksort2B_exsitu,           1},
    {"r_WQuicksort2B_insitu",           (DL_FUNC) &r_WQuicksort2B_insitu,           1},
    {"r_Zackpartleft_exsitu",           (DL_FUNC) &r_Zackpartleft_exsitu,           2},
    {"r_Zackpartleft_insitu",           (DL_FUNC) &r_Zackpartleft_insitu,           2},
    {"r_Zackpartright_exsitu",          (DL_FUNC) &r_Zackpartright_exsitu,           2},
    {"r_Zackpartright_insitu",          (DL_FUNC) &r_Zackpartright_insitu,           2},
    {"r_Quickpartleft2_exsitu",           (DL_FUNC) &r_Quickpartleft2_exsitu,           2},
    {"r_Quickpartleft2_insitu",           (DL_FUNC) &r_Quickpartleft2_insitu,           2},
    {"r_Quickpartright2_exsitu",          (DL_FUNC) &r_Quickpartright2_exsitu,           2},
    {"r_Quickpartright2_insitu",          (DL_FUNC) &r_Quickpartright2_insitu,           2},
    {"r_Zackselect_exsitu",             (DL_FUNC) &r_Zackselect_exsitu,             2},
    {"r_Zackselect_insitu",             (DL_FUNC) &r_Zackselect_insitu,             2},
    {"r_Zuckselect_exsitu",             (DL_FUNC) &r_Zuckselect_exsitu,             2},
    {"r_Zuckselect_insitu",             (DL_FUNC) &r_Zuckselect_insitu,             2},
    {"r_ZuckselectB_exsitu",            (DL_FUNC) &r_ZuckselectB_exsitu,            2},
    {"r_ZuckselectB_insitu",            (DL_FUNC) &r_ZuckselectB_insitu,            2},
    {"r_Duckselect_exsitu",             (DL_FUNC) &r_Duckselect_exsitu,             2},
    {"r_Duckselect_insitu",             (DL_FUNC) &r_Duckselect_insitu,             2},
    {"r_DuckselectB_exsitu",            (DL_FUNC) &r_DuckselectB_exsitu,            2},
    {"r_DuckselectB_insitu",            (DL_FUNC) &r_DuckselectB_insitu,            2},
    {"r_Zacksort_exsitu",               (DL_FUNC) &r_Zacksort_exsitu,               1},
    {"r_Zacksort_insitu",               (DL_FUNC) &r_Zacksort_insitu,               1},
    {"r_Zucksort_exsitu",               (DL_FUNC) &r_Zucksort_exsitu,               1},
    {"r_Zucksort_insitu",               (DL_FUNC) &r_Zucksort_insitu,               1},
    {"r_ZucksortD_exsitu",               (DL_FUNC) &r_ZucksortD_exsitu,             1},
    {"r_ZucksortD_insitu",               (DL_FUNC) &r_ZucksortD_insitu,             1},
    {"r_ZucksortA_exsitu",              (DL_FUNC) &r_ZucksortA_exsitu,              1},
    {"r_ZucksortA_insitu",              (DL_FUNC) &r_ZucksortA_insitu,              1},
    {"r_ZacksortB_exsitu",              (DL_FUNC) &r_ZacksortB_exsitu,              1},
    {"r_ZacksortB_insitu",              (DL_FUNC) &r_ZacksortB_insitu,              1},
    {"r_ZucksortB_exsitu",              (DL_FUNC) &r_ZucksortB_exsitu,              1},
    {"r_ZucksortB_insitu",              (DL_FUNC) &r_ZucksortB_insitu,              1},
    {"r_Ducksort_insitu",               (DL_FUNC) &r_Ducksort_insitu,               1},
    {"r_Ducksort_exsitu",               (DL_FUNC) &r_Ducksort_exsitu,               1},
    {"r_DucksortB_insitu",              (DL_FUNC) &r_DucksortB_insitu,              1},
    {"r_DucksortB_exsitu",              (DL_FUNC) &r_DucksortB_exsitu,              1},
    {"r_Zickselect_exsitu",             (DL_FUNC) &r_Zickselect_exsitu,             2},
    {"r_Zickselect_insitu",             (DL_FUNC) &r_Zickselect_insitu,             2},
    {"r_Zicksort_exsitu",               (DL_FUNC) &r_Zicksort_exsitu,               1},
    {"r_Zicksort_insitu",               (DL_FUNC) &r_Zicksort_insitu,               1},
    {"r_Zocksort_exsitu",               (DL_FUNC) &r_Zocksort_exsitu,               1},
    {"r_Zocksort_insitu",               (DL_FUNC) &r_Zocksort_insitu,               1},
    {NULL, NULL, 0}
};

void R_init_greeNsort(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
