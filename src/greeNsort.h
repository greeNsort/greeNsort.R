/*
# greeNsort package C-code declarations
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_greeNsort_h
#define ALREADY_DEFINED_greeNsort_h

// for data types
#include "config.h"

void Selectionsort_insitu(ValueT *x, IndexT n);
void Selectionsort_exsitu(ValueT *x, IndexT n);
void Selectionsort2_insitu(ValueT *x, IndexT n, PerfT *p);
void Selectionsort2_exsitu(ValueT *x, IndexT n, PerfT *p);

void MInsertionsort_l2r_insitu(IntValueT *x, IndexT n, IndexT m);
void MInsertionsort_l2r_exsitu(IntValueT *x, IndexT n, IndexT m);
void NInsertionsort_l2r_insitu(IntValueT *x, IndexT n, IndexT m);
void NInsertionsort_l2r_exsitu(IntValueT *x, IndexT n, IndexT m);

void Insertionsort_l2r_insitu(ValueT *x, IndexT n);
void Insertionsort_r2l_insitu(ValueT *x, IndexT n);
void Insertionsort_desc_l2r_insitu(ValueT *x, IndexT n);
void Insertionsort_desc_r2l_insitu(ValueT *x, IndexT n);
void Insertionorder_l2r_insitu(ValueT *x, int *o, int n);
void Insertionorder_r2l_insitu(ValueT *x, int *o, int n);

void Insertionsort_l2r_exsitu(ValueT *x, IndexT n);
void Insertionsort_r2l_exsitu(ValueT *x, IndexT n);
void Insertionsort_desc_l2r_exsitu(ValueT *x, IndexT n);
void Insertionsort_desc_r2l_exsitu(ValueT *x, IndexT n);
void Insertionorder_l2r_exsitu(ValueT *x, int *o, int n);
void Insertionorder_r2l_exsitu(ValueT *x, int *o, int n);

void Copysort_insitu(ValueT *x, IndexT n);
void Copysort_exsitu(ValueT *x, IndexT n);
void CopysortP_insitu(ValueT *x, IndexT n);
void CopysortP_exsitu(ValueT *x, IndexT n);

void Nocosort_insitu(ValueT *x, IndexT n);
void Nocosort_exsitu(ValueT *x, IndexT n);
void NocosortP_insitu(ValueT *x, IndexT n);
void NocosortP_exsitu(ValueT *x, IndexT n);

void Simplsort_insitu(ValueT *x, IndexT n);
void Simplsort_exsitu(ValueT *x, IndexT n);
void SimplsortP_insitu(ValueT *x, IndexT n);
void SimplsortP_exsitu(ValueT *x, IndexT n);

void Bimesort_insitu(ValueT *x, IndexT n);
void Bimesort_exsitu(ValueT *x, IndexT n);
void BimesortP_insitu(ValueT *x, IndexT n);
void BimesortP_exsitu(ValueT *x, IndexT n);

void MKnuthsortP_insitu(IntValueT *x, IndexT n, IndexT m);
void MKnuthsortP_exsitu(IntValueT *x, IndexT n, IndexT m);
void NKnuthsortP_insitu(IntValueT *x, IndexT n, IndexT m, PerfT *p);
void NKnuthsortP_exsitu(IntValueT *x, IndexT n, IndexT m, PerfT *p);

void Knuthsort_insitu(ValueT *x, IndexT n);
void Knuthsort_exsitu(ValueT *x, IndexT n);
void KnuthsortP_insitu(ValueT *x, IndexT n);
void KnuthsortP_exsitu(ValueT *x, IndexT n);
void PKnuthsortP_insitu(ValueT *x, IndexT n, double t);
void PKnuthsortP_exsitu(ValueT *x, IndexT n, double t);
void PFrogsort0P_insitu(ValueT *x, IndexT n, double t);
void PFrogsort0P_exsitu(ValueT *x, IndexT n, double t);
void PFrogsort1P_insitu(ValueT *x, IndexT n, double t);
void PFrogsort1P_exsitu(ValueT *x, IndexT n, double t);
void PFrogsort2P_insitu(ValueT *x, IndexT n, double f, double t, PerfT *p);
void PFrogsort2P_exsitu(ValueT *x, IndexT n, double f, double t, PerfT *p);
void PFrogsort3P_insitu(ValueT *x, IndexT n, double f, double t, PerfT *p);
void PFrogsort3P_exsitu(ValueT *x, IndexT n, double f, double t, PerfT *p);

void Katasort_insitu(ValueT *x, IndexT n);
void Katasort_exsitu(ValueT *x, IndexT n);
void KatasortP_insitu(ValueT *x, IndexT n);
void KatasortP_exsitu(ValueT *x, IndexT n);

void KnuthsortA_insitu(ValueT *x, IndexT n);
void KnuthsortA_exsitu(ValueT *x, IndexT n);
void KnuthsortAP_insitu(ValueT *x, IndexT n);
void KnuthsortAP_exsitu(ValueT *x, IndexT n);
void KatasortA_insitu(ValueT *x, IndexT n);
void KatasortA_exsitu(ValueT *x, IndexT n);
void KatasortAP_insitu(ValueT *x, IndexT n);
void KatasortAP_exsitu(ValueT *x, IndexT n);
void KatasortB_insitu(ValueT *x, IndexT n);
void KatasortB_exsitu(ValueT *x, IndexT n);
void KatasortBP_insitu(ValueT *x, IndexT n);
void KatasortBP_exsitu(ValueT *x, IndexT n);
void BimesortB_insitu(ValueT *x, IndexT n);
void BimesortB_exsitu(ValueT *x, IndexT n);
void BimesortBP_insitu(ValueT *x, IndexT n);
void BimesortBP_exsitu(ValueT *x, IndexT n);


void Omitsort_insitu(ValueT *x, IndexT n);
void Omitsort_exsitu(ValueT *x, IndexT n);
void OmitsortP_insitu(ValueT *x, IndexT n);
void OmitsortP_exsitu(ValueT *x, IndexT n);
void OctosortP_insitu(ValueT *x, IndexT n);
void OctosortP_exsitu(ValueT *x, IndexT n);


void Ininsort_insitu(ValueT *x, IndexT n);
void Ininsort_exsitu(ValueT *x, IndexT n);
void Ninisort_insitu(ValueT *x, IndexT n);
void Ninisort_exsitu(ValueT *x, IndexT n);

void GKnuthsort_insitu(ValueT *x, IndexT n);
void GKnuthsort_exsitu(ValueT *x, IndexT n);

void Frogsort0_insitu(ValueT *x, IndexT n);
void Frogsort0_exsitu(ValueT *x, IndexT n);
void Frogsort0P_insitu(ValueT *x, IndexT n);
void Frogsort0P_exsitu(ValueT *x, IndexT n);
void Frogsort1_insitu(ValueT *x, IndexT n);
void Frogsort1_exsitu(ValueT *x, IndexT n);
void Frogsort1P_insitu(ValueT *x, IndexT n);
void Frogsort1P_exsitu(ValueT *x, IndexT n);
void Frogsort1A_insitu(ValueT *x, IndexT n);
void Frogsort1A_exsitu(ValueT *x, IndexT n);
void Frogsort1AP_insitu(ValueT *x, IndexT n);
void Frogsort1AP_exsitu(ValueT *x, IndexT n);
void Frogsort1B_insitu(ValueT *x, IndexT n);
void Frogsort1B_exsitu(ValueT *x, IndexT n);
void Frogsort1BP_insitu(ValueT *x, IndexT n);
void Frogsort1BP_exsitu(ValueT *x, IndexT n);
void Frogsort2_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2P_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2P_exsitu(ValueT *x, IndexT n, double f, PerfT *p);

void MFrogsort2P_insitu(IntValueT *x, IndexT n, IndexT m, double f, PerfT *p);
void MFrogsort2P_exsitu(IntValueT *x, IndexT n, IndexT m, double f, PerfT *p);
void NFrogsort2P_insitu(IntValueT *x, IndexT n, IndexT m, double f, PerfT *p);
void NFrogsort2P_exsitu(IntValueT *x, IndexT n, IndexT m, double f, PerfT *p);

void Frogsort2A_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2A_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2AP_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2AP_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2B_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2B_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2BP_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort2BP_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3P_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3P_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3B_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3B_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3BP_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort3BP_exsitu(ValueT *x, IndexT n, double f, PerfT *p);
void Frogsort6_insitu(ValueT *x, IndexT n, double f, double f2, PerfT *p);
void Frogsort6_exsitu(ValueT *x, IndexT n, double f, double f2, PerfT *p);
void Frogsort6P_insitu(ValueT *x, IndexT n, double f, double f2, PerfT *p);
void Frogsort6P_exsitu(ValueT *x, IndexT n, double f, double f2, PerfT *p);
void Frogsort4_insitu(ValueT *x, IndexT n, IndexT b, PerfT *p);
void Frogsort4_exsitu(ValueT *x, IndexT n, IndexT b, PerfT *p);

void Geckosort0_insitu(ValueT *x, IndexT n);
void Geckosort0_exsitu(ValueT *x, IndexT n);
void Geckosort1_insitu(ValueT *x, IndexT n);
void Geckosort1_exsitu(ValueT *x, IndexT n);
void Geckosort1P_insitu(ValueT *x, IndexT n);
void Geckosort1P_exsitu(ValueT *x, IndexT n);
void Squidsort1P_insitu(ValueT *x, IndexT n);
void Squidsort1P_exsitu(ValueT *x, IndexT n);
void Squidsort2P_insitu(ValueT *x, IndexT n, double f, PerfT *p);
void Squidsort2P_exsitu(ValueT *x, IndexT n, double f, PerfT *p);

void TKnuthsort_insitu(ValueT *x, IndexT n);
void TKnuthsort_exsitu(ValueT *x, IndexT n);

void Crocosort_insitu(ValueT *x, IndexT n);
void Crocosort_exsitu(ValueT *x, IndexT n);

void Knuth3sort_insitu(ValueT *x, IndexT n);
void Knuth3sort_exsitu(ValueT *x, IndexT n);
void Knuth4sort_insitu(ValueT *x, IndexT n);
void Knuth4sort_exsitu(ValueT *x, IndexT n);
void Kata3sort_insitu(ValueT *x, IndexT n);
void Kata3sort_exsitu(ValueT *x, IndexT n);
void Kata4sort_insitu(ValueT *x, IndexT n);
void Kata4sort_exsitu(ValueT *x, IndexT n);

void Croco3sort_insitu(ValueT *x, IndexT n);
void Croco3sort_exsitu(ValueT *x, IndexT n);
void Croco4sort_insitu(ValueT *x, IndexT n);
void Croco4sort_exsitu(ValueT *x, IndexT n);
void Kroco3sort_insitu(ValueT *x, IndexT n);
void Kroco3sort_exsitu(ValueT *x, IndexT n);
void Kroco4sort_insitu(ValueT *x, IndexT n);
void Kroco4sort_exsitu(ValueT *x, IndexT n);


void DietACPsort_insitu(ValueT *x, IndexT n);
void DietACPsort_exsitu(ValueT *x, IndexT n);
void DietACPTsort_insitu(ValueT *x, IndexT n);
void DietACPTsort_exsitu(ValueT *x, IndexT n);

void DietCPsort_insitu(ValueT *x, IndexT n);
void DietCPsort_exsitu(ValueT *x, IndexT n);
void DietCPsortB_insitu(ValueT *x, IndexT n);
void DietCPsortB_exsitu(ValueT *x, IndexT n);
void DietcP2sort_insitu(ValueT *x, IndexT n);
void DietcP2sort_exsitu(ValueT *x, IndexT n);
void DietPcsort_insitu(ValueT *x, IndexT n);
void DietPcsort_exsitu(ValueT *x, IndexT n);
void DietCPTsort_insitu(ValueT *x, IndexT n);
void DietCPTsort_exsitu(ValueT *x, IndexT n);

void Kiwisort_insitu(ValueT *x, IndexT n);
void Kiwisort_exsitu(ValueT *x, IndexT n);
void KiwisortA_insitu(ValueT *x, IndexT n);
void KiwisortA_exsitu(ValueT *x, IndexT n);
void KiwisortB_insitu(ValueT *x, IndexT n);
void KiwisortB_exsitu(ValueT *x, IndexT n);
void Swansort_insitu(ValueT *x, IndexT n);
void Swansort_exsitu(ValueT *x, IndexT n);
void SwansortB_insitu(ValueT *x, IndexT n);
void SwansortB_exsitu(ValueT *x, IndexT n);
void Storksort_insitu(ValueT *x, IndexT n);
void Storksort_exsitu(ValueT *x, IndexT n);
void StorksortB_exsitu(ValueT *x, IndexT n);

void Zicksort_insitu(ValueT *x, IndexT n);
void Zicksort_exsitu(ValueT *x, IndexT n);

void Zocksort_insitu(ValueT *x, IndexT n);
void Zocksort_exsitu(ValueT *x, IndexT n);
void Zacksort_insitu(ValueT *x, IndexT n);
void Zacksort_exsitu(ValueT *x, IndexT n);
void Zucksort_insitu(ValueT *x, IndexT n);
void Zucksort_exsitu(ValueT *x, IndexT n);
void ZucksortD_insitu(ValueT *x, IndexT n);
void ZucksortD_exsitu(ValueT *x, IndexT n);
void ZucksortA_insitu(ValueT *x, IndexT n);
void ZucksortA_exsitu(ValueT *x, IndexT n);
void ZacksortB_insitu(ValueT *x, IndexT n);
void ZacksortB_exsitu(ValueT *x, IndexT n);
void ZucksortB_insitu(ValueT *x, IndexT n);
void ZucksortB_exsitu(ValueT *x, IndexT n);
void Ducksort_insitu(ValueT *x, IndexT n);
void Ducksort_exsitu(ValueT *x, IndexT n);
void DucksortB_insitu(ValueT *x, IndexT n);
void DucksortB_exsitu(ValueT *x, IndexT n);
void Chicksort_insitu(ValueT *x, IndexT n);
void Chicksort_exsitu(ValueT *x, IndexT n);
void ChicksortP_insitu(ValueT *x, IndexT n);
void ChicksortP_exsitu(ValueT *x, IndexT n);

void Dupisort_insitu(ValueT *x, IndexT n);
void Dupisort_exsitu(ValueT *x, IndexT n);
void DupisortP_insitu(ValueT *x, IndexT n);
void DupisortP_exsitu(ValueT *x, IndexT n);

void Tricksort_insitu(ValueT *x, IndexT n);
void Tricksort_exsitu(ValueT *x, IndexT n);
void TricksortP_insitu(ValueT *x, IndexT n);
void TricksortP_exsitu(ValueT *x, IndexT n);

void BFPRTsort_insitu(ValueT *x, IndexT n);
void Plugsort_insitu(ValueT *x, IndexT n);
void Pugsort_insitu(ValueT *x, IndexT n);
void Picksort_insitu(ValueT *x, IndexT n);
void Slicksort_insitu(ValueT *x, IndexT n);
void Sicksort_insitu(ValueT *x, IndexT n);

void Chunksort_insitu(ValueT *x, IndexT n, IndexT b, int d);
void Chunksort_exsitu(ValueT *x, IndexT n, IndexT b, int d);
void Quicksort1_exsitu(ValueT *x, IndexT n);
void Quicksort1_insitu(ValueT *x, IndexT n);
void Lomutosort_exsitu(ValueT *x, IndexT n);
void Lomutosort_insitu(ValueT *x, IndexT n);
void Quicksort2_insitu(ValueT *x, IndexT n);
void Quicksort2_exsitu(ValueT *x, IndexT n);
void Quicksort2P_insitu(ValueT *x, IndexT n);
void Quicksort2P_exsitu(ValueT *x, IndexT n);
void Quicksort2B_insitu(ValueT *x, IndexT n);
void Quicksort2B_exsitu(ValueT *x, IndexT n);
void PQuicksort2_insitu(ValueT *x, IndexT n, double t);
void PQuicksort2_exsitu(ValueT *x, IndexT n, double t);
void PQuicksort2B_insitu(ValueT *x, IndexT n, double t);
void PQuicksort2B_exsitu(ValueT *x, IndexT n, double t);
void PDucksort_insitu(ValueT *x, IndexT n, double t);
void PDucksort_exsitu(ValueT *x, IndexT n, double t);
void PDucksortB_insitu(ValueT *x, IndexT n, double t);
void PDucksortB_exsitu(ValueT *x, IndexT n, double t);

void Dotnetpart_insitu(ValueT *x, IndexT n, IndexT *partial);
void Dotnetpart_exsitu(ValueT *x, IndexT n, IndexT *partial);
void Quickpart2_insitu(ValueT *x, IndexT n, IndexT *partial);
void Quickpart2_exsitu(ValueT *x, IndexT n, IndexT *partial);
void Zackpart_insitu(ValueT *x, IndexT n, IndexT *partial);
void Zackpart_exsitu(ValueT *x, IndexT n, IndexT *partial);
void Zuckpart_insitu(ValueT *x, IndexT n, IndexT *partial);
void Zuckpart_exsitu(ValueT *x, IndexT n, IndexT *partial);
void Duckpart_insitu(ValueT *x, IndexT n, IndexT *partial);
void Duckpart_exsitu(ValueT *x, IndexT n, IndexT *partial);
void ZuckpartB_insitu(ValueT *x, IndexT n, IndexT *partial);
void ZuckpartB_exsitu(ValueT *x, IndexT n, IndexT *partial);
void DuckpartB_insitu(ValueT *x, IndexT n, IndexT *partial);
void DuckpartB_exsitu(ValueT *x, IndexT n, IndexT *partial);

void RQuicksort2_insitu(ValueT *x, IndexT n, int reorder_inplace, PerfT *p);
void RQuicksort2_exsitu(ValueT *x, IndexT n, PerfT *p);
void RQuicksort2B_insitu(ValueT *x, IndexT n, int reorder_inplace, PerfT *p);
void RQuicksort2B_exsitu(ValueT *x, IndexT n, PerfT *p);

void SQuicksort2_insitu(ValueT *x, IndexT n, PerfT *p);
void SQuicksort2_exsitu(ValueT *x, IndexT n, PerfT *p);
void SQuicksort2B_insitu(ValueT *x, IndexT n, PerfT *p);
void SQuicksort2B_exsitu(ValueT *x, IndexT n, PerfT *p);

void Quicksort3_insitu(ValueT *x, IndexT n);
void Quicksort3_exsitu(ValueT *x, IndexT n);
void Quicksort3P_insitu(ValueT *x, IndexT n);
void Quicksort3P_exsitu(ValueT *x, IndexT n);

ValueT approxMedian_insitu(ValueT *x, IndexT n);
ValueT approxMedian_exsitu(ValueT *x, IndexT n);

ValueT Quickselect2_insitu(ValueT *x, IndexT n, IndexT k);
ValueT Quickselect2_exsitu(ValueT *x, IndexT n, IndexT k);
ValueT Quickpartleft2_insitu(ValueT *x, IndexT n, IndexT k);
ValueT Quickpartleft2_exsitu(ValueT *x, IndexT n, IndexT k);
ValueT Quickpartright2_insitu(ValueT *x, IndexT n, IndexT k);
ValueT Quickpartright2_exsitu(ValueT *x, IndexT n, IndexT k);

ValueT BFPRTselect_insitu(ValueT *x, IndexT n, IndexT k);
ValueT BFPRTselect_exsitu(ValueT *x, IndexT n, IndexT k);
ValueT Pickselect_insitu(ValueT *x, IndexT n, IndexT k);
ValueT Pickselect_exsitu(ValueT *x, IndexT n, IndexT k);

RangeIndexT Zackpartleft_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zackpartleft_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zackpartright_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zackpartright_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zackselect_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zackselect_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zuckselect_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zuckselect_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT ZuckselectB_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT ZuckselectB_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Duckselect_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Duckselect_exsitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT DuckselectB_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT DuckselectB_exsitu(ValueT *x, IndexT n, IndexT k);

RangeIndexT Zickselect_insitu(ValueT *x, IndexT n, IndexT k);
RangeIndexT Zickselect_exsitu(ValueT *x, IndexT n, IndexT k);

void Jumpsort_insitu(ValueT *x, IndexT n, IndexT b, PerfT *p);
void Jumpsort_exsitu(ValueT *x, IndexT n, IndexT b, PerfT *p);
void Walksort_insitu(ValueT *x, IndexT n, IndexT b, PerfT *p);
void Walksort_exsitu(ValueT *x, IndexT n, IndexT b, PerfT *p);
void SqrtsortP_insitu(ValueT *x, IndexT n, PerfT *p);
void SqrtsortP_exsitu(ValueT *x, IndexT n, PerfT *p);
void GrailsqrtP_insitu(ValueT *x, IndexT n);
void GrailsqrtP_exsitu(ValueT *x, IndexT n);
void GrailsortP_insitu(ValueT *x, IndexT n);
void GrailsortP_exsitu(ValueT *x, IndexT n);
void InplaceMergesortP_insitu(ValueT *x, IndexT n);
void InplaceMergesortP_exsitu(ValueT *x, IndexT n);

void SInsertionsort_insitu(ValueT *x, IndexT n, PerfT *p);
void SInsertionsort_exsitu(ValueT *x, IndexT n, PerfT *p);
void RInsertionsort_insitu(ValueT *x, IndexT n, PerfT *p);
void RInsertionsort_exsitu(ValueT *x, IndexT n, PerfT *p);

void UInsertionsort_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void UInsertionsort_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void WInsertionsort_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void WInsertionsort_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void VInsertionsort_insitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VInsertionsort_exsitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VKnuthsort_insitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VKnuthsort_exsitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void PVKnuthsort_insitu(char *x, IndexT n, IndexT m, IndexT b, double t, PerfT *p);
void PVKnuthsort_exsitu(char *x, IndexT n, IndexT m, IndexT b, double t, PerfT *p);
void VKnuthsortA_insitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VKnuthsortA_exsitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VFrogsort1_insitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VFrogsort1_exsitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void PVFrogsort1_insitu(char *x, IndexT n, IndexT m, IndexT b, double t, PerfT *p);
void PVFrogsort1_exsitu(char *x, IndexT n, IndexT m, IndexT b, double t, PerfT *p);
void VFrogsort1A_insitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void VFrogsort1A_exsitu(char *x, IndexT n, IndexT m, IndexT b, PerfT *p);
void UKnuthsortP_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void UKnuthsortP_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void UZacksort_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void UZacksort_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void UZacksortB_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void UZacksortB_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void WQuicksort2_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void WQuicksort2_exsitu(char *x, IndexT n, IndexT m, PerfT *p);
void WQuicksort2B_insitu(char *x, IndexT n, IndexT m, PerfT *p);
void WQuicksort2B_exsitu(char *x, IndexT n, IndexT m, PerfT *p);




IndexT bsearch_asc_WL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_WR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_L(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_R(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT bsearch_desc_WL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_WR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_L(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_R(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT bsearch_asc_EQL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_EQR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_asc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);

IndexT bsearch_desc_EQL(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_EQR(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_GE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_GT(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_LE(ValueT *data, IndexT l, IndexT r, ValueT value);
IndexT bsearch_desc_LT(ValueT *data, IndexT l, IndexT r, ValueT value);

#endif
