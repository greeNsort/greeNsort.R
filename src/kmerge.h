/*
# greeNsort Katajainens k-ary merges for k in {2,3,4} and variants thereof
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_kmerge_h
#define ALREADY_DEFINED_kmerge_h

#include "algo.h"
#include "Insertionsort_l2r.h"

// merges the sorted sequences *l1..*r1 and *l2..*r2, .. to z
// uses Katajainens optimized k-way merge (modified)
// Code created by the R code generators in kmerge.R


  static void right_merge2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_merge2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_merge2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin2;

      y1 = *(++l1);

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(++l2);
      goto test12;

    fin2:
      for(;l2 <= r2;l2++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *l2);
  #endif
        *(z++) = *l2;
      }
    return;

    fin1:
      for(;l1 <= r1;l1++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *l1);
  #endif
        *(z++) = *l1;
      }
    return;
  }
  
  static void right_merge2_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge2_exhausts1(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_merge2_exhausts1(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_merge2_exhausts1(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check needed because known to not exhaust first */
      y2 = *(++l2);

    test12:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin2;
      y1 = *(++l1);
      goto test12;

    
    fin2:
      for(;l2 <= r2;l2++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *l2);
  #endif
        *(z++) = *l2;
      }
    return;
  }
  
  static void right_merge2_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge2_exhausts2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_merge2_exhausts2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_merge2_exhausts2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check needed because known to not exhaust first */
      y1 = *(++l1);

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(++l2);
      goto test12;

    
    fin1:
      for(;l1 <= r1;l1++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *l1);
  #endif
        *(z++) = *l1;
      }
    return;
  }
  
  static void left_merge2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_merge2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_merge2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin2;

      y1 = *(--r1);

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(--r2);
      goto test12;

    fin2:
      for(;l2 <= r2;r2--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *r2);
  #endif
        *(z--) = *r2;
      }
    return;

    fin1:
      for(;l1 <= r1;r1--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *r1);
  #endif
        *(z--) = *r1;
      }
    return;
  }
  
  static void left_merge2_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge2_exhausts1(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_merge2_exhausts1(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_merge2_exhausts1(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check needed because known to not exhaust first */
      y2 = *(--r2);

    test12:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin2;
      y1 = *(--r1);
      goto test12;

    
    fin2:
      for(;l2 <= r2;r2--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *r2);
  #endif
        *(z--) = *r2;
      }
    return;
  }
  
  static void left_merge2_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge2_exhausts2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_merge2_exhausts2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_merge2_exhausts2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check needed because known to not exhaust first */
      y1 = *(--r1);

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(--r2);
      goto test12;

    
    fin1:
      for(;l1 <= r1;r1--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *r1);
  #endif
        *(z--) = *r1;
      }
    return;
  }
  
  static void right_merge3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_merge3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_merge3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test31;

    
    fin1:
        right_merge2(z, l2, r2, l3, r3);
        return;
    fin2:
        right_merge2(z, l1, r1, l3, r3);
        return;
    fin3:
        right_merge2(z, l1, r1, l2, r2);
        return;
    

  }
  
  static void right_merge3_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge3_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_merge3_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_merge3_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test31;

    
    fin1:
      if (LE(*r2, *r3))
        right_merge2_exhausts1(z, l2, r2, l3, r3);
      else
        right_merge2_exhausts2(z, l2, r2, l3, r3);
    

  }
  
  static void right_merge3_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge3_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_merge3_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_merge3_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y1, y3))
      goto test12;
    goto test32;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test32;

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      goto test12;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test12;

    test32:
      if (LT(y3, y2))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      goto test32;

    
    fin2:
      if (LE(*r1, *r3))
        right_merge2_exhausts1(z, l1, r1, l3, r3);
      else
        right_merge2_exhausts2(z, l1, r1, l3, r3);
    

  }
  
  static void right_merge3_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge3_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_merge3_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_merge3_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y1, y2))
      goto test13;
    goto test23;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      goto test13;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      goto test23;

    
    fin3:
      if (LE(*r1, *r2))
        right_merge2_exhausts1(z, l1, r1, l2, r2);
      else
        right_merge2_exhausts2(z, l1, r1, l2, r2);
    

  }
  
  static void left_merge3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_merge3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_merge3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test31;

    
    fin1:
        left_merge2(z, l2, r2, l3, r3);
        return;
    fin2:
        left_merge2(z, l1, r1, l3, r3);
        return;
    fin3:
        left_merge2(z, l1, r1, l2, r2);
        return;
    

  }
  
  static void left_merge3_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge3_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_merge3_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_merge3_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test31;

    
    fin1:
      if (GT(*l2, *l3))
        left_merge2_exhausts1(z, l2, r2, l3, r3);
      else
        left_merge2_exhausts2(z, l2, r2, l3, r3);
    

  }
  
  static void left_merge3_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge3_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_merge3_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_merge3_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y1, y3))
      goto test12;
    goto test32;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test32;

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      goto test12;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test12;

    test32:
      if (GE(y3, y2))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      goto test32;

    
    fin2:
      if (GT(*l1, *l3))
        left_merge2_exhausts1(z, l1, r1, l3, r3);
      else
        left_merge2_exhausts2(z, l1, r1, l3, r3);
    

  }
  
  static void left_merge3_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge3_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_merge3_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_merge3_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y1, y2))
      goto test13;
    goto test23;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      goto test13;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      goto test23;

    
    fin3:
      if (GT(*l1, *l2))
        left_merge2_exhausts1(z, l1, r1, l2, r2);
      else
        left_merge2_exhausts2(z, l1, r1, l2, r2);
    

  }
  
  static void right_merge4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_merge4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_merge4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y2, y3)){
      if (LT(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (LT(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test34;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test24;

      if (GE(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test24;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test34;

      if (GE(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
          right_merge3(z, l2, r2, l3, r3, l4, r4);
        return;
    fin2:
          right_merge3(z, l1, r1, l3, r3, l4, r4);
        return;
    fin3:
          right_merge3(z, l1, r1, l2, r2, l4, r4);
        return;
    fin4:
          right_merge3(z, l1, r1, l2, r2, l3, r3);
        return;
    
  }
        
  static void right_merge4_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge4_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_merge4_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_merge4_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y2, y3)){
      if (LT(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (LT(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test34;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test24;

      if (GE(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test24;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test34;

      if (GE(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
      if (LE(*r2, *r3)){
        if (LE(*r2, *r4)){
          right_merge3_exhausts1(z, l2, r2, l3, r3, l4, r4);
        }else{
          right_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }else{
        if (LE(*r3, *r4)){
          right_merge3_exhausts2(z, l2, r2, l3, r3, l4, r4);
        }else{
          right_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void right_merge4_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge4_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_merge4_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_merge4_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y3)){
      if (LT(y4, y2))
        goto test14;
      else
        goto test12;
    }else{
      if (LT(y4, y2))
        goto test34;
      else
        goto test32;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test34;

    test14:
      if (LE(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y2))
        goto test14;

      if (GT(y1, y2))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test32;

    test12:
      if (LE(y1, y2))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      if (GE(y4, y2))
        goto test12;
      goto test14;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test14;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y2))
        goto test34;

      if (GE(y3, y2))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test12;

    test32:
      if (LT(y3, y2))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      if (GE(y4, y2))
        goto test32;
      goto test34;

    
    fin2:
      if (LE(*r1, *r3)){
        if (LE(*r1, *r4)){
          right_merge3_exhausts1(z, l1, r1, l3, r3, l4, r4);
        }else{
          right_merge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }else{
        if (LE(*r3, *r4)){
          right_merge3_exhausts2(z, l1, r1, l3, r3, l4, r4);
        }else{
          right_merge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void right_merge4_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge4_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_merge4_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_merge4_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y2)){
      if (LT(y4, y3))
        goto test14;
      else
        goto test13;
    }else{
      if (LT(y4, y3))
        goto test24;
      else
        goto test23;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test24;

    test14:
      if (LE(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y3))
        goto test14;

      if (GT(y1, y3))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      if (GE(y4, y3))
        goto test13;
      goto test14;



    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test14;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y3))
        goto test24;

      if (GT(y2, y3))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      if (GE(y4, y3))
        goto test23;
      goto test24;

    
    fin3:
      if (LE(*r1, *r2)){
        if (LE(*r1, *r4)){
          right_merge3_exhausts1(z, l1, r1, l2, r2, l4, r4);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }else{
        if (LE(*r2, *r4)){
          right_merge3_exhausts2(z, l1, r1, l2, r2, l4, r4);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }
    
  }
        
  static void right_merge4_exhausts4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_merge4_exhausts4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_merge4_exhausts4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_merge4_exhausts4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y2)){
      if (LE(y3, y4))
        goto test13;
      else
        goto test14;
    }else{
      if (LE(y3, y4))
        goto test23;
      else
        goto test24;
    }

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out13;

  #ifdef KMERGE_DEBUG
      Rprintf("out3a: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(++l3);
      if (LE(y3, y4))
        goto test13;

      if (GT(y1, y4))
        goto out41;

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test24;

    test14:
      if (LE(y1, y4))
        goto out14;

    out41:
  #ifdef KMERGE_DEBUG
      Rprintf("out41: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(++l4);
      if (GT(y3, y4))
        goto test14;
      goto test13;



    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out23;

  #ifdef KMERGE_DEBUG
      Rprintf("out3b: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(++l3);
      if (LE(y3, y4))
        goto test23;

      if (GT(y2, y4))
        goto out42;

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test14;

    test24:
      if (LE(y2, y4))
        goto out24;

    out42:
  #ifdef KMERGE_DEBUG
      Rprintf("out42: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(++l4);
      if (GT(y3, y4))
        goto test24;
      goto test23;

    
    fin4:
      if (LE(*r1, *r2)){
        if (LE(*r1, *r3)){
          right_merge3_exhausts1(z, l1, r1, l2, r2, l3, r3);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }else{
        if (LE(*r2, *r3)){
          right_merge3_exhausts2(z, l1, r1, l2, r2, l3, r3);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }
    
  }
        
  static void left_merge4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_merge4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_merge4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y2, y3)){
      if (GE(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (GE(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test34;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test24;

      if (LT(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test24;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test34;

      if (LT(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
          left_merge3(z, l2, r2, l3, r3, l4, r4);
        return;
    fin2:
          left_merge3(z, l1, r1, l3, r3, l4, r4);
        return;
    fin3:
          left_merge3(z, l1, r1, l2, r2, l4, r4);
        return;
    fin4:
          left_merge3(z, l1, r1, l2, r2, l3, r3);
        return;
    
  }
        
  static void left_merge4_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge4_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_merge4_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_merge4_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y2, y3)){
      if (GE(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (GE(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test34;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test24;

      if (LT(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test24;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test34;

      if (LT(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
      if (GT(*l2, *l3)){
        if (GT(*l2, *l4)){
          left_merge3_exhausts1(z, l2, r2, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }else{
        if (GT(*l3, *l4)){
          left_merge3_exhausts2(z, l2, r2, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void left_merge4_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge4_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_merge4_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_merge4_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y3)){
      if (GE(y4, y2))
        goto test14;
      else
        goto test12;
    }else{
      if (GE(y4, y2))
        goto test34;
      else
        goto test32;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test34;

    test14:
      if (GT(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y2))
        goto test14;

      if (LE(y1, y2))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test32;

    test12:
      if (GT(y1, y2))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      if (LT(y4, y2))
        goto test12;
      goto test14;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test14;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y2))
        goto test34;

      if (LT(y3, y2))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test12;

    test32:
      if (GE(y3, y2))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      if (LT(y4, y2))
        goto test32;
      goto test34;

    
    fin2:
      if (GT(*l1, *l3)){
        if (GT(*l1, *l4)){
          left_merge3_exhausts1(z, l1, r1, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }else{
        if (GT(*l3, *l4)){
          left_merge3_exhausts2(z, l1, r1, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void left_merge4_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge4_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_merge4_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_merge4_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y2)){
      if (GE(y4, y3))
        goto test14;
      else
        goto test13;
    }else{
      if (GE(y4, y3))
        goto test24;
      else
        goto test23;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test24;

    test14:
      if (GT(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y3))
        goto test14;

      if (LE(y1, y3))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      if (LT(y4, y3))
        goto test13;
      goto test14;



    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test14;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y3))
        goto test24;

      if (LE(y2, y3))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      if (LT(y4, y3))
        goto test23;
      goto test24;

    
    fin3:
      if (GT(*l1, *l2)){
        if (GT(*l1, *l4)){
          left_merge3_exhausts1(z, l1, r1, l2, r2, l4, r4);
        }else{
          left_merge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }else{
        if (GT(*l2, *l4)){
          left_merge3_exhausts2(z, l1, r1, l2, r2, l4, r4);
        }else{
          left_merge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }
    
  }
        
  static void left_merge4_exhausts4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_merge4_exhausts4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_merge4_exhausts4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_merge4_exhausts4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y2)){
      if (GT(y3, y4))
        goto test13;
      else
        goto test14;
    }else{
      if (GT(y3, y4))
        goto test23;
      else
        goto test24;
    }

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out13;

  #ifdef KMERGE_DEBUG
      Rprintf("out3a: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(--r3);
      if (GT(y3, y4))
        goto test13;

      if (LE(y1, y4))
        goto out41;

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test24;

    test14:
      if (GT(y1, y4))
        goto out14;

    out41:
  #ifdef KMERGE_DEBUG
      Rprintf("out41: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(--r4);
      if (LE(y3, y4))
        goto test14;
      goto test13;



    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out23;

  #ifdef KMERGE_DEBUG
      Rprintf("out3b: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(--r3);
      if (GT(y3, y4))
        goto test23;

      if (LE(y2, y4))
        goto out42;

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test14;

    test24:
      if (GT(y2, y4))
        goto out24;

    out42:
  #ifdef KMERGE_DEBUG
      Rprintf("out42: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(--r4);
      if (LE(y3, y4))
        goto test24;
      goto test23;

    
    fin4:
      if (GT(*l1, *l2)){
        if (GT(*l1, *l3)){
          left_merge3_exhausts1(z, l1, r1, l2, r2, l3, r3);
        }else{
          left_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }else{
        if (GT(*l2, *l3)){
          left_merge3_exhausts2(z, l1, r1, l2, r2, l3, r3);
        }else{
          left_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }
    
  }
        
  static void right_pimerge2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_pimerge2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_pimerge2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin2;

      y1 = *(++l1);

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(++l2);
      goto test12;

    fin2:
      /* nothing to do because rest is inplace */
    return;

    fin1:
      for(;l1 <= r1;l1++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *l1);
  #endif
        *(z++) = *l1;
      }
    return;
  }
  
  static void right_pimerge2_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge2_exhausts1(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_pimerge2_exhausts1(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_pimerge2_exhausts1(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check needed because known to not exhaust first */
      y2 = *(++l2);

    test12:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin2;
      y1 = *(++l1);
      goto test12;

    
    fin2:
      /* nothing to do because rest is inplace */
    return;
  }
  
  static void right_pimerge2_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge2_exhausts2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("right_pimerge2_exhausts2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("right_pimerge2_exhausts2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*l1, y2=*l2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check needed because known to not exhaust first */
      y1 = *(++l1);

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(++l2);
      goto test12;

    
    fin1:
      for(;l1 <= r1;l1++){
  #ifdef KMERGE_DEBUG
        Rprintf("fin1: %f\n", *l1);
  #endif
        *(z++) = *l1;
      }
    return;
  }
  
  static void left_pimerge2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_pimerge2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_pimerge2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin2;

      y1 = *(--r1);

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(--r2);
      goto test12;

    fin2:
      for(;l2 <= r2;r2--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *r2);
  #endif
        *(z--) = *r2;
      }
    return;

    fin1:
      /* nothing to do because rest is inplace */
    return;
  }
  
  static void left_pimerge2_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge2_exhausts1(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_pimerge2_exhausts1(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_pimerge2_exhausts1(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check needed because known to not exhaust first */
      y2 = *(--r2);

    test12:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin2;
      y1 = *(--r1);
      goto test12;

    
    fin2:
      for(;l2 <= r2;r2--){
  #ifdef KMERGE_DEBUG
        Rprintf("fin2: %f\n", *r2);
  #endif
        *(z--) = *r2;
      }
    return;
  }
  
  static void left_pimerge2_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge2_exhausts2(l1=%p r1=%p  l2=%p r2=%p z=%p\n", l1, r1, l2, r2, z);
    Rprintf("left_pimerge2_exhausts2(l1=%d r1=%d  l2=%d r2=%d\n", 0, r1-l1, l2-l1, r2-l1);
    Rprintf("left_pimerge2_exhausts2(l1=%f r1=%f  l2=%f r2=%f\n", *l1, *r1, *l2, *r2);
  #endif
    ValueT y1=*r1, y2=*r2;

    goto test12;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check needed because known to not exhaust first */
      y1 = *(--r1);

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin1;
      y2 = *(--r2);
      goto test12;

    
    fin1:
      /* nothing to do because rest is inplace */
    return;
  }
  
  static void right_pimerge3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_pimerge3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_pimerge3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test31;

    
    fin1:
        right_pimerge2(z, l2, r2, l3, r3);
        return;
    fin2:
        right_pimerge2(z, l1, r1, l3, r3);
        return;
    fin3:
        right_merge2(z, l1, r1, l2, r2);
        return;
    

  }
  
  static void right_pimerge3_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge3_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_pimerge3_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_pimerge3_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      goto test31;

    
    fin1:
      if (LE(*r2, *r3))
        right_pimerge2_exhausts1(z, l2, r2, l3, r3);
      else
        right_pimerge2_exhausts2(z, l2, r2, l3, r3);
    

  }
  
  static void right_pimerge3_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge3_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_pimerge3_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_pimerge3_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y1, y3))
      goto test12;
    goto test32;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test32;

    test12:
      if (LE(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      goto test12;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test12;

    test32:
      if (LT(y3, y2))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      goto test32;

    
    fin2:
      if (LE(*r1, *r3))
        right_pimerge2_exhausts1(z, l1, r1, l3, r3);
      else
        right_pimerge2_exhausts2(z, l1, r1, l3, r3);
    

  }
  
  static void right_pimerge3_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge3_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("right_pimerge3_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("right_pimerge3_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3;

    if (LE(y1, y2))
      goto test13;
    goto test23;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      goto test13;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      goto test23;

    
    fin3:
      if (LE(*r1, *r2))
        right_merge2_exhausts1(z, l1, r1, l2, r2);
      else
        right_merge2_exhausts2(z, l1, r1, l2, r2);
    

  }
  
  static void left_pimerge3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_pimerge3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_pimerge3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test31;

    
    fin1:
        left_merge2(z, l2, r2, l3, r3);
        return;
    fin2:
        left_pimerge2(z, l1, r1, l3, r3);
        return;
    fin3:
        left_pimerge2(z, l1, r1, l2, r2);
        return;
    

  }
  
  static void left_pimerge3_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge3_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_pimerge3_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_pimerge3_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y2, y3))
      goto test21;
    goto test31;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test21;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      goto test31;

    
    fin1:
      if (GT(*l2, *l3))
        left_merge2_exhausts1(z, l2, r2, l3, r3);
      else
        left_merge2_exhausts2(z, l2, r2, l3, r3);
    

  }
  
  static void left_pimerge3_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge3_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_pimerge3_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_pimerge3_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y1, y3))
      goto test12;
    goto test32;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test32;

    test12:
      if (GT(y1, y2))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      goto test12;

    out3:
  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test12;

    test32:
      if (GE(y3, y2))
        goto out3;

  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      goto test32;

    
    fin2:
      if (GT(*l1, *l3))
        left_pimerge2_exhausts1(z, l1, r1, l3, r3);
      else
        left_pimerge2_exhausts2(z, l1, r1, l3, r3);
    

  }
  
  static void left_pimerge3_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge3_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, z);
    Rprintf("left_pimerge3_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1);
    Rprintf("left_pimerge3_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f\n", *l1, *r1, *l2, *r2, *l3, *r3);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3;

    if (GT(y1, y2))
      goto test13;
    goto test23;

    out1:
  #ifdef KMERGE_DEBUG
      Rprintf("out1: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out1;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      goto test13;

    out2:
  #ifdef KMERGE_DEBUG
      Rprintf("out2: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out2;

  #ifdef KMERGE_DEBUG
      Rprintf("out3: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      goto test23;

    
    fin3:
      if (GT(*l1, *l2))
        left_pimerge2_exhausts1(z, l1, r1, l2, r2);
      else
        left_pimerge2_exhausts2(z, l1, r1, l2, r2);
    

  }
  
  static void right_pimerge4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_pimerge4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_pimerge4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y2, y3)){
      if (LT(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (LT(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test34;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test24;

      if (GE(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test24;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test34;

      if (GE(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
          right_pimerge3(z, l2, r2, l3, r3, l4, r4);
        return;
    fin2:
          right_pimerge3(z, l1, r1, l3, r3, l4, r4);
        return;
    fin3:
          right_pimerge3(z, l1, r1, l2, r2, l4, r4);
        return;
    fin4:
          right_merge3(z, l1, r1, l2, r2, l3, r3);
        return;
    
  }
        
  static void right_pimerge4_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge4_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_pimerge4_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_pimerge4_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y2, y3)){
      if (LT(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (LT(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test34;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test24;

      if (GE(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(++l2);
      if (GT(y2, y3))
        goto test31;

    test21:
      if (LT(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test24;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y1))
        goto test34;

      if (GE(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(++l3);
      if (LE(y2, y3))
        goto test21;

    test31:
      if (LT(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(++l1);
      if (GE(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
      if (LE(*r2, *r3)){
        if (LE(*r2, *r4)){
          right_pimerge3_exhausts1(z, l2, r2, l3, r3, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }else{
        if (LE(*r3, *r4)){
          right_pimerge3_exhausts2(z, l2, r2, l3, r3, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void right_pimerge4_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge4_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_pimerge4_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_pimerge4_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y3)){
      if (LT(y4, y2))
        goto test14;
      else
        goto test12;
    }else{
      if (LT(y4, y2))
        goto test34;
      else
        goto test32;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test34;

    test14:
      if (LE(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y2))
        goto test14;

      if (GT(y1, y2))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y3))
        goto test32;

    test12:
      if (LE(y1, y2))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      if (GE(y4, y2))
        goto test12;
      goto test14;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test14;

    test34:
      if (LE(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y2))
        goto test34;

      if (GE(y3, y2))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(++l3);
      if (LE(y1, y3))
        goto test12;

    test32:
      if (LT(y3, y2))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(++l2);
      if (GE(y4, y2))
        goto test32;
      goto test34;

    
    fin2:
      if (LE(*r1, *r3)){
        if (LE(*r1, *r4)){
          right_pimerge3_exhausts1(z, l1, r1, l3, r3, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }else{
        if (LE(*r3, *r4)){
          right_pimerge3_exhausts2(z, l1, r1, l3, r3, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void right_pimerge4_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge4_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_pimerge4_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_pimerge4_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y2)){
      if (LT(y4, y3))
        goto test14;
      else
        goto test13;
    }else{
      if (LT(y4, y3))
        goto test24;
      else
        goto test23;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test24;

    test14:
      if (LE(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y3))
        goto test14;

      if (GT(y1, y3))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      if (GE(y4, y3))
        goto test13;
      goto test14;



    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test14;

    test24:
      if (LE(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z++) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(++l4);
      if (LT(y4, y3))
        goto test24;

      if (GT(y2, y3))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z++) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(++l3);
      if (GE(y4, y3))
        goto test23;
      goto test24;

    
    fin3:
      if (LE(*r1, *r2)){
        if (LE(*r1, *r4)){
          right_pimerge3_exhausts1(z, l1, r1, l2, r2, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }else{
        if (LE(*r2, *r4)){
          right_pimerge3_exhausts2(z, l1, r1, l2, r2, l4, r4);
        }else{
          right_pimerge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }
    
  }
        
  static void right_pimerge4_exhausts4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("right_pimerge4_exhausts4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("right_pimerge4_exhausts4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("right_pimerge4_exhausts4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*l1, y2=*l2, y3=*l3, y4=*l4;

    if (LE(y1, y2)){
      if (LE(y3, y4))
        goto test13;
      else
        goto test14;
    }else{
      if (LE(y3, y4))
        goto test23;
      else
        goto test24;
    }

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test23;

    test13:
      if (LE(y1, y3))
        goto out13;

  #ifdef KMERGE_DEBUG
      Rprintf("out3a: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(++l3);
      if (LE(y3, y4))
        goto test13;

      if (GT(y1, y4))
        goto out41;

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z++) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(++l1);
      if (GT(y1, y2))
        goto test24;

    test14:
      if (LE(y1, y4))
        goto out14;

    out41:
  #ifdef KMERGE_DEBUG
      Rprintf("out41: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(++l4);
      if (GT(y3, y4))
        goto test14;
      goto test13;



    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test13;

    test23:
      if (LE(y2, y3))
        goto out23;

  #ifdef KMERGE_DEBUG
      Rprintf("out3b: %f\n", y3);
  #endif
      *(z++) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(++l3);
      if (LE(y3, y4))
        goto test23;

      if (GT(y2, y4))
        goto out42;

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z++) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(++l2);
      if (LE(y1, y2))
        goto test14;

    test24:
      if (LE(y2, y4))
        goto out24;

    out42:
  #ifdef KMERGE_DEBUG
      Rprintf("out42: %f\n", y4);
  #endif
      *(z++) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(++l4);
      if (GT(y3, y4))
        goto test24;
      goto test23;

    
    fin4:
      if (LE(*r1, *r2)){
        if (LE(*r1, *r3)){
          right_merge3_exhausts1(z, l1, r1, l2, r2, l3, r3);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }else{
        if (LE(*r2, *r3)){
          right_merge3_exhausts2(z, l1, r1, l2, r2, l3, r3);
        }else{
          right_merge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }
    
  }
        
  static void left_pimerge4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_pimerge4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_pimerge4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y2, y3)){
      if (GE(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (GE(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test34;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test24;

      if (LT(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test24;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test34;

      if (LT(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
          left_merge3(z, l2, r2, l3, r3, l4, r4);
        return;
    fin2:
          left_pimerge3(z, l1, r1, l3, r3, l4, r4);
        return;
    fin3:
          left_pimerge3(z, l1, r1, l2, r2, l4, r4);
        return;
    fin4:
          left_pimerge3(z, l1, r1, l2, r2, l3, r3);
        return;
    
  }
        
  static void left_pimerge4_exhausts1(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge4_exhausts1(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_pimerge4_exhausts1(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_pimerge4_exhausts1(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y2, y3)){
      if (GE(y4, y1))
        goto test24;
      else
        goto test21;
    }else{
      if (GE(y4, y1))
        goto test34;
      else
        goto test31;
    }

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test34;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test24;

      if (LT(y2, y1))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l1 exhausts first */
      y2 = *(--r2);
      if (LE(y2, y3))
        goto test31;

    test21:
      if (GE(y2, y1))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test21;
      goto test24;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test24;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l1 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y1))
        goto test34;

      if (LT(y3, y1))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l1 exhausts first */
      y3 = *(--r3);
      if (GT(y2, y3))
        goto test21;

    test31:
      if (GE(y3, y1))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      if (l1 >= r1)
        goto fin1;
      y1 = *(--r1);
      if (LT(y4, y1))
        goto test31;
      goto test34;

    
    fin1:
      if (GT(*l2, *l3)){
        if (GT(*l2, *l4)){
          left_merge3_exhausts1(z, l2, r2, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }else{
        if (GT(*l3, *l4)){
          left_merge3_exhausts2(z, l2, r2, l3, r3, l4, r4);
        }else{
          left_merge3_exhausts3(z, l2, r2, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void left_pimerge4_exhausts2(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge4_exhausts2(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_pimerge4_exhausts2(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_pimerge4_exhausts2(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y3)){
      if (GE(y4, y2))
        goto test14;
      else
        goto test12;
    }else{
      if (GE(y4, y2))
        goto test34;
      else
        goto test32;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test34;

    test14:
      if (GT(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y2))
        goto test14;

      if (LE(y1, y2))
        goto out21;

    out12:
  #ifdef KMERGE_DEBUG
      Rprintf("out12: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l2 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y3))
        goto test32;

    test12:
      if (GT(y1, y2))
        goto out12;

    out21:
  #ifdef KMERGE_DEBUG
      Rprintf("out21: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      if (LT(y4, y2))
        goto test12;
      goto test14;



    out34:
  #ifdef KMERGE_DEBUG
      Rprintf("out34: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test14;

    test34:
      if (GT(y3, y4))
        goto out34;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l2 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y2))
        goto test34;

      if (LT(y3, y2))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l2 exhausts first */
      y3 = *(--r3);
      if (GT(y1, y3))
        goto test12;

    test32:
      if (GE(y3, y2))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      if (l2 >= r2)
        goto fin2;
      y2 = *(--r2);
      if (LT(y4, y2))
        goto test32;
      goto test34;

    
    fin2:
      if (GT(*l1, *l3)){
        if (GT(*l1, *l4)){
          left_pimerge3_exhausts1(z, l1, r1, l3, r3, l4, r4);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }else{
        if (GT(*l3, *l4)){
          left_pimerge3_exhausts2(z, l1, r1, l3, r3, l4, r4);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l3, r3, l4, r4);
        }
      }
    
  }
        
  static void left_pimerge4_exhausts3(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge4_exhausts3(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_pimerge4_exhausts3(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_pimerge4_exhausts3(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y2)){
      if (GE(y4, y3))
        goto test14;
      else
        goto test13;
    }else{
      if (GE(y4, y3))
        goto test24;
      else
        goto test23;
    }

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test24;

    test14:
      if (GT(y1, y4))
        goto out14;

  #ifdef KMERGE_DEBUG
      Rprintf("out4a: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y3))
        goto test14;

      if (LE(y1, y3))
        goto out31;

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l3 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out13;

    out31:
  #ifdef KMERGE_DEBUG
      Rprintf("out31: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      if (LT(y4, y3))
        goto test13;
      goto test14;



    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test14;

    test24:
      if (GT(y2, y4))
        goto out24;

  #ifdef KMERGE_DEBUG
      Rprintf("out4b: %f\n", y4);
  #endif
      *(z--) = y4;
      /* no check of l4 needed, l3 exhausts first */
      y4 = *(--r4);
      if (GE(y4, y3))
        goto test24;

      if (LE(y2, y3))
        goto out32;

    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l3 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out23;

    out32:
  #ifdef KMERGE_DEBUG
      Rprintf("out32: %f\n", y3);
  #endif
      *(z--) = y3;
      if (l3 >= r3)
        goto fin3;
      y3 = *(--r3);
      if (LT(y4, y3))
        goto test23;
      goto test24;

    
    fin3:
      if (GT(*l1, *l2)){
        if (GT(*l1, *l4)){
          left_pimerge3_exhausts1(z, l1, r1, l2, r2, l4, r4);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }else{
        if (GT(*l2, *l4)){
          left_pimerge3_exhausts2(z, l1, r1, l2, r2, l4, r4);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l2, r2, l4, r4);
        }
      }
    
  }
        
  static void left_pimerge4_exhausts4(ValueT *z, ValueT *l1, ValueT *r1, ValueT *l2, ValueT *r2, ValueT *l3, ValueT *r3, ValueT *l4, ValueT *r4){
  #ifdef KMERGE_DEBUG
    Rprintf("left_pimerge4_exhausts4(l1=%p r1=%p  l2=%p r2=%p  l3=%p r3=%p  l4=%p r4=%p  z=%p\n", l1, r1, l2,  r2, l3, r3, l4, r4, z);
    Rprintf("left_pimerge4_exhausts4(l1=%d r1=%d  l2=%d r2=%d  l3=%d r3=%d  l4=%d r4=%d\n", 0, r1-l1, l2-l1,  r2-l1, l3-l1, r3-l1, l4-l1, r4-l1);
    Rprintf("left_pimerge4_exhausts4(l1=%f r1=%f  l2=%f r2=%f  l3=%f r3=%f  l4=%f r4=%f\n", *l1, *r1, *l2, *r2, *l3, *r3, *l4, *r4);
  #endif
    ValueT y1=*r1, y2=*r2, y3=*r3, y4=*r4;

    if (GT(y1, y2)){
      if (GT(y3, y4))
        goto test13;
      else
        goto test14;
    }else{
      if (GT(y3, y4))
        goto test23;
      else
        goto test24;
    }

    out13:
  #ifdef KMERGE_DEBUG
      Rprintf("out13: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test23;

    test13:
      if (GT(y1, y3))
        goto out13;

  #ifdef KMERGE_DEBUG
      Rprintf("out3a: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(--r3);
      if (GT(y3, y4))
        goto test13;

      if (LE(y1, y4))
        goto out41;

    out14:
  #ifdef KMERGE_DEBUG
      Rprintf("out14: %f\n", y1);
  #endif
      *(z--) = y1;
      /* no check of l1 needed, l4 exhausts first */
      y1 = *(--r1);
      if (LE(y1, y2))
        goto test24;

    test14:
      if (GT(y1, y4))
        goto out14;

    out41:
  #ifdef KMERGE_DEBUG
      Rprintf("out41: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(--r4);
      if (LE(y3, y4))
        goto test14;
      goto test13;



    out23:
  #ifdef KMERGE_DEBUG
      Rprintf("out23: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test13;

    test23:
      if (GT(y2, y3))
        goto out23;

  #ifdef KMERGE_DEBUG
      Rprintf("out3b: %f\n", y3);
  #endif
      *(z--) = y3;
      /* no check of l3 needed, l4 exhausts first */
      y3 = *(--r3);
      if (GT(y3, y4))
        goto test23;

      if (LE(y2, y4))
        goto out42;

    out24:
  #ifdef KMERGE_DEBUG
      Rprintf("out24: %f\n", y2);
  #endif
      *(z--) = y2;
      /* no check of l2 needed, l4 exhausts first */
      y2 = *(--r2);
      if (GT(y1, y2))
        goto test14;

    test24:
      if (GT(y2, y4))
        goto out24;

    out42:
  #ifdef KMERGE_DEBUG
      Rprintf("out42: %f\n", y4);
  #endif
      *(z--) = y4;
      if (l4 >= r4)
        goto fin4;
      y4 = *(--r4);
      if (LE(y3, y4))
        goto test24;
      goto test23;

    
    fin4:
      if (GT(*l1, *l2)){
        if (GT(*l1, *l3)){
          left_pimerge3_exhausts1(z, l1, r1, l2, r2, l3, r3);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }else{
        if (GT(*l2, *l3)){
          left_pimerge3_exhausts2(z, l1, r1, l2, r2, l3, r3);
        }else{
          left_pimerge3_exhausts3(z, l1, r1, l2, r2, l3, r3);
        }
      }
    
  }


#endif
