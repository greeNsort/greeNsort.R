/*
# greeNsort Header-Code for timing
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_lib_timing_h
#define ALREADY_DEFINED_lib_timing_h

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

#include <stdint.h>
#include <Rdefines.h>

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__)
  #include <sys/types.h>
#else
  #include <unistd.h>
#endif

/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/



/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

#ifndef _GREENSORT_TIMING_C_SRC

#endif


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

extern void initTicks();
extern void resetTicks();
extern void pauseTicks();
extern void resumeTicks();
extern void doneTicks();
extern int64_t getTicksPerSec();
extern int64_t getNewTicks();
extern double getNewSecs();
extern double getDeltaSecs();

extern void setSecsSnapped();
extern double getSecsSnapped();

#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__)
  extern void usleep(useconds_t usec);
#endif

#endif

/*****************************************************************************/
/**                                                                         **/
/**                                DEMO                                      **/
/**

#include <stdio.h>
#include <stdlib.h>
#include "lib_timing.h"

int main()
{
    int i;
    double r;
    const int N = 10000000;

    initTicks();
    for (i=0;i<N;i++)
        r =sqrt(i);
    printf("\nafter iniTicks)()=%f\n", getNewSecs());
    printf("\nafter getNewSecs()=%f\n", getNewSecs());

    resetTicks();
    printf("\nafter resetTicks()=%f\n", getNewSecs());

    for (i=0;i<N;i++)
        r =sqrt(i);
    printf("\nafter getNewSecs()=%f\n", getNewSecs());

    pauseTicks();
    for (i=0;i<N;i++)
        r =sqrt(i);
    resumeTicks();
    printf("\nafter pauseTicks()=%f\n", getNewSecs());

    for (i=0;i<N;i++)
        r =sqrt(i);
    printf("\nafter resumeTicks()=%f\n", getNewSecs());

    doneTicks();

    return 0;
}

**/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/

