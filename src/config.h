/*
# greeNsort C-code configuration
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_config_h
#define ALREADY_DEFINED_config_h

#include <R.h>  // needed here for definition of GetRNGstate PutRNGstate unif_rand
#include <Rdefines.h>  // needed here for definition of R_PosInf R_NegInf and R_alloc GetRNGstate PutRNGstate

// define this for conformance with CRAN policies when submitting (uses R's random number generator and memory management)
// define this for reliable replication of bugs with R's set.seed
// undefine this for fair performance testing (uses standard library random number generator and memory management)
#define CRAN_COMPATIBLE
//#undef CRAN_COMPATIBLE



// NOTE complete regression tests require 4x compiling and testing
// INSERTIONSORT_LIMIT 32 & define STABLE_TEST
// INSERTIONSORT_LIMIT 32 &  undef STABLE_TEST
// INSERTIONSORT_LIMIT  0 & define STABLE_TEST
// INSERTIONSORT_LIMIT  0 &  undef STABLE_TEST

// define STABLE_TEST for stability testing using partial keys otherwise normal use with full comparision
// undefine this for production and timing
//#define STABLE_TEST
#undef STABLE_TEST

// define USE_LOCALE for strcoll otherwise strcmp
#define USE_LOCALE
//#undef USE_LOCALE

// DUAL_PIVOT_TINY_SIZE was 17, DUAL_PIVOT_DIST_SIZE was 13
// DUAL_PIVOT_DIST_SIZE set to 0 (switched off) in order to minimize overhead (dual is still fastest with alltied data)

// #define TRIPLET_LIMIT 0
// #define INSERTIONSORT_LIMIT 0
// #define VINSERTIONSORT_LIMIT 0
// #define DUAL_PIVOT_TINY_SIZE INSERTIONSORT_LIMIT
// #define DUAL_PIVOT_DIST_SIZE 0
// #define THREAD_LIMIT_MERGE 2
// #define THREAD_LIMIT_SORT 2

// #define TRIPLET_LIMIT 1
// #define INSERTIONSORT_LIMIT 2
// #define VINSERTIONSORT_LIMIT 128
// #define DUAL_PIVOT_TINY_SIZE INSERTIONSORT_LIMIT
// #define DUAL_PIVOT_DIST_SIZE 12
// #define THREAD_LIMIT_MERGE 4
// #define THREAD_LIMIT_SORT 4

// #define TRIPLET_LIMIT 2
// #define INSERTIONSORT_LIMIT 4
// #define VINSERTIONSORT_LIMIT 128
// #define DUAL_PIVOT_TINY_SIZE INSERTIONSORT_LIMIT
// #define DUAL_PIVOT_DIST_SIZE 12
// #define THREAD_LIMIT_MERGE 8
// #define THREAD_LIMIT_SORT 8

#define TRIPLET_LIMIT 32
#define INSERTIONSORT_LIMIT 64
#define VINSERTIONSORT_LIMIT 128
#define DUAL_PIVOT_TINY_SIZE INSERTIONSORT_LIMIT
#define DUAL_PIVOT_DIST_SIZE 12
#define THREAD_LIMIT_MERGE 1024
#define THREAD_LIMIT_SORT 1024

#define QUICKSORT_BLOCKSIZE 48
#define MERGESORT_BLOCKSIZE 48

// Toggle two variants of inline comparisons that can trigger the compiler to use conditional moves CMOV
#define MERGESORT_BTUNE_WITH_REGISTER 1
//#undef MERGESORT_BTUNE_WITH_REGISTER

// choose whatever is faster on your compiler for gapped algorithms (GInsertionsort, GKnuthsort)
//#define INC2(i)(i++,i++)
//#define DEC2(i)(i--,i--)
#define INC2(i)i+=2
#define DEC2(i)i-=2

// For simplicity here we use R's 32bit integer type as integer type (we inherit the size limitations anyhow)
#define INDEX_T int
//#define INDEX_T int64_t
#define VALUE_T double


/* --- EXPERIMENTAL, NO WELL DEFINED BEHAVIOUR ------------------------------------------------------------- */

#undef DEBUG
#define NDEBUG 1
//#define DEBUG
//#undef NDEBUG

// Not consistently used
// #define SCANNED_RANGE
#undef SCANNED_RANGE


// for k-ary sorts (not used here)
// (K)
#define K 8
// (K-1)
#define K1 7
//(K*K)
#define KK 64


/* --- Header Implementations ------------------------------------------------------------- */

typedef INDEX_T IndexT;
typedef VALUE_T ValueT;

typedef IndexT CountT;

typedef int IntValueT;

typedef struct IntIndTStruct {
  IntValueT key;
  IndexT rec;
} IntIndT;


/*
#define RECORD_LENGTH 64
typedef struct RecordTStruct {
  int key;
  int load[RECORD_LENGTH-1];
} RecordT;
*/

typedef struct RangeIndexTStruct {
  IndexT min;
  IndexT max;
} RangeIndexT;
typedef IndexT CountT;

typedef struct PerfTStruct {
  double n;
  double b;
  double p;
  double t;
  double size;
  double secs;
  double base;
  double core;
  double unco;
  double dram;
} PerfT;

#ifdef CRAN_COMPATIBLE
#  define MALLOC(n,typ) (typ *) Calloc((n),typ)
#  define FREE(x) Free(x)
#else
#  define MALLOC(n,typ) (typ *) malloc(sizeof(typ)*(n))
#  define FREE(x) free(x)
#endif

#ifdef	NDEBUG
#define	  assert(EX) ((void)0)
#define   debugprint(...)((void)0)
#else
#define	  assert(EX) (void)((EX) || (error("assert(" #EX ") file %s line %d", __FILE__, __LINE__), 0))
#define   debugprint(...) {Rprintf( __VA_ARGS__); R_FlushConsole();}
#endif	/* NDEBUG */

#ifdef SCANNED_RANGE
extern double scanned_range;
#endif


#endif
