/*
# greeNsort DIET count header
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_DIET_count_h
#define ALREADY_DEFINED_DIET_count_h

#include "algo.h"
#include <stdbool.h>

// // asymmetrically count Low+Tie vs. High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieLeft(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[0] = i-l;
//     c[1] = 0;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++)
//         if (LE(x[i], v))
//           c[0]++;
//         else
//           c[1]++;
//     }
//     return done;
// }
//
// // asymmetrically count  Low vs. Tie+High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieRight(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[1] = i-l;
//     c[0] = 0;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++)
//         if (GE(x[i], v))
//           c[1]++;
//         else
//           c[0]++;
//     }
//     return done;
// }


// // this version is faster although comparing twice (less branch misprediction)
// // asymmetrically count Low+Tie vs. High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieLeft(
//   ValueT *x  // pointer to data
// , IndexT l      // leftmost position
// , IndexT r      // rightmost position
// , ValueT v   // pivot value
// , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//   c[0] = i-l;
//   c[1] = 0;
//   if (i>r)
//     done = 1;
//   else{
//     done = 0;
//     // main loop
//     for (; i<=r; i++){
//       if (LE(x[i], v))
//         c[0]++;
//       if (GT(x[i], v))
//         c[1]++;
//     }
//   }
//   return done;
// }
//
// // asymmetrically count  Low vs. Tie+High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieRight(
//   ValueT *x  // pointer to data
// , IndexT l      // leftmost position
// , IndexT r      // rightmost position
// , ValueT v   // pivot value
// , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//   c[1] = i-l;
//   c[0] = 0;
//   if (i>r)
//     done = 1;
//   else{
//     done = 0;
//     // main loop
//     for (; i<=r; i++){
//       if (GE(x[i], v))
//         c[1]++;
//       if (LT(x[i], v))
//         c[0]++;
//     }
//   }
//   return done;
// }




// // this version is even faster, we store the comparision in a register
// // asymmetrically count Low+Tie vs. High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieLeft(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   bool b;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[0] = i-l;
//     c[1] = 0;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++){
//         b = GT(x[i], v);
//         c[b]++;
//       }
//     }
//     return done;
// }
//
// // asymmetrically count  Low vs. Tie+High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieRight(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   bool b;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[1] = i-l;
//     c[0] = 0;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++){
//         b = GE(x[i], v);
//         c[b]++;
//       }
//     }
//     return done;
// }



// // in this version we remove redundant counting (which also reduces branch-misprediction)
// // asymmetrically count Low+Tie vs. High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieLeft(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[0] = i-l;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++)
//         if (LE(x[i], v))
//           c[0]++;
//     }
//     c[1] = (r - l + 1) - c[0];
//     return done;
// }
//
// // asymmetrically count  Low vs. Tie+High
// // returns TRUE if all values equal the pivot
// static IndexT DIET_count_TieRight(
//     ValueT *x  // pointer to data
//   , IndexT l      // leftmost position
//   , IndexT r      // rightmost position
//   , ValueT v   // pivot value
//   , IndexT *c     // RETURNED: pointer to 2 counters
// ){
//   if (r<l)
//     return 1;
//   IndexT i, done;
//   // DIET loop
//   for (i=l; i<=r; i++)
//     if (NE(x[i], v))
//       break;
//     c[1] = i-l;
//     if (i>r)
//       done = 1;
//     else{
//       done = 0;
//       // main loop
//       for (; i<=r; i++)
//         if (GE(x[i], v))C
//           c[1]++;
//     }
//     c[0] = (r - l + 1) - c[1];
//     return done;
// }


// in this version we count on s single register, which improves locality, particularly for DIET_Pc
// asymmetrically count Low+Tie vs. High
// returns TRUE if all values equal the pivot
static IndexT DIET_count_TieLeft(
    ValueT *x  // pointer to data
  , IndexT l      // leftmost position
  , IndexT r      // rightmost position
  , ValueT v   // pivot value
  , IndexT *c     // RETURNED: pointer to 2 counters
){
  if (r<l)
    return 1;
  IndexT c0, i, done;
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(x[i], v))
      break;
  }
  c0 = i-l;
  if (i>r){
    done = 1;
  }else{
    done = 0;
    // main loop
    for (; i<=r; i++){
      if (LE(x[i], v))
        c0++;
    }
  }
  c[0] = c0;
  c[1] = (r - l + 1) - c0;
  return done;
}

// asymmetrically count  Low vs. Tie+High
// returns TRUE if all values equal the pivot
static IndexT DIET_count_TieRight(
    ValueT *x  // pointer to data
  , IndexT l      // leftmost position
  , IndexT r      // rightmost position
  , ValueT v   // pivot value
  , IndexT *c     // RETURNED: pointer to 2 counters
){
  if (r<l)
    return 1;
  IndexT c1, i, done;
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(x[i], v))
      break;
  }
  c1 = i-l;
  if (i>r){
    done = 1;
  }else{
    done = 0;
    // main loop
    for (; i<=r; i++){
      if (GE(x[i], v))
        c1++;
    }
  }
  c[1] = c1;
  c[0] = (r - l + 1) - c1;
  return done;
}


// symmetrically count above and below pivot
static IndexT DIET_count_Tri(
    ValueT *x  // pointer to data
  , IndexT l      // leftmost position
  , IndexT r      // rightmost position
  , ValueT v   // pivot value
  , IndexT *c     // RETURNED: pointer to 2 counters
){
  IndexT c0=0, c1=0, i, done;
  // DIET loop
  for (i=l; i<=r; i++){
    if (NE(x[i], v))
      break;
  }
  if (i>r)
    done = 1;
  else
    done = 0;
  // MAIN loop
  for (; i<=r; i++){
    if (LT(x[i], v))
      c0++;
    else if (GT(x[i], v))
      c1++;
  }
  c[0] = c0;
  c[1] = c1;
  return done;
}

#endif
