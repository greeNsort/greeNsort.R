/*
# greeNsort Header-Code for energy measurement
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_lib_energy_h
#define ALREADY_DEFINED_lib_energy_h
#ifdef __cplusplus
 extern "C" {
#endif

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

#include "powercap_config.h"

/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#define MAX_ENERGY_RANGE_UJ "max_energy_range_uj"
#define ENERGY_UJ "energy_uj"
#define RAPL_NAME "name"

#define PATH_EMPTY ""

#ifdef PATH_POWERCAP_PACKAGE
#define FILE_POWERCAP_PACKAGE_NAME PATH_POWERCAP_PACKAGE RAPL_NAME
#endif
#ifdef PATH_POWERCAP_CORE
#define FILE_POWERCAP_CORE_NAME PATH_POWERCAP_CORE RAPL_NAME
#endif
#ifdef PATH_POWERCAP_UNCORE
#define FILE_POWERCAP_UNCORE_NAME PATH_POWERCAP_UNCORE RAPL_NAME
#endif
#ifdef PATH_POWERCAP_DRAM
#define FILE_POWERCAP_DRAM_NAME PATH_POWERCAP_DRAM RAPL_NAME
#endif

#ifdef PATH_POWERCAP_PACKAGE
#define FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ PATH_POWERCAP_PACKAGE MAX_ENERGY_RANGE_UJ
#endif
#ifdef PATH_POWERCAP_CORE
#define FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ PATH_POWERCAP_CORE MAX_ENERGY_RANGE_UJ
#endif
#ifdef PATH_POWERCAP_UNCORE
#define FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ PATH_POWERCAP_UNCORE MAX_ENERGY_RANGE_UJ
#endif
#ifdef PATH_POWERCAP_DRAM
#define FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ PATH_POWERCAP_DRAM MAX_ENERGY_RANGE_UJ
#endif

#ifdef PATH_POWERCAP_PACKAGE
#define FILE_POWERCAP_PACKAGE_ENERGY_UJ PATH_POWERCAP_PACKAGE ENERGY_UJ
#endif
#ifdef PATH_POWERCAP_CORE
#define FILE_POWERCAP_CORE_ENERGY_UJ PATH_POWERCAP_CORE ENERGY_UJ
#endif
#ifdef PATH_POWERCAP_UNCORE
#define FILE_POWERCAP_UNCORE_ENERGY_UJ PATH_POWERCAP_UNCORE ENERGY_UJ
#endif
#ifdef PATH_POWERCAP_DRAM
#define FILE_POWERCAP_DRAM_ENERGY_UJ PATH_POWERCAP_DRAM ENERGY_UJ
#endif


/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/


typedef struct PCapEnergyTStruct {
  double package;
  double core;
  double uncore;
  double dram;
} PCapEnergyT;

typedef struct GreensortEnergyTStruct {
  double base;
  double core;
  double unco;
  double dram;
} GreensortEnergyT;

/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

#ifndef _GREENSORT_ENERGY_C_SRC

#endif


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

extern PCapEnergyT GreensortEnergyMax();
extern PCapEnergyT GreensortEnergyNow();
extern GreensortEnergyT GreensortEnergyDelta(PCapEnergyT *energy_uj);

#ifdef __cplusplus
 }
#endif

#endif

/*****************************************************************************/
/**                                                                         **/
/**                                DEMO                                      **/
/**

#include <stdio.h>
#include <unistd.h>
#include "lib_energy.h"

 int main()
 {
 PCapEnergyT pe;
 GreensortEnergyT ge;

 pe = GreensortEnergyNow();

 sleep(2);

 ge = GreensortEnergyDelta(&pe);

 printf("\npowercap_package_energy_uj = %lf\n", pe.package);
 printf("  powercap_core_energy_uj = %lf\n", pe.core);
 printf("  powercap_uncore_energy_uj = %lf\n", pe.uncor);
 printf("   powercap_dram_energy_uj = %lf\n", pe.dram);

 printf("greensort_energy_basic_j = %lf\n", ge.base);
 printf("greensort_energy_core_j = %lf\n", ge.core);
 printf("greensort_energy_unco_j = %lf\n", ge.unco);
 printf(" greensort_energy_dram_j = %lf\n", ge.dram);

 exit(0);
 }

**/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/

