/*
# greeNsort Header-Code for energy measurement
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#define _GREENSORT_ENERGY_C_SRC


/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/


#include <stdio.h>
#include <R.h>  // needed here for definition of error()
#include "lib_energy.h"


/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#define ERROR_RETURN(retval) { error("Error %s %s:line %d: \n", retval,__FILE__,__LINE__);  }

/*****************************************************************************/
/**                                                                         **/
/**                      TYPEDEFS AND STRUCTURES                            **/
/**                                                                         **/
/*****************************************************************************/

/*****************************************************************************/
/**                                                                         **/
/**                   PROTOTYPYPES OF LOCAL FUNCTIONS                       **/
/**                                                                         **/
/*****************************************************************************/

/* static */


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

/* no static no extern */



/*****************************************************************************/
/**                                                                         **/
/**                          GLOBAL VARIABLES                               **/
/**                                                                         **/
/*****************************************************************************/

/* static */


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

/* no extern */

PCapEnergyT GreensortEnergyMax(){
  FILE* ptr;

  PCapEnergyT energy_uj;

#ifdef PATH_POWERCAP_PACKAGE
  ptr = fopen(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.package)) != 1)
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ);
  fclose(ptr);
#else
  energy_uj.package = 0;
#endif


#ifdef PATH_POWERCAP_CORE
  ptr = fopen(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.core)) != 1)
    ERROR_RETURN(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ);
  fclose(ptr);
#else
  energy_uj.core = 0;
#endif

#ifdef PATH_POWERCAP_UNCORE
  ptr = fopen(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.uncore)) != 1)
    ERROR_RETURN(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ);
  fclose(ptr);
#else
  energy_uj.uncore = 0;
#endif

#ifdef PATH_POWERCAP_DRAM
  ptr = fopen(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.dram)) != 1)
    ERROR_RETURN(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ);
  fclose(ptr);
#else
  energy_uj.dram = 0;
#endif

  return energy_uj;
}


PCapEnergyT GreensortEnergyNow(){
  FILE* ptr;

  PCapEnergyT energy_uj;

#ifdef PATH_POWERCAP_PACKAGE
  ptr = fopen(FILE_POWERCAP_PACKAGE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.package)) != 1)
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj.package = 0;
#endif

#ifdef PATH_POWERCAP_CORE
  ptr = fopen(FILE_POWERCAP_CORE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_CORE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.core)) != 1)
    ERROR_RETURN(FILE_POWERCAP_CORE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj.core = 0;
#endif

#ifdef PATH_POWERCAP_UNCORE
  ptr = fopen(FILE_POWERCAP_UNCORE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_UNCORE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.uncore)) != 1)
    ERROR_RETURN(FILE_POWERCAP_UNCORE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj.uncore = 0;
#endif

#ifdef PATH_POWERCAP_DRAM
  ptr = fopen(FILE_POWERCAP_DRAM_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_DRAM_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj.dram)) != 1)
    ERROR_RETURN(FILE_POWERCAP_DRAM_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj.dram = 0;
#endif

  return energy_uj;
}


GreensortEnergyT GreensortEnergyDelta(PCapEnergyT *energy_uj){
  FILE* ptr;

  PCapEnergyT last_uj, max_energy_range_uj;
  GreensortEnergyT green_uj;

  // secure old values
  last_uj.package = energy_uj->package;
  last_uj.core = energy_uj->core;
  last_uj.uncore = energy_uj->uncore;
  last_uj.dram = energy_uj->dram;

  // read new values into energy_uj
#ifdef PATH_POWERCAP_PACKAGE
  ptr = fopen(FILE_POWERCAP_PACKAGE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj->package)) != 1)
    ERROR_RETURN(FILE_POWERCAP_PACKAGE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj->package = 0;
#endif

#ifdef PATH_POWERCAP_CORE
  ptr = fopen(FILE_POWERCAP_CORE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_CORE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj->core)) != 1)
    ERROR_RETURN(FILE_POWERCAP_CORE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj->core = 0;
#endif

#ifdef PATH_POWERCAP_UNCORE
  ptr = fopen(FILE_POWERCAP_UNCORE_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_UNCORE_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj->uncore)) != 1)
    ERROR_RETURN(FILE_POWERCAP_UNCORE_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj->uncore = 0;
#endif

#ifdef PATH_POWERCAP_DRAM
  ptr = fopen(FILE_POWERCAP_DRAM_ENERGY_UJ, "r");
  if (ptr == NULL) {
    ERROR_RETURN(FILE_POWERCAP_DRAM_ENERGY_UJ);
  }
  if (fscanf(ptr, "%lf", &(energy_uj->dram)) != 1)
    ERROR_RETURN(FILE_POWERCAP_DRAM_ENERGY_UJ);
  fclose(ptr);
#else
  energy_uj->dram = 0;
#endif

  // handle wrap-around in last_uj assuming it will not happen twice
  // energy_uj remains unchanged
#ifdef PATH_POWERCAP_PACKAGE
  if (energy_uj->package < last_uj.package){
    ptr = fopen(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ, "r");
    if (ptr == NULL) {
      ERROR_RETURN(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ);
    }
    if (fscanf(ptr, "%lf", &(max_energy_range_uj.package)) != 1)
      ERROR_RETURN(FILE_POWERCAP_PACKAGE_MAX_ENERGY_RANGE_UJ);
    fclose(ptr);
    last_uj.package -= max_energy_range_uj.package;
  }
#endif

#ifdef PATH_POWERCAP_CORE
  if (energy_uj->core < last_uj.core){
    ptr = fopen(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ, "r");
    if (ptr == NULL) {
      ERROR_RETURN(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ);
    }
    if (fscanf(ptr, "%lf", &(max_energy_range_uj.core)) != 1)
      ERROR_RETURN(FILE_POWERCAP_CORE_MAX_ENERGY_RANGE_UJ);
    fclose(ptr);
    last_uj.core -= max_energy_range_uj.core;
  }
#endif

#ifdef PATH_POWERCAP_UNCORE
  if (energy_uj->uncore < last_uj.uncore){
    ptr = fopen(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ, "r");
    if (ptr == NULL) {
      ERROR_RETURN(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ);
    }
    if (fscanf(ptr, "%lf", &(max_energy_range_uj.uncore)) != 1)
      ERROR_RETURN(FILE_POWERCAP_UNCORE_MAX_ENERGY_RANGE_UJ);
    fclose(ptr);
    last_uj.uncore -= max_energy_range_uj.uncore;
  }
#endif

#ifdef PATH_POWERCAP_DRAM
  if (energy_uj->dram < last_uj.dram){
    ptr = fopen(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ, "r");
    if (ptr == NULL) {
      ERROR_RETURN(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ);
    }
    if (fscanf(ptr, "%lf", &(max_energy_range_uj.dram)) != 1)
      ERROR_RETURN(FILE_POWERCAP_DRAM_MAX_ENERGY_RANGE_UJ);
    fclose(ptr);
    last_uj.dram -= max_energy_range_uj.dram;
  }
#endif

  // put differences in last_uj
  last_uj.package = energy_uj->package - last_uj.package;
  last_uj.core = energy_uj->core - last_uj.core;
  last_uj.uncore = energy_uj->uncore - last_uj.uncore;
  last_uj.dram = energy_uj->dram - last_uj.dram;

  // properly break down and scale
  //green_uj.total = (last_uj.package + last_uj.dram) / 1000000;
  // =
  green_uj.base = (last_uj.package - last_uj.core - last_uj.uncore) / 1000000;
  // +
  green_uj.core = last_uj.core / 1000000;
  // +
  green_uj.unco = last_uj.uncore / 1000000;
  // +
  green_uj.dram = last_uj.dram / 1000000;;

  return green_uj;
}



/*****************************************************************************/
/**                                                                         **/
/**                           LOCAL FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

/* static */


/*****************************************************************************/
/**                                                                         **/
/**                                EOF                                      **/
/**                                                                         **/
/*****************************************************************************/

