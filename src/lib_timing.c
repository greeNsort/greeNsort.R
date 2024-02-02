/*
# greeNsort C-Code for timing
# Copyright (C) 2010-2024 Dr. Jens Oehlschaegel
# BSD-3 clause
# Provided 'as is', use at your own risk
*/

#define _GREENSORT_TIMING_C_SRC


#if defined(WIN32) || defined(WIN64) || defined(_WIN32_) || defined(_WIN64_) || defined(__WIN32__) || defined(__WIN64__)


/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/


#include "lib_timing.h"


/*****************************************************************************/
/**                                                                         **/
/**                      DEFINITIONS AND MACROS                             **/
/**                                                                         **/
/*****************************************************************************/

#include <windows.h>
#include <Winbase.h>  /* for GetTickCount64 */


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

static int64_t TickOrigin;
static int64_t TicksPerSec;
static int64_t TicksPaused;
static double SecsSnapped;


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

/* no extern */


void initTicks(){
  LARGE_INTEGER li;
  QueryPerformanceFrequency(&li);
  TicksPerSec = li.QuadPart;
  QueryPerformanceCounter(&li);
  TickOrigin = li.QuadPart;
}

void resetTicks(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  TickOrigin = li.QuadPart;
  /* TickOrigin = get_tick_count(); */
}

void doneTicks(){
}

int64_t getTicksPerSec(){
  LARGE_INTEGER li;
  QueryPerformanceFrequency(&li);
  TicksPerSec = li.QuadPart;
  return TicksPerSec;
}

int64_t getNewTicks(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  /* return get_tick_count() - TickOrigin; */
  return li.QuadPart - TickOrigin;
}

double getNewSecs(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  /* return get_tick_count() - TickOrigin; */
  return (li.QuadPart - TickOrigin)/((double)TicksPerSec);
}

double getDeltaSecs(){
  LARGE_INTEGER li;
  QueryPerformanceCounter(&li);
  /* return get_tick_count() - TickOrigin; */
  double ret = (li.QuadPart - TickOrigin)/((double)TicksPerSec);
  TickOrigin = li.QuadPart;
  return ret;
}

void pauseTicks(){
  TicksPaused = getNewTicks();
}
void resumeTicks(){
  resetTicks();
  TickOrigin -= TicksPaused;
}

void setSecsSnapped(){
    SecsSnapped = getNewSecs();
}
double getSecsSnapped(){
    return SecsSnapped;
}

void usleep(useconds_t usec)
{
  HANDLE timer;
  LARGE_INTEGER ft;

  ft.QuadPart = -(10 * (__int64)usec);

  timer = CreateWaitableTimer(NULL, TRUE, NULL);
  SetWaitableTimer(timer, &ft, 0, NULL, NULL, 0);
  WaitForSingleObject(timer, INFINITE);
  CloseHandle(timer);
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

#else

/*****************************************************************************/
/**                                                                         **/
/**                            MODULES USED                                 **/
/**                                                                         **/
/*****************************************************************************/

#include "lib_timing.h"


#include <stdio.h>
#include <signal.h>
#include <time.h>
#define THOUSAND 1000.0
#define MILLION 1000000.0
#define BILLION 1000000000.0

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

static timer_t time_ID;
static struct itimerspec hightimer;
static struct itimerspec lowtimer;


/*****************************************************************************/
/**                                                                         **/
/**                        EXPORTED FUNCTIONS                               **/
/**                                                                         **/
/*****************************************************************************/

/* no extern */


void resetTicks(){
    hightimer.it_interval.tv_sec = 0;
    hightimer.it_interval.tv_nsec = 0;
    hightimer.it_value.tv_sec = MILLION; /* a large number */
    hightimer.it_value.tv_nsec = 0;
    timer_settime(time_ID, 0, &hightimer, NULL);
}


void initTicks(){
    if (timer_create(CLOCK_REALTIME, NULL, &time_ID) != 0) {
      error("Could not create a timer based on CLOCK_REALTIME");
    }
    resetTicks();
}

void doneTicks(){
    if (timer_delete(time_ID) != 0) {
      error("Could not delete a timer based on CLOCK_REALTIME");
    }
}

int64_t getTicksPerSec(){
  return MILLION;
}

int64_t getNewTicks(){
  timer_gettime(time_ID, &lowtimer);
  return MILLION*(hightimer.it_value.tv_sec - lowtimer.it_value.tv_sec) + (hightimer.it_value.tv_nsec - lowtimer.it_value.tv_nsec)/THOUSAND;
}

double getNewSecs(){
  timer_gettime(time_ID, &lowtimer);
  return (hightimer.it_value.tv_sec - lowtimer.it_value.tv_sec) + (hightimer.it_value.tv_nsec - lowtimer.it_value.tv_nsec)/BILLION;
}

double getDeltaSecs(){
  timer_gettime(time_ID, &lowtimer);
  double ret = (hightimer.it_value.tv_sec - lowtimer.it_value.tv_sec) + (hightimer.it_value.tv_nsec - lowtimer.it_value.tv_nsec)/BILLION;
  hightimer.it_interval.tv_sec = 0;
  hightimer.it_interval.tv_nsec = 0;
  hightimer.it_value.tv_sec = MILLION; /* a large number */
  hightimer.it_value.tv_nsec = 0;
  timer_settime(time_ID, 0, &hightimer, NULL);
return ret;
}

void pauseTicks(){
  timer_gettime(time_ID, &lowtimer);
  if (timer_delete(time_ID) != 0) {
    error("Could not delete a timer based on CLOCK_REALTIME");
  }
}
void resumeTicks(){
  if (timer_create(CLOCK_REALTIME, NULL, &time_ID) != 0) {
    error("Could not create a timer based on CLOCK_REALTIME");
  }
  timer_settime(time_ID, 0, &lowtimer, NULL);
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

#undef THOUSAND
#undef MILLION
#undef BILLION

#endif


