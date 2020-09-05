/* profiler.h: -*- C -*-  Structures used in the Meta-HTMl profiler. */

/*  Copyright (c) 1997 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Mon May 12 14:29:02 1997.  */

#if !defined (_PROFILER_H_)
#define _PROFILER_H_

#if defined (__cplusplus)
extern "C"
{
#endif

typedef struct
{
  long times_called;	/* Number of times this function has been invoked. */
  double usecs_spent;	/* Number of microseconds spent in this function. */
} PROFILE_INFO;

#if defined (__cplusplus)
}
#endif

#endif
