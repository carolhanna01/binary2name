/* Compute the number of business days needed to respond to a PR.
   Copyright (C) 1993, 1994, 1995, 2007 Free Software Foundation, Inc.
   Contributed by Tim Wicinski (wicinski@barn.com).

This file is part of GNU GNATS.

GNU GNATS is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU GNATS is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GNATS; see the file COPYING.  If not, see
<http://www.gnu.org/licenses/>.
*/

#include "gnats.h"

#define HOURS_WORK_WEEK ((businessDayEnd (database) - businessDayStart (database)) * (businessWeekEnd (database) - businessWeekStart (database)+1))
#define OFFHOURS (24 - (businessDayEnd (database) - businessDayStart (database)))

#define SECONDS_DAY 86400	/* 24 * 60 * 60 */
#define SECONDS_HOUR 3600	/* 60 * 60 */
#define SECONDS_WEEK (SECONDS_DAY * 7)

struct tm *
get_response_time (const DatabaseInfo database, unsigned int rtime)
{
  struct tm *t;
  static struct tm res_t;
  time_t seconds;
  unsigned int err, wday, hour;

  seconds = time (0);
  t = localtime (&seconds);

  wday = t->tm_wday;
  hour = t->tm_hour;

  /* Round it out to the nearest hour.  */
  err = 60 - t->tm_min;
  if (err <= 30)
    {
      seconds += (err * 60);
      hour++;
    }
  else
    seconds -= (t->tm_min * 60);

  /* FIXME - This will compute the response time, but it is a bit 
     overwrought with bloat.  This can be trimmed down.  */

  /* Skip ahead by weeks */
  seconds += SECONDS_WEEK * (rtime / HOURS_WORK_WEEK);
  rtime %= HOURS_WORK_WEEK;
  
  /* Check when the bug report arrived; if PR arrives before bweek starts,
     add time until start of business week.  */

  /* add in the hours from the night before.  do this before 
     checking for end of week in case bug is marked at WEEK_END,
     after businessDayEnd () (ie, after friday at 5). */
  if (hour >= businessDayEnd (database))
    {
      seconds += ((24 - hour) + businessDayStart (database)) * SECONDS_HOUR;
      hour = businessDayStart (database);
      wday++;
    }
  
  /* If PR arrives before bday starts, then do some simple rounding
     up to the beginning of the business day. */
  else if (hour < businessDayStart (database))
    {
      seconds += (businessDayStart (database) - hour) * SECONDS_HOUR;
      hour = businessDayStart (database);
    }
  
  while (1)
    {
      /* Same as above, but the end of week is slightly different.  */
      if (wday > businessWeekEnd (database))
	{
	  /* Same as addition above, but compute end of week and
	     beginning of week seperately for now (offdays?). */
	  seconds += ((wday - businessWeekEnd (database)) 
		      + businessWeekStart (database))
	    * SECONDS_DAY;
	  wday = businessWeekStart (database);
	}
      
      /* Add up here, to the end of the first day, the number of days until
	 the bweek starts, and then the hours until the bday begins.  */
      else if (wday < businessWeekStart (database))
	{
	  seconds += (businessWeekStart (database) - wday) * SECONDS_DAY;
	  wday = businessWeekStart (database);
	}
      
      /* Actual computing of the business hours. Note that some response
	 times might stretch into the weekend (thus the loop).  */
      err = businessDayEnd (database) - hour;
      if (err > 0 && err < rtime)
	{
	  /* The # of hours is more than are left today.  */
	  rtime -= err;

	  /* Skip to the next day.  */
	  seconds += ((err + OFFHOURS) * SECONDS_HOUR);
	  hour = businessDayStart (database);
	  wday++;
	}
      else
	{
	  seconds += rtime * SECONDS_HOUR;
	  break;
	}

    }

  memcpy (&res_t, localtime (&seconds), sizeof (struct tm));
  return &res_t;
}
