/* event.c - Event table
   Copyright (C) 2001 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
   02111-1307, USA.  */

/* Written by Marc Tardif <intmktg@cam.org>.  */

/* The table in this program defines a subset of events.  For each
   possible input character, a smaller set of events is returned.

   The same table is also used for matching the first character of
   each pattern.  For this reason, each event is actually a pair
   where even numbered events (see event.h) also indicate a pattern
   match.
*/

#include "event.h"

unsigned char event[] =
{
  NU, UN, UN, UN, UN, UN, UN, UN, UN, SP, NL, SP, NP, SP, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  SP, UN, DQ, UN, UN, UN, AM, SQ, UN, UN, UN, UN, UN, DA, UN, UN,
  DI, DI, DI, DI, DI, DI, DI, DI, DI, DI, UN, UN, LT, UN, GT, UN,
  UN, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, 
  LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, UN, UN, UN, UN, UN,
  UN, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, 
  LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, LE, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  SP, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN,
  UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN, UN
};

