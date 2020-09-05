/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __STD_H

#define __STD_H



#include "config.h"



#include <stdio.h>

#include <sys/times.h>

#include <sys/types.h>

#include <stdlib.h>

#include <signal.h>

#include <unistd.h>

#include <time.h>



#if TIMEVAL

struct timeval

  {

    time_t tv_sec;              /* Seconds.  */

    time_t tv_usec;             /* Microseconds.  */

  };

#endif



#endif /* __STD_H */

