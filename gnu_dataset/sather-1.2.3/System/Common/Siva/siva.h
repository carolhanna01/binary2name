/*------------------------->  ANSI C - headerfile  <-------------------------*/
/* Copyright (C) 199x by International Computer Science Institute            */
/* This file is part of the GNU Sather library. It is free software; you may */
/* redistribute  and/or modify it under the terms of the GNU Library General */
/* Public  License (LGPL)  as published  by the  Free  Software  Foundation; */
/* either version 3 of the license, or (at your option) any later version.   */
/* This  library  is distributed  in the  hope that it will  be  useful, but */
/* WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY */
/* or FITNESS FOR A PARTICULAR PURPOSE. See Doc/LGPL for more details.       */
/* The license text is also available from:  Free Software Foundation, Inc., */
/* 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                     */
/*------------>  Please email comments to <bug-sather@gnu.org>  <------------*/

#include <brahma.h>

/* Uncomment to turn on the heavy-weight statistics instumentation */
/* #define SI_STATISTICS */

/* Avoid conflict with Brahma's use of EXTERNAL macro */
#ifdef EXTERNAL
# undef EXTERNAL
#endif

#define K                       1024
#define M                       (K*K)
#define THOUSAND                1000
#define MILLION                 1000000

#if defined(SI_SERIAL_BOEHM)
# include "serial_boehm.h"
#elif defined(SI_SERIAL_SOLARIS)
# include "serial_solaris.h"
# include "zones.h"
#elif defined(SI_SERIAL_SOLARIS_HYPER)
# include "serial_solaris_hyper.h"
# include "zones.h"
#elif defined(SI_SMP_SOLARIS)
# include "smp_solaris.h"
# include "zones.h"
#elif defined(SI_SMP_WIN32)
# include "smp_win32.h"
# include "zones.h"
#elif defined(SI_SMP_LINUX)
# include "smp_linux.h"
# include "zones.h"
#else
# error Undefined platform in Siva.h
#endif
