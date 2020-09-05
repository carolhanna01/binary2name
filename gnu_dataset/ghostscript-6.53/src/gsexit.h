/* Copyright (C) 1994 Aladdin Enterprises.  All rights reserved.
  
  This file is part of GNU Ghostscript.
  
  GNU Ghostscript is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
  to anyone for the consequences of using it or for whether it serves any
  particular purpose or works at all, unless he says so in writing.  Refer
  to the GNU General Public License for full details.
  
  Everyone is granted permission to copy, modify and redistribute GNU
  Ghostscript, but only under the conditions described in the GNU General
  Public License.  A copy of this license is supposed to have been given
  to you along with GNU Ghostscript so you can know your rights and
  responsibilities.  It should be in a file named COPYING.  Among other
  things, the copyright notice and this notice must be preserved on all
  copies.
*/

/*$RCSfile: gsexit.h,v $ $Revision: 1.2.2.1 $ */
/* Declarations for exits */

#ifndef gsexit_INCLUDED
#  define gsexit_INCLUDED

void gs_exit_with_code(P2(int exit_status, int code));
void gs_exit(P1(int exit_status));

#define gs_abort() gs_exit(1)

/* The only reason we export gs_exit_status is so that window systems */
/* with alert boxes can know whether to pause before exiting if */
/* the program terminates with an error.  There must be a better way .... */
extern int gs_exit_status;

#endif /* gsexit_INCLUDED */
