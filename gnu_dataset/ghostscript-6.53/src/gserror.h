/* Copyright (C) 1994, 1997, 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gserror.h,v $ $Revision: 1.2.2.1 $ */
/* Error return macros */

#ifndef gserror_INCLUDED
#  define gserror_INCLUDED

int gs_log_error(P3(int, const char *, int));
#ifndef DEBUG
#  define gs_log_error(err, file, line) (err)
#endif
#define gs_note_error(err) gs_log_error(err, __FILE__, __LINE__)
#define return_error(err) return gs_note_error(err)
#define return_if_error(expr)\
  BEGIN int code_ = (expr); if ( code_ < 0 ) return code_; END

#endif /* gserror_INCLUDED */
