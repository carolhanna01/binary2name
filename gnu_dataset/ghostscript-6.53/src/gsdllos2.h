/* Copyright (C) 1994-1996, Russell Lang.  All rights reserved.
  
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

/*$RCSfile: gsdllos2.h,v $ $Revision: 1.2.2.1 $ */
/* gsdll extension for OS/2 platforms */

#ifndef gsdllos2_INCLUDED
#  define gsdllos2_INCLUDED

/* DLL exported functions */
/* for load time dynamic linking */
unsigned long gsdll_get_bitmap(unsigned char *device, unsigned char **pbitmap);

/* Function pointer typedefs */
/* for run time dynamic linking */
typedef long (*GSDLLAPI PFN_gsdll_get_bitmap) (unsigned char *, unsigned char **);

#endif /* gsdllos2_INCLUDED */
