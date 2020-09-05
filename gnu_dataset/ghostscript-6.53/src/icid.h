/* Copyright (C) 2000 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: icid.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to zcid.c */

#ifndef icid_INCLUDED
#  define icid_INCLUDED

/* Get the information from a CIDSystemInfo dictionary. */
int cid_system_info_param(P2(gs_cid_system_info_t *, const ref *));

#endif /* icid_INCLUDED */
