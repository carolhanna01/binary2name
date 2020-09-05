/* Copyright (C) 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gsparamx.h,v $ $Revision: 1.3.2.1 $ */
/* Interface to extended parameter dictionary utilities */

#ifndef gsparamx_INCLUDED
#  define gsparamx_INCLUDED

/* Test whether a parameter's string value is equal to a C string. */
bool gs_param_string_eq(P2(const gs_param_string *pcs, const char *str));

/*
 * Put parameters of various types.  These propagate ecode, presumably
 * the previous accumulated error code.
 */
int param_put_enum(P5(gs_param_list * plist, gs_param_name param_name,
		      int *pvalue, const char *const pnames[], int ecode));
int param_put_bool(P4(gs_param_list * plist, gs_param_name param_name,
		      bool * pval, int ecode));
int param_put_int(P4(gs_param_list * plist, gs_param_name param_name,
		     int * pval, int ecode));
int param_put_long(P4(gs_param_list * plist, gs_param_name param_name,
		      long * pval, int ecode));

/* Copy one parameter list to another, recursively if necessary. */
int param_list_copy(P2(gs_param_list *plto, gs_param_list *plfrom));

#endif /* gsparamx_INCLUDED */
