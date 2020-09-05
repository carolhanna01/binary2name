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

/*$RCSfile: gdevdcrd.h,v $ $Revision: 1.2.2.1 $ */
/* Interface for creating a sample device CRD */

#ifndef gdevdcrd_INCLUDED
#define gdevdcrd_INCLUDED

/* Implement get_params for a sample device CRD. */
int sample_device_crd_get_params(P3(gx_device *pdev, gs_param_list *plist,
				    const char *crd_param_name));

#endif	/* gdevdcrd_INCLUDED */
