/* Copyright (C) 1997, 1998 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: gxhttype.h,v $ $Revision: 1.2.2.1 $ */
/* Client halftone type enumeration */

#ifndef gxhttype_INCLUDED
#  define gxhttype_INCLUDED

/* Halftone types */
typedef enum {
    ht_type_none,		/* is this needed? */
    ht_type_screen,		/* set by setscreen */
    ht_type_colorscreen,	/* set by setcolorscreen */
    ht_type_spot,		/* Type 1 halftone dictionary */
    ht_type_threshold,		/* Type 3 halftone dictionary */
    ht_type_threshold2,		/* Extended Type 3 halftone dictionary */
				/* (Type 3 with either 8- or 16-bit */
				/* samples, bytestring instead of string */
				/* thresholds, and 1 or 2 rectangles) */
    ht_type_multiple,		/* Type 5 halftone dictionary */
    ht_type_multiple_colorscreen,  /* Type 5 halftone dictionary */
				/* created from Type 2 or Type 4 */
				/* halftone dictionary  */
    ht_type_client_order	/* client-defined, creating a gx_ht_order */
} gs_halftone_type;

#endif /* gxhttype_INCLUDED */
