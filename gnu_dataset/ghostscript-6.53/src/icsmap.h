/* Copyright (C) 1994, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: icsmap.h,v $ $Revision: 1.2.2.1 $ */
/* Interface to shared routines for loading the cached color space maps. */

#ifndef icsmap_INCLUDED
#  define icsmap_INCLUDED

/*
 * Set up to load a cached map for an Indexed or substituted Separation
 * color space.  The implementation is in zcsindex.c.  When the map1
 * procedure is called, the following structure is on the e_stack:
 */
#define num_csme 5
#  define csme_num_components (-4)	/* t_integer */
#  define csme_map (-3)		/* t_struct (bytes) */
#  define csme_proc (-2)	/* -procedure- */
#  define csme_hival (-1)	/* t_integer */
#  define csme_index 0		/* t_integer */
/*
 * Note that the underlying color space parameter is a direct space, not a
 * base space, since the underlying space of an Indexed color space may be
 * a Separation or DeviceN space.
 */
int zcs_begin_map(P6(i_ctx_t *i_ctx_p, gs_indexed_map ** pmap,
		     const ref * pproc, int num_entries,
		     const gs_direct_color_space * base_space,
		     op_proc_t map1));

#endif /* icsmap_INCLUDED */
