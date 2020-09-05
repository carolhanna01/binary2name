/* Copyright (C) 1994, 1996, 1999 Aladdin Enterprises.  All rights reserved.
  
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

/*$RCSfile: icolor.h,v $ $Revision: 1.2.2.1 $ */
/* Declarations for transfer function & similar cache remapping */

#ifndef icolor_INCLUDED
#  define icolor_INCLUDED

/*
 * Define the number of stack slots needed for zcolor_remap_one.
 * The client is responsible for doing check_e/ostack or the equivalent
 * before calling zcolor_remap_one.
 */
extern const int zcolor_remap_one_ostack;
extern const int zcolor_remap_one_estack;

/*
 * Schedule the sampling and reloading of a cache.  Note that if
 * zcolor_remap_one recognize the procedure as being of a special form, it
 * may not schedule anything, but it still returns o_push_estack.  (This is
 * a change as of release 5.95; formerly, it returned 0 in this case.)
 */
int zcolor_remap_one(P5(i_ctx_t *, const ref *, gx_transfer_map *,
			const gs_state *, op_proc_t));

/* Reload a cache with entries in [0..1] after sampling. */
int zcolor_remap_one_finish(P1(i_ctx_t *));

/* Reload a cache with entries in [-1..1] after sampling. */
int zcolor_remap_one_signed_finish(P1(i_ctx_t *));

/* Recompute the effective transfer functions and invalidate the current */
/* color after cache reloading. */
int zcolor_reset_transfer(P1(i_ctx_t *));

/* Invalidate the current color after cache reloading. */
int zcolor_remap_color(P1(i_ctx_t *));

#endif /* icolor_INCLUDED */
