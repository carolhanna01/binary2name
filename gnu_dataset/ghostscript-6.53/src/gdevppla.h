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

/*$RCSfile: gdevppla.h,v $ $Revision: 1.2.2.1 $ */
/* Support for printer devices with planar buffering. */
/* Requires gdevprn.h */

#ifndef gdevppla_INCLUDED
#  define gdevppla_INCLUDED

/* Set the buf_procs in a printer device to planar mode. */
int gdev_prn_set_procs_planar(P1(gx_device *pdev));

/* Open a printer device, conditionally setting it to be planar. */
int gdev_prn_open_planar(P2(gx_device *pdev, bool upb));

/* Augment get/put_params to add UsePlanarBuffer. */
int gdev_prn_get_params_planar(P3(gx_device * pdev, gs_param_list * plist,
				  bool *pupb));
int gdev_prn_put_params_planar(P3(gx_device * pdev, gs_param_list * plist,
				  bool *pupb));

/* Create a planar buffer device. */
/* Use this instead of the default if UsePlanarBuffer is true. */
int gdev_prn_create_buf_planar(P5(gx_device **pbdev, gx_device *target,
				  const gx_render_plane_t *render_plane,
				  gs_memory_t *mem, bool for_band));

/* Determine the space needed by a planar buffer device. */
/* Use this instead of the default if UsePlanarBuffer is true. */
int gdev_prn_size_buf_planar(P5(gx_device_buf_space_t *space,
				gx_device *target,
				const gx_render_plane_t *render_plane,
				int height, bool for_band));

#endif /* gdevppla_INCLUDED */
