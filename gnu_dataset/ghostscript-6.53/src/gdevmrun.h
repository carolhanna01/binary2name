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

/*$RCSfile: gdevmrun.h,v $ $Revision: 1.2.2.1 $ */
/* Definition of run-length encoded memory device */

#ifndef gdevmrun_INCLUDED
#  define gdevmrun_INCLUDED

/*
 * This memory device stores full-size pixels with run-length
 * encoding if possible, switching to the standard uncompressed
 * representation if necessary.
 */

#include "gxdevmem.h"

/*
 * Define the device, built on a memory device.
 */
typedef struct gx_device_run_s {
    gx_device_memory md;	/* must be first */
    uint runs_per_line;
    int umin, umax1;		/* some range of uninitialized lines */
    int smin, smax1;		/* some range in standard (not run) form */
    /*
     * Save memory device procedures that we replace with run-oriented
     * ones, for use with the uncompressed representation.
     */
    struct sp_ {
	dev_proc_copy_mono((*copy_mono));
	dev_proc_copy_color((*copy_color));
	dev_proc_fill_rectangle((*fill_rectangle));
	dev_proc_copy_alpha((*copy_alpha));
	dev_proc_strip_tile_rectangle((*strip_tile_rectangle));
	dev_proc_strip_copy_rop((*strip_copy_rop));
	dev_proc_get_bits_rectangle((*get_bits_rectangle));
    } save_procs;
} gx_device_run;

/*
 * Convert a memory device to run-length form.  The mdev argument should be
 * const, but it isn't because we need to call gx_device_white.
 */
int gdev_run_from_mem(P2(gx_device_run *rdev, gx_device_memory *mdev));

#endif /* gdevmrun_INCLUDED */
