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

/*$RCSfile: gxband.h,v $ $Revision: 1.2.2.1 $ */
/* Band-processing parameters for Ghostscript */

#ifndef gxband_INCLUDED
#  define gxband_INCLUDED

#include "gxclio.h"

/*
 * Define the parameters controlling banding.
 */
typedef struct gx_band_params_s {
    int BandWidth;		/* (optional) band width in pixels */
    int BandHeight;		/* (optional) */
    long BandBufferSpace;	/* (optional) */
} gx_band_params_t;

#define BAND_PARAMS_INITIAL_VALUES 0, 0, 0

/*
 * Define information about the colors used on a page.
 */
typedef struct gx_colors_used_s {
    gx_color_index or;		/* the "or" of all the used colors */
    bool slow_rop;		/* true if any RasterOps that can't be */
				/* executed plane-by-plane on CMYK devices */
} gx_colors_used_t;

/*
 * We want to store color usage information for each band in the page_info
 * structure, but we also want this structure to be of a fixed (and
 * reasonable) size.  We do this by allocating a fixed number of colors_used
 * structures in the page_info structure, and if there are more bands than
 * we have allocated, we simply reduce the precision of the information by
 * letting each colors_used structure cover multiple bands.
 *
 * 30 entries would be large enough to cover A4 paper (11.3") at 600 dpi
 * with 256-scan-line bands.  We pick 50 somewhat arbitrarily.
 */
#define PAGE_INFO_NUM_COLORS_USED 50

/*
 * Define the information for a saved page.
 */
typedef struct gx_band_page_info_s {
    char cfname[gp_file_name_sizeof];	/* command file name */
    clist_file_ptr cfile;	/* command file, normally 0 */
    char bfname[gp_file_name_sizeof];	/* block file name */
    clist_file_ptr bfile;	/* block file, normally 0 */
    uint tile_cache_size;	/* size of tile cache */
    long bfile_end_pos;		/* ftell at end of bfile */
    gx_band_params_t band_params;  /* parameters used when writing band list */
				/* (actual values, no 0s) */
    int scan_lines_per_colors_used; /* number of scan lines per colors_used */
				/* entry (a multiple of the band height) */
    gx_colors_used_t band_colors_used[PAGE_INFO_NUM_COLORS_USED];  /* colors used on the page */
} gx_band_page_info_t;
#define PAGE_INFO_NULL_VALUES\
  { 0 }, 0, { 0 }, 0, 0, 0, { BAND_PARAMS_INITIAL_VALUES },\
  0x3fffffff, { { 0 } }

/*
 * By convention, the structure member containing the above is called
 * page_info.  Define shorthand accessors for its members.
 */
#define page_cfile page_info.cfile
#define page_cfname page_info.cfname
#define page_bfile page_info.bfile
#define page_bfname page_info.bfname
#define page_tile_cache_size page_info.tile_cache_size
#define page_bfile_end_pos page_info.bfile_end_pos
#define page_band_height page_info.band_params.BandHeight

#endif /* ndef gxband_INCLUDED */
