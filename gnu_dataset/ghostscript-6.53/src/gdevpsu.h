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

/*$RCSfile: gdevpsu.h,v $ $Revision: 1.2.2.2 $ */
/* Interface to PostScript-writing utilities */

#ifndef gdevpsu_INCLUDED
#  define gdevpsu_INCLUDED

/* Define parameters and state for PostScript-writing drivers. */
typedef struct gx_device_pswrite_common_s {
    float LanguageLevel;
    bool ProduceEPS;
    int ProcSet_version;
    long bbox_position;		/* set when writing file header */
} gx_device_pswrite_common_t;
#define PSWRITE_COMMON_PROCSET_VERSION 1000 /* for definitions in gdevpsu.c */
#define PSWRITE_COMMON_VALUES(ll, eps, psv)\
  {ll, eps, PSWRITE_COMMON_PROCSET_VERSION + (psv)}

/* ---------------- Low level ---------------- */

/* Write a 0-terminated array of strings as lines. */
void psw_print_lines(P2(FILE *f, const char *const lines[]));

/* ---------------- File level ---------------- */

/*
 * Write the file header, up through the BeginProlog.  This must write to a
 * file, not a stream, because it may be called during finalization.
 */
void psw_begin_file_header(P5(FILE *f, const gx_device *dev,
			      const gs_rect *pbbox,
			      gx_device_pswrite_common_t *pdpc, bool ascii));

/* End the file header.*/
void psw_end_file_header(P1(FILE *f));

/* End the file. */
void psw_end_file(P5(FILE *f, const gx_device *dev,
                     const gx_device_pswrite_common_t *pdpc,
                     const gs_rect *pbbox, int page_count));

/* ---------------- Page level ---------------- */

/*
 * Write the page header.
 */
void psw_write_page_header(P5(stream *s, const gx_device *dev,
                              const gx_device_pswrite_common_t *pdpc,
                              bool do_scale, long page_ord));
/*
 * Write the page trailer.  We do this directly to the file, rather than to
 * the stream, because we may have to do it during finalization.
 */
void psw_write_page_trailer(P3(FILE *f, int num_copies, int flush));

#endif /* gdevpsu_INCLUDED */

